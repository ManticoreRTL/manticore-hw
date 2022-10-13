package manticore.machine.xrt

import collection.mutable.{Map => MMap}
import collection.mutable.ArrayBuffer
import manticore.machine.Helpers

object Coordinates {
  case class GridLoc(c: Int, r: Int)
  case class TorusLoc(x: Int, y: Int)
  case class ClockRegion(x: Int, y: Int)
}

trait Floorplan {
  import Coordinates._

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////// NAMES ////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def computeArrayCellName: String = {
    s"${getManticoreKernelInstName()}/manticore/compute_array"
  }

  def wrappedCoreCellName(x: Int, y: Int): String = {
    s"${computeArrayCellName}/core_${x}_${y}"
  }

  def procCellName(x: Int, y: Int): String = {
    // We explicitly do not consider the registers in the ProcessorWithSendPipe as part of the "core" since we
    // want vivado to have freedom to place them where it wants. Hence why we see "/processor" and don't just stop
    // at "/core_x_y".
    s"${wrappedCoreCellName(x, y)}/processor"
  }

  // The auxiliary circuitry that should be placed near the core at position x<x>y<y>.
  def procAuxiliaryCellNames(x: Int, y: Int): Seq[String] = {
    if (x == 0 && y == 0) {
      Seq(
        // These are the cells that should be co-located with the privileged core.
        // s"${getManticoreKernelInstName()}/axi_cache/front_pipe",
        // s"${getManticoreKernelInstName()}/axi_cache/cache",
        // s"${getManticoreKernelInstName()}/m_axi_bank_0_clock_crossing",
        // s"${getManticoreKernelInstName()}/s_axi_clock_crossing",
        // s"${getManticoreKernelInstName()}/slave",
        // s"${getManticoreKernelInstName()}/manticore/bootloader",
        // s"${getManticoreKernelInstName()}/manticore/memory_intercept"
      )
    } else {
      Seq()
    }
  }

  def controllerCellName(): String = {
    s"${getManticoreKernelInstName()}/manticore/controller"
  }

  def clockDistributionCellName(): String = {
    s"${getManticoreKernelInstName()}/clock_distribution"
  }

  def cacheCellName(): String = {
    s"${getManticoreKernelInstName()}/axi_cache/cache"
  }

  // Map[bank_idx, bank_name]
  def cacheBankCellNames(): Map[Int, String] = {
    Range
      .inclusive(0, 3)
      .map { i =>
        i -> s"${cacheCellName()}/bank_${i}/impl/uram_inst/xpm_memory_base_inst/gen_wr_a.gen_word_narrow.mem_reg_uram_0"
      }
      .toMap
  }

  def registerFileCellName(x: Int, y: Int): String = {
    s"${procCellName(x, y)}/register_file"
  }

  def registerFileBankCellNames(x: Int, y: Int): Map[Int, String] = {
    Range
      .inclusive(1, 4)
      .map { i =>
        i -> s"${registerFileCellName(x, y)}/rs${i}bank/impl/bram_inst/xpm_memory_base_inst/gen_wr_a.gen_word_narrow.mem_reg_bram_0"
      }
      .toMap
  }

  def switchCellName(x: Int, y: Int): String = {
    s"${computeArrayCellName}/switch_${x}_${y}"
  }

  def procSendRecvPipeCellName(x: Int, y: Int): String = {
    s"${wrappedCoreCellName(x, y)}/sendRecvPipe"
  }

  object ProcessorToSwitch {
    def cellName(x: Int, y: Int): String                      = s"${procSendRecvPipeCellName(x, y)}/procToSwitchPipe"
    def procSideCellName(x: Int, y: Int): String              = s"${cellName(x, y)}/procSide_pipe"
    def slrCrossingProcSideCellName(x: Int, y: Int): String   = s"${cellName(x, y)}/slrCrossingProcSide_pipe"
    def slrCrossingSwitchSideCellName(x: Int, y: Int): String = s"${cellName(x, y)}/slrCrossingSwitchSide_pipe"
    def switchSideCellName(x: Int, y: Int): String            = s"${cellName(x, y)}/switchSide_pipe"
  }

  object SwitchToProcessor {
    def cellName(x: Int, y: Int): String                      = s"${procSendRecvPipeCellName(x, y)}/switchToProcPipe"
    def switchSideCellName(x: Int, y: Int): String            = s"${cellName(x, y)}/switchSide_pipe"
    def slrCrossingSwitchSideCellName(x: Int, y: Int): String = s"${cellName(x, y)}/slrCrossingSwitchSide_pipe"
    def slrCrossingProcSideCellName(x: Int, y: Int): String   = s"${cellName(x, y)}/slrCrossingProcSide_pipe"
    def procSideCellName(x: Int, y: Int): String              = s"${cellName(x, y)}/procSide_pipe"
  }

  object CoreResetTree {
    def cellName: String = s"${computeArrayCellName}/core_reset_tree"

    def topCellName: String                          = s"${cellName}/top_reset_tree"
    def topControllerSideCellName: String            = s"${topCellName}/controllerSide_pipe"
    def topControllerSideSlrCrossingCellName: String = s"${topCellName}/controllerSideSlrCrossing_pipe"
    def topCoreSideSlrCrossingCellName: String       = s"${topCellName}/coreSideSlrCrossing_pipe"
    def topCoreSideTreeCellName: String              = s"${topCellName}/reset_tree"

    def bottomCellName: String                          = s"${cellName}/bottom_reset_tree"
    def bottomControllerSideCellName: String            = s"${bottomCellName}/controllerSide_pipe"
    def bottomControllerSideSlrCrossingCellName: String = s"${bottomCellName}/controllerSideSlrCrossing_pipe"
    def bottomCoreSideSlrCrossingCellName: String       = s"${bottomCellName}/coreSideSlrCrossing_pipe"
    def bottomCoreSideTreeCellName: String              = s"${bottomCellName}/reset_tree"

    def privilegedCellName: String = s"${cellName}/privileged_reset_pipe"
  }

  object SwitchResetTree {
    def cellName: String = s"${computeArrayCellName}/switch_reset_tree"
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////// Geometry //////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // Mapping from an abstract position on a 2D grid to a Core in a torus network.
  // This generates the torus network coordinates at the appropriate place in
  // a grid so the folded nature of the torus network is visible, yet retains the
  // "plain" 2D grid coordinates so we know where we are on the plane.
  //
  // Note that GridLoc(r = 0, c = 0) is the bottom-left corner of a rectangle for us.
  def getGridLocToTorusLocMap(
      dimX: Int,
      dimY: Int,
      // Position within the grid to which torus location x0y0 should be aligned.
      anchor: GridLoc
  ): Map[GridLoc, TorusLoc] = {
    def getPlainMapping(): Map[GridLoc, TorusLoc] = {
      def getGridAxisToTorusAxisMap(dim: Int): Map[Int, Int] = {
        // Algorithm (c-to-x map, but same for r-to-y):
        // - i = 0
        // - c[i] -> 0
        // - i++
        // - c[i] -> 1
        // - i++
        // - dec_from_max = 0
        // - inc_from_min = 0
        // - While (c[i] exists)
        //   - if i even:
        //     - c[i] -> dimX - 1 - dec_from_max
        //     - dec_from_max++
        //   - else i odd:
        //     - c[i] -> 2 + inc_from_min
        //     - inc_from_min++

        val mapping = MMap.empty[Int, Int]

        var i            = 0
        var dec_from_max = 0
        var inc_from_min = 0

        mapping += i -> 0
        i += 1
        mapping += i -> 1
        i += 1

        while (i < dim) {
          val isEven = (i % 2) == 0
          if (isEven) {
            mapping += i -> (dim - 1 - dec_from_max)
            dec_from_max += 1
          } else {
            mapping += i -> (2 + inc_from_min)
            inc_from_min += 1
          }
          i += 1
        }

        mapping.toMap
      }

      val cToX = getGridAxisToTorusAxisMap(dimX)
      val rToY = getGridAxisToTorusAxisMap(dimY)

      val gridToTorus = MMap.empty[GridLoc, TorusLoc]
      cToX.foreach { case (c, x) =>
        rToY.foreach { case (r, y) =>
          gridToTorus += GridLoc(c, r) -> TorusLoc(x, y)
        }
      }

      gridToTorus.toMap
    }

    def getAnchoredMapping(
        plainGridToTorus: Map[GridLoc, TorusLoc]
    ): Map[GridLoc, TorusLoc] = {
      // Modulus that ensures result is positive.
      def modPos(a: Int, m: Int): Int = {
        val res = a % m
        if (res < 0) res + m else res
      }

      val x0y0 = TorusLoc(0, 0)

      var rotatedTorusToGrid = plainGridToTorus.map(_.swap)

      // Rotate torus X values until the gridLoc X value matches that of the anchor.
      while (rotatedTorusToGrid(x0y0).c != anchor.c) {
        rotatedTorusToGrid = rotatedTorusToGrid.map { case (TorusLoc(x, y), gridLoc) =>
          TorusLoc(modPos(x + 1, dimX), y) -> gridLoc
        }
      }

      // Rotate torus Y values until the gridLoc Y value matches that of the anchor.
      while (rotatedTorusToGrid(x0y0).r != anchor.r) {
        rotatedTorusToGrid = rotatedTorusToGrid.map { case (TorusLoc(x, y), gridLoc) =>
          TorusLoc(x, modPos(y + 1, dimY)) -> gridLoc
        }
      }

      val rotatedGridToTorus = rotatedTorusToGrid.toMap.map(_.swap)
      rotatedGridToTorus
    }

    val plainMapping    = getPlainMapping()
    val anchoredMapping = getAnchoredMapping(plainMapping)
    anchoredMapping
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////// Constraints /////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def getPblockConstrains(dimX: Int, dimY: Int): String = {
    val pblockCells = MMap.empty[Pblock, Seq[String]].withDefaultValue(Seq.empty)

    val coreToPblock   = getCoreToPblockMap(dimX, dimY)
    val switchToPblock = getSwitchToPblockMap(dimX, dimY)

    coreToPblock
      .groupMap(_._2)(_._1)
      .toSeq
      .sortBy { case (pblock, cores) =>
        pblock.name
      }
      .foreach { case (pblock, cores) =>
        val cells = cores.toSeq
          .sortBy(core => (core.y, core.x))
          .map { core =>
            val coreCell = procCellName(core.x, core.y)
            coreCell
          }

        pblockCells(pblock) ++= cells
      }

    switchToPblock
      .groupMap(_._2)(_._1)
      .toSeq
      .sortBy { case (pblock, cores) =>
        pblock.name
      }
      .foreach { case (pblock, switch) =>
        val cells = switch.toSeq
          .sortBy(core => (core.y, core.x))
          .map { switch =>
            val switchCell = switchCellName(switch.x, switch.y)
            switchCell
          }

        pblockCells(pblock) ++= cells
      }

    val constraints = ArrayBuffer.empty[String]

    // TODO (skashani): Find a way to do natural sorting here as otherwise the pblocks are hard to read.
    pblockCells.toSeq
      .sortBy { case (pblock, cells) => pblock.name }
      .foreach { case (pblock, cells) =>
        constraints += pblock.toTcl(cells)
      }

    constraints.mkString("\n")
  }

  def getClockConstraints(): String = {

    case class ClockDistributionPblock(cr: ClockRegion) extends Pblock {
      override val name: String = "pblock_manticore_clock_distribution"

      override val resources: String = s"{ CLOCKREGION_X${cr.x}Y${cr.y} }"
    }

    val rootClockRegion = getRootClock()

    val pblock = ClockDistributionPblock(rootClockRegion).toTcl(
      Seq(clockDistributionCellName(), controllerCellName())
    )

    // No need to set wiz/inst/clk_out1 as a root clock as it is not driven by a global clock buffer (the output of the
    // MMCM is marked as a local clock in vivado).
    // If you add the constraint, you will get the following warning at implementation time.
    //
    //   WARNING: [Vivado 12-4417] Setting USER_CLOCK_ROOT on net 'level0_i/ulp/ManticoreKernel_1/inst/clock_distribution/wiz/inst/clk_out1' which is not driven by a global clock buffer. The resulting constraint will not have the desired effect.
    //
    // val clockWizardClockOutNetName = s"${clockDistributionCellName()}/wiz/inst/clk_out1"

    // Vivado renamed this wire from clock_distribution/clock_distribution_control_clock"!
    val controlClockNetName = s"${clockDistributionCellName()}/CLK"
    val computeClockNetName = s"${clockDistributionCellName()}/clock_distribution_compute_clock"

    val netsStr = Seq(
      // clockWizardClockOutNetName,
      controlClockNetName,
      computeClockNetName
    ).map(net => s"\t\t${net} \\").mkString("\n")

    val clkRootConstraints =
      s"""|
          |set_property CLOCK_DELAY_GROUP ManticoreClk [get_nets [list \\
          |${netsStr}
          |]]
          |set_property USER_CLOCK_ROOT X${rootClockRegion.x}Y${rootClockRegion.y} [get_nets [list \\
          |${netsStr}
          |]]
          |""".stripMargin

    Seq(
      pblock,
      clkRootConstraints
    ).mkString("\n")
  }

  def getPrivilegedAreaConstraints(): String = {
    val privilegedClockRegions = getPrivilegedArea()

    case class PrivilegedAreaPblock(crs: Set[ClockRegion]) extends Pblock {
      override val name: String = "pblock_privileged_area"

      override val resources: String = {
        val str = crs.toSeq.sortBy(cr => (cr.y, cr.x)).map(cr => s"CLOCKREGION_X${cr.x}Y${cr.y}").mkString(" ")
        s"{ ${str} }"
      }
    }

    val constraints = PrivilegedAreaPblock(privilegedClockRegions).toTcl(
      Seq(procCellName(0, 0)) ++ Seq(switchCellName(0, 0)) ++ procAuxiliaryCellNames(0, 0)
    )

    constraints
  }

  def toTcl(dimX: Int, dimY: Int): String = {
    Seq(
      getPblockConstrains(dimX, dimY),
      getPrivilegedAreaConstraints(),
      getClockConstraints(),
      getCustomConstraints(dimX, dimY).getOrElse("")
    ).mkString("\n")
  }

  // Must be defined by subclasses which floorplan specific devices.
  def getName(): String
  def getRootClock(): ClockRegion
  def getPrivilegedArea(): Set[ClockRegion]
  def getManticoreKernelInstName(): String
  def getCoreToPblockMap(dimX: Int, dimY: Int): Map[TorusLoc, Pblock]
  def getSwitchToPblockMap(dimX: Int, dimY: Int): Map[TorusLoc, Pblock]
  def getCustomConstraints(dimX: Int, dimY: Int): Option[String] = None
}

trait Pblock {
  val name: String
  val resources: String

  def toTcl(
      cells: Seq[String]
  ): String = {
    val cellsStr = cells.map(cell => s"\t\t${cell} \\").mkString("\n")
    s"""|
        |create_pblock ${name}
        |resize_pblock ${name} -add ${resources}
        |add_cells_to_pblock ${name} [get_cells [list \\
        |${cellsStr}
        |]]
        |""".stripMargin
  }
}
