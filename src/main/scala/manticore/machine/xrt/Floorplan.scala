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

  def getProcessorCellName(x: Int, y: Int): String = {
    // We explicitly do not consider the registers in the ProcessorWithSendPipe as part of the "core" since we
    // want vivado to have freedom to place them where it wants. Hence why we see "/processor" and don't just stop
    // at "/core_x_y".
    s"${getManticoreKernelInstName()}/manticore/compute_array/core_${x}_${y}/processor"
  }

  // The auxiliary circuitry that should be placed near the core at position x<x>y<y>.
  def getCoreAuxiliaryCellNames(x: Int, y: Int): Seq[String] = {
    if (x == 0 && y == 0) {
      Seq(
        s"${getManticoreKernelInstName()}/axi_cache",
        s"${getManticoreKernelInstName()}/clock_distribution",
        s"${getManticoreKernelInstName()}/m_axi_bank_0_clock_crossing",
        s"${getManticoreKernelInstName()}/s_axi_clock_crossing",
        s"${getManticoreKernelInstName()}/slave",
        s"${getManticoreKernelInstName()}/manticore/bootloader",
        s"${getManticoreKernelInstName()}/manticore/controller",
        s"${getManticoreKernelInstName()}/manticore/memory_intercept"
      )
    } else {
      Seq()
    }
  }

  def getSwitchCellName(x: Int, y: Int): String = {
    s"${getManticoreKernelInstName()}/manticore/compute_array/switch_${x}_${y}"
  }

  // Mapping from an abstract position on a 2D grid to a Core in a torus network.
  // This generates the torus network coordinates at the appropriate place in
  // a grid so the folded nature of the torus network is visible, yet retains the
  // "plain" 2D grid coordinates so we know where we are on the plane.
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
            val coreCell = getProcessorCellName(core.x, core.y)
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
            val switchCell = getSwitchCellName(switch.x, switch.y)
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

  def getPrivilegedAreaConstraints(): String = {
    val rootClockRegion = getRootClock()

    val privilegedClockRegions = getPrivilegedArea()

    case class ClockDistributionPblock(crs: Set[ClockRegion]) extends Pblock {
      override val name: String = "ManticoreClkDistribution"

      override val resources: String = {
        val crsStr = crs.toSeq.sortBy(cr => (cr.y, cr.x)).map(cr => s"CLOCKREGION_X${cr.x}Y${cr.y}").mkString(" ")
        s"{ ${crsStr} }"
      }
    }

    val clkDistributionConstraints = ClockDistributionPblock(privilegedClockRegions).toTcl(
      getCoreAuxiliaryCellNames(0, 0) :+ getProcessorCellName(0, 0)
    )

    val clockWizardClockOutNetName = s"${getManticoreKernelInstName()}/clock_distribution/wiz/inst/clk_out1"
    val controlClockNetName = s"${getManticoreKernelInstName()}/clock_distribution/clock_distribution_control_clock"
    val computeClockNetName = s"${getManticoreKernelInstName()}/clock_distribution/clock_distribution_compute_clock"

    val netsStr = Seq(
      clockWizardClockOutNetName,
      controlClockNetName,
      computeClockNetName
    ).map(net => s"\t\t${net} \\").mkString("\n")

    val clkRootConstraints =
      s"""|
          |# set_property CLOCK_DELAY_GROUP ManticoreClk [get_nets [list \\
          |# ${netsStr}
          |# ]]
          |set_property USER_CLOCK_ROOT X${rootClockRegion.x}Y${rootClockRegion.y} [get_nets [list \\
          |${netsStr}
          |]]
          |""".stripMargin

    Seq(
      clkDistributionConstraints,
      clkRootConstraints
    ).mkString("\n")
  }

  def getSlrCrossingConstraints(): String = {
    s"set_property USER_SLL_REG TRUE [get_cells -hierarchical -regexp .*${Helpers.slrCrossingSuffix}.*]"
  }

  def toTcl(dimX: Int, dimY: Int): String = {
    Seq(
      getPblockConstrains(dimX, dimY),
      getPrivilegedAreaConstraints(),
      getSlrCrossingConstraints()
    ).mkString("\n")
  }

  // Must be defined by subclasses which floorplan specific devices.
  def getName(): String
  def getRootClock(): ClockRegion
  def getPrivilegedArea(): Set[ClockRegion]
  def getManticoreKernelInstName(): String
  def getCoreToPblockMap(dimX: Int, dimY: Int): Map[TorusLoc, Pblock]
  def getSwitchToPblockMap(dimX: Int, dimY: Int): Map[TorusLoc, Pblock]
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
