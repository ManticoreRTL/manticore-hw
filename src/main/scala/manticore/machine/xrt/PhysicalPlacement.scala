package manticore.machine.xrt

import com.google.ortools.Loader
import com.google.ortools.sat.CpModel
import com.google.ortools.sat.CpSolver
import com.google.ortools.sat.CpSolverStatus
import com.google.ortools.sat.LinearExpr
import com.google.ortools.sat.BoolVar
import com.google.ortools.sat.LinearArgument
import com.google.ortools.constraintsolver.IntVar

import collection.mutable.{Map => MMap}

class PhysicalPlacement(dimX: Int, dimY: Int, anchor: (Int, Int), maxCores: Int = 5) {

  def pblockConstraint: String = {

    val regions = (for (cx <- 0 until dimX; cy <- 0 until dimY) yield { (cx, cy) -> getCoreRegion(cx, cy) })
    // regions.groupBy(_._2).
    val pblocks = regions.groupMap(_._2)(_._1).toSeq

    pblocks.zipWithIndex
      .map { case ((clockRegions, cores), pix) =>
        val pbName           = s"mantiblock_${pix}"
        val containedRegions = clockRegions.sortBy(cr => (cr.x, cr.y)).map(_.toString).mkString(":")
        val cells = cores
          .map { case (x, y) =>
            val others = if (x == 0 && y == 0) {
              s"\t\tlevel0_i/ulp/ManticoreKernel_1/inst/manticore/controller \\\n" +
                s"\t\tlevel0_i/ulp/ManticoreKernel_1/inst/clock_distribution   \\\n"

            } else {
              ""
            }

            others + s"\t\tlevel0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_${x}_${y} \\"
          }
          .mkString("\n")
        s"""|
          |create_pblock $pbName
          |resize_pblock $pbName -add ${containedRegions}
          |add_cells_to_pblock $pbName [ get_cell [ list \\
          |$cells
          |]]
          |
          |""".stripMargin
      }
      .mkString("\n")

  }

  case class ClockRegion(x: Int, y: Int) {
    override def toString: String = s"CLOCKREGION_X${x}Y${y}"
  }
  type PBlock = Seq[ClockRegion]

  def getCoreRegion(x: Int, y: Int): Seq[ClockRegion] = {
    solution(x)(y)
  }
  private lazy val solution: IndexedSeq[IndexedSeq[Seq[ClockRegion]]] = {
    Loader.loadNativeLibraries()
    val model  = new CpModel()
    val uRange = Range(0, 4)
    val vRange = Range(0, 15)
    val xRange = Range(0, dimX)
    val yRange = Range(0, dimY)
    val shellZone =
      uRange.map { u => vRange.map { v => (u, v) } }.flatten.filter { case (u, v) => u >= 2 && (v >= 5 && v <= 9) }
    // location variables, loc(x)(y)(u)(v) == true implies that core xy is placed into clock regions uv
    val loc = xRange.map { x =>
      yRange.map { y =>
        uRange.map { u =>
          vRange.map { v =>
            model.newIntVar(0, 1, s"loc_${x}_${y}_${u}_${v}")
          }
        }
      }
    }

    // each core  can be placed in a single location
    for (x <- xRange; y <- yRange) {

      val coreBindingCount = LinearExpr.sum(loc(x)(y).flatten.map(_.build()).toArray)
      model.addEquality(coreBindingCount, model.newConstant(1))
      // val sumExpr = new LinearExpr()
      // model.add

    }

    // at most 5 cores can be in 1 clock region
    val utilization = for (u <- uRange; v <- vRange) yield {
      val l     = for (x <- xRange; y <- yRange) yield { loc(x)(y)(u)(v).build() }
      val util  = LinearExpr.sum(l.toArray)
      val utilV = model.newIntVar(0, 1000, s"util_${u}_${v}")
      model.addLessOrEqual(util, maxCores)
      model.addEquality(utilV, util)
      utilV
    }

    // define position variable for each core that points to its location on the
    // FPGA (i.e., clock region)
    val pos = xRange.map { x =>
      yRange.map { y =>
        // xRange.
        val pu = model.newIntVar(uRange.start, uRange.last + 1, s"pu_${x}_${y}")
        val pv = model.newIntVar(vRange.start, vRange.last + 1, s"pv_${x}_${y}")
        val puSum = LinearExpr.weightedSum(
          loc(x)(y).flatten.map(_.build()).toArray,
          (for (u <- uRange; v <- vRange) yield { u.toLong }).toArray
        )
        val pvSum = LinearExpr.weightedSum(
          loc(x)(y).flatten.map(_.build()).toArray,
          (for (u <- uRange; v <- vRange) yield { v.toLong }).toArray
        )
        model.addEquality(pu, puSum)
        model.addEquality(pv, pvSum)
        (pu, pv)
      }
    }

    def neighbors(x: Int, y: Int): Seq[(Int, Int)] = {

      def wrap(v: Int, dim: Int): Seq[Int] = {
        if (v == 0) {
          Seq(dim - 1, v + 1)
        } else if (v == dim - 1) {
          Seq(v - 1, 0)
        } else {
          Seq(v - 1, v + 1)
        }
      }
      wrap(x, dimX).map((_, y)) ++ wrap(y, dimY).map((x, _))
    }

    for (x <- xRange; y <- yRange) {
      // the distance from each core to its neighbor should be at most
      // 1 clock region in each direction
      // println(s"$x,$y neighbors are ${neighbors(x, y)}")
      for ((nx, ny) <- neighbors(x, y)) {
        val diffU = LinearExpr.weightedSum(Array(pos(x)(y)._1, pos(nx)(ny)._1), Array(1, -1))
        val diffV = LinearExpr.weightedSum(Array(pos(x)(y)._2, pos(nx)(ny)._2), Array(1, -1))

        model.addLessOrEqual(diffU, 1)
        model.addGreaterOrEqual(diffU, -1)

        model.addLessOrEqual(diffV, 1)
        model.addGreaterOrEqual(diffV, -1)

      }
    }

    // disallow placement in the shell

    for ((u, v) <- shellZone) {
      for (x <- xRange; y <- yRange) {
        model.addEquality(loc(x)(y)(u)(v), 0)
      }
    }

    // anchor the first core
    model.addEquality(loc(0)(0)(anchor._1 - 1)(anchor._2), 1)
    println(s"Solving placement for ${dimX}x${dimY} anchored at ${anchor}")
    val solver = new CpSolver()
    val status = solver.solve(model)

    println(s"Solution: ${status}")

    // for (y <- yRange) {
    //   for (x <- xRange) {
    //     val row = loc(x)(y).flatten.map { solver.value }.mkString(" ")
    //     println(row)
    //   }
    // }
    // for (x <- xRange; y <- yRange) {
    //   println(s"core ${x}, ${y} is at ${solver.value(pos(x)(y)._1) + 1},${solver.value(pos(x)(y)._2)}")
    //   // for (u <- uRange; v <- vRange) {
    //   //     if (solver.value(loc(x)(y)(u)(v)) == 1) {
    //   //         println(s"In other words at ${u}, ${v}")
    //   //     }
    //   // }
    // }
    xRange.map { x =>
      yRange.map { y =>
        val u: Int = solver.value(pos(x)(y)._1).toInt
        val v: Int = solver.value(pos(x)(y)._2).toInt

        if (u == uRange.start || u == uRange.last) {
          /// the first and last columns do not have URAMs, but have BRAMs
          /// so we include them in the clock region for relaxing routing
          Seq(
            ClockRegion(u, v),
            ClockRegion(u + 1, v)
          )
        } else {
          // u + 1 because the first column of the FPGA does not have
          // URAMs and the offset we used in the solver starts from the
          // second column
          Seq(ClockRegion(u + 1, v))
        }

      }
    }
  }

}

class IterativePlacement(
    dimX: Int,
    dimY: Int,
    anchor: IterativePlacement.Pblock
) {
  import IterativePlacement._

  // SLR2 and SLR0 can technically have 4 pblocks per row such that each pblock has roughly the
  // same number of URAM columns (1) and BRAM columns (3-4).
  //
  // However, half of SLR1 is occupied by the shell and this takes up two of the URAM columns.
  // This leaves us with 2 pblocks per row in SLR1 such that each pblock has roughly the same
  // number of URAM columns (1) and BRAM columns (3-4).
  //
  // To simplify floorplanning, I will assume SLR2 and SLR0 also have 2 pblocks per row such that
  // the device looks like a grid. I just need to remember that the "2" pblocks per row in SLR2 and
  // SLR0 have double the capacity of those in SLR1.
  //
  // Resource-wise, we have the following constraints:
  //
  // - 12 36Kb BRAMs per column => 4 36Kb BRAMs per core => max 3 cores per BRAM column.
  // - 16 32KB URAMs per column => 2 32KB URAMs per core => max 8 cores per URAM column.
  // - SLR1 has 7 BRAM columns and 2 URAM columns => max(7*3, 2*8) = 16 cores per row.
  // - SLR2/SLR0 have 12 BRAM columns and 4 URAM columns => max(12*3, 4*8) = 32 cores per row.
  //
  // Let's be realistic and say max 10-12 cores per row in SLR1 and 20-24 cores per row in SLR2/SLR0.
  //
  // The placement algorithm for GridLocs (which are torus nodes) to pblocks is therefore as follows:
  // - Place 2 rows of the grid per clock region row in SLR2.
  // - Place 1 row  of the grid per clock region row in SLR1.
  // - Place 2 rows of the grid per clock region row in SLR0.
  //
  // This allows for a maximum dimY value of 25:
  // - 2 gridLoc rows * 5 clockRegion rows = 10 gridLoc rows in SLR2.
  // - 1 gridLoc row  * 5 clockRegion rows =  5 gridLoc rows in SLR1.
  // - 2 gridLoc rows * 5 clockRegion rows = 10 gridLoc rows in SLR0.

  def solution: Map[Core, Pblock] = {

    def getDefaultSolution: Map[Core, Pblock] = {
      def inSlr1(clockRegionRow: Int): Boolean = (5 <= clockRegionRow) && (clockRegionRow <= 9)

      val gridLocToCore = getGridLocToCoreMap(dimX, dimY)

      // // Debug
      // for (r <- Range.inclusive(0, dimY - 1)) {
      //   for (c <- Range.inclusive(0, dimX - 1)) {
      //     val gridLoc = GridLoc(c, r)
      //     val core    = gridLocToCore(gridLoc)
      //     print(s"${gridLoc}->${core}    ")
      //   }
      //   println()
      // }

      val gridLocRows = gridLocToCore
        .groupMap(_._1.r)(_._1)
        .map { case (gridLoc, group) =>
          gridLoc -> group.toSeq.sortBy(_.c)
        }

      assert(dimX <= 12)
      assert(dimY <= 25)

      val res = MMap.empty[Core, Pblock]

      var gridLocY       = 0
      var clockRegionRow = 0
      while (gridLocY < dimY) {
        var rowsTaken = 0
        val rowBound  = if (inSlr1(clockRegionRow)) 1 else 2

        while (rowsTaken < rowBound) {
          gridLocRows(gridLocY).foreach { gridLoc =>
            // Assign locs in row to either the left or right pblock.
            val side = if (gridLoc.c < dimX / 2) Left else Right
            val core = gridLocToCore(gridLoc)
            res += core -> Pblock(clockRegionRow, side)
          }
          rowsTaken += 1
          gridLocY += 1
        }

        clockRegionRow += 1
      }

      res.toMap
    }

    def rotateSolution(
        coreToPblock: Map[Core, Pblock],
        anchor: Pblock
    ): Map[Core, Pblock] = {
      // Modulus that ensures result is positive.
      def modPos(a: Int, m: Int): Int = {
        val res = a % m
        if (res < 0) res + m else res
      }

      val x0y0 = Core(0, 0)

      var rotatedCoreToPblock = coreToPblock

      // Rotate core X values until we are on the same side as the anchor.
      while (rotatedCoreToPblock(x0y0).side != anchor.side) {
        rotatedCoreToPblock = rotatedCoreToPblock.map { case (Core(x, y), pblock) =>
          Core(modPos(x + 1, dimX), y) -> pblock
        }
      }

      // Rotate core Y values until we are in the same clock region row as the anchor.
      while (rotatedCoreToPblock(x0y0).clockRegionRow != anchor.clockRegionRow) {
        rotatedCoreToPblock = rotatedCoreToPblock.map { case (Core(x, y), pblock) =>
          Core(x, modPos(y + 1, dimY)) -> pblock
        }
      }

      rotatedCoreToPblock
    }

    val defaultSolution = getDefaultSolution

    // The solution found above assumes core x0y0 is on the bottom-left of the chip in Pblock(0, Left).
    // However, we have a desired anchor point for core x0y0 and need to respect it for the clock circuitry
    // to be in the center of the chip.
    // We therefore rotate the cores until x0y0 ends up in the desired Pblock. This is possible due to the
    // torus design.
    val anchoredSolution = rotateSolution(defaultSolution, anchor)

    anchoredSolution
  }

  def pblockConstraint: String = {
    val coreToPblock = solution

    coreToPblock
      .groupMap(_._2)(_._1)
      .toSeq
      .sortBy { case (pblock, cores) =>
        (pblock.clockRegionRow, pblock.side.toString())
      }
      .map { case (pblock, cores) =>
        val cells = cores.toSeq
          .sortBy { case Core(x, y) =>
            (y, x)
          }
          .map { core =>
            val others = if (core.x == 0 && core.y == 0) {
              s"\t\tlevel0_i/ulp/ManticoreKernel_1/inst/manticore/controller \\\n" +
                s"\t\tlevel0_i/ulp/ManticoreKernel_1/inst/clock_distribution   \\\n"
            } else {
              ""
            }
            others + s"\t\tlevel0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_${core.x}_${core.y} \\"
          }
          .mkString("\n")

        val pbName      = s"pblock_Y${pblock.clockRegionRow}_${pblock.side.toString()}"
        val pbResources = pblockDeviceResources(pblock)

        s"""|
            |create_pblock ${pbName}
            |resize_pblock ${pbName} -add ${pbResources}
            |add_cells_to_pblock ${pbName} [ get_cell [ list \\
            |${cells}
            |]]
            |
            |""".stripMargin
      }
      .mkString("\n")
  }
}

object IterativePlacement {
  case class GridLoc(c: Int, r: Int)
  case class Core(x: Int, y: Int)

  sealed abstract class Side
  object Left extends Side {
    override def toString(): String = "Left"
  }
  object Right extends Side {
    override def toString(): String = "Right"
  }
  case class Pblock(clockRegionRow: Int, side: Side)

  // format: off
  val pblockDeviceResources = Map(
    Pblock(0, Left)   -> "{ BUFCE_LEAF_X0Y0:BUFCE_LEAF_X431Y3 BUFCE_ROW_X0Y0:BUFCE_ROW_X0Y23 BUFCE_ROW_FSR_X0Y0:BUFCE_ROW_FSR_X95Y0 BUFGCE_X0Y0:BUFGCE_X0Y23 BUFGCE_DIV_X0Y0:BUFGCE_DIV_X0Y3 BUFGCTRL_X0Y0:BUFGCTRL_X0Y7 BUFG_GT_X0Y0:BUFG_GT_X0Y23 BUFG_GT_SYNC_X0Y0:BUFG_GT_SYNC_X0Y14 DSP48E2_X0Y0:DSP48E2_X9Y23 MMCM_X0Y0:MMCM_X0Y0 PLL_X0Y0:PLL_X0Y1 PLL_SELECT_SITE_X0Y0:PLL_SELECT_SITE_X0Y7 RAMB18_X0Y0:RAMB18_X5Y23 RAMB36_X0Y0:RAMB36_X5Y11 SLICE_X0Y0:SLICE_X83Y59 URAM288_X0Y0:URAM288_X1Y15 }",
    Pblock(0, Right)  -> "{ BUFCE_LEAF_X432Y0:BUFCE_LEAF_X863Y3 BUFCE_ROW_X1Y0:BUFCE_ROW_X1Y23 BUFCE_ROW_FSR_X96Y0:BUFCE_ROW_FSR_X188Y0 BUFGCE_X1Y0:BUFGCE_X1Y23 BUFGCE_DIV_X1Y0:BUFGCE_DIV_X1Y3 BUFGCTRL_X1Y0:BUFGCTRL_X1Y7 BUFG_GT_X1Y0:BUFG_GT_X1Y23 BUFG_GT_SYNC_X1Y0:BUFG_GT_SYNC_X1Y14 DSP48E2_X10Y0:DSP48E2_X18Y23 MMCM_X1Y0:MMCM_X1Y0 PLL_X1Y0:PLL_X1Y1 PLL_SELECT_SITE_X1Y0:PLL_SELECT_SITE_X1Y7 RAMB18_X6Y0:RAMB18_X11Y23 RAMB36_X6Y0:RAMB36_X11Y11 SLICE_X84Y0:SLICE_X168Y59 URAM288_X2Y0:URAM288_X3Y15 }",
    Pblock(1, Left)   -> "{ BUFCE_LEAF_X0Y4:BUFCE_LEAF_X431Y7 BUFCE_ROW_X0Y24:BUFCE_ROW_X0Y47 BUFCE_ROW_FSR_X0Y1:BUFCE_ROW_FSR_X95Y1 BUFGCE_X0Y24:BUFGCE_X0Y47 BUFGCE_DIV_X0Y4:BUFGCE_DIV_X0Y7 BUFGCTRL_X0Y8:BUFGCTRL_X0Y15 BUFG_GT_X0Y24:BUFG_GT_X0Y47 BUFG_GT_SYNC_X0Y15:BUFG_GT_SYNC_X0Y29 DSP48E2_X0Y24:DSP48E2_X9Y47 MMCM_X0Y1:MMCM_X0Y1 PLL_X0Y2:PLL_X0Y3 PLL_SELECT_SITE_X0Y8:PLL_SELECT_SITE_X0Y15 RAMB18_X0Y24:RAMB18_X5Y47 RAMB36_X0Y12:RAMB36_X5Y23 SLICE_X0Y60:SLICE_X83Y119 URAM288_X0Y16:URAM288_X1Y31 }",
    Pblock(1, Right)  -> "{ BUFCE_LEAF_X432Y4:BUFCE_LEAF_X863Y7 BUFCE_ROW_X1Y24:BUFCE_ROW_X1Y47 BUFCE_ROW_FSR_X96Y1:BUFCE_ROW_FSR_X188Y1 BUFGCE_X1Y24:BUFGCE_X1Y47 BUFGCE_DIV_X1Y4:BUFGCE_DIV_X1Y7 BUFGCTRL_X1Y8:BUFGCTRL_X1Y15 BUFG_GT_X1Y24:BUFG_GT_X1Y47 BUFG_GT_SYNC_X1Y15:BUFG_GT_SYNC_X1Y29 DSP48E2_X10Y24:DSP48E2_X18Y47 MMCM_X1Y1:MMCM_X1Y1 PLL_X1Y2:PLL_X1Y3 PLL_SELECT_SITE_X1Y8:PLL_SELECT_SITE_X1Y15 RAMB18_X6Y24:RAMB18_X11Y47 RAMB36_X6Y12:RAMB36_X11Y23 SLICE_X84Y60:SLICE_X168Y119 URAM288_X2Y16:URAM288_X3Y31 }",
    Pblock(2, Left)   -> "{ BUFCE_LEAF_X0Y8:BUFCE_LEAF_X431Y11 BUFCE_ROW_X0Y48:BUFCE_ROW_X0Y71 BUFCE_ROW_FSR_X0Y2:BUFCE_ROW_FSR_X95Y2 BUFGCE_X0Y48:BUFGCE_X0Y71 BUFGCE_DIV_X0Y8:BUFGCE_DIV_X0Y11 BUFGCTRL_X0Y16:BUFGCTRL_X0Y23 BUFG_GT_X0Y48:BUFG_GT_X0Y71 BUFG_GT_SYNC_X0Y30:BUFG_GT_SYNC_X0Y44 DSP48E2_X0Y48:DSP48E2_X9Y71 MMCM_X0Y2:MMCM_X0Y2 PLL_X0Y4:PLL_X0Y5 PLL_SELECT_SITE_X0Y16:PLL_SELECT_SITE_X0Y23 RAMB18_X0Y48:RAMB18_X5Y71 RAMB36_X0Y24:RAMB36_X5Y35 SLICE_X0Y120:SLICE_X83Y179 URAM288_X0Y32:URAM288_X1Y47 }",
    Pblock(2, Right)  -> "{ BUFCE_LEAF_X432Y8:BUFCE_LEAF_X863Y11 BUFCE_ROW_X1Y48:BUFCE_ROW_X1Y71 BUFCE_ROW_FSR_X96Y2:BUFCE_ROW_FSR_X188Y2 BUFGCE_X1Y48:BUFGCE_X1Y71 BUFGCE_DIV_X1Y8:BUFGCE_DIV_X1Y11 BUFGCTRL_X1Y16:BUFGCTRL_X1Y23 BUFG_GT_X1Y48:BUFG_GT_X1Y71 BUFG_GT_SYNC_X1Y30:BUFG_GT_SYNC_X1Y44 DSP48E2_X10Y48:DSP48E2_X18Y71 MMCM_X1Y2:MMCM_X1Y2 PLL_X1Y4:PLL_X1Y5 PLL_SELECT_SITE_X1Y16:PLL_SELECT_SITE_X1Y23 RAMB18_X6Y48:RAMB18_X11Y71 RAMB36_X6Y24:RAMB36_X11Y35 SLICE_X84Y120:SLICE_X168Y179 URAM288_X2Y32:URAM288_X3Y47 }",
    Pblock(3, Left)   -> "{ BUFCE_LEAF_X0Y12:BUFCE_LEAF_X431Y15 BUFCE_ROW_X0Y72:BUFCE_ROW_X0Y95 BUFCE_ROW_FSR_X0Y3:BUFCE_ROW_FSR_X95Y3 BUFGCE_X0Y72:BUFGCE_X0Y95 BUFGCE_DIV_X0Y12:BUFGCE_DIV_X0Y15 BUFGCTRL_X0Y24:BUFGCTRL_X0Y31 BUFG_GT_X0Y72:BUFG_GT_X0Y95 BUFG_GT_SYNC_X0Y45:BUFG_GT_SYNC_X0Y59 DSP48E2_X0Y72:DSP48E2_X9Y95 MMCM_X0Y3:MMCM_X0Y3 PLL_X0Y6:PLL_X0Y7 PLL_SELECT_SITE_X0Y24:PLL_SELECT_SITE_X0Y31 RAMB18_X0Y72:RAMB18_X5Y95 RAMB36_X0Y36:RAMB36_X5Y47 SLICE_X0Y180:SLICE_X83Y239 URAM288_X0Y48:URAM288_X1Y63 }",
    Pblock(3, Right)  -> "{ BUFCE_LEAF_X432Y12:BUFCE_LEAF_X863Y15 BUFCE_ROW_X1Y72:BUFCE_ROW_X1Y95 BUFCE_ROW_FSR_X96Y3:BUFCE_ROW_FSR_X188Y3 BUFGCE_X1Y72:BUFGCE_X1Y95 BUFGCE_DIV_X1Y12:BUFGCE_DIV_X1Y15 BUFGCTRL_X1Y24:BUFGCTRL_X1Y31 BUFG_GT_X1Y72:BUFG_GT_X1Y95 BUFG_GT_SYNC_X1Y45:BUFG_GT_SYNC_X1Y59 DSP48E2_X10Y72:DSP48E2_X18Y95 MMCM_X1Y3:MMCM_X1Y3 PLL_X1Y6:PLL_X1Y7 PLL_SELECT_SITE_X1Y24:PLL_SELECT_SITE_X1Y31 RAMB18_X6Y72:RAMB18_X11Y95 RAMB36_X6Y36:RAMB36_X11Y47 SLICE_X84Y180:SLICE_X168Y239 URAM288_X2Y48:URAM288_X3Y63 }",
    Pblock(4, Left)   -> "{ BUFCE_LEAF_X0Y16:BUFCE_LEAF_X431Y19 BUFCE_ROW_X0Y96:BUFCE_ROW_X0Y119 BUFCE_ROW_FSR_X0Y4:BUFCE_ROW_FSR_X95Y4 BUFGCE_X0Y96:BUFGCE_X0Y119 BUFGCE_DIV_X0Y16:BUFGCE_DIV_X0Y19 BUFGCTRL_X0Y32:BUFGCTRL_X0Y39 BUFG_GT_X0Y96:BUFG_GT_X0Y119 BUFG_GT_SYNC_X0Y60:BUFG_GT_SYNC_X0Y74 DSP48E2_X0Y96:DSP48E2_X9Y119 MMCM_X0Y4:MMCM_X0Y4 PLL_X0Y8:PLL_X0Y9 PLL_SELECT_SITE_X0Y32:PLL_SELECT_SITE_X0Y39 RAMB18_X0Y96:RAMB18_X5Y119 RAMB36_X0Y48:RAMB36_X5Y59 SLICE_X0Y240:SLICE_X83Y299 URAM288_X0Y64:URAM288_X1Y79 }",
    Pblock(4, Right)  -> "{ BUFCE_LEAF_X432Y16:BUFCE_LEAF_X863Y19 BUFCE_ROW_X1Y96:BUFCE_ROW_X1Y119 BUFCE_ROW_FSR_X96Y4:BUFCE_ROW_FSR_X188Y4 BUFGCE_X1Y96:BUFGCE_X1Y119 BUFGCE_DIV_X1Y16:BUFGCE_DIV_X1Y19 BUFGCTRL_X1Y32:BUFGCTRL_X1Y39 BUFG_GT_X1Y96:BUFG_GT_X1Y119 BUFG_GT_SYNC_X1Y60:BUFG_GT_SYNC_X1Y74 DSP48E2_X10Y96:DSP48E2_X18Y119 MMCM_X1Y4:MMCM_X1Y4 PLL_X1Y8:PLL_X1Y9 PLL_SELECT_SITE_X1Y32:PLL_SELECT_SITE_X1Y39 RAMB18_X6Y96:RAMB18_X11Y119 RAMB36_X6Y48:RAMB36_X11Y59 SLICE_X84Y240:SLICE_X168Y299 URAM288_X2Y64:URAM288_X3Y79 }",
    Pblock(5, Left)   -> "{ BUFCE_LEAF_X0Y20:BUFCE_LEAF_X263Y23 BUFCE_ROW_FSR_X0Y5:BUFCE_ROW_FSR_X60Y5 BUFG_GT_X0Y120:BUFG_GT_X0Y143 BUFG_GT_SYNC_X0Y75:BUFG_GT_SYNC_X0Y89 DSP48E2_X0Y120:DSP48E2_X6Y143 RAMB18_X0Y120:RAMB18_X3Y143 RAMB36_X0Y60:RAMB36_X3Y71 SLICE_X0Y300:SLICE_X49Y359 URAM288_X0Y80:URAM288_X0Y95 }",
    Pblock(5, Right)  -> "{ BUFCE_LEAF_X264Y20:BUFCE_LEAF_X455Y23 BUFCE_ROW_X0Y120:BUFCE_ROW_X0Y143 BUFCE_ROW_FSR_X61Y5:BUFCE_ROW_FSR_X101Y5 BUFGCE_X0Y120:BUFGCE_X0Y143 BUFGCE_DIV_X0Y20:BUFGCE_DIV_X0Y23 BUFGCTRL_X0Y40:BUFGCTRL_X0Y47 DSP48E2_X7Y120:DSP48E2_X10Y143 MMCM_X0Y5:MMCM_X0Y5 PLL_X0Y10:PLL_X0Y11 PLL_SELECT_SITE_X0Y40:PLL_SELECT_SITE_X0Y47 RAMB18_X4Y120:RAMB18_X6Y143 RAMB36_X4Y60:RAMB36_X6Y71 SLICE_X50Y300:SLICE_X87Y359 URAM288_X1Y80:URAM288_X1Y95 }",
    Pblock(6, Left)   -> "{ BUFCE_LEAF_X0Y24:BUFCE_LEAF_X263Y27 BUFCE_ROW_FSR_X0Y6:BUFCE_ROW_FSR_X60Y6 BUFG_GT_X0Y144:BUFG_GT_X0Y167 BUFG_GT_SYNC_X0Y90:BUFG_GT_SYNC_X0Y104 DSP48E2_X0Y144:DSP48E2_X6Y167 RAMB18_X0Y144:RAMB18_X3Y167 RAMB36_X0Y72:RAMB36_X3Y83 SLICE_X0Y360:SLICE_X49Y419 URAM288_X0Y96:URAM288_X0Y111 }",
    Pblock(6, Right)  -> "{ BUFCE_LEAF_X264Y24:BUFCE_LEAF_X455Y27 BUFCE_ROW_X0Y144:BUFCE_ROW_X0Y167 BUFCE_ROW_FSR_X61Y6:BUFCE_ROW_FSR_X101Y6 BUFGCE_X0Y144:BUFGCE_X0Y167 BUFGCE_DIV_X0Y24:BUFGCE_DIV_X0Y27 BUFGCTRL_X0Y48:BUFGCTRL_X0Y55 DSP48E2_X7Y144:DSP48E2_X10Y167 MMCM_X0Y6:MMCM_X0Y6 PLL_X0Y12:PLL_X0Y13 PLL_SELECT_SITE_X0Y48:PLL_SELECT_SITE_X0Y55 RAMB18_X4Y144:RAMB18_X6Y167 RAMB36_X4Y72:RAMB36_X6Y83 SLICE_X50Y360:SLICE_X87Y419 URAM288_X1Y96:URAM288_X1Y111 }",
    Pblock(7, Left)   -> "{ BUFCE_LEAF_X0Y28:BUFCE_LEAF_X263Y31 BUFCE_ROW_FSR_X0Y7:BUFCE_ROW_FSR_X60Y7 BUFG_GT_X0Y168:BUFG_GT_X0Y191 BUFG_GT_SYNC_X0Y105:BUFG_GT_SYNC_X0Y119 DSP48E2_X0Y168:DSP48E2_X6Y191 RAMB18_X0Y168:RAMB18_X3Y191 RAMB36_X0Y84:RAMB36_X3Y95 SLICE_X0Y420:SLICE_X49Y479 URAM288_X0Y112:URAM288_X0Y127 }",
    Pblock(7, Right)  -> "{ BUFCE_LEAF_X264Y28:BUFCE_LEAF_X455Y31 BUFCE_ROW_X0Y168:BUFCE_ROW_X0Y191 BUFCE_ROW_FSR_X61Y7:BUFCE_ROW_FSR_X101Y7 BUFGCE_X0Y168:BUFGCE_X0Y191 BUFGCE_DIV_X0Y28:BUFGCE_DIV_X0Y31 BUFGCTRL_X0Y56:BUFGCTRL_X0Y63 DSP48E2_X7Y168:DSP48E2_X10Y191 MMCM_X0Y7:MMCM_X0Y7 PLL_X0Y14:PLL_X0Y15 PLL_SELECT_SITE_X0Y56:PLL_SELECT_SITE_X0Y63 RAMB18_X4Y168:RAMB18_X6Y191 RAMB36_X4Y84:RAMB36_X6Y95 SLICE_X50Y420:SLICE_X87Y479 URAM288_X1Y112:URAM288_X1Y127 }",
    Pblock(8, Left)   -> "{ BUFCE_LEAF_X0Y32:BUFCE_LEAF_X263Y35 BUFCE_ROW_FSR_X0Y8:BUFCE_ROW_FSR_X60Y8 BUFG_GT_X0Y192:BUFG_GT_X0Y215 BUFG_GT_SYNC_X0Y120:BUFG_GT_SYNC_X0Y134 DSP48E2_X0Y192:DSP48E2_X6Y215 RAMB18_X0Y192:RAMB18_X3Y215 RAMB36_X0Y96:RAMB36_X3Y107 SLICE_X0Y480:SLICE_X49Y539 URAM288_X0Y128:URAM288_X0Y143 }",
    Pblock(8, Right)  -> "{ BUFCE_LEAF_X264Y32:BUFCE_LEAF_X455Y35 BUFCE_ROW_X0Y192:BUFCE_ROW_X0Y215 BUFCE_ROW_FSR_X61Y8:BUFCE_ROW_FSR_X101Y8 BUFGCE_X0Y192:BUFGCE_X0Y215 BUFGCE_DIV_X0Y32:BUFGCE_DIV_X0Y35 BUFGCTRL_X0Y64:BUFGCTRL_X0Y71 DSP48E2_X7Y192:DSP48E2_X10Y215 MMCM_X0Y8:MMCM_X0Y8 PLL_X0Y16:PLL_X0Y17 PLL_SELECT_SITE_X0Y64:PLL_SELECT_SITE_X0Y71 RAMB18_X4Y192:RAMB18_X6Y215 RAMB36_X4Y96:RAMB36_X6Y107 SLICE_X50Y480:SLICE_X87Y539 URAM288_X1Y128:URAM288_X1Y143 }",
    Pblock(9, Left)   -> "{ BUFCE_LEAF_X0Y36:BUFCE_LEAF_X263Y39 BUFCE_ROW_FSR_X0Y9:BUFCE_ROW_FSR_X60Y9 BUFG_GT_X0Y216:BUFG_GT_X0Y239 BUFG_GT_SYNC_X0Y135:BUFG_GT_SYNC_X0Y149 DSP48E2_X0Y216:DSP48E2_X6Y239 RAMB18_X0Y216:RAMB18_X3Y239 RAMB36_X0Y108:RAMB36_X3Y119 SLICE_X0Y540:SLICE_X49Y599 URAM288_X0Y144:URAM288_X0Y159 }",
    Pblock(9, Right)  -> "{ BUFCE_LEAF_X264Y36:BUFCE_LEAF_X455Y39 BUFCE_ROW_X0Y216:BUFCE_ROW_X0Y239 BUFCE_ROW_FSR_X61Y9:BUFCE_ROW_FSR_X101Y9 BUFGCE_X0Y216:BUFGCE_X0Y239 BUFGCE_DIV_X0Y36:BUFGCE_DIV_X0Y39 BUFGCTRL_X0Y72:BUFGCTRL_X0Y79 DSP48E2_X7Y216:DSP48E2_X10Y239 MMCM_X0Y9:MMCM_X0Y9 PLL_X0Y18:PLL_X0Y19 PLL_SELECT_SITE_X0Y72:PLL_SELECT_SITE_X0Y79 RAMB18_X4Y216:RAMB18_X6Y239 RAMB36_X4Y108:RAMB36_X6Y119 SLICE_X50Y540:SLICE_X87Y599 URAM288_X1Y144:URAM288_X1Y159 }",
    Pblock(10, Left)  -> "{ BUFCE_LEAF_X0Y40:BUFCE_LEAF_X431Y43 BUFCE_ROW_X0Y240:BUFCE_ROW_X0Y263 BUFCE_ROW_FSR_X0Y10:BUFCE_ROW_FSR_X95Y10 BUFGCE_X0Y240:BUFGCE_X0Y263 BUFGCE_DIV_X0Y40:BUFGCE_DIV_X0Y43 BUFGCTRL_X0Y80:BUFGCTRL_X0Y87 BUFG_GT_X0Y240:BUFG_GT_X0Y263 BUFG_GT_SYNC_X0Y150:BUFG_GT_SYNC_X0Y164 DSP48E2_X0Y240:DSP48E2_X9Y263 MMCM_X0Y10:MMCM_X0Y10 PLL_X0Y20:PLL_X0Y21 PLL_SELECT_SITE_X0Y80:PLL_SELECT_SITE_X0Y87 RAMB18_X0Y240:RAMB18_X5Y263 RAMB36_X0Y120:RAMB36_X5Y131 SLICE_X0Y600:SLICE_X83Y659 URAM288_X0Y160:URAM288_X1Y175 }",
    Pblock(10, Right) -> "{ BUFCE_LEAF_X432Y40:BUFCE_LEAF_X863Y43 BUFCE_ROW_X1Y240:BUFCE_ROW_X1Y263 BUFCE_ROW_FSR_X96Y10:BUFCE_ROW_FSR_X188Y10 BUFGCE_X1Y240:BUFGCE_X1Y263 BUFGCE_DIV_X1Y40:BUFGCE_DIV_X1Y43 BUFGCTRL_X1Y80:BUFGCTRL_X1Y87 BUFG_GT_X1Y240:BUFG_GT_X1Y263 BUFG_GT_SYNC_X1Y150:BUFG_GT_SYNC_X1Y164 DSP48E2_X10Y240:DSP48E2_X18Y263 MMCM_X1Y10:MMCM_X1Y10 PLL_X1Y20:PLL_X1Y21 PLL_SELECT_SITE_X1Y80:PLL_SELECT_SITE_X1Y87 RAMB18_X6Y240:RAMB18_X11Y263 RAMB36_X6Y120:RAMB36_X11Y131 SLICE_X84Y600:SLICE_X168Y659 URAM288_X2Y160:URAM288_X3Y175 }",
    Pblock(11, Left)  -> "{ BUFCE_LEAF_X0Y44:BUFCE_LEAF_X431Y47 BUFCE_ROW_X0Y264:BUFCE_ROW_X0Y287 BUFCE_ROW_FSR_X0Y11:BUFCE_ROW_FSR_X95Y11 BUFGCE_X0Y264:BUFGCE_X0Y287 BUFGCE_DIV_X0Y44:BUFGCE_DIV_X0Y47 BUFGCTRL_X0Y88:BUFGCTRL_X0Y95 BUFG_GT_X0Y264:BUFG_GT_X0Y287 BUFG_GT_SYNC_X0Y165:BUFG_GT_SYNC_X0Y179 DSP48E2_X0Y264:DSP48E2_X9Y287 MMCM_X0Y11:MMCM_X0Y11 PLL_X0Y22:PLL_X0Y23 PLL_SELECT_SITE_X0Y88:PLL_SELECT_SITE_X0Y95 RAMB18_X0Y264:RAMB18_X5Y287 RAMB36_X0Y132:RAMB36_X5Y143 SLICE_X0Y660:SLICE_X83Y719 URAM288_X0Y176:URAM288_X1Y191 }",
    Pblock(11, Right) -> "{ BUFCE_LEAF_X432Y44:BUFCE_LEAF_X863Y47 BUFCE_ROW_X1Y264:BUFCE_ROW_X1Y287 BUFCE_ROW_FSR_X96Y11:BUFCE_ROW_FSR_X188Y11 BUFGCE_X1Y264:BUFGCE_X1Y287 BUFGCE_DIV_X1Y44:BUFGCE_DIV_X1Y47 BUFGCTRL_X1Y88:BUFGCTRL_X1Y95 BUFG_GT_X1Y264:BUFG_GT_X1Y287 BUFG_GT_SYNC_X1Y165:BUFG_GT_SYNC_X1Y179 DSP48E2_X10Y264:DSP48E2_X18Y287 MMCM_X1Y11:MMCM_X1Y11 PLL_X1Y22:PLL_X1Y23 PLL_SELECT_SITE_X1Y88:PLL_SELECT_SITE_X1Y95 RAMB18_X6Y264:RAMB18_X11Y287 RAMB36_X6Y132:RAMB36_X11Y143 SLICE_X84Y660:SLICE_X168Y719 URAM288_X2Y176:URAM288_X3Y191 }",
    Pblock(12, Left)  -> "{ BUFCE_LEAF_X0Y48:BUFCE_LEAF_X431Y51 BUFCE_ROW_X0Y288:BUFCE_ROW_X0Y311 BUFCE_ROW_FSR_X0Y12:BUFCE_ROW_FSR_X95Y12 BUFGCE_X0Y288:BUFGCE_X0Y311 BUFGCE_DIV_X0Y48:BUFGCE_DIV_X0Y51 BUFGCTRL_X0Y96:BUFGCTRL_X0Y103 BUFG_GT_X0Y288:BUFG_GT_X0Y311 BUFG_GT_SYNC_X0Y180:BUFG_GT_SYNC_X0Y194 DSP48E2_X0Y288:DSP48E2_X9Y311 MMCM_X0Y12:MMCM_X0Y12 PLL_X0Y24:PLL_X0Y25 PLL_SELECT_SITE_X0Y96:PLL_SELECT_SITE_X0Y103 RAMB18_X0Y288:RAMB18_X5Y311 RAMB36_X0Y144:RAMB36_X5Y155 SLICE_X0Y720:SLICE_X83Y779 URAM288_X0Y192:URAM288_X1Y207 }",
    Pblock(12, Right) -> "{ BUFCE_LEAF_X432Y48:BUFCE_LEAF_X863Y51 BUFCE_ROW_X1Y288:BUFCE_ROW_X1Y311 BUFCE_ROW_FSR_X96Y12:BUFCE_ROW_FSR_X188Y12 BUFGCE_X1Y288:BUFGCE_X1Y311 BUFGCE_DIV_X1Y48:BUFGCE_DIV_X1Y51 BUFGCTRL_X1Y96:BUFGCTRL_X1Y103 BUFG_GT_X1Y288:BUFG_GT_X1Y311 BUFG_GT_SYNC_X1Y180:BUFG_GT_SYNC_X1Y194 DSP48E2_X10Y288:DSP48E2_X18Y311 MMCM_X1Y12:MMCM_X1Y12 PLL_X1Y24:PLL_X1Y25 PLL_SELECT_SITE_X1Y96:PLL_SELECT_SITE_X1Y103 RAMB18_X6Y288:RAMB18_X11Y311 RAMB36_X6Y144:RAMB36_X11Y155 SLICE_X84Y720:SLICE_X168Y779 URAM288_X2Y192:URAM288_X3Y207 }",
    Pblock(13, Left)  -> "{ BUFCE_LEAF_X0Y52:BUFCE_LEAF_X431Y55 BUFCE_ROW_X0Y312:BUFCE_ROW_X0Y335 BUFCE_ROW_FSR_X0Y13:BUFCE_ROW_FSR_X95Y13 BUFGCE_X0Y312:BUFGCE_X0Y335 BUFGCE_DIV_X0Y52:BUFGCE_DIV_X0Y55 BUFGCTRL_X0Y104:BUFGCTRL_X0Y111 BUFG_GT_X0Y312:BUFG_GT_X0Y335 BUFG_GT_SYNC_X0Y195:BUFG_GT_SYNC_X0Y209 DSP48E2_X0Y312:DSP48E2_X9Y335 MMCM_X0Y13:MMCM_X0Y13 PLL_X0Y26:PLL_X0Y27 PLL_SELECT_SITE_X0Y104:PLL_SELECT_SITE_X0Y111 RAMB18_X0Y312:RAMB18_X5Y335 RAMB36_X0Y156:RAMB36_X5Y167 SLICE_X0Y780:SLICE_X83Y839 URAM288_X0Y208:URAM288_X1Y223 }",
    Pblock(13, Right) -> "{ BUFCE_LEAF_X432Y52:BUFCE_LEAF_X863Y55 BUFCE_ROW_X1Y312:BUFCE_ROW_X1Y335 BUFCE_ROW_FSR_X96Y13:BUFCE_ROW_FSR_X188Y13 BUFGCE_X1Y312:BUFGCE_X1Y335 BUFGCE_DIV_X1Y52:BUFGCE_DIV_X1Y55 BUFGCTRL_X1Y104:BUFGCTRL_X1Y111 BUFG_GT_X1Y312:BUFG_GT_X1Y335 BUFG_GT_SYNC_X1Y195:BUFG_GT_SYNC_X1Y209 DSP48E2_X10Y312:DSP48E2_X18Y335 MMCM_X1Y13:MMCM_X1Y13 PLL_X1Y26:PLL_X1Y27 PLL_SELECT_SITE_X1Y104:PLL_SELECT_SITE_X1Y111 RAMB18_X6Y312:RAMB18_X11Y335 RAMB36_X6Y156:RAMB36_X11Y167 SLICE_X84Y780:SLICE_X168Y839 URAM288_X2Y208:URAM288_X3Y223 }",
    Pblock(14, Left)  -> "{ BUFCE_LEAF_X0Y56:BUFCE_LEAF_X431Y59 BUFCE_ROW_X0Y336:BUFCE_ROW_X0Y359 BUFCE_ROW_FSR_X0Y14:BUFCE_ROW_FSR_X95Y14 BUFGCE_X0Y336:BUFGCE_X0Y359 BUFGCE_DIV_X0Y56:BUFGCE_DIV_X0Y59 BUFGCTRL_X0Y112:BUFGCTRL_X0Y119 BUFG_GT_X0Y336:BUFG_GT_X0Y359 BUFG_GT_SYNC_X0Y210:BUFG_GT_SYNC_X0Y224 DSP48E2_X0Y336:DSP48E2_X9Y359 MMCM_X0Y14:MMCM_X0Y14 PLL_X0Y28:PLL_X0Y29 PLL_SELECT_SITE_X0Y112:PLL_SELECT_SITE_X0Y119 RAMB18_X0Y336:RAMB18_X5Y359 RAMB36_X0Y168:RAMB36_X5Y179 SLICE_X0Y840:SLICE_X83Y899 URAM288_X0Y224:URAM288_X1Y239 }",
    Pblock(14, Right) -> "{ BUFCE_LEAF_X432Y56:BUFCE_LEAF_X863Y59 BUFCE_ROW_X1Y336:BUFCE_ROW_X1Y359 BUFCE_ROW_FSR_X96Y14:BUFCE_ROW_FSR_X188Y14 BUFGCE_X1Y336:BUFGCE_X1Y359 BUFGCE_DIV_X1Y56:BUFGCE_DIV_X1Y59 BUFGCTRL_X1Y112:BUFGCTRL_X1Y119 BUFG_GT_X1Y336:BUFG_GT_X1Y359 BUFG_GT_SYNC_X1Y210:BUFG_GT_SYNC_X1Y224 DSP48E2_X10Y336:DSP48E2_X18Y359 MMCM_X1Y14:MMCM_X1Y14 PLL_X1Y28:PLL_X1Y29 PLL_SELECT_SITE_X1Y112:PLL_SELECT_SITE_X1Y119 RAMB18_X6Y336:RAMB18_X11Y359 RAMB36_X6Y168:RAMB36_X11Y179 SLICE_X84Y840:SLICE_X168Y899 URAM288_X2Y224:URAM288_X3Y239 }",
  )
  // format: on

  // Mapping from an abstract position on a 2D grid to a Core in a torus network.
  // This generates the torus network coordinates at the appropriate place in
  // a grid so the folded nature of the torus network is visible, yet retains the
  // "plain" 2D grid coordinates so we know where we are on the plane.
  def getGridLocToCoreMap(dimX: Int, dimY: Int): Map[GridLoc, Core] = {
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

    val gridLocToCore = MMap.empty[GridLoc, Core]
    cToX.foreach { case (c, x) =>
      rToY.foreach { case (r, y) =>
        gridLocToCore += GridLoc(c, r) -> Core(x, y)
      }
    }

    gridLocToCore.toMap
  }
}

object Tester extends App {

  import IterativePlacement._

  val dimX = 10
  val dimY = 25

  // val anchor = (2, 7)
  // val maxCores = 5
  // val placer = new PhysicalPlacement(dimX, dimY, anchor, maxCores)
  // println(placer.pblockConstraint)

  val anchor         = Pblock(7, Right)
  val placer         = new IterativePlacement(dimX, dimY, anchor)
  val solutionTclStr = placer.pblockConstraint
  println(solutionTclStr)
}
