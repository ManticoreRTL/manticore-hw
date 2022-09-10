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

  def solution: Map[Core, Pblock] = {

    def getDefaultSolution: Map[Core, Pblock] = {
      def inSlr1(clockRegionRow: Int): Boolean = (5 <= clockRegionRow) && (clockRegionRow <= 9)

      val gridLocToCore = getGridLocToCoreMap(dimX, dimY)

      val gridLocRows = gridLocToCore
        .groupMap(_._1.r)(_._1)
        .map { case (gridLoc, group) =>
          gridLoc -> group.toSeq.sortBy(_.c)
        }

      assert(dimX <= 12)
      assert(dimY <= 25)

      val res = MMap.empty[Core, Pblock]

      var y              = 0
      var clockRegionRow = 0
      while (y < dimY) {
        var rowsTaken = 0
        val rowBound  = if (inSlr1(clockRegionRow)) 1 else 2

        while (rowsTaken < rowBound) {
          gridLocRows(y).foreach { gridLoc =>
            // Assign locs in row to either the left or right pblock.
            val side = if (gridLoc.c < dimX / 2) Left else Right
            val core = gridLocToCore(gridLoc)
            res += core -> Pblock(clockRegionRow, side)
          }
          rowsTaken += 1
          y += 1
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

      // Swap Pblock sides if x0y0 is not on the correct side.
      val horizontallyRotatedCoreToPblock = if (coreToPblock(x0y0).side != anchor.side) {
        // Invert the "side" of all cores.
        coreToPblock.map { case (core, pblock) =>
          val newSide = if (pblock.side == Left) Right else Left
          core -> pblock.copy(side = newSide)
        }
      } else {
        coreToPblock
      }

      val coreInAnchor = horizontallyRotatedCoreToPblock
        .find { case (core, pblock) =>
          pblock == anchor
        }
        .get
        ._1

      val yIncr = coreInAnchor.y - x0y0.y

      val verticallyRotatedCoreToPblock = horizontallyRotatedCoreToPblock.map { case (core, pblock) =>
        val newCore = Core(core.x, modPos(core.y + yIncr, dimY))
        newCore -> pblock
      }

      verticallyRotatedCoreToPblock
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
  // val anchor   = (2, 7)
  val anchor   = Pblock(7, Right)
  val maxCores = 5

  // val placer = new PhysicalPlacement(dimX, dimY, anchor, maxCores)
  // println(placer.pblockConstraint)

  val placer = new IterativePlacement(dimX, dimY, anchor)
  val solutionStr = placer.solution.toSeq
    .sortBy { case (core, pblock) =>
      (pblock.clockRegionRow, pblock.side.toString())
    }
    .mkString("\n")
  println(solutionStr)
}
