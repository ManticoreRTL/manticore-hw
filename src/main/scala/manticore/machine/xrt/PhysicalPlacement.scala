package manticore.machine.xrt

import com.google.ortools.Loader
import com.google.ortools.sat.CpModel
import com.google.ortools.sat.CpSolver
import com.google.ortools.sat.CpSolverStatus
import com.google.ortools.sat.LinearExpr
import com.google.ortools.sat.BoolVar
import com.google.ortools.sat.LinearArgument
import com.google.ortools.constraintsolver.IntVar

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

object Tester extends App {

  val dimX   = 10
  val dimY   = 10
  val anchor = (1, 10)

  val placer = new PhysicalPlacement(10, 10, (2, 7), 5)
  println(placer.pblockConstraint)
  // Loader.loadNativeLibraries()
  // val model = new CpModel()

  // val uRange = Range(0, 4)
  // val vRange = Range(0, 15)
  // val xRange = Range(0, dimX)
  // val yRange = Range(0, dimY)
  // val shellZone =
  //   uRange.map { u => vRange.map { v => (u, v) } }.flatten.filter { case (u, v) => u >= 2 && (v >= 5 && v <= 9) }

  // // location variables, loc(x)(y)(u)(v) == true implies that core xy is placed into clock regions uv
  // val loc = xRange.map { x =>
  //   yRange.map { y =>
  //     uRange.map { u =>
  //       vRange.map { v =>
  //         model.newIntVar(0, 1, s"loc_${x}_${y}_${u}_${v}")
  //       }
  //     }
  //   }
  // }

  // // each core  can be placed in a single location
  // for (x <- xRange; y <- yRange) {

  //   val coreBindingCount = LinearExpr.sum(loc(x)(y).flatten.map(_.build()).toArray)
  //   model.addEquality(coreBindingCount, model.newConstant(1))
  //   // val sumExpr = new LinearExpr()
  //   // model.add

  // }

  // // at most 5 cores can be in 1 clock region
  // val utilization = for (u <- uRange; v <- vRange) yield {
  //   val l     = for (x <- xRange; y <- yRange) yield { loc(x)(y)(u)(v).build() }
  //   val util  = LinearExpr.sum(l.toArray)
  //   val utilV = model.newIntVar(0, 1000, s"util_${u}_${v}")
  //   model.addLessOrEqual(util, 5)
  //   model.addEquality(utilV, util)
  //   utilV
  // }

  // // define position variable for each core that points to its location on the
  // // FPGA (i.e., clock region)
  // val pos = xRange.map { x =>
  //   yRange.map { y =>
  //     // xRange.
  //     val pu = model.newIntVar(uRange.start, uRange.last + 1, s"pu_${x}_${y}")
  //     val pv = model.newIntVar(vRange.start, vRange.last + 1, s"pv_${x}_${y}")
  //     val puSum = LinearExpr.weightedSum(
  //       loc(x)(y).flatten.map(_.build()).toArray,
  //       (for (u <- uRange; v <- vRange) yield { u.toLong }).toArray
  //     )
  //     val pvSum = LinearExpr.weightedSum(
  //       loc(x)(y).flatten.map(_.build()).toArray,
  //       (for (u <- uRange; v <- vRange) yield { v.toLong }).toArray
  //     )
  //     model.addEquality(pu, puSum)
  //     model.addEquality(pv, pvSum)
  //     (pu, pv)
  //   }
  // }

  // def neighbors(x: Int, y: Int): Seq[(Int, Int)] = {

  //   def wrap(v: Int, dim: Int): Seq[Int] = {
  //     if (v == 0) {
  //       Seq(dim - 1, v + 1)
  //     } else if (v == dim - 1) {
  //       Seq(v - 1, 0)
  //     } else {
  //       Seq(v - 1, v + 1)
  //     }
  //   }
  //   wrap(x, dimX).map((_, y)) ++ wrap(y, dimY).map((x, _))
  // }

  // for (x <- xRange; y <- yRange) {
  //   // the distance from each core to its neighbor should be at most
  //   // 1 clock region in each direction
  //   println(s"$x,$y neighbors are ${neighbors(x, y)}")
  //   for ((nx, ny) <- neighbors(x, y)) {
  //     val diffU = LinearExpr.weightedSum(Array(pos(x)(y)._1, pos(nx)(ny)._1), Array(1, -1))
  //     val diffV = LinearExpr.weightedSum(Array(pos(x)(y)._2, pos(nx)(ny)._2), Array(1, -1))

  //     model.addLessOrEqual(diffU, 1)
  //     model.addGreaterOrEqual(diffU, -1)

  //     model.addLessOrEqual(diffV, 1)
  //     model.addGreaterOrEqual(diffV, -1)

  //   }
  // }

  // // disallow placement in the shell

  // for ((u, v) <- shellZone) {
  //   for (x <- xRange; y <- yRange) {
  //     model.addEquality(loc(x)(y)(u)(v), 0)
  //   }
  // }

  // // anchor the first core
  // model.addEquality(loc(0)(0)(anchor._1)(anchor._2), 1)

  // val solver = new CpSolver()
  // val status = solver.solve(model)

  // println(status)

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

}
