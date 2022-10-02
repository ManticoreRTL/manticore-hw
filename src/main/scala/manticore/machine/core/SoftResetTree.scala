package manticore.machine.core

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage

import manticore.machine.Helpers

class BoolTreeIO(dimx: Int, dimy: Int) extends Bundle {
  val source = Input(Bool())
  val taps   = Output(Vec(dimx, Vec(dimy, Bool())))
  // val last   = Output(Bool())
}

class BoolTree(
    dimx: Int,
    dimy: Int,
    fanout_factor: Int
) extends Module {

  private var latency = 0

  def getLatency: Int = latency

  val io = IO(new BoolTreeIO(dimx, dimy))

  def fo(source: Bool): IndexedSeq[Bool] = IndexedSeq.fill(fanout_factor) { Helpers.RegDontTouch(source) }

  def mkBranches(
      res: IndexedSeq[Bool],
      count: Int = 1
  ): IndexedSeq[Bool] = {
    if (count >= (dimx * dimy)) {
      res
    } else {
      latency += 1
      mkBranches(res.flatMap(fo), count * fanout_factor)
    }
  }

  val source = Helpers.RegDontTouch(io.source)
  latency += 1
  val leaves = mkBranches(IndexedSeq(source))

  for (x <- 0 until dimx) {
    for (y <- 0 until dimy) {
      io.taps(x)(y) := leaves(x + y * dimx)
    }
  }

  // io.last := Helpers.RegDontTouch(leaves.last)
}

object ResetTree {
  val latencyBeforeFanout: Int = 3

  val fanoutFactor: Int = 4

  // We want 2 registers at least in the first, and 1 register in the next SLR.
  // The 2nd register in the first SLR and the 1st register in the next SLR will be marked as USER_SLL_REG by the
  // placement algorithm.
  require(latencyBeforeFanout >= 3)
}

class ResetTreeIO(dimx: Int, dimy: Int) extends Bundle {
  val taps = Output(Vec(dimx, Vec(dimy, Bool())))
  // val last = Output(Bool())
}

class SwitchResetTree(dimx: Int, dimy: Int) extends Module {
  val io = IO(new ResetTreeIO(dimx, dimy))

  // The switches are in the same SLR as the controller. Since we don't cross an SLR, we don't need to use hierarchy
  // to explicitly mark registers as USER_SLL_REG (since there are none).
  val beforeFanoutPipe = Helpers.InlinePipeWithStyle(
    reset.asBool,
    latency = ResetTree.latencyBeforeFanout
  )

  // We reset all switches with a single tree.
  val reset_tree = Module(new BoolTree(dimx, dimy, ResetTree.fanoutFactor))
  reset_tree.io.source := beforeFanoutPipe

  io.taps := reset_tree.io.taps
}

class CoreResetTree(dimx: Int, dimy: Int) extends Module {
  val io = IO(new ResetTreeIO(dimx, dimy))

  class SubTreeWithSlrCrossing(dimx: Int, dimy: Int) extends Module {
    val io = IO(new BoolTreeIO(dimx, dimy))

    val preFanoutLatency = ResetTree.latencyBeforeFanout

    val controllerSide            = Helpers.WrappedPipeWithStyle(io.source, preFanoutLatency - 2)
    val controllerSideSlrCrossing = Helpers.WrappedPipeWithStyle(controllerSide, 1)
    val coreSideSlrCrossing       = Helpers.WrappedPipeWithStyle(controllerSideSlrCrossing, 1)

    val reset_tree = Module(new BoolTree(dimx, dimy, ResetTree.fanoutFactor))
    reset_tree.io.source := coreSideSlrCrossing

    io.taps := reset_tree.io.taps

    def getLatency: Int = preFanoutLatency + reset_tree.getLatency
  }

  val (topDimx, topDimy)       = (dimx, dimy / 2)
  val (bottomDimx, bottomDimy) = (dimx, dimy - dimy / 2)

  val top_reset_tree    = Module(new SubTreeWithSlrCrossing(topDimx, topDimy))
  val bottom_reset_tree = Module(new SubTreeWithSlrCrossing(bottomDimx, bottomDimy))

  // The trees above may not have the same latency if the y dimension results in a different logarithm of the fanout
  // factor. We must equalize the latencies to ensure all cores are reset at the same time.
  val treeLatencyDiff = math.abs(top_reset_tree.getLatency - bottom_reset_tree.getLatency)

  val (topExtraLatency, bottomExtraLatency) = if (top_reset_tree.getLatency > bottom_reset_tree.getLatency) {
    // Add latency to the bottom reset tree.
    (0, treeLatencyDiff)
  } else {
    // Add latency to the top reset tree.
    (treeLatencyDiff, 0)
  }

  top_reset_tree.io.source    := Helpers.InlinePipeWithStyle(reset.asBool, topExtraLatency)
  bottom_reset_tree.io.source := Helpers.InlinePipeWithStyle(reset.asBool, bottomExtraLatency)

  // Sanity-check.
  assert(top_reset_tree.getLatency + topExtraLatency == bottom_reset_tree.getLatency + bottomExtraLatency)

  val total_reset_latency = top_reset_tree.getLatency + topExtraLatency

  // The privileged core must NOT be reset with the above trees as these trees will be floorplanned and that floorplan
  // may clash with that of the privileged core.
  // Instead the privileged core is reset with a dedicated LINEAR reset that has the same latency as the trees.
  val privileged_reset = Helpers.WrappedPipeWithStyle(
    reset.asBool,
    latency = total_reset_latency,
    style = Helpers.SrlStyle.SrlReg
  )

  for (x <- 0 until dimx) {
    for (y <- 0 until dimy) {
      if (x == 0 && y == 0) {
        io.taps(x)(y) := privileged_reset
      } else if (y < dimy / 2) {
        io.taps(x)(y) := top_reset_tree.io.taps(x)(y)
      } else {
        io.taps(x)(y) := bottom_reset_tree.io.taps(x)(y - dimy / 2)
      }
    }
  }
}
