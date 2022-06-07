package manticore.machine.core.alu

import chisel3._
import chisel3.util.HasBlackBoxResource

// This multiplier has a latency of 2 cycles.
class MultiplierInterface(dataWidth: Int) extends Bundle {
  val in0       = Input(UInt(dataWidth.W))
  val in1       = Input(UInt(dataWidth.W))
  val out       = Output(UInt(dataWidth.W))
  val valid_in  = Input(Bool())
  val valid_out = Output(Bool())
}

class Multiplier(dataWidth: Int) extends Module {

  val io = IO(new MultiplierInterface(dataWidth))

  class MultiplierDsp48(dataWidth: Int) extends BlackBox with HasBlackBoxResource {
    require(dataWidth <= 16)

    val io = IO(new Bundle {
      val clock     = Input(Clock())
      val in0       = Input(UInt(dataWidth.W))
      val in1       = Input(UInt(dataWidth.W))
      val out       = Output(UInt(dataWidth.W))
      val valid_in  = Input(Bool())
      val valid_out = Output(Bool())
    })

    addResource("/verilog/MultiplierDsp48/MultiplierDsp48.v")
  }

  val dsp = Module(new MultiplierDsp48(dataWidth))

  dsp.io.clock    := clock
  dsp.io.in0      := io.in0
  dsp.io.in1      := io.in1
  dsp.io.valid_in := io.valid_in

  io.out       := dsp.io.out
  io.valid_out := dsp.io.valid_out

}
