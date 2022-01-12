package manticore.core

import Chisel._
import chisel3.stage.ChiselStage


class Sub extends Module {
  val io = IO(new Bundle {
    val xIn = Input(UInt(8.W))
    val yOut = Output(UInt(8.W))
    val s = Input(Bool())
  })

  val sub_res = Wire(UInt(8.W))
  sub_res := io.xIn

  sub_res := io.xIn - 1.U
  when(io.s) {
    sub_res := io.xIn - 10.U
  }
  io.yOut := sub_res

}

object Gen extends App {

  new ChiselStage().emitSystemVerilog(new Sub())
}