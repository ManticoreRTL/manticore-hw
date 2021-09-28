package example

import Chisel._
import chisel3.VecInit
import chisel3.stage.ChiselStage
import firrtl.transforms.NoDCEAnnotation


class ConstantFirFilterInterface extends Bundle {
  val Xn: UInt = Input(UInt(16.W))
  val Yn: UInt = Output(UInt(16.W))
}
class ConstantFirFilter(val coefs: Seq[Int]) extends Module{

  val io = IO(new ConstantFirFilterInterface)

  case class Tap(tap: UInt, coef: Int)
  val taps = coefs.reverseMap { c =>
    val tap = RegInit(UInt(16.W), 0.U)
    Tap(tap, c)
  }

  taps.sliding(2, 1).foreach{ case Seq(ti, tj) =>
    tj.tap := ti.tap
  }

  io.Yn := taps.map{ t =>
    val term = Wire(UInt(16.W))
    term := t.tap * t.coef.U(16.W)
    term
  }.reduce(_+_)

  taps.head.tap := io.Xn

}

class WrappedConstantFirFilter(val test_data: Seq[Int], val coeffs: Seq[Int]) extends Module {


  val io = IO(new Bundle{})
  val X_MEM_SIZE = test_data.size
  val Y_MEM_SIZE = test_data.size - coeffs.size
  val x_mem = VecInit(test_data.map(_.U(16.W)))
  val x_ptr = RegInit(UInt((log2Ceil(X_MEM_SIZE) + 1).W), 0.U)
  val y_mem = Mem(Y_MEM_SIZE, UInt(16.W))
  val y_ptr = RegInit(UInt((log2Ceil(Y_MEM_SIZE) + 1).W), 0.U)

  val filter = Module(new ConstantFirFilter(coeffs))

  filter.io.Xn := x_mem(x_ptr)



  when (x_ptr < X_MEM_SIZE.U) {
    x_ptr := x_ptr + 1.U
  }
  when(x_ptr >= coeffs.size.U && y_ptr < Y_MEM_SIZE.U) {
    y_ptr := y_ptr + 1.U
    y_mem(y_ptr) := filter.io.Yn
  }

}



object ConstantFirFilterGenerator extends App {

  new ChiselStage().emitVerilog(new ConstantFirFilter(Seq(187, 212, 61)))
  new ChiselStage().emitVerilog(new WrappedConstantFirFilter(Range(1, 101), Seq(187, 212, 61)), annotations = Seq(NoDCEAnnotation))

  import manticore.assembly.Instruction._

  // some aliasing for readability
  val y_ptr = R(4)
  val x_ptr = R(3)
  val const_1 = R(2)
  val reset_reg = R(1)
  val const_0 = R(0)
  val mux = 0xcaca
  val yn_0 = R(5)
  val yn_1 = R(6)
  val yn_2 = R(7)
  val instructions = Array(
    SetValue(const_1, 1),
    SetValue(const_0, 0),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Add2(R(10), const_1, y_ptr)

  )
}