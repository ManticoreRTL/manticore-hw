package manticore.core.alu

import Chisel._
import chisel3.stage.ChiselStage
import manticore.ISA

import scala.language.implicitConversions

class ALUInterface(DATA_BITS: Int) extends Bundle {

  val in = Input(new Bundle {
    val x = UInt(DATA_BITS.W)
    val y = UInt(DATA_BITS.W)
    val carry = UInt(1.W)
    val carry_en = Bool()
    val select = UInt(1.W)
  })
  val out           = Output(UInt(DATA_BITS.W))
  val carry_out     = Output(UInt(1.W))
  val funct         = Input(UInt(4.W))
  val select        = Input(Bool())
}

object StandardALU {
  object Functs extends Enumeration {
    type Functs = Value
    val ADD2, SUB2, MUL2, AND2, OR2, XOR2, SLL, // logical left shift
    SRL, // logical right shift (zeros padded to the right)
    SRA, SEQ, SLTS, MUX = Value

  }

}

class StandardALUComb(DATA_BITS: Int) extends Module {
  val io = IO(new ALUInterface(DATA_BITS = DATA_BITS))



  val Functs = StandardALU.Functs
  val shamnt = Wire(UInt(log2Ceil(DATA_BITS).W))

  val sum_res = Wire(UInt((DATA_BITS + 1).W))
  def widened(w: UInt): UInt = {
    val as_wider = Wire(UInt((DATA_BITS + 1).W))
    as_wider := w
    as_wider
  }

  sum_res := widened(io.in.x) + widened(io.in.y) + widened(io.in.carry & io.in.carry_en)
  io.carry_out := sum_res >> (DATA_BITS.U)

  shamnt := io.in.y(log2Ceil(DATA_BITS) - 1, 0)
  switch(io.funct) {

    is(Functs.ADD2.id.U) {
      io.out := sum_res(DATA_BITS - 1, 0)
    }
    is(Functs.SUB2.id.U) {
      io.out := io.in.x - io.in.y
    }
    is(Functs.MUL2.id.U) {
      //TODO: change back to mul!
      io.out := io.in.x * io.in.y
    }
    is(Functs.AND2.id.U) {
      io.out := io.in.x & io.in.y
    }
    is(Functs.OR2.id.U) {
      io.out := io.in.x | io.in.y
    }
    is(Functs.XOR2.id.U) {
      io.out := io.in.x ^ io.in.y
    }
    is(Functs.SLL.id.U) {
      io.out := io.in.x << shamnt
    }
    is(Functs.SRL.id.U) {
      io.out := io.in.x >> shamnt
    }
    is(Functs.SRA.id.U) {
      io.out := (io.in.x.asSInt >> shamnt).asUInt
    }
    is(Functs.SEQ.id.U) {
      io.out := (io.in.x === io.in.y).asUInt
    }
    is(Functs.SLTS.id.U) {
      io.out := (io.in.x.asSInt < io.in.y.asSInt).asUInt
    }
    is(Functs.MUX.id.U) {
      when(io.select) {
        io.out := io.in.y
      } otherwise {
        io.out := io.in.x
      }
    }
  }

}

object StandardALUGenerator extends App {

  new ChiselStage()
    .emitVerilog(new StandardALUComb(4), Array("--target-dir", "gen-dir"))
  // new ChiselStage().emitVerilog(new StandardALU(4))

}
