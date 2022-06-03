package manticore.machine.core.alu

import Chisel._
import chisel3.stage.ChiselStage
import manticore.machine.ISA

import scala.language.implicitConversions

class ALUInterface(DATA_BITS: Int) extends Bundle {

  val in = Input(new Bundle {
    val x = UInt(DATA_BITS.W)
    val y = UInt(DATA_BITS.W)
    val carry = UInt(1.W)
    val select = Bool()
    val mask = UInt(DATA_BITS.W)
  })
  val out           = Output(UInt(DATA_BITS.W))
  val carry_out     = Output(UInt(1.W))
  val funct         = Input(UInt(4.W))
}

object StandardALU {
  object Functs extends Enumeration {
    type Functs = Value
    val ADD2, SUB2, MUL2, AND2, OR2, XOR2, SLL, // logical left shift
    SRL, // logical right shift (zeros padded to the right)
    SRA, SEQ, SLT, SLTS, MUX, ADDC = Value
  }
}

class StandardALUComb(DATA_BITS: Int) extends Module {
  val io = IO(new ALUInterface(DATA_BITS = DATA_BITS))

  val Functs = StandardALU.Functs

  val shamnt = Wire(UInt(log2Ceil(DATA_BITS).W))
  val sum_res = Wire(UInt((DATA_BITS + 1).W))
  val sum_with_carry = Wire(UInt((DATA_BITS + 1).W))
  val alu_res = Wire(UInt(DATA_BITS.W))

  def widened(w: UInt): UInt = {
    val as_wider = Wire(UInt((DATA_BITS + 1).W))
    as_wider := w
    as_wider
  }

  shamnt := io.in.y(log2Ceil(DATA_BITS) - 1, 0)
  sum_res := widened(io.in.x) + widened(io.in.y)
  sum_with_carry := sum_res + widened(io.in.carry)

  switch(io.funct) {
    is(Functs.ADD2.id.U) {
      alu_res := sum_res(DATA_BITS - 1, 0)
    }
    is(Functs.SUB2.id.U) {
      alu_res := io.in.x - io.in.y
    }
    is(Functs.MUL2.id.U) {
      //TODO: change back to mul!
      alu_res := io.in.x + io.in.y
    }
    is(Functs.AND2.id.U) {
      alu_res := io.in.x & io.in.y
    }
    is(Functs.OR2.id.U) {
      alu_res := io.in.x | io.in.y
    }
    is(Functs.XOR2.id.U) {
      alu_res := io.in.x ^ io.in.y
    }
    is(Functs.SLL.id.U) {
      alu_res := io.in.x << shamnt
    }
    is(Functs.SRL.id.U) {
      alu_res := io.in.x >> shamnt
    }
    is(Functs.SRA.id.U) {
      alu_res := (io.in.x.asSInt >> shamnt).asUInt
    }
    is(Functs.SEQ.id.U) {
      alu_res := (io.in.x === io.in.y).asUInt
    }
    is(Functs.SLT.id.U) {
      alu_res := (io.in.x < io.in.y).asUInt
    }
    is(Functs.SLTS.id.U) {
      alu_res := (io.in.x.asSInt < io.in.y.asSInt).asUInt
    }
    is(Functs.MUX.id.U) {
      when(io.in.select) {
        alu_res := io.in.y
      } otherwise {
        alu_res := io.in.x
      }
    }
    is(Functs.ADDC.id.U) {
      alu_res := sum_with_carry(DATA_BITS - 1, 0)
    }
  }

  io.carry_out := sum_with_carry >> (DATA_BITS.U)

  // The mask is used for instructions like slices.
  io.out := alu_res & io.in.mask

}

object StandardALUGenerator extends App {

  new ChiselStage()
    .emitVerilog(new StandardALUComb(4), Array("--target-dir", "gen-dir"))
  // new ChiselStage().emitVerilog(new StandardALU(4))

}
