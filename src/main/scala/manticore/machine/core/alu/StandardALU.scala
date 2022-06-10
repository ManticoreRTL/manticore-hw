package manticore.machine.core.alu

import Chisel._
import chisel3.stage.ChiselStage
import manticore.machine.ISA

import scala.language.implicitConversions
import manticore.machine.ManticoreBaseISA

class ALUInterface(DATA_BITS: Int) extends Bundle {

  val in = Input(new Bundle {
    val x      = UInt(DATA_BITS.W)
    val y      = UInt(DATA_BITS.W)
    val carry  = UInt(1.W)
    val select = Bool()
    val mask   = UInt(DATA_BITS.W)
  })
  val out       = Output(UInt(DATA_BITS.W))
  val carry_out = Output(UInt(1.W))
  val funct     = Input(UInt(4.W))
}

class StandardALUComb(DATA_BITS: Int) extends Module {
  val io = IO(new ALUInterface(DATA_BITS = DATA_BITS))

  val shamnt         = Wire(UInt(log2Ceil(DATA_BITS).W))
  val sum_res        = Wire(UInt((DATA_BITS + 1).W))
  val sum_with_carry = Wire(UInt((DATA_BITS + 1).W))
  val alu_res        = Wire(UInt(DATA_BITS.W))

  def widened(w: UInt): UInt = {
    val as_wider = Wire(UInt((DATA_BITS + 1).W))
    as_wider := w
    as_wider
  }

  shamnt         := io.in.y(log2Ceil(DATA_BITS) - 1, 0)
  sum_res        := widened(io.in.x) + widened(io.in.y)
  sum_with_carry := sum_res + widened(io.in.carry)

  switch(io.funct) {
    is(ISA.Functs.ADD2.id.U) {
      alu_res := sum_res(DATA_BITS - 1, 0)
    }
    is(ISA.Functs.SUB2.id.U) {
      alu_res := io.in.x - io.in.y
    }
    // The multiplier is handled through a parallel path to the Execute and Memory
    // stages. We leave this code here commented out for documentation purposes.
    // Note that the output of the standard ALU will be undefined if it received
    // the Functs.MUL2 command, but this is fine as the external multiplier's output
    // is written back to the regsiter file in any case.
    // is(Functs.MUL2.id.U) {
    //   alu_res := io.in.x * io.in.y
    // }
    is(ISA.Functs.AND2.id.U) {
      alu_res := io.in.x & io.in.y
    }
    is(ISA.Functs.OR2.id.U) {
      alu_res := io.in.x | io.in.y
    }
    is(ISA.Functs.XOR2.id.U) {
      alu_res := io.in.x ^ io.in.y
    }
    is(ISA.Functs.SLL.id.U) {
      alu_res := io.in.x << shamnt
    }
    is(ISA.Functs.SRL.id.U) {
      alu_res := io.in.x >> shamnt
    }
    is(ISA.Functs.SRA.id.U) {
      alu_res := (io.in.x.asSInt >> shamnt).asUInt
    }
    is(ISA.Functs.SEQ.id.U) {
      alu_res := (io.in.x === io.in.y).asUInt
    }
    is(ISA.Functs.SLTU.id.U) {
      alu_res := (io.in.x < io.in.y).asUInt
    }
    is(ISA.Functs.SLTS.id.U) {
      alu_res := (io.in.x.asSInt < io.in.y.asSInt).asUInt
    }
    is(ISA.Functs.MUX.id.U) {
      when(io.in.select) {
        alu_res := io.in.y
      } otherwise {
        alu_res := io.in.x
      }
    }
    is(ISA.Functs.ADDC.id.U) {
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
