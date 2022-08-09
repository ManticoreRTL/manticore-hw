package manticore.machine.core.alu

import Chisel._
import chisel3.stage.ChiselStage
import chisel3.util.HasBlackBoxResource
import manticore.machine.ISA
import manticore.machine.ManticoreBaseISA

import scala.language.implicitConversions

class ALUInterface(DATA_BITS: Int) extends Bundle {

  val in = Input(new Bundle {
    val x      = UInt(DATA_BITS.W)
    val y      = UInt(DATA_BITS.W)
    val carry  = UInt(1.W)
    val select = Bool()
    val mask   = UInt(DATA_BITS.W)
  })
  val out       = Output(UInt(DATA_BITS.W))
  val mul_out   = Output(UInt((2 * DATA_BITS).W))
  val carry_out = Output(UInt(1.W))
  val funct     = Input(UInt(4.W))

  val valid_in  = Input(Bool()) // Asserted only for MUL and MULH
  val valid_out = Output(Bool())
}

class StandardALUComb(DATA_BITS: Int) extends Module {
  val io = IO(new ALUInterface(DATA_BITS = DATA_BITS))

  class AluDsp48(DATA_BITS: Int) extends BlackBox with HasBlackBoxResource {
    require(DATA_BITS <= 16)

    val io = IO(new Bundle {
      val clock    = Input(Clock())
      val in0      = Input(UInt(DATA_BITS.W))
      val in1      = Input(UInt(DATA_BITS.W))
      val in2      = Input(UInt(DATA_BITS.W))
      val carryin  = Input(UInt(1.W))
      val opmode   = Input(UInt(9.W))
      val alumode  = Input(UInt(4.W))
      val setinst  = Input(UInt(2.W))
      val out      = Output(UInt(DATA_BITS.W))
      val mul_out  = Output(UInt((2 * DATA_BITS).W))
      val carryout = Output(UInt(1.W))
      val valid_in = Input(Bool())
      val valid_out = Output(Bool())
    })

    addResource("/verilog/AluDsp48/AluDsp48.v")
  }

  def RegNext2[T <: Data](src: T): T = {
    RegNext(RegNext(src))
  }

  val shamnt = Wire(UInt(log2Ceil(DATA_BITS).W))
  // val sum_res        = Wire(UInt((DATA_BITS + 1).W))
  // val sum_with_carry = Wire(UInt((DATA_BITS + 1).W))

  val dsp         = Module(new AluDsp48(DATA_BITS))
  val opmode      = Wire(UInt(9.W))
  val alumode     = Wire(UInt(4.W))
  val setinst     = Wire(UInt(2.W))
  val without_dsp = Wire(Bool())

  val alu_res   = Wire(UInt(DATA_BITS.W))
  val shift_out = Wire(UInt(DATA_BITS.W))
  // val carryout = Wire(UInt(1.W))

  without_dsp := (io.funct === ISA.Functs.SLL.id.U ||
    io.funct === ISA.Functs.SRL.id.U ||
    io.funct === ISA.Functs.SRA.id.U ||
    io.funct === ISA.Functs.MUX.id.U).asBool

  // def widened(w: UInt): UInt = {
  //   val as_wider = Wire(UInt((DATA_BITS + 1).W))
  //   as_wider := w
  //   as_wider
  // }

  shamnt := io.in.y(log2Ceil(DATA_BITS) - 1, 0)
  // sum_res        := widened(io.in.x) + widened(io.in.y)
  // sum_with_carry := sum_res + widened(io.in.carry)

  //                 | OPMODE[8:0] | ALUMODE[3:0] | Notes
  //   --------------|-------------|--------------|------------------------------
  //                 |  876543210  |     3210     |
  //                 |  vvvvvvvvv  |     vvvv     |
  //   and(b,c)      |  000110011  |     1100     | ug579 pg 29, 38 // W = 0, X = A:B, Y = 0, Z = C // P = X AND Z
  //   or(b,c)       |  000111011  |     1100     | ug579 pg 29, 38 // W = 0, X = A:B, Y = 1, Z = C // P = X OR Z
  //   xor(b,c)      |  000110011  |     0100     | ug579 pg 29, 38 // W = 0, X = A:B, Y = 0, Z = C // P = X XOR Z
  //   add(b,c)      |  000110011  |     0000     | ug579 pg 30, 32 // W = 0, X = A:B, Y = 0, Z = C // P = Z + W + X + Y + CIN
  //   addc(b,c,cin) |  000110011  |     0000     | ug579 pg 30, 32 // W = 0, X = A:B, Y = 0, Z = C // P = Z + W + X + Y + CIN
  //   sub(b,c)      |  000110011  |     0011     | ug579 pg 30, 32 // W = 0, X = A:B, Y = 0, Z = C // P = Z - (W + X + Y + CIN)
  //   mul(a,b)      |  000000101  |     0000     | ug579 pg 29     // W = 0, X = M  , Y = M, Z = 0 // P = X * Y
  //                 |             |              | (ALUMODE does not matter and we set it to ADD)
  //   seq(b,c)      |  000110011  |     0011     | // Use subtraction. External circuit detects comparison result.
  //   sltu(b,c)     |  000110011  |     0011     | // Use subtraction. External circuit detects comparison result.
  //   slts(b,c)     |  000110011  |     0011     | // Use subtraction. External circuit detects comparison result.

  switch(io.funct) {
    // Most of the calculation are now done with DSP blocks
    is(ISA.Functs.ADD2.id.U) {
      opmode  := "b000110011".asUInt(9.W)
      alumode := "b0000".asUInt(4.W)
      setinst := 0.asUInt(2.W)
      // alu_res := sum_res(DATA_BITS - 1, 0)
    }
    is(ISA.Functs.SUB2.id.U) {
      opmode  := "b000110011".asUInt(9.W)
      alumode := "b0011".asUInt(4.W)
      setinst := 0.asUInt(2.W)
      // alu_res := io.in.x - io.in.y
    }
    // The multiplier used to be handled through a parallel path to Execute and
    // Memory stages, but as the other operations are now calculated using DSPs,
    // we moved the multiplier back to here in the hope of reducing routing congestions.
    // Note that only MUL and MULH operations take 3 cycles to execute in DSP, while
    // others have 2 cycle latency.
    is(ISA.Functs.MUL2.id.U) {
      opmode  := "b000000101".asUInt(9.W)
      alumode := "b0000".asUInt(4.W)
      setinst := 0.asUInt(2.W)
    }
    is(ISA.Functs.MUL2H.id.U) {
      opmode  := "b000000101".asUInt(9.W)
      alumode := "b0000".asUInt(4.W)
      setinst := 0.asUInt(2.W)
    }
    is(ISA.Functs.AND2.id.U) {
      opmode  := "b000110011".asUInt(9.W)
      alumode := "b1100".asUInt(4.W)
      setinst := 0.asUInt(2.W)
      // alu_res := io.in.x & io.in.y
    }
    is(ISA.Functs.OR2.id.U) {
      opmode  := "b000111011".asUInt(9.W)
      alumode := "b1100".asUInt(4.W)
      setinst := 0.asUInt(2.W)
      // alu_res := io.in.x | io.in.y
    }
    is(ISA.Functs.XOR2.id.U) {
      opmode  := "b000110011".asUInt(9.W)
      alumode := "b0100".asUInt(4.W)
      setinst := 0.asUInt(2.W)
      // alu_res := io.in.x ^ io.in.y
    }
    is(ISA.Functs.SEQ.id.U) {
      opmode  := "b000110011".asUInt(9.W)
      alumode := "b0011".asUInt(4.W)
      setinst := 1.asUInt(2.W)
      // alu_res := (io.in.x === io.in.y).asUInt
    }
    is(ISA.Functs.SLTU.id.U) {
      opmode  := "b000110011".asUInt(9.W)
      alumode := "b0011".asUInt(4.W)
      setinst := 2.asUInt(2.W)
      // alu_res := (io.in.x < io.in.y).asUInt
    }
    is(ISA.Functs.SLTS.id.U) {
      opmode  := "b000110011".asUInt(9.W)
      alumode := "b0011".asUInt(4.W)
      setinst := 3.asUInt(2.W)
      // alu_res := (io.in.x.asSInt < io.in.y.asSInt).asUInt
    }
    is(ISA.Functs.ADDC.id.U) {
      opmode  := "b000110011".asUInt(9.W)
      alumode := "b0000".asUInt(4.W)
      setinst := 0.asUInt(2.W)
      // alu_res := sum_with_carry(DATA_BITS - 1, 0)
    }

    // The shift and mux operations are calculated without DSP
    is(ISA.Functs.SLL.id.U) {
      shift_out := io.in.x << shamnt
    }
    is(ISA.Functs.SRL.id.U) {
      shift_out := io.in.x >> shamnt
    }
    is(ISA.Functs.SRA.id.U) {
      shift_out := (io.in.x.asSInt >> shamnt).asUInt
    }
    is(ISA.Functs.MUX.id.U) {
      when(io.in.select) {
        shift_out := io.in.y
      } otherwise {
        shift_out := io.in.x
      }
    }
  }

  dsp.io.clock   := clock
  dsp.io.in0     := io.in.x
  dsp.io.in1     := io.in.y
  dsp.io.in2     := io.in.x // Only used for MUL
  dsp.io.carryin := io.in.carry
  dsp.io.opmode  := opmode
  dsp.io.alumode := alumode
  dsp.io.setinst := setinst
  dsp.io.valid_in := io.valid_in

  when(!RegNext2(without_dsp)) {
    alu_res := dsp.io.out
  } otherwise {
    alu_res := RegNext2(shift_out)
  }

  // The mask is used for instructions like slices.
  io.out       := alu_res & RegNext2(io.in.mask)
  io.mul_out   := dsp.io.mul_out
  io.carry_out := dsp.io.carryout
  io.valid_out := dsp.io.valid_out

  // io.carry_out := RegNext(sum_with_carry >> (DATA_BITS.U))
  // io.out := RegNext(alu_res & io.in.mask)

}

object StandardALUGenerator extends App {

  new ChiselStage()
    .emitVerilog(new StandardALUComb(4), Array("--target-dir", "gen-dir"))
  // new ChiselStage().emitVerilog(new StandardALU(4))

}
