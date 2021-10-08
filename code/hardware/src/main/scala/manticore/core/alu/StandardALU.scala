package manticore.core.alu


import Chisel._
import chisel3.stage.ChiselStage
import manticore.ISA

import scala.language.implicitConversions



class ALUInterface(DATA_BITS: Int) extends Bundle {

  val in = Input(new Bundle {
    val x = UInt(DATA_BITS.W)
    val y = UInt(DATA_BITS.W)
  })
  val out = Output(UInt(DATA_BITS.W))
  val funct = Input(UInt(4.W))

}

object StandardALU {
  object Functs extends Enumeration {
    type Functs = Value
    val ADD2,
        SUB2,
        MUL2,
        AND2,
        OR2,
        XOR2,
        SLL, // logical left shift
        SRL, // logical right shift (zeros padded to the right)
        SEQ,
        SLTS,
        SLTU,
        SGTS,
        SGTU,
        MUX = Value

  }

}

class StandardALUComb(DATA_BITS: Int) extends Module {
  val io = IO(new ALUInterface(DATA_BITS = DATA_BITS))

  val select_reg = Reg(Bool())

  val Functs = StandardALU.Functs

  switch(io.funct) {

    is(Functs.ADD2.id.U) {
      io.out := io.in.x + io.in.y
    }
    is(Functs.SUB2.id.U) {
      io.out := io.in.x - io.in.y
    }
    is(Functs.MUL2.id.U) {
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
      io.out := io.in.x << io.in.y(log2Ceil(DATA_BITS) - 1, 0)
    }
    is(Functs.SRL.id.U) {
      io.out := io.in.x >> io.in.y(log2Ceil(DATA_BITS) - 1, 0)
    }
    is(Functs.SEQ.id.U) {
      io.out := (io.in.x === io.in.y).asUInt
      select_reg := (io.in.x === io.in.y)
    }
    is(Functs.SLTS.id.U) {
      io.out := (io.in.x.asSInt < io.in.y.asSInt).asUInt
      select_reg := (io.in.x.asSInt < io.in.y.asSInt)
    }
    is(Functs.SLTU.id.U) {
      io.out := (io.in.x < io.in.y).asUInt
      select_reg := (io.in.x < io.in.y)
    }
    is(Functs.SGTS.id.U) {
      io.out := (io.in.x.asSInt > io.in.y.asSInt).asUInt
      select_reg := (io.in.x.asSInt > io.in.y.asSInt)
    }
    is(Functs.SGTU.id.U) {
      io.out := (io.in.x > io.in.y).asUInt
      select_reg := (io.in.x > io.in.y)
    }
    is(Functs.MUX.id.U) {
      when (select_reg) {
        io.out := io.in.y
      } otherwise {
        io.out := io.in.x
      }
    }
  }

}
class StandardALU(DATA_BITS: Int) extends Module {

  val io = IO(new ALUInterface(DATA_BITS = DATA_BITS))

  val select = Wire(Bool())
  val select_reg = Reg(Bool())

  val Functs = StandardALU.Functs

  val funct_reg = Reg(UInt(4.W))
  funct_reg := io.funct

  val res_reg = Reg(UInt(DATA_BITS.W))

  val add_reg = Reg(UInt(DATA_BITS.W))
  add_reg := io.in.x + io.in.y

  val sub_reg = Reg(UInt(DATA_BITS.W))
  sub_reg := io.in.x - io.in.y

  val mul_reg = Reg(UInt(DATA_BITS.W))
  mul_reg := io.in.x * io.in.y

  val and_reg = Reg(UInt(DATA_BITS.W))
  and_reg := io.in.x & io.in.y


  val or_reg = Reg(UInt(DATA_BITS.W))
  or_reg := io.in.x | io.in.y

  val xor_reg = Reg(UInt(DATA_BITS.W))
  xor_reg := io.in.x ^ io.in.y

  val seq_reg = Reg(UInt(DATA_BITS.W))
  when (io.in.x === io.in.y) {
    seq_reg := 1.U
  } otherwise {
    seq_reg := 0.U
  }

  val slts_reg = Reg(UInt(DATA_BITS.W))
  when (io.in.x.asSInt < io.in.y.asSInt) {
    slts_reg := 1.U
  } otherwise {
    slts_reg := 0.U
  }

  val sltu_reg = Reg(UInt(DATA_BITS.W))
  when (io.in.x < io.in.y ) {
    sltu_reg := 1.U
  } otherwise {
    sltu_reg := 0.U
  }

  val sgts_reg = Reg(UInt(DATA_BITS.W))
  when (io.in.x.asSInt > io.in.y.asSInt) {
    sgts_reg := 1.U
  } otherwise {
    sgts_reg := 0.U
  }

  val sgtu_reg = Reg(UInt(DATA_BITS.W))
  when (io.in.x > io.in.y ) {
    sgtu_reg := 1.U
  } otherwise {
    sgtu_reg := 0.U
  }


  val mux_reg = Reg(UInt(DATA_BITS.W))

  when (select_reg) {
    mux_reg := io.in.y
  } otherwise {
    mux_reg := io.in.x
  }

  val sll_reg = Reg(UInt(DATA_BITS.W))
  sll_reg := io.in.x << io.in.y(log2Ceil(DATA_BITS) - 1, 0)

  val srl_reg = Reg(UInt(DATA_BITS.W))
  srl_reg := io.in.x >> io.in.y(log2Ceil(DATA_BITS) - 1, 0)

  switch(funct_reg) {

    is(Functs.ADD2.id.U) {
      res_reg := add_reg
    }
    is(Functs.SUB2.id.U) {
      res_reg := sub_reg
    }
    is(Functs.MUL2.id.U) {
      res_reg := mul_reg
    }
    is(Functs.AND2.id.U) {
      res_reg := and_reg
    }
    is(Functs.OR2.id.U) {
      res_reg := or_reg
    }
    is(Functs.XOR2.id.U) {
      res_reg := xor_reg
    }
    is(Functs.SLL.id.U) {
      res_reg := sll_reg
    }
    is(Functs.SRL.id.U) {
      res_reg := srl_reg
    }
    is(Functs.SEQ.id.U) {
      res_reg := seq_reg
      select_reg := seq_reg
    }
    is(Functs.SLTS.id.U) {
      res_reg := slts_reg
      select_reg := slts_reg
    }
    is(Functs.SLTU.id.U) {
      res_reg := sltu_reg
      select_reg := sltu_reg
    }
    is(Functs.SGTS.id.U) {
      res_reg := sgts_reg
      select_reg := sgts_reg
    }
    is(Functs.SGTU.id.U) {
      res_reg := sgtu_reg
      select_reg := sgtu_reg
    }
    is(Functs.MUX.id.U) {
      res_reg := mux_reg
    }
  }
  io.out := res_reg
}

object StandardALUGenerator extends App{

  new ChiselStage().emitVerilog(new StandardALUComb(4), Array("--target-dir", "gen-dir"))
  // new ChiselStage().emitVerilog(new StandardALU(4))

}