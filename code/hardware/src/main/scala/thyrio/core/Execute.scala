/**
 * Copyright 2021 Mahyar Emami
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */
package thyrio.core


import Chisel._
import chisel3.stage.ChiselStage
import thyrio.{ISA, ThyrioISA}
import thyrio.core.ExecuteInterface.{LocalMemoryInterface, PipeIn, PipeOut}
import thyrio.core.alu.{ALUInput, CustomALU, CustomALUComb, StandardALU, StandardALUComb}

import scala.util.Random


object ExecuteInterface {
  type OpcodePipe = Decode.OpcodePipe
  type PipeIn = Decode.PipeOut
  class PipeOut(config: ISA) extends Bundle {
    val opcode = new Decode.OpcodePipe
    val data = UInt(config.DATA_BITS.W)
    val result = UInt(config.DATA_BITS.W)
    val rd = UInt(config.ID_BITS.W)
    val immediate = UInt(config.DATA_BITS.W)
  }

  class LocalMemoryInterface(config: ISA) extends Bundle {
    // read interface to the local memory
    val address = Output(UInt(11.W))
    val dout = Input(UInt(config.DATA_BITS.W))
    val we = Output(Bool()) // should be false.B
  }



}

class ExecuteInterface(config: ISA) extends Bundle {
  val pipe_in: PipeIn = Input(new PipeIn(config))
  val regs_in = Input(new ALUInput(config.DATA_BITS))
  val pipe_out = Output(new PipeOut(config))
//  val lmem_if = new LocalMemoryInterface(config)

}


class ExecuteBase (config: ISA, EQUATIONS: Seq[Seq[Int]]) extends  Module {
  val io = IO(new ExecuteInterface(config))
}
class ExecutePiped (config: ISA, EQUATIONS: Seq[Seq[Int]]) extends ExecuteBase(config, EQUATIONS) {


  val custom_alu = Module(new CustomALU(config.DATA_BITS, EQUATIONS))

  val standard_alu = Module(new StandardALU(config.DATA_BITS))


  // Custom ALU connections
  custom_alu.io.in.x := io.regs_in.x
  custom_alu.io.in.y := io.regs_in.y
  custom_alu.io.in.u := io.regs_in.u
  custom_alu.io.in.v := io.regs_in.v
  custom_alu.io.funct := io.pipe_in.funct



  when(io.pipe_in.opcode.arith | io.pipe_in.opcode.expect) {
    standard_alu.io.in.y := io.regs_in.y
    when(io.pipe_in.opcode.expect) {
      standard_alu.io.funct := StandardALU.Functs.SEQ.id.U
    } otherwise {
      standard_alu.io.funct := io.pipe_in.funct
    }
  } otherwise {
    standard_alu.io.funct := StandardALU.Functs.ADD2.id.U
//    require(config.Immediate.length >= config.DATA_BITS)
//    when(io.pipe_in.opcode.set || io.pipe_in.opcode.send) {
    standard_alu.io.in.y := io.pipe_in.immediate
//    } otherwise {
//      standard_alu.io.in.y := io.pipe_in.immediate(config.ID_BITS - 1, 0)
//    }

  }

  when(io.pipe_in.opcode.set || io.pipe_in.opcode.send) {
    standard_alu.io.in.x := 0.U
  } otherwise {
    standard_alu.io.in.x := io.regs_in.x
  }


  // taps
  def makeTaps[T <: Data](source: T, count: Int)(cons: => T) = {
    require(count >= 1)
    val regs = Reg(Vec(count, cons))
    regs(0) := source
    regs.sliding(2, 1).foreach {
      case Seq(first, second) => second := first
    }
    regs.last
  }

  val data_tap = makeTaps(io.regs_in.y, 2){
    UInt(config.DATA_BITS.W)
  }

  //  val opcode_taps = Reg(Vec(2, new OpcodePipe))
  //  opcode_taps(0) := io.pipe_in.opcode
  //  opcode_taps(1) := opcode_taps(0)
  val opcode_tap = makeTaps(io.pipe_in.opcode, 2){
    new Decode.OpcodePipe
  }
  val rd_tap = makeTaps(io.pipe_in.rd, 2) {
    UInt(config.DATA_BITS.W)
  }

  val imm_tap = makeTaps(io.pipe_in.immediate, 2) {
    UInt(config.DATA_BITS.W)
  }

  val pipe_out = Reg(new PipeOut(config))

  when(opcode_tap.cust0) {
    pipe_out.result := custom_alu.io.out
  } otherwise {
    pipe_out.result := standard_alu.io.out
  }

  pipe_out.opcode := opcode_tap
  pipe_out.data := data_tap
  pipe_out.rd := rd_tap
  pipe_out.immediate := imm_tap

  io.pipe_out := pipe_out


//  // start the read early,
//  io.lmem_if.address := standard_alu.io.out
//  pipe_out.load_data := io.lmem_if.dout
//  // read only, disable writing
//  io.lmem_if.we := false.B

}


class ExecuteComb (config: ISA, EQUATIONS: Seq[Seq[Int]]) extends ExecuteBase(config, EQUATIONS)  {



  val custom_alu = Module(new CustomALUComb(config.DATA_BITS, EQUATIONS))

  val standard_alu = Module(new StandardALUComb(config.DATA_BITS))


  // Custom ALU connections
  custom_alu.io.in.x := io.regs_in.x
  custom_alu.io.in.y := io.regs_in.y
  custom_alu.io.in.u := io.regs_in.u
  custom_alu.io.in.v := io.regs_in.v
  custom_alu.io.funct := io.pipe_in.funct



  when(io.pipe_in.opcode.arith | io.pipe_in.opcode.expect) {
    standard_alu.io.in.y := io.regs_in.y
    when(io.pipe_in.opcode.expect) {
      standard_alu.io.funct := StandardALU.Functs.SEQ.id.U
    } otherwise {
      standard_alu.io.funct := io.pipe_in.funct
    }
  } otherwise {
    standard_alu.io.funct := StandardALU.Functs.ADD2.id.U
    //    require(config.Immediate.length >= config.DATA_BITS)
    //    when(io.pipe_in.opcode.set || io.pipe_in.opcode.send) {
    standard_alu.io.in.y := io.pipe_in.immediate
    //    } otherwise {
    //      standard_alu.io.in.y := io.pipe_in.immediate(config.ID_BITS - 1, 0)
    //    }

  }

  when(io.pipe_in.opcode.set || io.pipe_in.opcode.send) {
    standard_alu.io.in.x := 0.U
  } otherwise {
    standard_alu.io.in.x := io.regs_in.x
  }


  val pipe_out = Reg(new PipeOut(config))

  when(io.pipe_in.opcode.cust0) {
    pipe_out.result := custom_alu.io.out
  } otherwise {
    pipe_out.result := standard_alu.io.out
  }
  val tmp = Reg(UInt(config.DATA_BITS.W))

  tmp := io.regs_in.y

  pipe_out.opcode := io.pipe_in.opcode
  pipe_out.data := io.regs_in.y
  pipe_out.rd := io.pipe_in.rd
  pipe_out.immediate := io.pipe_in.immediate

  io.pipe_out := pipe_out


}
object ExecuteGen extends App {
  val rdgen = Random

  // create random LUT equations
  val equations = Seq.fill(32)(Seq.fill(16)(rdgen.nextInt(1 << 16)))


  new ChiselStage().emitVerilog(new ExecutePiped(ThyrioISA, equations))
  new ChiselStage().emitVerilog(new ExecuteComb(ThyrioISA, equations))
}