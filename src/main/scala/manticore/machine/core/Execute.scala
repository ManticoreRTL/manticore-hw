/** Copyright 2021 Mahyar Emami
  *
  * Permission is hereby granted, free of charge, to any person obtaining a copy
  * of this software and associated documentation files (the "Software"), to
  * deal in the Software without restriction, including without limitation the
  * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  * sell copies of the Software, and to permit persons to whom the Software is
  * furnished to do so, subject to the following conditions:
  *
  * The above copyright notice and this permission notice shall be included in
  * all copies or substantial portions of the Software.
  *
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  * IN THE SOFTWARE.
  */
package manticore.machine.core

import Chisel._
import chisel3.stage.ChiselStage
import manticore.machine.ISA
import manticore.machine.ManticoreBaseISA
import manticore.machine.core.ExecuteInterface.GlobalMemoryInterface
import manticore.machine.core.ExecuteInterface.PipeIn
import manticore.machine.core.ExecuteInterface.PipeOut
import manticore.machine.core.alu.ALUInput
import manticore.machine.core.alu.CustomALU
import manticore.machine.core.alu.CustomALUComb
import manticore.machine.core.alu.StandardALU
import manticore.machine.core.alu.StandardALUComb
import manticore.machine.memory.CacheCommand
import manticore.machine.memory.CacheConfig

import scala.util.Random

// class ForwardBundle(dataWidth: Int, addressWidth: Int) extends Bundle {
//   val value   = UInt(dataWidth.W)
//   val address = UInt(addressWidth.W)
//   val en      = Bool()
// }
case class ForwardingTuple(value: UInt, address: UInt, en: Bool)

object ForwardPath {
  def apply(
      value: UInt,
      address: UInt,
      paths: Seq[ForwardingTuple]
  ): UInt = {

    require(paths.forall(_.value.getWidth == value.getWidth))
    require(paths.forall(_.address.getWidth == address.getWidth))
    if (paths.isEmpty) {
      // no forwarding
      value
    } else {
      paths.reverse.foldLeft(value) { case (prev, fpath) =>
        val temp = Wire(value.cloneType)
        temp := Mux(fpath.address === address && fpath.en, fpath.value, prev)
        temp

      }
    }
  }
}


object ExecuteInterface {
  type OpcodePipe = Decode.OpcodePipe
  type PipeIn     = Decode.PipeOut
  class PipeOut(config: ISA) extends Bundle {
    val opcode    = new Decode.OpcodePipe
    val data      = UInt(config.DataBits.W)
    val result    = UInt(config.DataBits.W)
    val rd        = UInt(config.IdBits.W)
    // val carry_rd  = UInt(log2Ceil(config.CarryCount).W)
    // val carry_wen = Bool()
    // val carry_din = UInt(1.W)
    val immediate = UInt(config.DataBits.W)
    val gmem      = new GlobalMemoryInterface(config)
    val pred      = Bool()
  }

  class GlobalMemoryInterface(config: ISA) extends Bundle {
    val address = UInt((config.IdBits * 3).W)
    val command = CacheCommand.Type()
    val start   = Bool()
    require(config.DataBits == CacheConfig.DataBits)
    val wdata = UInt(config.DataBits.W)
  }

}



class ExecuteInterface(config: ISA) extends Bundle {
  val pipe_in: PipeIn = Input(new PipeIn(config))
  val regs_in         = Input(new ALUInput(config.DataBits))
  val carry_in        = Input(UInt(1.W))
  val pipe_out        = Output(new PipeOut(config))
  val debug_time      = Input(UInt(64.W))

  val carry_rd  = UInt(log2Ceil(config.CarryCount).W)
  val carry_wen = Bool()
  val carry_din = UInt(1.W)
}

class ExecuteBase(
    config: ISA,
    equations: Seq[Seq[Int]],
    debug_tag: String = "UNTAGGED",
    debug_enable: Boolean = false
) extends Module {
  val io = IO(new ExecuteInterface(config))
}

class ExecuteComb(
    config: ISA,
    equation: Seq[Seq[Int]],
    debug_tag: String = "UNTAGGED",
    debug_enable: Boolean = false
) extends ExecuteBase(config, equation, debug_tag, debug_enable) {

  val custom_alu = Module(new CustomALUComb(config.DataBits, equation))

  val standard_alu = Module(new StandardALUComb(config.DataBits))

  // Custom ALU connections
  custom_alu.io.in.x  := io.regs_in.x
  custom_alu.io.in.y  := io.regs_in.y
  custom_alu.io.in.u  := io.regs_in.u
  custom_alu.io.in.v  := io.regs_in.v
  custom_alu.io.funct := io.pipe_in.funct

  when(io.pipe_in.opcode.arith | io.pipe_in.opcode.expect) {
    // standard_alu.io.in.y := io.regs_in.y
    standard_alu.io.in.y := io.regs_in.y
    when(io.pipe_in.opcode.expect) {
      standard_alu.io.funct := StandardALU.Functs.SEQ.id.U
    } otherwise {
      standard_alu.io.funct := io.pipe_in.funct
    }
  } otherwise {
    standard_alu.io.funct := StandardALU.Functs.ADD2.id.U
    // with non-arith instructions, funct can be any value, including the
    // funct for Mux (which is stateful) so we should set it to zero to
    // ensure no stateful ALU operations are performed.
    standard_alu.io.in.y := io.pipe_in.immediate
  }

  standard_alu.io.in.select := io.regs_in.u

  standard_alu.io.in.carry := io.carry_in

  when(io.pipe_in.opcode.set || io.pipe_in.opcode.send) {
    standard_alu.io.in.x := 0.U
  } otherwise {
    standard_alu.io.in.x := io.regs_in.x
  }

  val pipe_out = Reg(new PipeOut(config))

  when(io.pipe_in.opcode.cust) {
    pipe_out.result := custom_alu.io.out
  } otherwise {
    pipe_out.result := standard_alu.io.out
  }

  pipe_out.opcode    := io.pipe_in.opcode
  pipe_out.data      := io.regs_in.y
  pipe_out.rd        := io.pipe_in.rd
  pipe_out.immediate := io.pipe_in.immediate
  when(io.pipe_in.opcode.set_carry) {
    io.carry_rd := io.pipe_in.rd
  } otherwise {
    // notice that rs4 needs to be registered before given to the output pipe
    val rs4_reg = Reg(UInt(log2Ceil(config.CarryCount).W))
    rs4_reg           := io.pipe_in.rs4
    io.carry_rd       := rs4_reg
  }
  when(io.pipe_in.opcode.set_carry) {
    io.carry_din := io.pipe_in.immediate(0)
  } otherwise {
    io.carry_din := standard_alu.io.carry_out
  }
  io.carry_wen :=
    (io.pipe_in.opcode.arith & (io.pipe_in.funct === StandardALU.Functs.ADDC.id.U)) | (io.pipe_in.opcode.set_carry)
  io.pipe_out := pipe_out

  val pred_reg = Reg(Bool())

  // enable/disable predicate
  when(io.pipe_in.opcode.predicate) {
    pred_reg := io.regs_in.x === 1.U
  }

  io.pipe_out.pred := pred_reg

  if (config.WithGlobalMemory) {
    val gmem_if_reg = Reg(new GlobalMemoryInterface(config))
    gmem_if_reg.address := io.regs_in.y ## io.regs_in.u ## io.regs_in.v
    when(io.pipe_in.opcode.gload) {
      gmem_if_reg.command := CacheCommand.Read
    }.elsewhen(io.pipe_in.opcode.gstore) {
      gmem_if_reg.command := CacheCommand.Write
    }
    gmem_if_reg.start := (io.pipe_in.opcode.gstore && pred_reg) | io.pipe_in.opcode.gload
    gmem_if_reg.wdata := io.regs_in.x
    io.pipe_out.gmem  := gmem_if_reg
  }

  // print what's happenning
  if (debug_enable) {
    def dprintf(fmt: String, data: Bits*) =
      if (debug_enable)
        printf(s"[%d : ${debug_tag}] " + fmt, (io.debug_time +: data): _*)

    // if there are multiple operations, emit and error message
    val num_decoded = Wire(UInt(32.W))
    num_decoded :=
      io.pipe_in.opcode.arith.toUInt +
        io.pipe_in.opcode.cust.toUInt +
        io.pipe_in.opcode.expect.toUInt +
        io.pipe_in.opcode.gload.toUInt +
        io.pipe_in.opcode.gstore.toUInt +
        io.pipe_in.opcode.lstore.toUInt +
        io.pipe_in.opcode.lload.toUInt +
        io.pipe_in.opcode.nop.toUInt +
        io.pipe_in.opcode.predicate.toUInt +
        io.pipe_in.opcode.send.toUInt +
        io.pipe_in.opcode.set.toUInt

    when(num_decoded > 1.U) {
      dprintf("\tERROR multiple decoded operations (%d)!\n", num_decoded)
    }

  }

}
object ExecuteGen extends App {

  val rdgen = Random

  // create random LUT equations
  val equations = Seq.fill(32)(Seq.fill(16)(rdgen.nextInt(1 << 16)))

  new ChiselStage().emitVerilog(new ExecuteComb(ManticoreBaseISA, equations))
}
