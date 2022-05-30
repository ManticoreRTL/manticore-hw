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
import manticore.machine.core.alu.CustomAlu
import manticore.machine.core.alu.CustomFunctionConfigInterface
import manticore.machine.core.alu.StandardALU
import manticore.machine.core.alu.StandardALUComb
import manticore.machine.memory.CacheCommand
import manticore.machine.memory.CacheConfig
import scala.util.Random
import manticore.machine.ManticoreFullISA

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
    val opcode    = new Decode.OpcodePipe(config.numFuncts)
    val data      = UInt(config.DataBits.W)
    val result    = UInt(config.DataBits.W)
    val rd        = UInt(config.IdBits.W)
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
  val pipe_in    = Input(new PipeIn(config))
  val regs_in    = Input(new ALUInput(config.DataBits))
  val carry_in   = Input(UInt(1.W))
  val pipe_out   = Output(new PipeOut(config))
  val debug_time = Input(UInt(64.W))

  val carry_rd  = Output(UInt(log2Ceil(config.CarryCount).W))
  val carry_wen = Output(Bool())
  val carry_din = Output(UInt(1.W))

  val lutdata_waddr = Output(UInt(config.FunctBits.W))
  val lutdata_wen = Output(Bool())
  val lutdata_din = Input(Vec(config.numFuncts, UInt(config.DataBits.W)))
}

class ExecuteBase(
    config: ISA,
    debug_tag: String = "UNTAGGED",
    debug_enable: Boolean = false
) extends Module {
  val io = IO(new ExecuteInterface(config))
}

class ExecuteComb(
    config: ISA,
    equations: Seq[Seq[BigInt]],
    debug_tag: String = "UNTAGGED",
    debug_enable: Boolean = false
) extends ExecuteBase(config, debug_tag, debug_enable) {

  val custom_alu = Module(new CustomAlu(config.DataBits, config.FunctBits - 1, config.LutArity, equations))

  val standard_alu = Module(new StandardALUComb(config.DataBits))

  // Custom ALU inputs
  custom_alu.io.rsx := Vec(io.regs_in.rs1, io.regs_in.rs2, io.regs_in.rs3, io.regs_in.rs4)
  for (i <- Range(0, config.numFuncts / 2)) {
    // ALL luts are configured in parallel. It is not possible to configure them one by one.
    // This avoids the use of (conf.FunctBits * config.DataBits) LUTs to perform an AND on this
    // large fan-out path.
    custom_alu.io.config(i).writeEnable := io.pipe_in.opcode.configure_lut(i)
    custom_alu.io.config(i).loadData := io.lutdata_din(i)
  }
  custom_alu.io.selector := io.pipe_in.funct

  when(io.pipe_in.opcode.arith | io.pipe_in.opcode.expect) {
    standard_alu.io.in.y := io.regs_in.rs2
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

  standard_alu.io.in.select := io.regs_in.rs3

  standard_alu.io.in.carry := io.carry_in

  when(io.pipe_in.opcode.set || io.pipe_in.opcode.send) {
    standard_alu.io.in.x := 0.U
  } otherwise {
    standard_alu.io.in.x := io.regs_in.rs1
  }

  when(io.pipe_in.opcode.cust) {
    io.pipe_out.result := custom_alu.io.out
  } otherwise {
    io.pipe_out.result := standard_alu.io.out
  }

  io.pipe_out.opcode    := io.pipe_in.opcode
  io.pipe_out.data      := io.regs_in.rs2
  io.pipe_out.rd        := io.pipe_in.rd
  io.pipe_out.immediate := io.pipe_in.immediate
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

  io.lutdata_wen := io.pipe_in.opcode.set_lut_data
  io.lutdata_waddr := io.pipe_in.funct

  val pred_reg = Reg(Bool())

  // enable/disable predicate
  when(io.pipe_in.opcode.predicate) {
    pred_reg := io.regs_in.rs1 === 1.U
  }

  io.pipe_out.pred := pred_reg

  if (config.WithGlobalMemory) {
    val gmem_if_reg = Reg(new GlobalMemoryInterface(config))
    gmem_if_reg.address := io.regs_in.rs2 ## io.regs_in.rs3 ## io.regs_in.rs4
    when(io.pipe_in.opcode.gload) {
      gmem_if_reg.command := CacheCommand.Read
    }.elsewhen(io.pipe_in.opcode.gstore) {
      gmem_if_reg.command := CacheCommand.Write
    }
    gmem_if_reg.start := (io.pipe_in.opcode.gstore && pred_reg) | io.pipe_in.opcode.gload
    gmem_if_reg.wdata := io.regs_in.rs1
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
        io.pipe_in.opcode.set.toUInt +
        io.pipe_in.opcode.set_lut_data.toUInt +
        io.pipe_in.opcode.configure_lut(0) // This is a replicated signal, so just 1 of them suffices.

    when(num_decoded > 1.U) {
      dprintf("\tERROR multiple decoded operations (%d)!\n", num_decoded)
    }

  }

}
object ExecuteGen extends App {

  val rgen = Random

  val equations = Seq.fill(1 << ManticoreFullISA.FunctBits) {
    Seq.fill(ManticoreFullISA.DataBits) { BigInt(rgen.nextInt(1 << 16)) }
  }

  new ChiselStage().emitVerilog(new ExecuteComb(ManticoreBaseISA, equations))

}
