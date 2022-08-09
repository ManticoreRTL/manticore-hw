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
import manticore.machine.ManticoreFullISA
import manticore.machine.core.ExecuteInterface.GlobalMemoryInterface
import manticore.machine.core.ExecuteInterface.PipeIn
import manticore.machine.core.ExecuteInterface.PipeOut
import manticore.machine.core.alu.ALUInput
import manticore.machine.core.alu.CustomAlu
import manticore.machine.core.alu.CustomFunctionConfigInterface
import manticore.machine.core.alu.StandardALUComb
import manticore.machine.memory.CacheCommand
import manticore.machine.memory.CacheConfig

import scala.util.Random

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
    val opcode     = new Decode.OpcodePipe(config.numFuncts)
    val data       = UInt(config.DataBits.W)
    val result     = UInt(config.DataBits.W)
    val result_mul = UInt((2 * config.DataBits).W)
    val rd         = UInt(config.IdBits.W)
    val immediate  = UInt(config.DataBits.W)
    val gmem       = new GlobalMemoryInterface(config)
    val pred       = Bool()
  }

  class GlobalMemoryInterface(config: ISA) extends Bundle {
    val address = UInt((config.IdBits * 3).W)
    val command = CacheCommand.Type()
    val start   = Bool()
    require(config.DataBits == CacheConfig.DataBits)
    val wdata = UInt(config.DataBits.W)
  }
}

class ExecuteInterface(
    config: ISA
) extends Bundle {
  val pipe_in    = Input(new PipeIn(config))
  val regs_in    = Input(new ALUInput(config.DataBits))
  val carry_in   = Input(UInt(1.W))
  val pipe_out   = Output(new PipeOut(config))
  val debug_time = Input(UInt(64.W))

  val carry_rd  = Output(UInt(log2Ceil(config.CarryCount).W))
  val carry_wen = Output(Bool())
  val carry_din = Output(UInt(1.W))

  val lutdata_din = Input(Vec(config.numFuncts, UInt(config.DataBits.W)))

  val valid_in  = Input(Bool()) // Asserted only for MUL and MULH
  val valid_out = Output(Bool())
}

class ExecuteComb(
    config: ISA,
    equations: Seq[Seq[BigInt]],
    debug_tag: String = "UNTAGGED",
    debug_enable: Boolean = false,
    custom_alu_enable: Boolean = true
) extends Module {

  def RegNext2[T <: Data](src: T): T = {
    RegNext(RegNext(src))
  }

  def RegNext3[T <: Data](src: T): T = {
    RegNext(RegNext(RegNext(src)))
  }

  def RegNext4[T <: Data](src: T): T = {
    RegNext(RegNext(RegNext(RegNext(src))))
  }

  def RegNext5[T <: Data](src: T): T = {
    RegNext(RegNext(RegNext(RegNext(RegNext(src)))))
  }

  val io = IO(new ExecuteInterface(config))

  val pred_reg    = Reg(Bool())
  val gmem_if_reg = Reg(new GlobalMemoryInterface(config))

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Custom ALU ////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  val custom_alu = Module(
    new CustomAlu(config.DataBits, config.FunctBits, config.LutArity, equations, custom_alu_enable)
  )
  custom_alu.io.rsx := Vec(io.regs_in.rs1, io.regs_in.rs2, io.regs_in.rs3, io.regs_in.rs4)
  for (i <- Range(0, config.numFuncts)) {
    // ALL luts are configured in parallel. It is not possible to configure them one by one.
    // This avoids the use of (conf.FunctBits * config.DataBits) LUTs to perform an AND on this
    // large fan-out path.
    custom_alu.io.config(i).writeEnable := RegNext(io.pipe_in.opcode.configure_luts(i))
    custom_alu.io.config(i).loadData    := io.lutdata_din(i)
  }
  custom_alu.io.selector := RegNext(io.pipe_in.funct)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Standard ALU //////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  val standard_alu = Module(new StandardALUComb(config.DataBits))

  // MUX for standard_alu.io.in.mask
  val alu_mask_in = Wire(UInt(config.DataBits.W))
  when(io.pipe_in.opcode.slice) {
    // Mask to use on the output of the ALU (for slicing after the ALU has
    // performed SRL on rs).
    alu_mask_in := io.pipe_in.immediate
  } otherwise {
    // Keep the full output of the ALU.
    alu_mask_in := Fill(config.DataBits, 1.B).asUInt
  }
  standard_alu.io.in.mask := RegNext(alu_mask_in)

  // MUX for standard_alu.io.in.y
  when(RegNext(io.pipe_in.opcode.arith) | RegNext(io.pipe_in.opcode.expect)) {
    standard_alu.io.in.y := io.regs_in.rs2
  } otherwise {
    val alu_y_in = Wire(UInt(config.DataBits.W))
    when(io.pipe_in.opcode.slice) {
      alu_y_in := io.pipe_in.slice_ofst
    } otherwise {
      alu_y_in := io.pipe_in.immediate
    }
    standard_alu.io.in.y := RegNext(alu_y_in)
  }

  // MUX for standard_alu.io.funct
  val alu_funct_in = Wire(UInt(4.W))
  when(io.pipe_in.opcode.arith | io.pipe_in.opcode.expect) {
    when(io.pipe_in.opcode.expect) {
      alu_funct_in := ISA.Functs.SEQ.id.U
    } otherwise {
      alu_funct_in := io.pipe_in.funct
    }
  } otherwise {
    when(io.pipe_in.opcode.slice) {
      // When configured to perform a slice, the funct field already
      // has the code for SRL.
      alu_funct_in := io.pipe_in.funct
    } otherwise {
      // TODO (skashani): This comment from Mahyar seems wrong as MUX is assembled
      // as an ARITH instruction. To check with him later.
      //
      // with non-arith instructions, funct can be any value, including the
      // funct for Mux (which is stateful) so we should set it to zero to
      // ensure no stateful ALU operations are performed.
      alu_funct_in := ISA.Functs.ADD2.id.U
    }
  }
  standard_alu.io.funct := RegNext(alu_funct_in)

  standard_alu.io.in.select := io.regs_in.rs3
  standard_alu.io.in.carry  := io.carry_in

  when(RegNext(io.pipe_in.opcode.set || io.pipe_in.opcode.send)) {
    standard_alu.io.in.x := 0.U
  } otherwise {
    standard_alu.io.in.x := io.regs_in.rs1
  }

  standard_alu.io.valid_in := RegNext(io.valid_in)

  when(RegNext4(io.pipe_in.opcode.cust)) {
    io.pipe_out.result := RegNext2(custom_alu.io.out)
  } otherwise {
    io.pipe_out.result := RegNext(standard_alu.io.out)
  }
  io.pipe_out.result_mul := RegNext(standard_alu.io.mul_out)
  io.valid_out           := RegNext(standard_alu.io.valid_out)

  io.pipe_out.opcode    := RegNext4(io.pipe_in.opcode)
  io.pipe_out.data      := RegNext3(io.regs_in.rs2)
  io.pipe_out.rd        := RegNext4(io.pipe_in.rd)
  io.pipe_out.immediate := RegNext4(io.pipe_in.immediate)

  // Need to check the num of regs for carry outputs
  when(RegNext4(io.pipe_in.opcode.set_carry)) {
    io.carry_rd := RegNext4(io.pipe_in.rd)
  } otherwise {
    // notice that rs4 needs to be registered before given to the output pipe
    io.carry_rd := RegNext4(io.pipe_in.rs4)
  }
  when(RegNext4(io.pipe_in.opcode.set_carry)) {
    io.carry_din := RegNext4(io.pipe_in.immediate(0))
  } otherwise {
    io.carry_din := RegNext(standard_alu.io.carry_out)
  }
  io.carry_wen :=
    RegNext4(
      (io.pipe_in.opcode.arith & (io.pipe_in.funct) === ISA.Functs.ADDC.id.U) | (
        io.pipe_in.opcode.set_carry
      )
    )

  // enable/disable predicate
  when(RegNext(io.pipe_in.opcode.predicate)) {
    pred_reg := io.regs_in.rs1 === 1.U
  }

  io.pipe_out.pred := RegNext3(pred_reg)

  if (config.WithGlobalMemory) {
    val gload_next  = RegNext(io.pipe_in.opcode.gload)
    val gstore_next = RegNext(io.pipe_in.opcode.gstore)
    gmem_if_reg.address := io.regs_in.rs2 ## io.regs_in.rs3 ## io.regs_in.rs4
    when(gload_next) {
      gmem_if_reg.command := CacheCommand.Read
    }.elsewhen(gstore_next) {
      gmem_if_reg.command := CacheCommand.Write
    }
    gmem_if_reg.start := (gstore_next && pred_reg) | gload_next
    gmem_if_reg.wdata := io.regs_in.rs1
    io.pipe_out.gmem  := RegNext2(gmem_if_reg)
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
        io.pipe_in.opcode.configure_luts(0) // This is a replicated signal, so just 1 of them suffices.

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
