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
import manticore.machine.{ISA, ManticoreBaseISA}
import manticore.machine.core.ExecuteInterface.{GlobalMemoryInterface, PipeIn, PipeOut}
import manticore.machine.core.alu.{ALUInput, CustomALU, CustomALUComb, StandardALU, StandardALUComb}
import manticore.machine.memory.{CacheCommand, CacheConfig}

import scala.util.Random

object ExecuteInterface {
  type OpcodePipe = Decode.OpcodePipe
  type PipeIn     = Decode.PipeOut
  class PipeOut(config: ISA) extends Bundle {
    val opcode    = new Decode.OpcodePipe
    val data      = UInt(config.DataBits.W)
    val result    = UInt(config.DataBits.W)
    val rd        = UInt(config.IdBits.W)
    val carry_rd  = UInt(log2Ceil(config.CarryCount).W)
    val carry_wen = Bool()
    val carry_din = UInt(1.W)
    val immediate = UInt(config.DataBits.W)
    val gmem      = new GlobalMemoryInterface(config)
    val pred      = Bool()
  }

//  class LocalMemoryInterface(config: ISA) extends Bundle {
//    // read interface to the local memory
//    val address = Output(UInt(11.W))
//    val dout    = Input(UInt(config.DataBits.W))
//    val we      = Output(Bool()) // should be false.B
//  }

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
    standard_alu.io.in.y := io.regs_in.y
    when(io.pipe_in.opcode.expect) {
      standard_alu.io.funct         := StandardALU.Functs.SEQ.id.U
    } otherwise {
      standard_alu.io.funct         := io.pipe_in.funct
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

  when(io.pipe_in.opcode.cust0) {
    pipe_out.result := custom_alu.io.out
  } otherwise {
    pipe_out.result := standard_alu.io.out
  }

  pipe_out.opcode    := io.pipe_in.opcode
  pipe_out.data      := io.regs_in.y
  pipe_out.rd        := io.pipe_in.rd
  pipe_out.immediate := io.pipe_in.immediate
  when (io.pipe_in.opcode.set_carry) {
    pipe_out.carry_rd := io.pipe_in.rd
  } otherwise {
    // notice that rs4 needs to be registered before given to the output pipe
    val rs4_reg = Reg(UInt(log2Ceil(config.CarryCount).W))
    rs4_reg := io.pipe_in.rs4
    pipe_out.carry_rd  := rs4_reg
  }
  when (io.pipe_in.opcode.set_carry) {
   pipe_out.carry_din := io.pipe_in.immediate(0)
  } otherwise {
    pipe_out.carry_din := standard_alu.io.carry_out
  }
  pipe_out.carry_wen :=
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
        io.pipe_in.opcode.cust0.toUInt +
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

    // otherwise show whats happening

    when(io.pipe_in.opcode.arith) {

      dprintf(
        "\tR(%d) <= Arith(R(%d) = %d, OP(%d), R(%d) = %d) = %d\n",
        io.pipe_in.rd,
        io.pipe_in.rs1,
        io.regs_in.x,
        io.pipe_in.funct,
        io.pipe_in.rs2,
        io.regs_in.x,
        standard_alu.io.out
      )

    }

    when(io.pipe_in.opcode.cust0) {
      dprintf(
        "\tR(%d) <= Cust0(R(%d) = %d, OP(%d), R(%d) = %d) = %d\n",
        io.pipe_in.rd,
        io.pipe_in.rs1,
        io.regs_in.x,
        io.pipe_in.funct,
        io.pipe_in.rs2,
        io.regs_in.x,
        custom_alu.io.out
      )
    }

    when(io.pipe_in.opcode.expect) {

      dprintf(
        "\t%d <= Expect(R(%d) = %d, R(%d) = %d)\n",
        standard_alu.io.out,
        io.pipe_in.rs1,
        io.regs_in.x,
        io.pipe_in.rs2,
        io.regs_in.y
      )

    }

    when(io.pipe_in.opcode.gload) {

      dprintf(
        "\tR(%d) <= GlobalLoad(addr = R(%d) # R(%d) # R(%d) = 0x%x # 0x%x # 0x%x = 0x%x)\n",
        io.pipe_in.rd,
        io.pipe_in.rs2,
        io.pipe_in.rs3,
        io.pipe_in.rs4,
        io.regs_in.y,
        io.regs_in.u,
        io.regs_in.v,
        io.regs_in.y ## io.regs_in.u ## io.regs_in.v
      )
    }

    when(io.pipe_in.opcode.gstore) {

      dprintf(
        "\tGlobalStore(value = R(%d) = %d, addr = R(%d) # R(%d) # R(%d) = 0x%x # 0x%x # 0x%x = 0x%x)\n",
        io.pipe_in.rs1,
        io.regs_in.x,
        io.pipe_in.rs2,
        io.pipe_in.rs3,
        io.pipe_in.rs4,
        io.regs_in.y,
        io.regs_in.u,
        io.regs_in.v,
        io.regs_in.y ## io.regs_in.u ## io.regs_in.v
      )
    }

    when(io.pipe_in.opcode.lload) {
      dprintf(
        "\tR(%d) <= LocalLoad(addr = R(%d) + %d = %d + %d = %d)\n",
        io.pipe_in.rd,
        io.pipe_in.rs1,
        io.pipe_in.immediate,
        io.regs_in.x,
        io.pipe_in.immediate,
        standard_alu.io.out
      )
    }

    when(io.pipe_in.opcode.lstore) {
      dprintf(
        "\tLocalStore(value = R(%d) = %d, addr = R(%d) + %d = %d + %d = %d)\n",
        io.pipe_in.rs2,
        io.regs_in.y,
        io.pipe_in.rs1,
        io.pipe_in.immediate,
        io.regs_in.x,
        io.pipe_in.immediate,
        standard_alu.io.out
      )
    }

    when(io.pipe_in.opcode.nop) {
      // dprintf("\tNOP\n")
    }

    when(io.pipe_in.opcode.predicate) {
      dprintf("\tPredicate(value = R(%d) = %d)\n", io.pipe_in.rs1, io.regs_in.x)

    }

    when(io.pipe_in.opcode.set) {
      dprintf("\t R(%d) <= Set(%d)\n", io.pipe_in.rd, io.pipe_in.immediate)
    }

  }

}
object ExecuteGen extends App {

  val rdgen = Random

  // create random LUT equations
  val equations = Seq.fill(32)(Seq.fill(16)(rdgen.nextInt(1 << 16)))

  new ChiselStage().emitVerilog(new ExecuteComb(ManticoreBaseISA, equations))
}
