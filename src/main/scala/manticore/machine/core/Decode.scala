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
package manticore.machine.core

import Chisel._
import chisel3.stage.ChiselStage
import manticore.machine.ISA
import manticore.machine.InstructionField
import manticore.machine.ManticoreBaseISA



object Decode {

  class OpcodePipe(numFuncts: Int) extends Bundle {
    val cust: Bool = Bool()
    val arith: Bool = Bool()
    val lload: Bool = Bool()
    val lstore: Bool = Bool()
    val send: Bool = Bool()
    val set: Bool = Bool()
    val expect: Bool = Bool()
    val gload: Bool = Bool()
    val gstore: Bool = Bool()
    val predicate: Bool = Bool()
    val nop: Bool = Bool()
    val set_carry: Bool = Bool()
    val set_lut_data: Bool = Bool()
    val configure_lut: Vec[Bool] = Vec(numFuncts, Bool())
  }

  class PipeOut(config: ISA) extends Bundle {
    private def regIdOut = UInt(config.IdBits.W)
    val rd: UInt = regIdOut
    val rs1: UInt = regIdOut
    val rs2: UInt = regIdOut
    val rs3: UInt = regIdOut
    val rs4: UInt = regIdOut
    val opcode: OpcodePipe = new OpcodePipe(config.numFuncts)
    val funct: UInt = UInt(config.FunctBits.W)
    val immediate: UInt = UInt(config.DataBits.W)
  }

}
class DecodeInterface(config: ISA) extends Bundle {
  val instruction = Input(UInt(config.NumBits.W))
  val pipe_out = Output(new Decode.PipeOut(config))
}

class Decode(config: ISA) extends Module {

  val io = IO(new DecodeInterface(config))

  def getField(field: InstructionField): UInt = io.instruction(field.toIndex, field.fromIndex)

  val opcode: UInt = Wire(UInt(config.OpcodeBits.W))
  opcode := io.instruction(config.OpcodeBits - 1, 0)

  val opcode_regs = Reg(new Decode.OpcodePipe(config.numFuncts))

  def setEqual[T <: Data](reg: T, expected_opcode: Int): Unit =
    when (opcode === expected_opcode.U) {
      reg := true.B
    } otherwise {
      reg := false.B
    }

  setEqual(opcode_regs.cust, config.Custom.value)
  setEqual(opcode_regs.arith, config.Arithmetic.value)
  setEqual(opcode_regs.lload, config.LocalLoad.value)
  setEqual(opcode_regs.lstore, config.LocalStore.value)
  setEqual(opcode_regs.expect, config.Expect.value)
  setEqual(opcode_regs.set, config.SetValue.value)
  setEqual(opcode_regs.gload, config.GlobalLoad.value)
  setEqual(opcode_regs.gstore, config.GlobalStore.value)
  setEqual(opcode_regs.send, config.Send.value)
  setEqual(opcode_regs.predicate, config.Predicate.value)
  setEqual(opcode_regs.set_carry, config.SetCarry.value)
  setEqual(opcode_regs.set_lut_data, config.SetLutData.value)

  when (opcode === config.ConfigureLuts.value.U) {
    opcode_regs.configure_lut := Vec.fill(config.numFuncts)(1.B)
  } otherwise {
    opcode_regs.configure_lut := Vec.fill(config.numFuncts)(0.B)
  }

  // Whole instruction must be 0, not just the opcode. This is just a sanity check.
  opcode_regs.nop := io.instruction === 0.U

  io.pipe_out.opcode := opcode_regs

  val funct_reg = Reg(UInt(config.Funct.W))

  val immediate_reg = Reg(UInt(config.DataBits.W))
  val rd_reg = Reg(UInt(config.IdBits.W))

  funct_reg := getField(config.Funct)
  io.pipe_out.funct := funct_reg

  immediate_reg := io.instruction.head(config.DataBits)
  io.pipe_out.immediate := immediate_reg

  rd_reg := getField(config.DestReg)
  io.pipe_out.rd := rd_reg

  io.pipe_out.rs1 := getField(config.SourceReg1)
  io.pipe_out.rs2 := getField(config.SourceReg2)
  io.pipe_out.rs3 := getField(config.SourceReg3)
  io.pipe_out.rs4 := getField(config.SourceReg4)

}


object DecodeGenerator extends App {
  def apply(config: ISA) = new Decode(config)
  def emitVerilog() = new ChiselStage().emitSystemVerilog(apply(ManticoreBaseISA))
  emitVerilog()
}