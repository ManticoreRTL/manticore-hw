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
import manticore.machine.InstructionField
import manticore.machine.ManticoreBaseISA

object Decode {

  class OpcodePipe(numFuncts: Int) extends Bundle {
    val cust: Bool         = Bool()
    val arith: Bool        = Bool()
    val lload: Bool        = Bool()
    val lstore: Bool       = Bool()
    val send: Bool         = Bool()
    val set: Bool          = Bool()
    val expect: Bool       = Bool()
    val gload: Bool        = Bool()
    val gstore: Bool       = Bool()
    val predicate: Bool    = Bool()
    val nop: Bool          = Bool()
    val set_carry: Bool    = Bool()
    val set_lut_data: Bool = Bool()
    // We intentionally use a vector of bool here instead of a simple bool
    // as we want to force the FPGA tools to create a dedicated register
    // for every LUT vector. Otherwise it will fan-out a single signal to
    // many LUTs and it may not achieve the same frequency scaling.
    val configure_luts: Vec[Bool] = Vec(numFuncts, Bool())
    val slice: Bool               = Bool()
    // Multiplications are done by a parallel datapath to the Execute and Memory
    // stages. One of these signals will be high in parallel to arith.
    val mul: Bool  = Bool() // unsigned  low result
    val mulh: Bool = Bool() // unsigned high result
  }

  class PipeOut(config: ISA) extends Bundle {
    private def regIdOut   = UInt(config.IdBits.W)
    val rd: UInt           = regIdOut
    val rs1: UInt          = regIdOut
    val rs2: UInt          = regIdOut
    val rs3: UInt          = regIdOut
    val rs4: UInt          = regIdOut
    val opcode: OpcodePipe = new OpcodePipe(config.numFuncts)
    val funct: UInt        = UInt(config.FunctBits.W)
    val immediate: UInt    = UInt(config.DataBits.W)
    // The slice mask is encoded in the immediate field, but the slice
    // offset is encoded in log2Ceil(config.DataBits) additional bits.
    // Only the slice instruction uses these extra bits.
    val slice_ofst: UInt = UInt(log2Ceil(config.DataBits).W)
  }

}
class DecodeInterface(config: ISA) extends Bundle {
  val instruction = Input(UInt(config.NumBits.W))
  val pipe_out    = Output(new Decode.PipeOut(config))
}

class Decode(config: ISA) extends Module {

  val io = IO(new DecodeInterface(config))

  def getField(field: InstructionField): UInt = io.instruction(field.toIndex, field.fromIndex)

  val opcode     = Wire(UInt(config.OpcodeBits.W))
  val funct      = Wire(UInt(config.FunctField.W))
  val immediate  = Wire(UInt(config.DataBits.W))
  val rd         = Wire(UInt(config.IdBits.W))
  val slice_ofst = Wire(UInt(log2Ceil(config.DataBits).W))
  val rs1        = Wire(UInt(config.IdBits.W))
  val rs2        = Wire(UInt(config.IdBits.W))
  val rs3        = Wire(UInt(config.IdBits.W))
  val rs4        = Wire(UInt(config.IdBits.W))

  opcode     := getField(config.OpcodeField)
  funct      := getField(config.FunctField)
  immediate  := getField(config.ImmediateField)
  rd         := getField(config.DestRegField)
  slice_ofst := getField(config.SliceOfstField)
  rs1        := getField(config.SourceReg1Field)
  rs2        := getField(config.SourceReg2Field)
  rs3        := getField(config.SourceReg3Field)
  rs4        := getField(config.SourceReg4Field)

  val opcode_regs    = Reg(new Decode.OpcodePipe(config.numFuncts))
  val funct_reg      = Reg(UInt(config.FunctField.W))
  val immediate_reg  = Reg(UInt(config.DataBits.W))
  val slice_ofst_reg = Reg(UInt(log2Ceil(config.DataBits).W))
  val rd_reg         = Reg(UInt(config.IdBits.W))

  val is_arith = Wire(Bool())
  is_arith := (opcode === config.Arithmetic.value.U)

  // Whole instruction must be 0, not just the opcode. This is just a sanity check.
  opcode_regs.nop            := io.instruction === 0.U
  opcode_regs.cust           := (opcode === config.Custom.value.U)
  opcode_regs.arith          := is_arith
  opcode_regs.mul            := is_arith && (funct === ISA.Functs.MUL2.id.U || funct === ISA.Functs.MUL2S.id.U)
  opcode_regs.mulh           := is_arith && (funct === ISA.Functs.MUL2H.id.U)
  opcode_regs.lload          := (opcode === config.LocalLoad.value.U)
  opcode_regs.lstore         := (opcode === config.LocalStore.value.U)
  opcode_regs.expect         := (opcode === config.Expect.value.U)
  opcode_regs.set            := (opcode === config.SetValue.value.U)
  opcode_regs.gload          := (opcode === config.GlobalLoad.value.U)
  opcode_regs.gstore         := (opcode === config.GlobalStore.value.U)
  opcode_regs.send           := (opcode === config.Send.value.U)
  opcode_regs.predicate      := (opcode === config.Predicate.value.U)
  opcode_regs.set_carry      := (opcode === config.SetCarry.value.U)
  opcode_regs.slice          := (opcode === config.Slice.value.U)
  opcode_regs.set_lut_data   := (opcode === config.SetLutData.value.U)
  opcode_regs.configure_luts := Vec.fill(config.numFuncts)((opcode === config.ConfigureLuts.value.U))

  funct_reg      := funct
  immediate_reg  := immediate
  slice_ofst_reg := slice_ofst
  rd_reg         := rd

  io.pipe_out.opcode     := opcode_regs
  io.pipe_out.funct      := funct_reg
  io.pipe_out.immediate  := immediate_reg
  io.pipe_out.rd         := rd_reg
  io.pipe_out.slice_ofst := slice_ofst_reg

  // These are NOT registers and are sent directly to the register files.
  // The response comes back 1 cycle later in the Execute stage.
  io.pipe_out.rs1 := rs1
  io.pipe_out.rs2 := rs2
  io.pipe_out.rs3 := rs3
  io.pipe_out.rs4 := rs4
}

object DecodeGenerator extends App {
  def apply(config: ISA) = new Decode(config)
  def emitVerilog() =
    new ChiselStage().emitSystemVerilog(apply(ManticoreBaseISA))
  emitVerilog()
}
