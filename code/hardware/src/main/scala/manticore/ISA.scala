package manticore

import Chisel.fromIntToWidth
import chisel3.internal.firrtl.{IntervalRange, Width}
import manticore.assembly.Instruction

import scala.language.implicitConversions


sealed abstract class InstructionField(val fromIndex: Int, val length: Int) {
  val toIndex = fromIndex + length - 1
  def W = length.W
}






trait ISA {
  def NumPcBits: Int
  def NumBits: Int
  def DataBits: Int
  def IdBits: Int
  def FunctBits: Int
  def OpcodeBits:  Int = NumBits - 5 * IdBits - FunctBits
  def WithGlobalMemory: Boolean


  sealed class Opcode(val value: Int) extends InstructionField(0, OpcodeBits)
  type OpcodeType = Opcode
  /**
   * Opcode unique objects, can serve as types
   */
  object Nop extends Opcode(Instruction.Opcode.NOP.id)
  object SetValue extends Opcode(Instruction.Opcode.SET.id)
  object Custom0 extends Opcode(Instruction.Opcode.CUST0.id)
  object Arithmetic extends Opcode (Instruction.Opcode.ARITH.id)
  object LocalLoad extends Opcode (Instruction.Opcode.LLOAD.id)
  object LocalStore extends Opcode (Instruction.Opcode.LSTORE.id)
  object Expect extends Opcode (Instruction.Opcode.EXPECT.id)
  object GlobalLoad extends Opcode (Instruction.Opcode.GLOAD.id)
  object GlobalStore extends Opcode (Instruction.Opcode.GSTORE.id)
  object Send extends Opcode(Instruction.Opcode.SEND.id)


  object DestReg extends  InstructionField(OpcodeBits, IdBits)
  object Funct extends InstructionField(DestReg.toIndex + 1, FunctBits)
  object SourceReg1 extends InstructionField(Funct.toIndex + 1, IdBits)
  object SourceReg2 extends InstructionField(SourceReg1.toIndex + 1, IdBits)
  object SourceReg3 extends InstructionField(SourceReg2.toIndex + 1, IdBits)
  object SourceReg4 extends InstructionField(SourceReg3.toIndex + 1, IdBits)
//  object ImmediateLow extends InstructionField(OPCODE_BITS, ID_BITS)
//  object ImmediateHigh extends InstructionField(SourceReg2.toIndex + 1, 2 * ID_BITS)
  object Immediate extends  InstructionField(SourceReg1.toIndex + 1, 3 * IdBits)


}

object ManticoreBaseISA extends ISA {
  val NumPcBits: Int = 12
  val DataBits: Int = 16
  val NumBits: Int = 64
  val IdBits: Int = 11
  val FunctBits: Int = 5
  val WithGlobalMemory: Boolean = false

}

object ManticoreFullISA extends ISA {
  val NumPcBits: Int = ManticoreBaseISA.NumPcBits
  val DataBits: Int = ManticoreBaseISA.DataBits
  val NumBits: Int = ManticoreBaseISA.NumBits
  val IdBits: Int = ManticoreBaseISA.IdBits
  val FunctBits: Int = ManticoreBaseISA.FunctBits
  val WithGlobalMemory: Boolean = true
}