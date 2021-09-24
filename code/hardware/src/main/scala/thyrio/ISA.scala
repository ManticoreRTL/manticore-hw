package thyrio

import Chisel.fromIntToWidth
import chisel3.internal.firrtl.{IntervalRange, Width}
import thyrio.assembly.Instruction

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
  object SetValue extends Opcode(Instruction.Opcode.SET.id)
  object Custom0 extends Opcode(Instruction.Opcode.CUST0.id)
  object Arithmetic extends Opcode (Instruction.Opcode.ARITH.id)
  object LocalLoad extends Opcode (Instruction.Opcode.LLOAD.id)
  object LocalStore extends Opcode (Instruction.Opcode.LSTORE.id)
  object Expect extends Opcode (Instruction.Opcode.EXPECT.id)
  object GlobalLoad extends Opcode (Instruction.Opcode.GLOAD.id)
  object GlobalStore extends Opcode (Instruction.Opcode.GSTORE.id)
  object Send extends Opcode(Instruction.Opcode.SEND.id)
//  object Nop extends Opcode(Instruction.Opcode.NOP.id)

  object DestReg extends  InstructionField(OpcodeBits, IdBits)
  object Funct extends InstructionField(DestReg.toIndex + 1, FunctBits)
  object SourceReg1 extends InstructionField(Funct.toIndex + 1, IdBits)
  object SourceReg2 extends InstructionField(SourceReg1.toIndex + 1, IdBits)
  object SourceReg3 extends InstructionField(SourceReg2.toIndex + 1, IdBits)
  object SourceReg4 extends InstructionField(SourceReg3.toIndex + 1, IdBits)
//  object ImmediateLow extends InstructionField(OPCODE_BITS, ID_BITS)
//  object ImmediateHigh extends InstructionField(SourceReg2.toIndex + 1, 2 * ID_BITS)
  object Immediate extends  InstructionField(SourceReg1.toIndex + 1, 3 * IdBits)


//  object UnusedImmediateLow extends InstructionField(ImmediateLow.fromIndex, ImmediateLow.length)
//  object UnusedImmediateHigh extends InstructionField(ImmediateHigh.fromIndex, ImmediateHigh.length)
//  object UnusedFunct extends  InstructionField(Funct.fromIndex, Funct.length)
//  object StoreDataReg extends InstructionField(SourceReg2.fromIndex, SourceReg2.length)
//  object StoreOffsetReg extends InstructionField(SourceReg1.fromIndex, SourceReg1.length)
//  object UnusedSourceReg1 extends InstructionField(SourceReg1.fromIndex, SourceReg1.length)
//  implicit def instructionFieldToSeq(x: InstructionField): Seq[InstructionField] = Seq(x)
//
//  def defineSignature(fields: Seq[InstructionField]): Seq[InstructionField] = {
//    lazy val length = fields.map(_.length).sum
//    require(length == NUM_BITS,
//      s"Invalid instruction signature of length %d (expected %d)".format(length, NUM_BITS))
//    fields.reverse
//  }
//
//  lazy val Custom0Signature: Seq[InstructionField] = defineSignature(
//    Custom0 ++ DestReg ++ Funct ++ SourceReg1 ++ SourceReg2 ++ SourceReg3 ++ SourceReg4
//  )
//
//  lazy val ArithmeticSignature: Seq[InstructionField] = defineSignature(
//    Arithmetic ++ DestReg ++ Funct ++ SourceReg1 ++ SourceReg2 ++ SourceReg3 ++ SourceReg4
//  )
//
//  /**
//   * r[DestReg] <= m[r[SourceReg1] + Immediate]
//   */
//  lazy val LocalLoadSignature: Seq[InstructionField] = defineSignature(
//    LocalLoad ++ DestReg ++ UnusedFunct ++ SourceReg1 ++ Immediate
//  )
//
//  /**
//   * m[r[StoreOffsetReg] + {ImmediateHigh, ImmediateLow}] <= r[StoreDataReg]
//   */
//  lazy val LocalStoreSignature: Seq[InstructionField] = defineSignature(
//    LocalStore ++ ImmediateLow ++ UnusedFunct ++ StoreOffsetReg ++ StoreDataReg ++ ImmediateHigh
//  )
//
//  /**
//   * error <= r[SourceReg1] =/= r[SourceReg2]
//   */
//  lazy val ExpectSignature: Seq[InstructionField] = defineSignature(
//    Expect ++ UnusedImmediateLow ++ Funct ++ UnusedFunct ++ SourceReg1 ++ SourceReg2 ++ UnusedImmediateHigh
//  )
//
//  /**
//   * packet <= { data: r[SourceReg2], address: ImmediateHigh(ImmediateHigh.length -: ID_BITS), {xHops, yHops}: ImmediateLow..
//   */
//  lazy val SendSignature: Seq[InstructionField] = defineSignature(
//    Send ++ ImmediateLow ++ UnusedFunct ++ UnusedSourceReg1 ++ SourceReg2 ++ ImmediateHigh
//  )
//
//  /**
//   * SetValue (aka Receive):
//   * r[DestReg] <= Immediate(DATA_BITS - 1, 0)
//   */
//  lazy val SetValueSignature: Seq[InstructionField] = defineSignature(
//    SetValue ++ DestReg ++ UnusedFunct ++ UnusedSourceReg1 ++ Immediate
//  )
//
//  /**
//   * GlobalLoad: (little-endian)
//   * r[DestReg] <= Mem({r[SourceReg4], r[SourceReg3], r[SourceReg2], r[SourceReg1])
//   */
//  lazy val GlobalLoadSignature: Seq[InstructionField] = defineSignature(
//    GlobalStore ++ DestReg ++ UnusedFunct ++ SourceReg1 ++ SourceReg2 ++ SourceReg3 ++ SourceReg4
//  )



}

object ThyrioISA extends ISA {
  val NumPcBits: Int = 12
  val DataBits: Int = 16
  val NumBits: Int = 64
  val IdBits: Int = 11
  val FunctBits: Int = 5
  val WithGlobalMemory: Boolean = false

}

object ThyrioISAWithGlobalMemory extends ISA {
  val NumPcBits: Int = ThyrioISA.NumPcBits
  val DataBits: Int = ThyrioISA.DataBits
  val NumBits: Int = ThyrioISA.NumBits
  val IdBits: Int = ThyrioISA.IdBits
  val FunctBits: Int = ThyrioISA.FunctBits
  val WithGlobalMemory: Boolean = true
}