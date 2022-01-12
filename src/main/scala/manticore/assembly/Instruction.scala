package manticore.assembly

import scala.language.implicitConversions

object Instruction {

  sealed trait RegisterTrait {
    def DATA_WIDTH: Int    = 16
    def ADDRESS_WIDTH: Int = 11
  }

  case class Register(index: Int, name: String = "") extends RegisterTrait {
    override def toString: String = {
      s"$$r${index}${if (name.nonEmpty) "_" + name else ""}"
    }
  }

  object R {
    def apply(index: Int) = Register(index)
  }

  implicit def registerToIndex(reg: Register): Int = reg.index

  object Opcode extends Enumeration {
    type Type = Value
    val NOP, SET, CUST0, ARITH, LLOAD, LSTORE, EXPECT, GLOAD, GSTORE, SEND,
        PREDICATE = Value
  }

  sealed abstract class Instruction(val opcode: Opcode.Type)

  type Equation = Seq[Int]
  case class CustomFunction(equation: Equation) {
    def ==(other: CustomFunction): Boolean = {
      (equation.size == other.equation.size) &&
      equation.zip(other.equation).forall(x => x._1 == x._2)
    }

    override def toString: String =
      s"CustomFunction${equation.map(s"0x%x".format(_))}"

  }

  case class Custom0(
      rd: Register,
      function: CustomFunction,
      rs1: Register,
      rs2: Register,
      rs3: Register,
      rs4: Register
  ) extends Instruction(Opcode.CUST0)

  sealed abstract class ArithmeticInstruction extends Instruction(Opcode.ARITH)

  sealed abstract class BinaryArithmeticInstruction(
      rd: Register,
      rs1: Register,
      rs2: Register,
      op: String
  ) extends ArithmeticInstruction {
    override def toString(): String = s"${op}\t${rd}, ${rs1}, ${rs2}"
  }

  case class Add2(rd: Register, rs1: Register, rs2: Register)
      extends BinaryArithmeticInstruction(rd, rs1, rs2, "ADD")

  case class Or2(rd: Register, rs1: Register, rs2: Register)
      extends BinaryArithmeticInstruction(rd, rs1, rs2, "OR")

  case class And2(rd: Register, rs1: Register, rs2: Register)
      extends BinaryArithmeticInstruction(rd, rs1, rs2, "AND")

  case class Xor2(rd: Register, rs1: Register, rs2: Register)
      extends BinaryArithmeticInstruction(rd, rs1, rs2, "XOR")
  case class Mult2(rd: Register, rs1: Register, rs2: Register)
      extends BinaryArithmeticInstruction(rd, rs1, rs2, "MULT")

  case class SetEqual(rd: Register, rs1: Register, rs2: Register)
      extends BinaryArithmeticInstruction(rd, rs1, rs2, "SEQ")
  case class SetLessThanUnsigned(rd: Register, rs1: Register, rs2: Register)
      extends BinaryArithmeticInstruction(rd, rs1, rs2, "SLTU")
  case class SetLessThanSigned(rd: Register, rs1: Register, rs2: Register)
      extends BinaryArithmeticInstruction(rd, rs1, rs2, "SLTS")
  case class SetGreaterThanUnsigned(rd: Register, rs1: Register, rs2: Register)
      extends BinaryArithmeticInstruction(rd, rs1, rs2, "SGTU")
  case class SetGreaterThanSigned(rd: Register, rs1: Register, rs2: Register)
      extends BinaryArithmeticInstruction(rd, rs1, rs2, "SGTS")
  case class Mux2(rd: Register, rs1: Register, rs2: Register)
      extends BinaryArithmeticInstruction(rd, rs1, rs2, "MUX")

  case class LocalLoad(rd: Register, base: Register, offset: Long)
      extends Instruction(Opcode.LLOAD) {
    override def toString: String = s"LLOAD\t${rd}, ${base}(${offset})"
  }
  case class LocalStore(rs: Register, base: Register, offset: Long)
      extends Instruction(Opcode.LSTORE) {
    override def toString: String = s"LSTORE\t${rs}, ${base}(${offset})"
  }

  case class Expect(value: Register, expected: Register, id: Int)
      extends Instruction(Opcode.EXPECT) {
    override def toString: String = s"EXPECT\t${value}, ${expected}, ${id}"
  }

  case class GlobalLoad(
      rd: Register,
      addrlo: Register,
      addrmid: Register,
      addrhi: Register
  ) extends Instruction(Opcode.GLOAD) {
    override def toString: String = s"GLOAD\t${rd}, [${addrhi}, ${addrmid}, ${addrlo}]"
  }
  case class GlobalStore(
      rs: Register,
      addrlo: Register,
      addrmid: Register,
      addrhi: Register
  ) extends Instruction(Opcode.GSTORE) {
    override def toString: String = s"GSTORE\t${rs}, [${addrhi}, ${addrmid}, ${addrlo}]"
  }

  case class SetValue(rd: Register, value: Int) extends Instruction(Opcode.SET) {
    override def toString: String = s"SET\t${rd}, ${value}"
  }
  case class Send(
      target: Register,
      rs: Register,
      addressX: Long,
      addressY: Long
  ) extends Instruction(Opcode.SEND) {
    override def toString: String = s"SEND\t${target}, ${rs}, ${addressX}, ${addressY}"
  }
  case class Predicate(rs: Register) extends Instruction(Opcode.PREDICATE) {
    override def toString: String = s"PREDICATE\t${rs}"
  }

  case class Nop() extends Instruction(Opcode.NOP) {
    override def toString: String = s"NOP"
  }

}

//object InstructionBuilder {
//
//
//  sealed abstract class PartialBuilder
//  class SetValueBuilder2(val rd: Register, val value: Long) extends PartialBuilder {
//    def end : SetValue = SetValue(rd, value)
//  }
//  class SetValueBuilder1(val rd: Register) extends PartialBuilder {
//    def value (value: Long): SetValue = SetValue(rd, value)
//  }
//
//
//  def RReg(x: Int): Register = Register(x)
//
//  implicit class SetValueBuilder00(val set: SetOpcode) extends PartialBuilder {
//    def rd (rd: Int): SetValueBuilder1 = new SetValueBuilder1(Register(rd))
//  }
//
//
//  implicit class Add3Builder00(val add3: Add3Opcode) extends PartialBuilder {
//    def rd (rd: Int): Add3Builder1 = new Add3Builder1(Register(rd))
//  }
//
//
//  class Add3Builder0 extends PartialBuilder {
//    def rd (rd: Int): Add3Builder1 = new Add3Builder1(Register(rd))
//  }
//
//
//
//  class Add3Builder1(val rd: Register) extends PartialBuilder {
//    def rs1 (rs1: Int): Add3Builder2 = new Add3Builder2(rd, Register(rs1))
//  }
//  class Add3Builder2(val rd: Register, val rs1: Register) extends PartialBuilder {
//    def rs2 (rs2: Int): Add3Builder3 = new Add3Builder3(rd, rs1, Register(rs2))
//  }
//  class Add3Builder3(val rd: Register, val rs1: Register, val rs2: Register) extends PartialBuilder {
//    def rs3 (rs3: Int): Add3 = Add3(rd, rs1, rs2, Register(rs3))
//  }
//
//  sealed abstract class OpcodeWord(val name: String)
//  final class SetOpcode extends OpcodeWord("Set")
//  final class Add3Opcode extends OpcodeWord("Add3")
//
//  val SET = new SetOpcode()
//  val ADD3 = new Add3Opcode()
//
//
//}
