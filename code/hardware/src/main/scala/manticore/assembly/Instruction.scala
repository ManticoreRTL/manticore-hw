package manticore.assembly

import scala.language.implicitConversions


object Instruction {


  sealed trait RegisterTrait {
    def DATA_WIDTH: Int = 16
    def ADDRESS_WIDTH: Int = 11
  }

  case class Register(index: Int) extends RegisterTrait {
    override def toString: String = s"R[${index}]"
  }

  object R {
    def apply(index: Int) = Register(index)
  }

  implicit def registerToIndex(reg: Register): Int = reg.index


  object Opcode extends Enumeration {
    type Type = Value
    val NOP, SET, CUST0, ARITH, LLOAD, LSTORE, EXPECT, GLOAD, GSTORE, SEND = Value
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

  case class Custom0(rd: Register, function: CustomFunction,
                     rs1: Register, rs2: Register, rs3: Register, rs4: Register)
    extends Instruction(Opcode.CUST0)



  sealed abstract class ArithmeticInstruction extends Instruction(Opcode.ARITH)
  case class Add2(rd: Register, rs1: Register, rs2: Register) extends ArithmeticInstruction
  case class Or2(rd: Register, rs1: Register, rs2: Register) extends ArithmeticInstruction
  case class And2(rd: Register, rs1: Register, rs2: Register) extends ArithmeticInstruction
  case class Xor2(rd: Register, rs1: Register, rs2: Register) extends ArithmeticInstruction
  case class Mult2(rd: Register, rs1: Register, rs2: Register) extends ArithmeticInstruction
  case class Not(rd: Register, rs1: Register) extends ArithmeticInstruction
  case class SetEqual(rd: Register, rs1: Register, rs2: Register) extends ArithmeticInstruction
  case class SetLessThanUnsigned(rd: Register, rs1: Register, rs2: Register) extends ArithmeticInstruction
  case class SetLessThanSigned(rd: Register, rs1: Register, rs2: Register) extends ArithmeticInstruction
  case class SetGreaterThanUnsigned(rd: Register, rs1: Register, rs2: Register) extends ArithmeticInstruction
  case class SetGreaterThanSigned(rd: Register, rs1: Register, rs2: Register) extends ArithmeticInstruction
  case class Mux2(rd: Register, rs1: Register, rs2: Register) extends  ArithmeticInstruction

  case class LocalLoad(rd: Register, base: Register, offset: Long) extends Instruction(Opcode.LLOAD)
  case class LocalStore(rs: Register, base: Register, offset: Long) extends Instruction(Opcode.LSTORE)

  case class Expect(value: Register, expected: Register, id: Int) extends Instruction(Opcode.EXPECT)

  case class GlobalLoad(rd: Register, addrlo: Register, addrmid: Register, addrhi: Register) extends
    Instruction(Opcode.GLOAD)
  case class GlobalStore(rs: Register, addrlo: Register, addrmid: Register, addrhi: Register) extends
    Instruction(Opcode.GSTORE)

  case class SetValue(rd: Register, value: Int) extends Instruction(Opcode.SET)

  case class Send(target: Register, rs: Register, addressX: Long, addressY: Long) extends Instruction(Opcode.SEND)

  case class Nop() extends Instruction(Opcode.NOP)

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