package manticore.machine.assembly

import scala.language.implicitConversions

object Instruction {

  sealed trait RegisterTrait {
    def DATA_WIDTH: Int    = 16
    def ADDRESS_WIDTH: Int = 11
  }

  case class Register(
      index: Int,
      name: String = ""
  ) extends RegisterTrait {
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
    val NOP, SET, CUST, ARITH, LLOAD, LSTORE, EXPECT, GLOAD, GSTORE, SEND, PREDICATE, SETCARRY, CONFIGCFU, SLICE = Value
  }

  sealed abstract class Instruction(val opcode: Opcode.Type)

  type Equation = Seq[BigInt]
  case class CustomFunction(equation: Equation) {
    def ==(other: CustomFunction): Boolean = {
      (equation.size == other.equation.size) &&
      equation.zip(other.equation).forall(x => x._1 == x._2)
    }

    override def toString: String =
      s"CustomFunction{${equation.map(s"0x%x".format(_)).mkString(",")}}"
  }

  case class Custom(
      rd: Register,
      function: CustomFunction,
      rs1: Register,
      rs2: Register,
      rs3: Register,
      rs4: Register
  ) extends Instruction(Opcode.CUST) {
    override def toString: String = s"CUSTOM\t${rd}, ${function}, ${rs1}, ${rs2}, ${rs3}, ${rs4}"
  }

  sealed abstract class ArithmeticInstruction extends Instruction(Opcode.ARITH)

  sealed abstract class BinaryArithmeticInstruction(
      rd: Register,
      rs1: Register,
      rs2: Register,
      op: String
  ) extends ArithmeticInstruction {
    override def toString(): String = s"${op}\t${rd}, ${rs1}, ${rs2}"
  }

  case class Add2(
      rd: Register,
      rs1: Register,
      rs2: Register
  ) extends BinaryArithmeticInstruction(rd, rs1, rs2, "ADD")

  case class Sub2(
      rd: Register,
      rs1: Register,
      rs2: Register
  ) extends BinaryArithmeticInstruction(rd, rs1, rs2, "SUB")

  case class Or2(
      rd: Register,
      rs1: Register,
      rs2: Register
  ) extends BinaryArithmeticInstruction(rd, rs1, rs2, "OR")

  case class And2(
      rd: Register,
      rs1: Register,
      rs2: Register
  ) extends BinaryArithmeticInstruction(rd, rs1, rs2, "AND")

  case class Xor2(
      rd: Register,
      rs1: Register,
      rs2: Register
  ) extends BinaryArithmeticInstruction(rd, rs1, rs2, "XOR")

  case class Mul2(
      rd: Register,
      rs1: Register,
      rs2: Register
  ) extends BinaryArithmeticInstruction(rd, rs1, rs2, "MUL")

  case class Mul2H(
      rd: Register,
      rs1: Register,
      rs2: Register
  ) extends BinaryArithmeticInstruction(rd, rs1, rs2, "MULH")

  case class SetEqual(
      rd: Register,
      rs1: Register,
      rs2: Register
  ) extends BinaryArithmeticInstruction(rd, rs1, rs2, "SEQ")

  case class SetLessThanUnsigned(
      rd: Register,
      rs1: Register,
      rs2: Register
  ) extends BinaryArithmeticInstruction(rd, rs1, rs2, "SLTU")

  case class SetLessThanSigned(
      rd: Register,
      rs1: Register,
      rs2: Register
  ) extends BinaryArithmeticInstruction(rd, rs1, rs2, "SLTS")

  case class Mux2(
      rd: Register,
      fval: Register,
      tval: Register,
      sel: Register
  ) extends ArithmeticInstruction {
    override def toString: String = s"MUX\t${rd}, ${fval}, ${tval}, ${sel}"
  }

  case class Addc(
      rd: Register,
      rs1: Register,
      rs2: Register,
      cin: Register
  ) extends ArithmeticInstruction {
    override def toString: String = s"ADDC\t${rd}, ${rs1}, ${rs2}, ${cin}"
  }

  case class ShiftLeftLogic(
      rd: Register,
      rs1: Register,
      rs2: Register
  ) extends BinaryArithmeticInstruction(rd, rs1, rs2, "SLL")

  case class ShiftRightLogic(
      rd: Register,
      rs1: Register,
      rs2: Register
  ) extends BinaryArithmeticInstruction(rd, rs1, rs2, "SRL")

  case class ShiftRightArithmetic(
      rd: Register,
      rs1: Register,
      rs2: Register
  ) extends BinaryArithmeticInstruction(rd, rs1, rs2, "SRA")

  case class LocalLoad(
      rd: Register,
      base: Register,
      offset: Long
  ) extends Instruction(Opcode.LLOAD) {
    override def toString: String = s"LLOAD\t${rd}, ${base}(${offset})"
  }

  case class LocalStore(
      rs: Register,
      base: Register,
      offset: Long
  ) extends Instruction(Opcode.LSTORE) {
    override def toString: String = s"LSTORE\t${rs}, ${base}(${offset})"
  }

  case class Expect(
      value: Register,
      expected: Register,
      id: Int
  ) extends Instruction(Opcode.EXPECT) {
    override def toString: String = s"EXPECT\t${value}, ${expected}, ${id}"
  }

  case class GlobalLoad(
      rd: Register,
      addrlo: Register,
      addrmid: Register,
      addrhi: Register
  ) extends Instruction(Opcode.GLOAD) {
    override def toString: String =
      s"GLOAD\t${rd}, [${addrhi}, ${addrmid}, ${addrlo}]"
  }

  case class GlobalStore(
      rs: Register,
      addrlo: Register,
      addrmid: Register,
      addrhi: Register
  ) extends Instruction(Opcode.GSTORE) {
    override def toString: String =
      s"GSTORE\t${rs}, [${addrhi}, ${addrmid}, ${addrlo}]"
  }

  case class SetValue(
      rd: Register,
      value: Int
  ) extends Instruction(Opcode.SET) {
    override def toString: String = s"SET\t${rd}, ${value}"
  }

  case class ConfigCfu(
      ramIdx: Int,
      functIdx: Int,
      equation: Int
  ) extends Instruction(Opcode.CONFIGCFU) {
    override def toString: String = s"CONFIGCFU\t${ramIdx}, ${functIdx}, ${equation}"
  }

  case class Send(
      target: Register,
      rs: Register,
      addressX: Long,
      addressY: Long
  ) extends Instruction(Opcode.SEND) {
    override def toString: String =
      s"SEND\t${target}, ${rs}, ${addressX}, ${addressY}"
  }

  case class Predicate(
      rs: Register
  ) extends Instruction(Opcode.PREDICATE) {
    override def toString: String = s"PREDICATE\t${rs}"
  }

  case class Nop() extends Instruction(Opcode.NOP) {
    override def toString: String = s"NOP"
  }

  case class Slice(
      rd: Register,
      rs: Register,
      offset: Int,
      length: Int
  ) extends Instruction(Opcode.SLICE) {
    override def toString: String = s"SLICE ${rd}, ${rs}[${offset} +: ${length}]"
  }

}
