package manticore.machine.assembly

import manticore.machine.ISA
import manticore.machine.ManticoreBaseISA
import chisel3.util.log2Ceil

object Assembler {

  import Instruction._

  private def clipped(x: Int, bits: Int = 16): Int = ((1 << bits) - 1) & x

  private case class BinaryField(value: Long, num_bits: Int)
  private case class BinaryInstructionBuilder(
      val fields: Seq[BinaryField] = Seq()
  ) {
    def ++(value: Int, num_bits: Int): BinaryInstructionBuilder = {
      val pos = fields.map(_.num_bits).sum.toLong
      BinaryInstructionBuilder(
        BinaryField(
          (clipped(value, num_bits).toLong) << pos,
          num_bits
        ) +: fields
      )
    }
    def build: Long = {
      val bits = fields.map(_.num_bits).sum
      require(
        bits == 64,
        "Failed building instruction " + fields + " because the length is " + bits
      )
      fields.map(_.value).reduce(_ | _)
    }
  }

  def assemble(
      instruction: Instruction
  )(implicit equation: Seq[Equation]): Long = {

    def arithmetic(
        funct: ISA.Functs.Functs
    )(rd: Register, rs1: Register, rs2: Register, sel: Register = R(0)): Long = {
      val inst = BinaryInstructionBuilder() ++
        (ManticoreBaseISA.Arithmetic.value, ManticoreBaseISA.OpcodeBits) ++
        (rd.index, ManticoreBaseISA.IdBits) ++
        (funct.id, ManticoreBaseISA.FunctBits) ++
        (rs1.index, ManticoreBaseISA.IdBits) ++
        (rs2.index, ManticoreBaseISA.IdBits) ++
        (sel.index, ManticoreBaseISA.IdBits) ++
        (0, ManticoreBaseISA.IdBits)
      inst.build
    }

    instruction match {
      case Custom(rd, func, rs1, rs2, rs3, rs4) =>
        val funct_id = equation.indexOf(func.equation)
        val inst = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.Custom.value, ManticoreBaseISA.OpcodeBits) ++
          (rd.index, ManticoreBaseISA.IdBits) ++
          (funct_id, ManticoreBaseISA.FunctBits) ++
          (rs1.index, ManticoreBaseISA.IdBits) ++
          (rs2.index, ManticoreBaseISA.IdBits) ++
          (rs3.index, ManticoreBaseISA.IdBits) ++
          (rs4.index, ManticoreBaseISA.IdBits)
        inst.build

      case SetLutData(id, value) =>
        val inst = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.SetLutData.value, ManticoreBaseISA.OpcodeBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (id, ManticoreBaseISA.FunctBits) ++
          (0, 4 * ManticoreBaseISA.IdBits - ManticoreBaseISA.DataBits) ++
          (value, ManticoreBaseISA.DataBits)
        inst.build

      case ConfigureLuts(rs1, rs2, rs3, rs4) =>
        val inst = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.ConfigureLuts.value, ManticoreBaseISA.OpcodeBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (0, ManticoreBaseISA.FunctBits) ++
          (rs1.index, ManticoreBaseISA.IdBits) ++
          (rs2.index, ManticoreBaseISA.IdBits) ++
          (rs3.index, ManticoreBaseISA.IdBits) ++
          (rs4.index, ManticoreBaseISA.IdBits)
        inst.build

      case Slice(rd, rs, offset, length) =>
        // We compute the slice mask at compile time and embed it in the
        // instruction's immediate field.
        // We use the ALU's built-in SRL instruction and just mask the
        // output appropriately.
        val inst = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.Slice.value, ManticoreBaseISA.OpcodeBits) ++
          (rd.index, ManticoreBaseISA.IdBits) ++
          (ISA.Functs.SRL.id, ManticoreBaseISA.FunctBits) ++
          (rs.index, ManticoreBaseISA.IdBits) ++
          (0, 3 * ManticoreBaseISA.IdBits - log2Ceil(ManticoreBaseISA.DataBits) - ManticoreBaseISA.DataBits) ++
          (offset.toInt, log2Ceil(ManticoreBaseISA.DataBits)) ++
          ((1 << length.toInt) - 1, ManticoreBaseISA.DataBits)
        inst.build

      case Add2(rd, rs1, rs2) =>
        arithmetic(ISA.Functs.ADD2)(rd, rs1, rs2)
      case Sub2(rd, rs1, rs2) =>
        arithmetic(ISA.Functs.SUB2)(rd, rs1, rs2)
      case Or2(rd, rs1, rs2) =>
        arithmetic(ISA.Functs.OR2)(rd, rs1, rs2)
      case And2(rd, rs1, rs2) =>
        arithmetic(ISA.Functs.AND2)(rd, rs1, rs2)
      case Xor2(rd, rs1, rs2) =>
        arithmetic(ISA.Functs.XOR2)(rd, rs1, rs2)
      case Mul2(rd, rs1, rs2) =>
        arithmetic(ISA.Functs.MUL2)(rd, rs1, rs2)
      case Mul2H(rd, rs1, rs2) =>
        arithmetic(ISA.Functs.MUL2H)(rd, rs1, rs2)
      case SetEqual(rd, rs1, rs2) =>
        arithmetic(ISA.Functs.SEQ)(rd, rs1, rs2)
      case SetLessThanSigned(rd, rs1, rs2) =>
        arithmetic(ISA.Functs.SLTS)(rd, rs1, rs2)
      case SetLessThanUnsigned(rd, rs1, rs2) =>
        arithmetic(ISA.Functs.SLTU)(rd, rs1, rs2)
      case ShiftLeftLogic(rd, rs1, rs2) =>
        arithmetic(ISA.Functs.SLL)(rd, rs1, rs2)
      case ShiftRightLogic(rd, rs1, rs2) =>
        arithmetic(ISA.Functs.SRL)(rd, rs1, rs2)
      case ShiftRightArithmetic(rd, rs1, rs2) =>
        arithmetic(ISA.Functs.SRA)(rd, rs1, rs2)
      case Mux2(rd, tval, fval, sel) =>
        arithmetic(ISA.Functs.MUX)(rd, tval, fval, sel)
      case LocalLoad(rd, base, offset) =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.LocalLoad.value, ManticoreBaseISA.OpcodeBits) ++
          (rd.index, ManticoreBaseISA.IdBits) ++
          (ISA.Functs.ADD2.id, ManticoreBaseISA.FunctBits) ++
          (base.index, ManticoreBaseISA.IdBits) ++
          (0, 3 * ManticoreBaseISA.IdBits - ManticoreBaseISA.DataBits) ++
          (offset.toInt, ManticoreBaseISA.DataBits)
        inst.build
      case LocalStore(rs, base, offset) =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.LocalStore.value, ManticoreBaseISA.OpcodeBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (ISA.Functs.ADD2.id, ManticoreBaseISA.FunctBits) ++
          (base.index, ManticoreBaseISA.IdBits) ++
          (rs.index, ManticoreBaseISA.IdBits) ++
          (0, 2 * ManticoreBaseISA.IdBits - ManticoreBaseISA.DataBits) ++
          (offset.toInt, ManticoreBaseISA.DataBits)
        inst.build
      case SetValue(rd, value) =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.SetValue.value, ManticoreBaseISA.OpcodeBits) ++
          (rd.index, ManticoreBaseISA.IdBits) ++
          (ISA.Functs.ADD2.id, ManticoreBaseISA.FunctBits) ++
          (0, 4 * ManticoreBaseISA.IdBits - ManticoreBaseISA.DataBits) ++
          (value, ManticoreBaseISA.DataBits)
        inst.build
      case Expect(value, expected, id) =>
        require(2 * ManticoreBaseISA.IdBits >= ManticoreBaseISA.DataBits)
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.Expect.value, ManticoreBaseISA.OpcodeBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (ISA.Functs.SEQ.id, ManticoreBaseISA.FunctBits) ++
          (value.index, ManticoreBaseISA.IdBits) ++
          (expected.index, ManticoreBaseISA.IdBits) ++
          (0, 2 * ManticoreBaseISA.IdBits - ManticoreBaseISA.DataBits) ++
          (id, ManticoreBaseISA.DataBits)
        inst.build
      case Nop() =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.Nop.value, ManticoreBaseISA.OpcodeBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (ISA.Functs.ADD2.id, ManticoreBaseISA.FunctBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (0, 2 * ManticoreBaseISA.IdBits)
        inst.build
      case Send(target, rs, addrX, addrY) =>
        require(ManticoreBaseISA.DataBits % 2 == 0)
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.Send.value, ManticoreBaseISA.OpcodeBits) ++
          (target.index, ManticoreBaseISA.IdBits) ++
          (ISA.Functs.ADD2.id, ManticoreBaseISA.FunctBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (rs.index, ManticoreBaseISA.IdBits) ++
          (0, 2 * ManticoreBaseISA.IdBits - ManticoreBaseISA.DataBits) ++
          (addrX.toInt + (addrY << (ManticoreBaseISA.DataBits / 2)).toInt, ManticoreBaseISA.DataBits)
        inst.build
      case GlobalLoad(rd, addrlo, addrmid, addrhi) =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.GlobalLoad.value, ManticoreBaseISA.OpcodeBits) ++
          (rd.index, ManticoreBaseISA.IdBits) ++
          (ISA.Functs.ADD2.id, ManticoreBaseISA.FunctBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (addrhi.index, ManticoreBaseISA.IdBits) ++
          (addrmid.index, ManticoreBaseISA.IdBits) ++
          (addrlo.index, ManticoreBaseISA.IdBits)
        inst.build
      case GlobalStore(rs, addrlo, addrmid, addrhi) =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.GlobalStore.value, ManticoreBaseISA.OpcodeBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (ISA.Functs.ADD2.id, ManticoreBaseISA.FunctBits) ++
          (rs, ManticoreBaseISA.IdBits) ++
          (addrhi, ManticoreBaseISA.IdBits) ++
          (addrmid, ManticoreBaseISA.IdBits) ++
          (addrlo.index, ManticoreBaseISA.IdBits)
        inst.build
      case Predicate(rs) =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.Predicate.value, ManticoreBaseISA.OpcodeBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (ISA.Functs.ADD2.id, ManticoreBaseISA.FunctBits) ++
          (rs, ManticoreBaseISA.IdBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (0, ManticoreBaseISA.IdBits)
        inst.build
      case _ => throw new Exception(s"${instruction} not implemented!")
    }
  }
}
