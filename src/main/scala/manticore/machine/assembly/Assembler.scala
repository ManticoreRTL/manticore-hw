package manticore.machine.assembly

import manticore.machine.ManticoreBaseISA
import manticore.machine.core.alu.StandardALU

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
        funct: StandardALU.Functs.Functs
    )(rd: Register, rs1: Register, rs2: Register): Long = {
      val inst = BinaryInstructionBuilder() ++
        (ManticoreBaseISA.Arithmetic.value, ManticoreBaseISA.OpcodeBits) ++
        (rd.index, ManticoreBaseISA.IdBits) ++
        (funct.id, ManticoreBaseISA.FunctBits) ++
        (rs1.index, ManticoreBaseISA.IdBits) ++
        (rs2.index, ManticoreBaseISA.IdBits) ++
        (0, 2 * ManticoreBaseISA.IdBits)
      inst.build
    }

    instruction match {
      case Custom(rd, func, rs1, rs2, rs3, rs4) =>
        val inst = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.Custom.value, ManticoreBaseISA.OpcodeBits) ++
          (rd.index, ManticoreBaseISA.IdBits) ++
          (equation.indexOf(func), ManticoreBaseISA.FunctBits) ++
          (rs1.index, ManticoreBaseISA.IdBits) ++
          (rs2.index, ManticoreBaseISA.IdBits) ++
          (rs3.index, ManticoreBaseISA.IdBits) ++
          (rs4.index, ManticoreBaseISA.IdBits)
        inst.build
      case Add2(rd, rs1, rs2) =>
        arithmetic(StandardALU.Functs.ADD2)(rd, rs1, rs2)
      case Or2(rd, rs1, rs2) =>
        arithmetic(StandardALU.Functs.OR2)(rd, rs1, rs2)
      case And2(rd, rs1, rs2) =>
        arithmetic(StandardALU.Functs.AND2)(rd, rs1, rs2)
      case Xor2(rd, rs1, rs2) =>
        arithmetic(StandardALU.Functs.XOR2)(rd, rs1, rs2)
      case Mult2(rd, rs1, rs2) =>
        arithmetic(StandardALU.Functs.MUL2)(rd, rs1, rs2)
      case SetEqual(rd, rs1, rs2) =>
        arithmetic(StandardALU.Functs.SEQ)(rd, rs1, rs2)
      case SetLessThanSigned(rd, rs1, rs2) =>
        arithmetic(StandardALU.Functs.SLTS)(rd, rs1, rs2)
      case Mux2(rd, rs1, rs2) =>
        arithmetic(StandardALU.Functs.MUX)(rd, rs1, rs2)
      case LocalLoad(rd, base, offset) =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.LocalLoad.value, ManticoreBaseISA.OpcodeBits) ++
          (rd.index, ManticoreBaseISA.IdBits) ++
          (StandardALU.Functs.ADD2.id, ManticoreBaseISA.FunctBits) ++
          (base.index, ManticoreBaseISA.IdBits) ++
          (0, 3 * ManticoreBaseISA.IdBits - ManticoreBaseISA.DataBits) ++
          (offset.toInt, ManticoreBaseISA.DataBits)
        inst.build
      case LocalStore(rs, base, offset) =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.LocalStore.value, ManticoreBaseISA.OpcodeBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (StandardALU.Functs.ADD2.id, ManticoreBaseISA.FunctBits) ++
          (base.index, ManticoreBaseISA.IdBits) ++
          (rs.index, ManticoreBaseISA.IdBits) ++
          (0, 2 * ManticoreBaseISA.IdBits - ManticoreBaseISA.DataBits) ++
          (offset.toInt, ManticoreBaseISA.DataBits)
        inst.build
      case SetValue(rd, value) =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.SetValue.value, ManticoreBaseISA.OpcodeBits) ++
          (rd.index, ManticoreBaseISA.IdBits) ++
          (StandardALU.Functs.ADD2.id, ManticoreBaseISA.FunctBits) ++
          (0, 4 * ManticoreBaseISA.IdBits - ManticoreBaseISA.DataBits) ++
          (value, ManticoreBaseISA.DataBits)
        inst.build
      case Expect(value, expected, id) =>
        require(2 * ManticoreBaseISA.IdBits >= ManticoreBaseISA.DataBits)
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.Expect.value, ManticoreBaseISA.OpcodeBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (StandardALU.Functs.SEQ.id, ManticoreBaseISA.FunctBits) ++
          (value.index, ManticoreBaseISA.IdBits) ++
          (expected.index, ManticoreBaseISA.IdBits) ++
          (0, 2 * ManticoreBaseISA.IdBits - ManticoreBaseISA.DataBits) ++
          (id, ManticoreBaseISA.DataBits)
        inst.build
      case Nop() =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.Nop.value, ManticoreBaseISA.OpcodeBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (StandardALU.Functs.ADD2.id, ManticoreBaseISA.FunctBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (0, 2 * ManticoreBaseISA.IdBits)
        inst.build
      case Send(target, rs, addrX, addrY) =>
        require(ManticoreBaseISA.DataBits % 2 == 0)
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.Send.value, ManticoreBaseISA.OpcodeBits) ++
          (target.index, ManticoreBaseISA.IdBits) ++
          (StandardALU.Functs.ADD2.id, ManticoreBaseISA.FunctBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (rs.index, ManticoreBaseISA.IdBits) ++
          (0, 2 * ManticoreBaseISA.IdBits - ManticoreBaseISA.DataBits) ++
          (addrX.toInt + (addrY << (ManticoreBaseISA.DataBits / 2)).toInt, ManticoreBaseISA.DataBits)
        inst.build
      case GlobalLoad(rd, addrlo, addrmid, addrhi) =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.GlobalLoad.value, ManticoreBaseISA.OpcodeBits) ++
          (rd.index, ManticoreBaseISA.IdBits) ++
          (StandardALU.Functs.ADD2.id, ManticoreBaseISA.FunctBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (addrhi.index, ManticoreBaseISA.IdBits) ++
          (addrmid.index, ManticoreBaseISA.IdBits) ++
          (addrlo.index, ManticoreBaseISA.IdBits)
        inst.build
      case GlobalStore(rs, addrlo, addrmid, addrhi) =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.GlobalStore.value, ManticoreBaseISA.OpcodeBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (StandardALU.Functs.ADD2.id, ManticoreBaseISA.FunctBits) ++
          (rs, ManticoreBaseISA.IdBits) ++
          (addrhi, ManticoreBaseISA.IdBits) ++
          (addrmid, ManticoreBaseISA.IdBits) ++
          (addrlo.index, ManticoreBaseISA.IdBits)
        inst.build
      case Predicate(rs) =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ManticoreBaseISA.Predicate.value, ManticoreBaseISA.OpcodeBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (StandardALU.Functs.ADD2.id, ManticoreBaseISA.FunctBits) ++
          (rs, ManticoreBaseISA.IdBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (0, ManticoreBaseISA.IdBits) ++
          (0, ManticoreBaseISA.IdBits)
        inst.build
      case _ => throw new Exception(s"${instruction} not implemented!")
    }
  }
}
