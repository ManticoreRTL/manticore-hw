package thyrio.assembly

import thyrio.ThyrioISA
import thyrio.core.alu.StandardALU



object Assembler {

  import thyrio.assembly.Instruction._


  private def clipped(x: Int, bits: Int = 16): Int = ((1 << bits) - 1) & x

  private case class BinaryField(value: Long, num_bits: Int)
  private case class BinaryInstructionBuilder(val fields: Seq[BinaryField] = Seq()) {
    def ++(value: Int, num_bits: Int): BinaryInstructionBuilder = {
      val pos = fields.map(_.num_bits).sum.toLong
      BinaryInstructionBuilder(BinaryField((clipped(value, num_bits).toLong) << pos, num_bits) +: fields)
    }
    def build: Long = {
      val bits = fields.map(_.num_bits).sum
      require(bits == 64, "Failed building instruction " + fields + " because the length is " + bits)
      fields.map(_.value).reduce(_|_)
    }
  }

  def assemble(inst: Instruction)(implicit equation: Seq[Equation]): Long = {

    def arithmetic(funct: StandardALU.Functs.Functs)(rd: Register, rs1: Register, rs2: Register): Long = {
      val inst = BinaryInstructionBuilder() ++
        (ThyrioISA.Arithmetic.value, ThyrioISA.OpcodeBits) ++
        (rd.index, ThyrioISA.IdBits) ++
        (funct.id, ThyrioISA.FunctBits) ++
        (rs1.index, ThyrioISA.IdBits) ++
        (rs2.index, ThyrioISA.IdBits) ++
        (0, 2 * ThyrioISA.IdBits)
      inst.build
    }

    inst match {
      case Custom0(rd, func, rs1, rs2, rs3, rs4) =>
        val inst = BinaryInstructionBuilder() ++
            (ThyrioISA.Custom0.value, ThyrioISA.OpcodeBits) ++
            (rd.index, ThyrioISA.IdBits) ++
            (equation.indexOf(func), ThyrioISA.FunctBits) ++
            (rs1.index, ThyrioISA.IdBits) ++
            (rs2.index, ThyrioISA.IdBits) ++
            (rs3.index, ThyrioISA.IdBits) ++
            (rs4.index, ThyrioISA.IdBits)
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
      case SetLessThanUnsigned(rd, rs1, rs2) =>
        arithmetic(StandardALU.Functs.SLTU)(rd, rs1, rs2)
      case SetLessThanSigned(rd, rs1, rs2) =>
        arithmetic(StandardALU.Functs.SLTS)(rd, rs1, rs2)
      case SetGreaterThanUnsigned(rd, rs1, rs2) =>
        arithmetic(StandardALU.Functs.SGTU)(rd, rs1, rs2)
      case SetGreaterThanSigned(rd, rs1, rs2) =>
        arithmetic(StandardALU.Functs.SGTS)(rd, rs1, rs2)
      case Mux2(rd, rs1, rs2) =>
        arithmetic(StandardALU.Functs.MUX)(rd, rs1, rs2)
      case LocalLoad(rd, base, offset) =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ThyrioISA.LocalLoad.value, ThyrioISA.OpcodeBits) ++
          (rd.index, ThyrioISA.IdBits) ++
          (StandardALU.Functs.ADD2.id, ThyrioISA.FunctBits) ++
          (base.index, ThyrioISA.IdBits) ++
          (0, 3 * ThyrioISA.IdBits - ThyrioISA.DataBits) ++
          (offset.toInt, ThyrioISA.DataBits)
        inst.build
      case LocalStore(rs, base, offset) =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ThyrioISA.LocalStore.value, ThyrioISA.OpcodeBits) ++
          (0, ThyrioISA.IdBits) ++
          (StandardALU.Functs.ADD2.id, ThyrioISA.FunctBits) ++
          (base.index, ThyrioISA.IdBits) ++
          (rs.index, ThyrioISA.IdBits) ++
          (0, 2 * ThyrioISA.IdBits - ThyrioISA.DataBits) ++
          (offset.toInt, ThyrioISA.DataBits)
        inst.build
      case SetValue(rd, value) =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ThyrioISA.SetValue.value, ThyrioISA.OpcodeBits) ++
          (rd.index, ThyrioISA.IdBits) ++
          (StandardALU.Functs.ADD2.id, ThyrioISA.FunctBits) ++
          (0, 4 * ThyrioISA.IdBits - ThyrioISA.DataBits) ++
          (value, ThyrioISA.DataBits)
        inst.build
      case Expect(value, expected, message) =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ThyrioISA.Expect.value, ThyrioISA.OpcodeBits) ++
          (0, ThyrioISA.IdBits) ++
          (StandardALU.Functs.SEQ.id, ThyrioISA.FunctBits) ++
          (value.index, ThyrioISA.IdBits) ++
          (expected.index, ThyrioISA.IdBits) ++
          (0, 2 * ThyrioISA.IdBits)
        inst.build
      case Nop() =>
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ThyrioISA.SetValue.value, ThyrioISA.OpcodeBits) ++
          (0, ThyrioISA.IdBits) ++
          (StandardALU.Functs.ADD2.id, ThyrioISA.FunctBits) ++
          (0, ThyrioISA.IdBits) ++
          (0, ThyrioISA.IdBits) ++
          (0, 2 * ThyrioISA.IdBits)
        inst.build
      case Send(target, rs, addrX, addrY) =>
        require(ThyrioISA.DataBits % 2 == 0)
        val inst: BinaryInstructionBuilder = BinaryInstructionBuilder() ++
          (ThyrioISA.Send.value, ThyrioISA.OpcodeBits) ++
          (target.index, ThyrioISA.IdBits) ++
          (StandardALU.Functs.ADD2.id, ThyrioISA.FunctBits) ++
          (0, ThyrioISA.IdBits) ++
          (rs.index, ThyrioISA.IdBits) ++
          (0, 2 * ThyrioISA.IdBits - ThyrioISA.DataBits) ++
          (addrX.toInt + (addrY << (ThyrioISA.DataBits / 2)).toInt, ThyrioISA.DataBits)
        inst.build
      case GlobalLoad(rd, addrlo, addrmid, addrhi, offset) => ???
      case GlobalStore(rs, addrlo, addrmid, addrhi, offset) => ???
      case _ => ???
    }


  }
}
