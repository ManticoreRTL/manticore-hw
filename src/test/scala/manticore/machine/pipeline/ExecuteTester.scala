package manticore.machine.pipeline

import Chisel._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.tester._
import chiseltest.ChiselScalatestTester
import chiseltest.VerilatorBackendAnnotation
import manticore.machine.ManticoreBaseISA
import manticore.machine.assembly.Instruction.Opcode
import manticore.machine.core.ExecuteBase
import manticore.machine.core.ExecuteComb
import manticore.machine.core.ExecuteInterface.OpcodePipe
import manticore.machine.core.alu.StandardALU.Functs
import manticore.machine.core.alu.StandardALU.Functs.Functs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExecuteTester extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  val rdgen = new scala.util.Random(0)

  // create random LUT equations
  val equations = Seq.fill(32)(Seq.fill(16)(BigInt(rdgen.nextInt(1 << 16))))

  behavior of "Execute stage"

  def computeCustom(rs1: Int, rs2: Int, rs3: Int, rs4: Int)(equ: Seq[BigInt]): Int = {
    import manticore.machine.assembly.Instruction.{Custom, CustomFunction, R, SetValue}
    import manticore.machine.assembly._
    val interpreter = new Interpreter
    interpreter.run(
      Array(
        SetValue(R(1), rs1),
        SetValue(R(2), rs2),
        SetValue(R(3), rs3),
        SetValue(R(4), rs4),
        Custom(R(5), CustomFunction(equ), R(1), R(2), R(3), R(4))
      )
    )
    interpreter.env.register_file(5)
  }

  def computeStandard(x: Int, y: Int, funct: Functs): Int = ALUSpec.compute(x, y, funct)

  def randomValue = rdgen.nextInt(1 << ManticoreBaseISA.DataBits)

  def randomOpcode = {
    val which = Opcode(rdgen.nextInt(9))
    (new OpcodePipe(32).Lit(
      _.cust -> (which == Opcode.CUST).B,
      _.arith -> (which == Opcode.ARITH).B,
      _.lload -> (which == Opcode.LLOAD).B,
      _.lstore -> (which == Opcode.LSTORE).B,
      _.send -> (which == Opcode.SEND).B,
      _.set -> (which == Opcode.SET).B,
      _.expect -> (which == Opcode.EXPECT).B,
      _.gload -> (which == Opcode.GLOAD).B,
      _.gstore -> (which == Opcode.GSTORE).B,
      _.nop -> (which == Opcode.NOP).B
    ), which)
  }

  def clipped(x: Int): Int = x & 0x0000FFFF

  case class ExpectedResult(opcode: Opcode.Type,
                            res: Int, funct: Int, rd: Int, x: Int, y: Int, u: Int, v: Int) {
    override def toString = s"opcode: ${opcode}, rd: ${rd}, " +
      s"funct: ${if (opcode == Opcode.ARITH) Functs(funct) else equations(funct)}, x: ${x}, " +
      s"y: ${y}, u: ${u}, v: ${v}, res: ${res}"
  }

  def setPipeIn(implicit dut: ExecuteBase): ExpectedResult = {
    val (opcode_pipe, opcode) = randomOpcode
    val x, y, u, v = randomValue
    val imm = randomValue
    val rd = rdgen.nextInt(1 << ManticoreBaseISA.IdBits)
    dut.io.regs_in.rs1.poke(x.U)
    dut.io.regs_in.rs2.poke(y.U)
    dut.io.regs_in.rs3.poke(u.U)
    dut.io.regs_in.rs4.poke(v.U)

    dut.io.pipe_in.opcode.poke(opcode_pipe)
    dut.io.pipe_in.immediate.poke(imm.U)
    dut.io.pipe_in.rd.poke(rd.U)

    // pre compute the data and result
    val e = opcode match {
      case Opcode.CUST =>
        val funct = rdgen.nextInt(32)
        val res = computeCustom(x, y, u, v) {
          equations(funct)
        }
        require(res >= 0)
        dut.io.pipe_in.funct.poke(funct.U)
        ExpectedResult(opcode, res, funct, rd, x, y, u, v)
      case Opcode.ARITH =>
        val funct = rdgen.nextInt(Functs.maxId - 1)
        val yy = if (Functs(funct) == Functs.SLL || Functs(funct) == Functs.SRL) rdgen.nextInt(16) else y
        dut.io.regs_in.rs2.poke(yy.U)
        val res = computeStandard(x, yy, Functs(funct))
        require(res >= 0)
        dut.io.pipe_in.funct.poke(funct.U)
        ExpectedResult(opcode, res, funct, rd, x, yy, u, v)
      case Opcode.EXPECT =>
        val funct = rdgen.nextInt(16)
        val res = computeStandard(x, y, Functs.SEQ)
        require(res >= 0)
        dut.io.pipe_in.funct.poke(funct.U)
        ExpectedResult(opcode, res, funct, rd, x, y, u, v)
      case o@_ =>
        val res =
          if (o == Opcode.SET || o == Opcode.SEND) {
            imm
          } else {
            computeStandard(x, imm, Functs.ADD2)
          }
        require(res >= 0)
        dut.io.pipe_in.funct.poke(Functs.ADD2.id.U)
        ExpectedResult(opcode, res, Functs.ADD2.id, rd, x, y, u, v)
    }
    dut.clock.step()
    e

  }

  def drainAfter(count: Int)(expected: Seq[ExpectedResult])(implicit dut: ExecuteBase): Unit = {
    def check(e: ExpectedResult): Unit = {
      dut.io.pipe_out.data.expect(e.y.U, s"Expected %s".format(e))
      dut.io.pipe_out.result.expect(e.res.U, s"Expected %s".format(e))
      //          dut.io.pipe_out.opcode.expect(e.opcode, s"Expected %s".format(e.opcode))
      dut.io.pipe_out.rd.expect(e.rd.U, s"Expected %s".format(e))
    }

    if (count > 0) {
      check(expected.head)
      val new_req = setPipeIn
      drainAfter(count - 1)(expected.tail :+ new_req)
    } else if (expected.nonEmpty) {
      check(expected.head)
      dut.clock.step()
      drainAfter(0)(expected.tail)
    }
  }

  it should "correctly handle computation and send out data and result in a single cycle" in
    test(new ExecuteComb(ManticoreBaseISA, equations = equations)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      drainAfter(3000)(Seq.fill(1) {
        setPipeIn(dut)
      })(dut)
      dut.clock.step()
    }

}
