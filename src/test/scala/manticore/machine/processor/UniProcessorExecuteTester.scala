package manticore.machine.processor

import chisel3._
import chisel3.util.log2Ceil
import chiseltest._
import manticore.machine.ISA.Functs._
import manticore.machine.ManticoreBaseISA
import manticore.machine.UIntWide
import manticore.machine.assembly.Assembler
import manticore.machine.assembly.Instruction
import manticore.machine.assembly.Instruction.Add2
import manticore.machine.assembly.Instruction.R
import manticore.machine.assembly.Instruction.Send
import manticore.machine.core.Processor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.file.Paths
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class UniProcessorExecuteTester extends AnyFlatSpec with Matchers with ChiselScalatestTester {

  val rdgen = new scala.util.Random(0)

  val numTests = 200 // Set at will so long as num instructions < 4096

  // Populate the processor's registers with random values. We reserve the entry at
  // address numRegs-1 to store the result of the operation under test.
  val initialRegs = ArrayBuffer.fill(ManticoreBaseISA.numRegs)(UIntWide(0, ManticoreBaseISA.DataBits))
  Range(1, initialRegs.size).foreach { addr =>
    initialRegs(addr) = UIntWide(rdgen.nextInt(1 << ManticoreBaseISA.DataBits), ManticoreBaseISA.DataBits)
  }

  def createProgram(): (
      Seq[Instruction.Instruction],
      Seq[(UIntWide, String)] // What we expect to be sent out (data only).
  ) = {

    val ZERO = UIntWide(0, ManticoreBaseISA.DataBits)
    val ONE  = UIntWide(1, ManticoreBaseISA.DataBits)

    val prog          = ArrayBuffer.empty[Instruction.Instruction]
    val expectedSends = ArrayBuffer.empty[(UIntWide, String)]

    // We cannot read the contents of the register file, so we instead compute a result and
    // send it outside the processor. We check for the expected result at the output interface.

    // We reserve the last register in the register file to hold the result of the operation under test.
    val rd = R(ManticoreBaseISA.numRegs - 1)
    Range(0, numTests).foreach { _ =>
      // I omit SLTS as I can't figure out how to do the signed comparison using unsigned numbers.
      val functs = Array(ADD2, SUB2, MUL2, MUL2H, AND2, OR2, XOR2, SLL, SRL, SRA, SEQ, SLTU, MUX)

      // We choose random source registers. Note that these are chosen to be different from
      // the register that holds the output of the operation under test.
      val rs1 = R(rdgen.between(0, ManticoreBaseISA.numRegs - 1))
      val rs2 = R(rdgen.between(0, ManticoreBaseISA.numRegs - 1))
      val rs3 = R(rdgen.between(0, ManticoreBaseISA.numRegs - 1))

      val rs1_val = initialRegs(rs1.index)
      val rs2_val = initialRegs(rs2.index)
      val rs3_val = initialRegs(rs3.index)

      val shamnt = UIntWide.clipped(rs2_val.toBigInt, log2Ceil(ManticoreBaseISA.DataBits))

      val (res_val, instr) = functs(rdgen.nextInt(functs.length)) match {
        case ADD2 =>
          val instr = Instruction.Add2(rd, rs1, rs2)
          val res   = rs1_val + rs2_val
          (res, instr)
        case SUB2 =>
          val instr = Instruction.Sub2(rd, rs1, rs2)
          val res   = rs1_val - rs2_val
          (res, instr)
        case MUL2 =>
          val instr = Instruction.Mul2(rd, rs1, rs2)
          val res   = rs1_val * rs2_val
          (res, instr)
        case MUL2H =>
          val instr = Instruction.Mul2H(rd, rs1, rs2)
          // UIntWide multiplication is clipped. We need to perform the compuation
          // with a BigInt to keep the high-order bits.
          val res = UIntWide(
            (rs1_val.toBigInt * rs2_val.toBigInt) >> ManticoreBaseISA.DataBits,
            ManticoreBaseISA.DataBits
          )
          (res, instr)
        case AND2 =>
          val instr = Instruction.And2(rd, rs1, rs2)
          val res   = rs1_val & rs2_val
          (res, instr)
        case OR2 =>
          val instr = Instruction.Or2(rd, rs1, rs2)
          val res   = rs1_val | rs2_val
          (res, instr)
        case XOR2 =>
          val instr = Instruction.Xor2(rd, rs1, rs2)
          val res   = rs1_val ^ rs2_val
          (res, instr)
        case SLL =>
          val instr = Instruction.ShiftLeftLogic(rd, rs1, rs2)
          val res   = rs1_val << shamnt.toInt
          (res, instr)
        case SRL =>
          val instr = Instruction.ShiftRightLogic(rd, rs1, rs2)
          val res   = rs1_val >> shamnt.toInt
          (res, instr)
        case SRA =>
          val instr = Instruction.ShiftRightArithmetic(rd, rs1, rs2)
          val res   = rs1_val >>> shamnt.toInt
          (res, instr)
        case SEQ =>
          val instr = Instruction.SetEqual(rd, rs1, rs2)
          val res   = if (rs1_val == rs2_val) ONE else ZERO
          (res, instr)
        case SLTU =>
          val instr = Instruction.SetLessThanUnsigned(rd, rs1, rs2)
          val res   = if (rs1_val < rs2_val) ONE else ZERO
          (res, instr)
        // case SLTS =>
        //   val instr = Instruction.SetLessThanSigned(rd, rs1, rs2)
        //   val rs1_sign = rs1_val >> (rs1_val.width - 1)
        //   val rs2_sign = rs2_val >> (rs2_val.width - 1)
        //   val res = (rs1_sign.toInt, rs2_sign.toInt) match {
        //     case (0, 0) => if (rs1_val < rs2_val) ONE else ZERO
        //     case (0, 1) => ZERO
        //     case (1, 0) => ONE
        //     case (1, 1) => if (rs1_val > rs2_val) ONE else ZERO
        //     case _ => throw new IllegalArgumentException
        //   }
        //   (res, instr)
        case MUX =>
          val instr  = Instruction.Mux2(rd, rs1, rs2, rs3)
          val select = rs3_val & ONE
          val res    = if (select == ONE) rs2_val else rs1_val
          (res, instr)
      }

      prog += instr

      Range(0, 7).foreach { _ =>
        prog += Instruction.Nop()
      }

      // The destination doesn't matter. We only care that we are sending.
      prog += Instruction.Send(rd, rd, 1, 1)

      val instrStr = s"${instr}, rs1 = ${rs1_val}, rs2 = ${rs2_val}, rs3 = ${rs3_val}"
      expectedSends += Tuple2(res_val, instrStr)
    }

    (prog.toSeq, expectedSends.toSeq)
  }

  // Create programs.
  val (prog, expectedSends) = createProgram()
  println(prog.mkString("\n"))
  println(s"num instructions = ${prog.size}")

  // At startup the processor is configured such that all its custom functions output 0 independently of their inputs.
  val zeroFunct  = Seq.fill(ManticoreBaseISA.DataBits)(BigInt(0))
  val zeroFuncts = Seq.fill(ManticoreBaseISA.numFuncts)(zeroFunct)

  // Must have no arguments as ChiselTester can only elaborate circuits with
  // a function that has no arguments.
  def makeProcessor = {
    new Processor(
      config = ManticoreBaseISA,
      DimX = 16,
      DimY = 16,
      equations = zeroFuncts,
      initial_registers = UniProcessorTestUtils.createMemoryDataFiles {
        initialRegs.map(reg => reg.toInt).toSeq
      } {
        Paths.get("test_data_dir" + File.separator + getTestName + File.separator + "rf.data").toAbsolutePath
      },
      initial_array = UniProcessorTestUtils.createMemoryDataFiles(
        Seq.fill(ManticoreBaseISA.numRegs)(0)
      ) {
        Paths.get("test_data_dir" + File.separator + getTestName + File.separator + "ra.data").toAbsolutePath
      },
      enable_custom_alu = false
    )
  }

  behavior of "Processor"

  it should "compute the registers correctly" in {

    val assembledInstructions = prog.map(instr => Assembler.assemble(instr)(zeroFuncts))

    test(makeProcessor).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
      val sleep_length    = 10
      val epilogue_length = 0
      val countdown       = 20

      UniProcessorTestUtils.programProcessor(
        assembledInstructions,
        epilogue_length,
        sleep_length,
        countdown,
        dut
      ) {
        rdgen.nextInt(10) == 0
      }

      def waitForStart(): Unit = {
        if (!dut.io.periphery.active.peek().litToBoolean) {
          dut.clock.step()
          waitForStart()
        }
      }

      def executeAndCheck(expectedSends: Seq[(UIntWide, String)]): Unit = {
        val checkOutput = dut.io.packet_out.valid.peek().litToBoolean
        val nextExpectedSends = if (checkOutput) {
          val received          = dut.io.packet_out.data.peekInt()
          val (expected, instr) = expectedSends.head
          println(s"Expected = ${expected.toBigInt}, received = ${received}, instr = ${instr}")
          dut.io.packet_out.data.expect(expected.toBigInt)
          // We've consumed one expected value, so next time we must check for the rest of the sends only.
          expectedSends.tail
        } else {
          // We did not see an active packet and therefore didn't check anything. We must continue to check
          // all the expected values.
          expectedSends
        }

        dut.clock.step()
        if (nextExpectedSends.nonEmpty) {
          executeAndCheck(nextExpectedSends)
        }
      }

      dut.clock.setTimeout(10000)

      waitForStart()
      executeAndCheck(expectedSends)
    }
  }
}
