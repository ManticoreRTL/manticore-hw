package manticore.machine.processor

import chisel3._
import chiseltest._
import manticore.machine.ManticoreBaseISA
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

class UniProcessorSliceTester extends AnyFlatSpec with Matchers with ChiselScalatestTester{

  val rdgen = new scala.util.Random(0)

  val numTests = 600 // Set at will so long as num instructions < 4096

  // Populate the processor's registers with random values. We reserve the entry at
  // address numRegs-1 to store the result of the slice.
  val initialRegs = ArrayBuffer.fill(ManticoreBaseISA.numRegs)(BigInt(0))
  Range(1, initialRegs.size).foreach { addr =>
    initialRegs(addr) = rdgen.nextInt(1 << ManticoreBaseISA.DataBits)
  }

  def createProgram(): (
    Seq[Instruction.Instruction],
    Seq[BigInt] // What we expect to be sent out (data only).
  ) = {
    val prog = ArrayBuffer.empty[Instruction.Instruction]
    val expectedSends = ArrayBuffer.empty[BigInt]

    // We cannot read the contents of the register file, so we instead compute a result and
    // send it outside the processor. We check for the expected result at the output interface.

    // We reserve the last register in the register file to hold the result of the slice.
    val rd = R(ManticoreBaseISA.numRegs - 1)
    Range(0, numTests).foreach { _ =>
      // We choose a random register, offset, and length.
      // The source register is chosen to be different from the reserved slice output register.
      val rs = R(rdgen.between(0, ManticoreBaseISA.numRegs - 1))
      val offset = rdgen.between(0, ManticoreBaseISA.DataBits)
      val length = rdgen.between(1, ManticoreBaseISA.DataBits - offset + 1)

      val rs_val = initialRegs(rs.index)
      prog += Instruction.Slice(rd, rs, offset, length)
      Range(0, 4).foreach { _ =>
        prog += Instruction.Nop()
      }
      // The destination doesn't matter. We only care that we are sending.
      prog += Instruction.Send(rd, rd, 1, 1)

      val expected = (rs_val >> offset) & ((BigInt(1) << length) - 1)
      expectedSends += expected
    }

    (prog.toSeq, expectedSends.toSeq)
  }

  // Create programs.
  val (prog, expectedSends) = createProgram()
  println(prog.mkString("\n"))
  println(s"num instructions = ${prog.size}")

  // At startup the processor is configured such that all its custom functions output 0 independently of their inputs.
  val zeroFunct = Seq.fill(ManticoreBaseISA.DataBits)(BigInt(0))
  val zeroFuncts = Seq.fill(ManticoreBaseISA.numFuncts)(zeroFunct)

  // Must have no arguments as ChiselTester can only elaborate circuits with
  // a function that has no arguments.
  def makeProcessor = {
    new Processor(
      config = ManticoreBaseISA,
      DimX = 16, DimY = 16,
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
      }
    )
  }

  behavior of "Processor"

  it should "slice the registers correctly" in {

    val assembledInstructions = prog.map(instr =>
      Assembler.assemble(instr)(zeroFuncts)
    )

    test(makeProcessor).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
      val sleep_length = 10
      val epilogue_length = 0
      val countdown = 20

      UniProcessorTestUtils.programProcessor(
        assembledInstructions, epilogue_length, sleep_length, countdown, dut
      ){
        rdgen.nextInt(10) == 0
      }

      def waitForStart(): Unit = {
        if (!dut.io.periphery.active.peek().litToBoolean) {
          dut.clock.step()
          waitForStart()
        }
      }

      def executeAndCheck(expectedSends: Seq[BigInt]): Unit = {
        val checkOutput = dut.io.packet_out.valid.peek().litToBoolean
        val nextExpectedSends = if (checkOutput) {
          val received = dut.io.packet_out.data.peekInt()
          println(s"Expected = ${expectedSends.head}, received = ${received}")
          dut.io.packet_out.data.expect(expectedSends.head)
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
