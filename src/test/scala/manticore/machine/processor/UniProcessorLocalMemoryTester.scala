package manticore.machine.processor

import chisel3._
import chiseltest._
import manticore.machine.ISA
import manticore.machine.ManticoreBaseISA
import manticore.machine.UIntWide
import manticore.machine.assembly
import manticore.machine.assembly.Assembler
import manticore.machine.assembly.Instruction.Add2
import manticore.machine.assembly.Instruction.Instruction
import manticore.machine.assembly.Instruction.LocalLoad
import manticore.machine.assembly.Instruction.LocalStore
import manticore.machine.assembly.Instruction.Nop
import manticore.machine.assembly.Instruction.Predicate
import manticore.machine.assembly.Instruction.R
import manticore.machine.assembly.Instruction.Send
import manticore.machine.core.Processor
import manticore.machine.core.ProcessorInterface
import manticore.machine.memory.CacheCommand
import manticore.machine.processor.UniProcessorTestUtils.ClockedProcessor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.file.Paths
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class UniProcessorLocalMemoryTester extends AnyFlatSpec with Matchers with ChiselScalatestTester {

  val rdgen = new scala.util.Random(0)

  // Populate the processor's registers with random values. 
  val initialRegs = ArrayBuffer.fill(ManticoreBaseISA.numRegs)(UIntWide(0, ManticoreBaseISA.DataBits))
  initialRegs(0) = UIntWide(rdgen.nextInt(1 << ManticoreBaseISA.IdBits), ManticoreBaseISA.DataBits) // base
  Range(1, initialRegs.size).foreach { addr =>
    initialRegs(addr) = UIntWide(rdgen.nextInt(1 << ManticoreBaseISA.DataBits), ManticoreBaseISA.DataBits)
  }

  val base = R(0)
  val rd = R(rdgen.between(1, ManticoreBaseISA.numRegs - 1))
  val rs1 = R(rdgen.between(1, ManticoreBaseISA.numRegs - 1))
  val rs2 = R(rdgen.between(1, ManticoreBaseISA.numRegs - 1))

  val rd_val = initialRegs(rd.index)
  val rs1_val = initialRegs(rs1.index)
  val rs2_val = initialRegs(rs2.index)
  val offset  = rdgen.between(0, 1 << 10)
  val program = Array[Instruction](
    Add2(rd, rs1, rs2),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(), // Predicate(const_1),
    LocalStore(rd, base, offset),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    LocalLoad(rd, base, offset),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Send(rd, rd, 1, 1),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
  )
  val expectedSends = rs1_val + rs2_val

  val prog = program.toSeq
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

  it should "correctly handle load and store instructions to local memory" in {

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

      def executeAndCheck(expectedSends: UIntWide): Unit = {
        val checkOutput = dut.io.packet_out.valid.peek().litToBoolean
        val nextExpectedSends = if (checkOutput) {
          val received          = dut.io.packet_out.data.peekInt()
          val expected = expectedSends
          // val (expected, instr) = expectedSends.head
          println(s"Expected = ${expected.toBigInt}, received = ${received}")
          dut.io.packet_out.data.expect(expected.toBigInt)
          // We've consumed one expected value, so next time we must check for the rest of the sends only.
          expectedSends
        } else {
          // We did not see an active packet and therefore didn't check anything. We must continue to check
          // all the expected values.
          expectedSends
        }

        dut.clock.step()
        // if (nextExpectedSends.nonEmpty) {
        //   executeAndCheck(nextExpectedSends)
        // }
      }

      dut.clock.setTimeout(10000)

      waitForStart()
      executeAndCheck(expectedSends)
    }
  }
}
