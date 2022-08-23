package manticore.machine.processor

import chisel3._
import chiseltest._
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

class UniProcessorConfigCfuTester extends AnyFlatSpec with Matchers with ChiselScalatestTester {

  val rdgen = new scala.util.Random(0)

  val numTests = 100 // Set at will so long as num instructions < 4096

  val numTestFuncts = 4 // Can not modify without explicitly adding a function in the code below.

  // This test programs the processor with 4 custom functions, then performs
  // random computations with them and checks their result.

  // We reserve the following registers in the processor:
  // addr 0 -> all zeros
  // addr 1 -> all ones
  // addr 2 -> funct_0 result
  // addr 3 -> funct_1 result
  // ...
  val allZerosReg = R(0)
  val allOnesReg  = R(1)
  val functRegs   = Seq.tabulate(numTestFuncts)(idx => R(allOnesReg.index + 1 + idx))

  // Populate the processor's registers with random values. We reserve xx+2 entries
  // shown below:
  // addr 0 -> all zeros
  // addr 1 -> all ones
  // addr 2 -> funct_0 result
  // addr 3 -> funct_1 result
  // addr 4 -> funct_2 result
  // addr 5 -> funct_3 result
  // ...
  // addr xx+2 -> funct_xx result.
  val initialRegs = ArrayBuffer.fill(ManticoreBaseISA.numRegs)(UIntWide(0, ManticoreBaseISA.DataBits))
  initialRegs(allZerosReg.index) = UIntWide(0, ManticoreBaseISA.DataBits)
  initialRegs(allOnesReg.index) = UIntWide((1 << ManticoreBaseISA.DataBits) - 1, ManticoreBaseISA.DataBits)
  Range(functRegs.last.index + 1, initialRegs.size).foreach { addr =>
    initialRegs(addr) = UIntWide(rdgen.nextInt(1 << ManticoreBaseISA.DataBits), ManticoreBaseISA.DataBits)
  }

  // To generate equations in python:
  //
  // def bool2char(b): return "0" if not b else "1"
  // def char2bool(c): return False if c == "0" else True
  // for i in range(16):
  //     binstr = f"{i:04b}"
  //     (a, b, c, d) = [char2bool(x) for x in list(binstr)]
  //     (astr, bstr, cstr, dstr) = list(binstr)
  //     res = bool2char(a and b and c and d)
  //     print(f"{dstr} {cstr} {bstr} {astr} -> {res}")

  // a & b & c & d : a | b | c | d : a ^ b ^ c ^ d : (a & b) | (c & d) :
  //
  //  d c b a -> o :  d c b a -> o :  d c b a -> o :     d c b a -> o  :
  //
  //  0 0 0 0 -> 0 :  0 0 0 0 -> 0 :  0 0 0 0 -> 0 :     0 0 0 0 -> 0  :
  //  1 0 0 0 -> 0 :  1 0 0 0 -> 1 :  1 0 0 0 -> 1 :     1 0 0 0 -> 0  :
  //  0 1 0 0 -> 0 :  0 1 0 0 -> 1 :  0 1 0 0 -> 1 :     0 1 0 0 -> 0  :
  //  1 1 0 0 -> 0 :  1 1 0 0 -> 1 :  1 1 0 0 -> 0 :     1 1 0 0 -> 1  :
  //
  //  0 0 1 0 -> 0 :  0 0 1 0 -> 1 :  0 0 1 0 -> 1 :     0 0 1 0 -> 0  :
  //  1 0 1 0 -> 0 :  1 0 1 0 -> 1 :  1 0 1 0 -> 0 :     1 0 1 0 -> 0  :
  //  0 1 1 0 -> 0 :  0 1 1 0 -> 1 :  0 1 1 0 -> 0 :     0 1 1 0 -> 0  :
  //  1 1 1 0 -> 0 :  1 1 1 0 -> 1 :  1 1 1 0 -> 1 :     1 1 1 0 -> 1  :
  //
  //  0 0 0 1 -> 0 :  0 0 0 1 -> 1 :  0 0 0 1 -> 1 :     0 0 0 1 -> 0  :
  //  1 0 0 1 -> 0 :  1 0 0 1 -> 1 :  1 0 0 1 -> 0 :     1 0 0 1 -> 0  :
  //  0 1 0 1 -> 0 :  0 1 0 1 -> 1 :  0 1 0 1 -> 0 :     0 1 0 1 -> 0  :
  //  1 1 0 1 -> 0 :  1 1 0 1 -> 1 :  1 1 0 1 -> 1 :     1 1 0 1 -> 1  :
  //
  //  0 0 1 1 -> 0 :  0 0 1 1 -> 1 :  0 0 1 1 -> 0 :     0 0 1 1 -> 1  :
  //  1 0 1 1 -> 0 :  1 0 1 1 -> 1 :  1 0 1 1 -> 1 :     1 0 1 1 -> 1  :
  //  0 1 1 1 -> 0 :  0 1 1 1 -> 1 :  0 1 1 1 -> 1 :     0 1 1 1 -> 1  :
  //  1 1 1 1 -> 1 :  1 1 1 1 -> 1 :  1 1 1 1 -> 0 :     1 1 1 1 -> 1  :
  //
  // equ0= 0x8000  : equ1 = 0xFFFE : equ2 = 0x6996 :     equ3 = 0xF888 :
  //
  // The equations above are for 1-bit LUTs. They should be replicated 16
  // times into a Seq[BigInt] to create an equation for a LUT vector.
  // The equations are therefore:
  //
  //    funct_0 = Seq(0x8000, 0x8000, ..., 0x8000)
  //    funct_1 = Seq(0xFFFE, 0xFFFE, ..., 0xFFFE)
  //    funct_2 = Seq(0x6996, 0x6996, ..., 0x6996)
  //    funct_3 = Seq(0xF888, 0xF888, ..., 0xF888)
  //
  // These are homogeneous equations as all bits in the LUT vector compute
  // the same thing.
  val funct_0   = Instruction.CustomFunction(Seq.fill(ManticoreBaseISA.DataBits)(BigInt(0x8000)))
  val funct_1   = Instruction.CustomFunction(Seq.fill(ManticoreBaseISA.DataBits)(BigInt(0xfffe)))
  val funct_2   = Instruction.CustomFunction(Seq.fill(ManticoreBaseISA.DataBits)(BigInt(0x6996)))
  val funct_3   = Instruction.CustomFunction(Seq.fill(ManticoreBaseISA.DataBits)(BigInt(0xf888)))
  val zeroFunct = Seq.fill(ManticoreBaseISA.DataBits)(BigInt(0))
  val all_functs = Seq(
    funct_0,
    funct_1,
    funct_2,
    funct_3
  )

  def createConfigCfuProgram(): Seq[Instruction.Instruction] = {
    // To program the CFU, we write to the 32x16 RAM one line (16 bits) at a time
    // using CONFIGCFU instructions.
    //
    // The sequence looks like this:

    // // Program the four functions to RAM0 (0th bit of the output)
    // CONFIGCFU 0, 0, 0x8000
    // CONFIGCFU 0, 1, 0xFFFE
    // CONFIGCFU 0, 2, 0x6996
    // CONFIGCFU 0, 3, 0xF888

    // // Program the four functions to RAM1 (1st bit of the output)
    // CONFIGCFU 1, 0, 0x8000
    // CONFIGCFU 1, 1, 0xFFFE
    // CONFIGCFU 1, 2, 0x6996
    // CONFIGCFU 1, 3, 0xF888

    // ...

    // // Program the four functions to RAM15 (15th bit of the output)
    // CONFIGCFU 15, 0, 0x8000
    // CONFIGCFU 15, 1, 0xFFFE
    // CONFIGCFU 15, 2, 0x6996
    // CONFIGCFU 15, 3, 0xF888

    val progConfigCfu = ArrayBuffer.empty[Instruction.Instruction]

    val zeroOne = Seq(0, 1)
    for { i <- Range(0, 16) } { // for each of 16 RAMs
      for { (funct, funct_idx) <- all_functs.zipWithIndex } {
        // Extract the given bit index of every equation in the current function.
        val ramLineContent = funct.equation(i)
        println(s"ram_index = ${i}, funct_idx = ${funct_idx}, ram_line_content = ${ramLineContent}")
        progConfigCfu += Instruction.ConfigCfu(i, funct_idx, ramLineContent.toInt)
      }
    }

    progConfigCfu.toSeq
  }

  def createCfuComputeProgram(): (
      Seq[Instruction.Instruction],
      Seq[(UIntWide, String)] // What we expect to be sent out (data only).
  ) = {
    val progCfuCompute = ArrayBuffer.empty[Instruction.Instruction]
    val expectedSends  = ArrayBuffer.empty[(UIntWide, String)]

    // Now we must compute something with the programmed custom CFU to check they are correct.
    // We cannot read the contents of the register file, so we instead compute a result and
    // send it outside the processor. We check for the expected result at the output interface.

    Range(0, numTests).foreach { _ =>
      // Choose 4 random args and compute the 4 custom functions.
      // It is important to choose registers which we know the value of in advance.
      // This means we cannot choose the registers used to store the results of
      // the custom functions themselves as previous executions of the functions
      // would modify the value stored in their output registers.
      val startIdx = functRegs.last.index + 1
      val rs1      = R(rdgen.between(startIdx, ManticoreBaseISA.numRegs))
      val rs2      = R(rdgen.between(startIdx, ManticoreBaseISA.numRegs))
      val rs3      = R(rdgen.between(startIdx, ManticoreBaseISA.numRegs))
      val rs4      = R(rdgen.between(startIdx, ManticoreBaseISA.numRegs))

      val rs1_val = initialRegs(rs1.index)
      val rs2_val = initialRegs(rs2.index)
      val rs3_val = initialRegs(rs3.index)
      val rs4_val = initialRegs(rs4.index)

      functRegs.zip(all_functs).foreach { case (reg, funct) =>
        progCfuCompute += Instruction.Custom(reg, funct, rs1, rs2, rs3, rs4)
      }

      val funct_0_instrStr =
        s"${progCfuCompute(progCfuCompute.size - 4)}, rs1 = ${rs1_val}, rs2 = ${rs2_val}, rs3 = ${rs3_val}, rs4 = ${rs4_val}"
      val funct_1_instrStr =
        s"${progCfuCompute(progCfuCompute.size - 3)}, rs1 = ${rs1_val}, rs2 = ${rs2_val}, rs3 = ${rs3_val}, rs4 = ${rs4_val}"
      val funct_2_instrStr =
        s"${progCfuCompute(progCfuCompute.size - 2)}, rs1 = ${rs1_val}, rs2 = ${rs2_val}, rs3 = ${rs3_val}, rs4 = ${rs4_val}"
      val funct_3_instrStr =
        s"${progCfuCompute(progCfuCompute.size - 1)}, rs1 = ${rs1_val}, rs2 = ${rs2_val}, rs3 = ${rs3_val}, rs4 = ${rs4_val}"

      // 10 Nops are required in current pipeline
      Range(0, 10).foreach { _ =>
        progCfuCompute += Instruction.Nop()
      }

      functRegs.zip(all_functs).foreach { case (reg, funct) =>
        // The destination doesn't matter. We only care that we are sending.
        progCfuCompute += Instruction.Send(reg, reg, 1, 1)
      }

      // a & b & c & d : a | b | c | d : a ^ b ^ c ^ d : (a & b) | (c & d) :
      val funct_0_expected = rs1_val & rs2_val & rs3_val & rs4_val
      val funct_1_expected = rs1_val | rs2_val | rs3_val | rs4_val
      val funct_2_expected = rs1_val ^ rs2_val ^ rs3_val ^ rs4_val
      val funct_3_expected = (rs1_val & rs2_val) | (rs3_val & rs4_val)
      expectedSends += Tuple2(funct_0_expected, funct_0_instrStr)
      expectedSends += Tuple2(funct_1_expected, funct_1_instrStr)
      expectedSends += Tuple2(funct_2_expected, funct_2_instrStr)
      expectedSends += Tuple2(funct_3_expected, funct_3_instrStr)
    }

    // We need to add one Nop after the final Send. The reason is unclear so far.
    progCfuCompute += Instruction.Nop()

    (progCfuCompute.toSeq, expectedSends.toSeq)
  }

  // Create programs.
  val progConfigCfu                   = createConfigCfuProgram()
  val (progCfuCompute, expectedSends) = createCfuComputeProgram()
  val finalProgram                    = progConfigCfu ++ progCfuCompute
  println(finalProgram.mkString("\n"))
  println(s"num instructions = ${finalProgram.size}")

  // Must have no arguments as ChiselTester can only elaborate circuits with
  // a function that has no arguments.
  def makeProcessor = {
    // At startup the processor is configured such that all its custom functions output 0 independently of their inputs.
    val initEquations = Seq.fill(ManticoreBaseISA.numFuncts)(zeroFunct)

    new Processor(
      config = ManticoreBaseISA,
      DimX = 16,
      DimY = 16,
      equations = initEquations,
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
      enable_custom_alu = true
    )
  }

  behavior of "Processor"

  it should "configure the CFU" in {

    val equations             = all_functs.map(funct => funct.equation)
    val assembledInstructions = finalProgram.map(instr => Assembler.assemble(instr)(equations))

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
