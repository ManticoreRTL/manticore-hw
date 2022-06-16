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
import manticore.machine.UIntWide

class UniProcessorConfigureLutsTester extends AnyFlatSpec with Matchers with ChiselScalatestTester {

  val rdgen = new scala.util.Random(0)

  val numTests = 400 // Set at will so long as num instructions < 4096

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

  def createLutConfigProgram(): Seq[Instruction.Instruction] = {
    // To program the LUT vectors, we:
    //  1) Set the LUT data register for every vector.
    //  2) Set an "address" {a3, a2, a1, a0} in {rs4[x], rs3[x], rs2[x], rs1[x]} saying which bit should
    //     be written by future calls to CONFIGURELUTS.
    //  3) Emit a CONFIGURELUTS instruction.
    //
    // The sequence looks like this:

    // // Program bit 0 of all 16*32 LUTs in the custom ALU.
    // SET_LUT_DATA_REG 0, $const_0_0
    // SET_LUT_DATA_REG 1, $const_1_0
    // ...
    // SET LUT_DATA_REG 31, $const_31_0
    // // Populate rs1, rs2, rs3, rs4 beforehand such that:
    // // rs1[15:0] = "0000000000000000" (A0)
    // // rs2[15:0] = "0000000000000000" (A1)
    // // rs3[15:0] = "0000000000000000" (A2)
    // // rs4[15:0] = "0000000000000000" (A3)
    // CONFIG_LUT_VECTORS rs1, rs2, rs3, rs4
    //
    //
    // // Program bit 1 of all 16*32 LUTs in the custom ALU.
    // SET_LUT_DATA_REG 0, $const_0_1
    // SET_LUT_DATA_REG 1, $const_1_1
    // ...
    // SET_LUT_DATA_REG 31, $const_31_1
    // // Populate rs1, rs2, rs3, rs4 beforehand such that:
    // // rs1[15:0] = "1111111111111111" (A0)
    // // rs2[15:0] = "0000000000000000" (A1)
    // // rs3[15:0] = "0000000000000000" (A2)
    // // rs4[15:0] = "0000000000000000" (A3)
    // CONFIG_LUT_VECTORS rs1, rs2, rs3, rs4
    //
    //
    // // Program bit 2 of all 16*32 LUTs in the custom ALU.
    // SET_LUT_DATA_REG 0, $const_0_2
    // SET_LUT_DATA_REG 1, $const_1_2
    // ...
    // SET_LUT_DATA_REG 31, $const_31_2
    // // Populate rs1, rs2, rs3, rs4 beforehand such that:
    // // rs1[15:0] = "0000000000000000" (A0)
    // // rs2[15:0] = "1111111111111111" (A1)
    // // rs3[15:0] = "0000000000000000" (A2)
    // // rs4[15:0] = "0000000000000000" (A3)
    // CONFIG_LUT_VECTORS rs1, rs2, rs3, rs4
    //
    // ...
    //
    // // Program bit 15 of all 16*32 LUTs in the custom ALU.
    // SET_LUT_DATA_REG 0, $const_0_15
    // SET_LUT_DATA_REG 1, $const_1_15
    // ...
    // SET_LUT_DATA_REG 31, $const_31_15
    // // Populate rs1, rs2, rs3, rs4 beforehand such that:
    // // rs1[15:0] = "1111111111111111" (A0)
    // // rs2[15:0] = "1111111111111111" (A1)
    // // rs3[15:0] = "1111111111111111" (A2)
    // // rs4[15:0] = "1111111111111111" (A3)
    // CONFIG_LUT_VECTORS rs1, rs2, rs3, rs4

    val progLutConfig = ArrayBuffer.empty[Instruction.Instruction]

    val zeroOne = Seq(0, 1)
    for { a3 <- zeroOne; a2 <- zeroOne; a1 <- zeroOne; a0 <- zeroOne } {
      // Bit index to program is given by {a3, a2, a1, a0}
      val bitIdx = (a3 << 3) | (a2 << 2) | (a1 << 1) | (a0 << 0)

      for { (funct, funct_idx) <- all_functs.zipWithIndex } {
        // Extract the given bit index of every equation in the current function.
        val lutDataRegContents_str = funct.equation.map(equ => (equ >> bitIdx & 1).toInt).mkString
        val lutDataRegContents_int = Integer.parseInt(lutDataRegContents_str, 2)
        val funct_str              = s"{${funct.equation.map(equ => equ.toString(16)).mkString(", ")}}"

        // Debug
        println(s"funct = ${funct_str}, bit_index = ${bitIdx}, lut_data_reg = ${lutDataRegContents_str}")

        // Emit instructions to set LUT data register for bit bitIdx in each LUT vector.
        progLutConfig += Instruction.SetLutData(R(funct_idx), lutDataRegContents_int)
      }

      def bitIdxToReg(idx: Int) = if (idx == 0) allZerosReg else allOnesReg
      progLutConfig += Instruction.ConfigureLuts(bitIdxToReg(a0), bitIdxToReg(a1), bitIdxToReg(a2), bitIdxToReg(a3))
    }

    progLutConfig.toSeq
  }

  def createLutComputeProgram(): (
      Seq[Instruction.Instruction],
      Seq[(UIntWide, String)] // What we expect to be sent out (data only).
  ) = {
    val progLutCompute = ArrayBuffer.empty[Instruction.Instruction]
    val expectedSends  = ArrayBuffer.empty[(UIntWide, String)]

    // Now we must compute something with the programmed custom LUTs to check they are correct.
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
        progLutCompute += Instruction.Custom(reg, funct, rs1, rs2, rs3, rs4)
      }
      val funct_0_instrStr =
        s"${progLutCompute(progLutCompute.size - 4)}, rs1 = ${rs1_val}, rs2 = ${rs2_val}, rs3 = ${rs3_val}, rs4 = ${rs4_val}"
      val funct_1_instrStr =
        s"${progLutCompute(progLutCompute.size - 3)}, rs1 = ${rs1_val}, rs2 = ${rs2_val}, rs3 = ${rs3_val}, rs4 = ${rs4_val}"
      val funct_2_instrStr =
        s"${progLutCompute(progLutCompute.size - 2)}, rs1 = ${rs1_val}, rs2 = ${rs2_val}, rs3 = ${rs3_val}, rs4 = ${rs4_val}"
      val funct_3_instrStr =
        s"${progLutCompute(progLutCompute.size - 1)}, rs1 = ${rs1_val}, rs2 = ${rs2_val}, rs3 = ${rs3_val}, rs4 = ${rs4_val}"

      functRegs.zip(all_functs).foreach { case (reg, funct) =>
        // The destination doesn't matter. We only care that we are sending.
        progLutCompute += Instruction.Send(reg, reg, 1, 1)
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

    (progLutCompute.toSeq, expectedSends.toSeq)
  }

  // Create programs.
  val progLutConfig                   = createLutConfigProgram()
  val (progLutCompute, expectedSends) = createLutComputeProgram()
  val finalProgram                    = progLutConfig ++ progLutCompute
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

  it should "program the LUTs" in {

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
