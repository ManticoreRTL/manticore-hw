package manticore

import Chisel._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.tester.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chisel3.tester.experimental.sanitizeFileName
import chisel3.tester.{ChiselScalatestTester, testableClock, testableData, validToDriver}
import org.scalatest.{FlatSpec, Matchers}
import manticore.assembly.Instruction.{Instruction, Register}
import manticore.core.{BareNoCBundle, Processor}
import manticore.assembly.{Assembler, Interpreter}
import chiseltest.internal.{VerilatorBackendAnnotation => USE_VERILATOR, WriteVcdAnnotation => DUMP_VCD}

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path, Paths}
import scala.annotation.tailrec
import scala.language.postfixOps

class UniProcessorFirFilterTester extends FlatSpec with Matchers with ChiselScalatestTester {


  val rdgen = new scala.util.Random(0)

  val coeffs = Seq(187, 212, 61)
  //  val x_vals = Range(1, 101)
  val x_vals = Seq.fill(100) {
    1
  }
  val y_vals = x_vals sliding(coeffs.size, 1) map { xs: Seq[Int] =>
    coeffs zip xs map { case (c, x) => (x * c) & ((1 << ManticoreBaseISA.DataBits) - 1) } sum
  } toSeq
  val x_vals_start = 1024
  val y_vals_start = 1024 + x_vals.size * 2

  def initEnvironment: (Array[Instruction], Map[String, Register], Interpreter) = {

    val interp = new Interpreter


    import manticore.assembly.Instruction._

    /// Register containing constant
    val const_0 = R(0)
    interp.env.register_file(const_0) = 0
    val const_1 = R(1)
    interp.env.register_file(const_1) = 1

    val const_b0 = R(2)
    interp.env.register_file(const_b0) = coeffs(0)

    val const_b1 = R(3)
    interp.env.register_file(const_b1) = coeffs(1)

    val const_b2 = R(4)
    interp.env.register_file(const_b2) = coeffs(2)

    val const_num_inputs = R(5)
    interp.env.register_file(const_num_inputs) = x_vals.size

    val const_num_outputs = R(6)
    interp.env.register_file(const_num_outputs) = x_vals.size + coeffs.size
    /// make a reset pulse
    interp.env.register_array(0) = 1

    x_vals.zipWithIndex.foreach { case (x, i) =>
      interp.env.register_array(x_vals_start + i) = x
    }


    val x_vals_base = R(7)
    interp.env.register_file(x_vals_base) = x_vals_start
    val y_vals_base = R(8)
    interp.env.register_file(y_vals_base) = y_vals_start
    var ix: Int = 20

    def freshReg = {
      val reg = R(ix)
      ix = ix + 1
      reg
    }

    // registers with dynamic values
    val reset_ptr = freshReg
    interp.env.register_file(reset_ptr) = 0

    val y_ptr = freshReg
    interp.env.register_file(y_ptr) = 0
    val x_ptr = freshReg
    interp.env.register_file(x_ptr) = 0

    val y_value = freshReg
    val reset_value = freshReg

    val tap0 = freshReg
    val tap1 = freshReg
    val tap2 = freshReg
    val tap0b0 = freshReg
    val tap1b1 = freshReg
    val tap2b2 = freshReg
    val sum01 = freshReg
    val sum = freshReg
    val x_ptr_plus_1 = freshReg
    val y_ptr_plus_1 = freshReg

    val reset_cond = freshReg
    val tmp = freshReg


    val program = Array[Instruction](
      Mult2(tap0b0, const_b0, tap0),
      Mult2(tap1b1, const_b1, tap1),
      Add2(x_ptr_plus_1, x_ptr, const_1),
      Mult2(tap2b2, const_b2, tap2),
      Add2(sum01, tap0b0, tap1b1),
      Add2(y_ptr_plus_1, y_ptr, const_1),
      LocalLoad(reset_value, reset_ptr, 0),
      Add2(sum, sum01, tap2b2),
      Add2(reset_ptr, reset_ptr, const_1),
      SetEqual(reset_cond, reset_value, const_1),
      LocalStore(sum, y_ptr, 0),
      Add2(tap0, tap1, const_0),
      Add2(tap1, tap2, const_0),
      LocalLoad(tap2, x_ptr, 0),
      LocalLoad(tmp, y_ptr, 0),
      Mux2(x_ptr, x_ptr_plus_1, x_vals_base),
      Mux2(y_ptr, y_ptr_plus_1, y_vals_base),
      Send(y_value, tmp, 4, 4) // send the computed value
    )


    (program, Map("y_ptr" -> y_ptr, "x_ptr" -> x_ptr, "y_value" -> y_value), interp)
  }


  def streamInstructions(p: Array[Instruction])(implicit dut: Processor): Unit = {
    val instructions = p.map(Assembler.assemble(_)(equations))
    instructions foreach { inst =>
      //      println(s"0x%016x".format(inst))
      dut.io.packet_in.poke(
        new BareNoCBundle(ManticoreBaseISA).Lit(
          _.data -> (inst & 0x0000FFFF).U,
          _.address -> 0.U,
          _.valid -> true.B
        )
      )
      dut.clock.step()

      dut.io.packet_in.poke(
        new BareNoCBundle(ManticoreBaseISA).Lit(
          _.data -> ((inst >> 16) & 0x0000FFFF).U,
          _.address -> 0.U,
          _.valid -> true.B
        )
      )
      dut.clock.step()

      dut.io.packet_in.poke(
        new BareNoCBundle(ManticoreBaseISA).Lit(
          _.data -> ((inst >> 32) & 0x0000FFFF).U,
          _.address -> 0.U,
          _.valid -> true.B
        )
      )
      dut.clock.step()

      dut.io.packet_in.poke(
        new BareNoCBundle(ManticoreBaseISA).Lit(
          _.data -> ((inst >> 48) & 0x0000FFFF).U,
          _.address -> 0.U,
          _.valid -> true.B
        )
      )
      dut.clock.step()

    }
    dut.io.packet_in.poke(
      new BareNoCBundle(ManticoreBaseISA).Lit(
        _.data -> 0.U,
        _.address -> 0.U,
        _.valid -> false.B
      )
    )
  }

  // create MUX LUT equations
  val equations: Seq[Seq[Int]] = Seq.fill(32)(Seq.fill(16)(0xcaca))

  def createMemoryDataFiles(name: String)(data: => Seq[Int]): String = {
    val filepath = Paths.get("test_data_dir" + File.separator +
      sanitizeFileName(scalaTestContext.value.get.name) + File.separator + name).toAbsolutePath
    Files.createDirectories(filepath.getParent)
    val fw = new PrintWriter(filepath.toFile)
    data.foreach { v =>
      fw.println(s"%016d".format(v.toBinaryString.toLong))
    }
    fw.close()
    filepath.toString
  }

  behavior of "Processor"

  it should "compute correct fir filter output values" in {

    val (program, vars, interp) = initEnvironment


    test {
      new Processor(config = ManticoreBaseISA, DimX = 16, DimY = 16,
        equations = equations,
        initial_registers = UniProcessorTestUtils.createMemoryDataFiles {
          interp.env.register_file.toSeq
        } {
          Paths.get("test_data_dir" + File.separator +
            sanitizeFileName(scalaTestContext.value.get.name) + File.separator + "rf.data").toAbsolutePath
        },
        initial_array = UniProcessorTestUtils.createMemoryDataFiles {
          interp.env.register_array.toSeq
        } {
          Paths.get("test_data_dir" + File.separator +
            sanitizeFileName(scalaTestContext.value.get.name) + File.separator + "ra.data").toAbsolutePath
        }
      )
    }.withAnnotations(Seq(USE_VERILATOR)) { implicit dut =>


      val num_vcycles = x_vals.size

      Range(0, num_vcycles).map { i =>
        interp.run(program)
      }

      val gold = Seq(0, coeffs(2), coeffs(2) * x_vals(1) + coeffs(1)) ++ y_vals

      // ensure the interpretation is correct
      gold.slice(0, num_vcycles - 1).zipWithIndex.foreach { case (e, i) =>
        assertResult(e)(interp.env.register_array(i + y_vals_start))
      }

      // stream instructions to the processor
      //      streamInstructions(program)

      UniProcessorTestUtils.programProcessor(
        program.map(Assembler.assemble(_)(equations)),
        2, 30, 50, dut
      ) {
        rdgen.nextInt(10) == 0
      }
      // start hardware simulation
      dut.clock.setTimeout((program.length + 30) * num_vcycles + 1000)
      var num_validated = 0
      @tailrec
      def executeChecked(unchecked: Seq[Int]): Unit = {
        if (unchecked.nonEmpty) {
          dut.clock.step()
          
          if (dut.io.packet_out.valid.peek().litToBoolean) {
            dut.io.packet_out.data.expect(unchecked.head.U)
            dut.io.packet_out.address.expect(vars("y_value").index.U)
            num_validated = num_validated + 1
            executeChecked(unchecked.tail)
          } else {
            executeChecked(unchecked)
          }
        }
      }

      executeChecked(Seq(0) ++ gold.slice(0, num_vcycles - 1))
      println(s"validated ${num_validated} outputs")
    }

  }


}
