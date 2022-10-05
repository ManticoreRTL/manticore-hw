package manticore.machine.processor

import Chisel._
import chiseltest._
import manticore.machine.ManticoreBaseISA
import manticore.machine.ManticoreFullISA
import manticore.machine.TestsCommon.RequiresVerilator
import manticore.machine.assembly
import manticore.machine.assembly.Assembler
import manticore.machine.assembly.Instruction.Expect
import manticore.machine.assembly.Instruction.Instruction
import manticore.machine.assembly.Instruction.LocalLoad
import manticore.machine.assembly.Instruction.LocalStore
import manticore.machine.assembly.Instruction.Nop
import manticore.machine.assembly.Instruction.Predicate
import manticore.machine.assembly.Instruction.R
import manticore.machine.assembly.Instruction.SetEqual
import manticore.machine.processor.UniProcessorTestUtils.ClockedProcessor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.file.Paths
import scala.annotation.tailrec
import scala.util.Random


class UniProcessorExceptionTester extends AnyFlatSpec with Matchers
  with ChiselScalatestTester {


  val rdgen = new Random(0)


  behavior of "Processor expect failure exception"
  it should "correctly register exceptions " taggedAs RequiresVerilator in {

    val config = ManticoreBaseISA

    val equations = Seq.fill(config.numFuncts) {
      Seq.fill(config.DataBits) {
        BigInt(rdgen.nextInt(1 << 16))
      }
    }
    val initial_reg_values = Seq.tabulate(1 << config.IdBits) {
      i => i
    }
    val initial_array_values = Seq.fill(1 << config.IdBits)(0)

    // this program should fail miserably
    /**
     * goes through all the register 200 to 300, and checks whether their value
     * equals 1, since the values of registers 200 to 300 are set to
     * 200 to 300, then all the Expect instructions should fail.
     */
    val exceptions_to_catch = initial_reg_values.slice(200, 300)
    val program = exceptions_to_catch.flatMap { reg_id =>
      Array[Instruction](
        Predicate(R(1)),
        LocalStore(R(reg_id), R(0), 0),
        SetEqual(R(reg_id), R(reg_id), R(0)), // should set R(reg_id) to 0
        Nop(),
        Nop(),
        Nop(),
        Nop(),
        Nop(),
        Nop(),
        Expect(R(reg_id), R(1), reg_id), // this should fail
        LocalLoad(R(reg_id), R(0), 0),
        Nop(),
        Nop(),
        Nop(),
        Nop(),
        Nop(),
        Nop()
      )
    }

    def makeProcessor(): ClockedProcessor =
      new ClockedProcessor(
        ManticoreFullISA,
        2, 2,
        equations,
        UniProcessorTestUtils.createMemoryDataFiles(
          initial_reg_values
        ) {
          Paths.get("test_data_dir" + File.separator +
            getTestName + File.separator + "rf.data").toAbsolutePath
        },
        UniProcessorTestUtils.createMemoryDataFiles(
          initial_array_values
        ) {
          Paths.get("test_data_dir" + File.separator +
            getTestName + File.separator + "ra.data").toAbsolutePath
        }
      )


    test(makeProcessor()).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>

      dut.io.clock_enable_n.poke(false.B)

      UniProcessorTestUtils.programProcessor(
        program.map(inst => Assembler.assemble(inst)(equations)),
        5, 50, 5, dut.io.proc.packet_in, dut.clock
      ) {
        true
      }

      @tailrec
      def waitForStart(): Unit = {
        if (dut.io.proc.periphery.active.peek().litToBoolean == false){
          dut.clock.step()
          waitForStart()
        }
      }

      @tailrec
      def catchIfAny(to_catch: Seq[Int]): Seq[Int] = {
        if (dut.io.proc.periphery.active.peek().litToBoolean == true) {
          if (dut.io.proc.periphery.exception.error.peek().litToBoolean) {
            dut.io.proc.periphery.exception.id.expect(to_catch.head.U)
            println(s"Successfully caught exception ${to_catch.head}, killing the clock to handle the exception!")
            dut.io.clock_enable_n.poke(true.B)
            dut.clock.step(rdgen.nextInt(20) + 2)
            dut.io.clock_enable_n.poke(false.B)
            dut.clock.step()
            catchIfAny(to_catch.tail)
          } else {
            dut.clock.step()
            catchIfAny(to_catch)
          }
        } else {
          to_catch
        }
      }

      waitForStart()
      catchIfAny(exceptions_to_catch).isEmpty should be(true)
      waitForStart()
      catchIfAny(exceptions_to_catch).isEmpty should be(true)
      waitForStart()
      catchIfAny(exceptions_to_catch).isEmpty should be(true)
      waitForStart()
      catchIfAny(exceptions_to_catch).isEmpty should be(true)
      waitForStart()
      catchIfAny(exceptions_to_catch).isEmpty should be(true)

    }

  }

}
