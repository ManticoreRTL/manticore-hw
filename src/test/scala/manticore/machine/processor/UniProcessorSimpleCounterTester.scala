package manticore.machine.processor

import chisel3._
import chiseltest._
import manticore.machine.ManticoreBaseISA
import manticore.machine.assembly.Assembler
import manticore.machine.assembly.Instruction.Add2
import manticore.machine.assembly.Instruction.Nop
import manticore.machine.assembly.Instruction.R
import manticore.machine.assembly.Instruction.Send
import manticore.machine.core.Processor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.file.Paths
import scala.annotation.tailrec

class UniProcessorSimpleCounterTester extends AnyFlatSpec with Matchers with ChiselScalatestTester{



  val rdgen = new scala.util.Random(0)

  val PROGRAM = Array(
    Add2(R(2), R(2), R(1)),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Nop(),
    Send(R(2), R(2), 2, 2),
    Send(R(3), R(3), 3, 3),
    Send(R(4), R(4), 4, 4),
    Send(R(5), R(5), 5, 5),
    Send(R(6), R(6), 6, 6),
  ) ++ Seq.fill(20) { Nop() }

  // create random LUT equations
  val equations: Seq[Seq[BigInt]] = Seq.fill(32)(Seq.fill(16)(BigInt(rdgen.nextInt(1 << 16))))

  def makeProcessor =
    new Processor(config = ManticoreBaseISA,
      DimX = 16, DimY = 16,
      equations = equations,
      initial_registers =
        UniProcessorTestUtils.createMemoryDataFiles {
          Range(0, 1 << ManticoreBaseISA.IdBits).updated(2, 0)
        } {
             Paths.get("test_data_dir" + File.separator +
                getTestName + File.separator + "rf.data").toAbsolutePath
      },
      initial_array =
        UniProcessorTestUtils.createMemoryDataFiles(
          Seq.fill(1 << ManticoreBaseISA.IdBits)(0)) {
          Paths.get("test_data_dir" + File.separator +
            getTestName + File.separator + "ra.data").toAbsolutePath
        }
    )

  behavior of "Processor"

  it should "match the interpretation of a counter" in {

    test(makeProcessor).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>

      val instructions = PROGRAM.map(Assembler.assemble(_)(equations))
      val sleep_length = 10
      val epilogue_length = 0
      val countdown = 20


      UniProcessorTestUtils.programProcessor(
        instructions.toIndexedSeq, epilogue_length, sleep_length, countdown, dut
      ){
        rdgen.nextInt(10) == 0
      }

      def executeAndCheck(counter: Int): Unit = {

        @tailrec
        def waitForStart(): Unit = {
          if (!dut.io.periphery.active.peek().litToBoolean) {
            dut.clock.step()
            waitForStart()
          }
        }

        @tailrec
        def execute(expected: Seq[Int], msgs: Seq[(Int, Int)]): Unit = {
          if (dut.io.periphery.active.peek().litToBoolean) {
            val next =
              if (dut.io.packet_out.valid.peek().litToBoolean) {
                if (dut.io.packet_out.address.peek().litValue.toInt == 2) {
                  // println(counter)
                  // println(s"Got ${dut.io.packet_out.data.peekInt()} expected ${counter}")
                  dut.io.packet_out.data.expect(counter.U)
                  dut.io.packet_out.xHops.expect(2.U)
                  dut.io.packet_out.yHops.expect(2.U)
                  (counter + 1, expected)
                } else {
                  dut.io.packet_out.data.expect(expected.head.U)
                  dut.io.packet_out.xHops.expect(expected.head.U)
                  dut.io.packet_out.yHops.expect(expected.head.U)
                  (counter, expected.tail)
                }
              } else {
                (counter, expected)
              }
            if (rdgen.nextInt(3) == 0 && msgs.nonEmpty) {
              dut.io.packet_in.valid.poke(true.B)
              dut.io.packet_in.data.poke(msgs.head._2.U)
              dut.io.packet_in.address.poke(msgs.head._1.U)
              dut.clock.step()
              execute(next._2, msgs.tail)
            } else {
              dut.clock.step()
              execute(next._2, msgs)
            }
          }
        }
        println(s"Starting vcycle $counter")
        waitForStart()
        execute(Range(3, 7), Nil)
      }
      dut.clock.setTimeout(10000)
      Range(0, 200).foreach(i => executeAndCheck(i + 1))

    }

  }

}
