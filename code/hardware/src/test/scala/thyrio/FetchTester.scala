package thyrio

import Chisel._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.tester.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chisel3.tester.{testableClock, testableData}
import chiseltest.ChiselScalatestTester
import org.scalatest.{FlatSpec, Matchers, durations}
import thyrio.core.{BareNoCBundle, Fetch}
import chiseltest.internal.{VerilatorBackendAnnotation => USE_VERILATOR, WriteVcdAnnotation => DUMP_VCD}

import scala.annotation.tailrec

class FetchTester extends FlatSpec with Matchers with ChiselScalatestTester {

  val rdgen = scala.util.Random
  val NUM_TESTS = 2
  def randomInstruction: Long = Math.abs(rdgen.nextLong())
  def emptyPacket: BareNoCBundle = new BareNoCBundle(ThyrioISA).Lit(
    _.data -> rdgen.nextInt(1 << 16).U,
    _.address -> rdgen.nextInt(1 << 11).U,
    _.valid -> false.B
  )
  behavior of "Fetch"

  it should "be able to receive instructions from the NoC and write them to the URAM4kx64" in {

    test(new Fetch(ThyrioISA)).withAnnotations(Seq(USE_VERILATOR)) { dut =>
      for(i <- Range(0, NUM_TESTS)) {
        val num_instructions = rdgen.nextInt(4096) + 1
        val instructions = Seq.fill(num_instructions){randomInstruction}

        def programUnit(inst_queue: Seq[(Long, Int)]): Unit = {
          if (inst_queue.nonEmpty) {
            val q =
              if (rdgen.nextInt(3) != 0) {
                dut.io.programmer.enable.poke(true.B)
                dut.io.programmer.instruction.poke(inst_queue.head._1.U)
                dut.io.programmer.address.poke(inst_queue.head._2.U)
                inst_queue.tail
              } else {
                dut.io.programmer.enable.poke(false.B)
                inst_queue
              }
            dut.clock.step()
            programUnit(q)
          } else {
            dut.io.programmer.enable.poke(false.B)
          }
        }

        programUnit(instructions.zipWithIndex)

        println(s"Enqueued %d initial instructions".format(instructions.size))

        def schedulingRoundChecked(instructions: Seq[Long]): Long = {
          def randomMessages: Seq[BareNoCBundle] = Seq.fill(rdgen.nextInt(4096 - instructions.size)){
            new BareNoCBundle(ThyrioISA).Lit(
              _.data -> rdgen.nextInt(1 << 16).U,
              _.address -> rdgen.nextInt(1 << 11).U,
              _.valid -> true.B
            )
          }
          @tailrec
          def executeCycles(expected: Seq[Long], validated: Long = 0)(epilogue_inst: Seq[(Long, Int)]): Long = {
            expected match {
              case inst +: Nil =>
                dut.io.instruction.expect(inst.U)
                dut.io.programmer.enable.poke(false.B)
                dut.io.execution_enable.poke(false.B)
                dut.clock.step()
                validated + 1
              case inst +: rest =>
                dut.io.instruction.expect(inst.U)
                if (rdgen.nextInt(2) == 1 && epilogue_inst.nonEmpty) {
                  // randomly generate instructions and append to the epilogue
                  dut.io.programmer.enable.poke(true.B)
                  dut.io.programmer.address.poke(epilogue_inst.head._2.U)
                  dut.io.programmer.instruction.poke(epilogue_inst.head._1.U)

                  dut.clock.step()
                  executeCycles(rest :+ epilogue_inst.head._1, validated + 1)(epilogue_inst.tail)
                } else {
                  dut.io.programmer.enable.poke(false.B)
                  dut.clock.step()
                  executeCycles(rest, validated + 1)(epilogue_inst)
                }
            }
          }
          dut.io.execution_enable.poke(true.B)
          dut.clock.step()

          val extra_insts =
            Seq.fill(rdgen.nextInt(4096 - instructions.size)){randomInstruction}
              .zipWithIndex
              .map{ case (inst, ix) =>
                (inst, ix + instructions.size)
              }

          dut.clock.setTimeout(instructions.size + extra_insts.size * 3)
          val validated = executeCycles(instructions, 0) { extra_insts }

          dut.clock.step(4)
          validated
        }

        // now start multiple virtual cycles with random number of messages
        println("Validated " + schedulingRoundChecked(instructions) + " instructions")
        println("Validated " + schedulingRoundChecked(instructions) + " instructions")
        println("Validated " + schedulingRoundChecked(instructions) + " instructions")
        println("Validated " + schedulingRoundChecked(instructions) + " instructions")
        println("Validated " + schedulingRoundChecked(instructions) + " instructions")

        dut.reset.poke(1.B)
        dut.clock.step(2)
        dut.reset.poke(0.B)
      }


    }
  }
}
