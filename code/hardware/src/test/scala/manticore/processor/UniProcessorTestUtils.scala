package manticore.processor

import Chisel._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.tester.{testableClock, testableData}
import chisel3.withClockAndReset
import manticore.{ISA, ManticoreBaseISA}
import manticore.core.{BareNoCBundle, ClockBuffer, Processor, ProcessorInterface}
import memory.CacheCommand

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object UniProcessorTestUtils {

  def createMemoryDataFiles(data: Seq[Int])(filepath: Path): String = {
    Files.createDirectories(filepath.getParent)
    val fw = new PrintWriter(filepath.toFile)
    data.foreach { v =>
      fw.println(s"%016d".format(v.toBinaryString.toLong))
    }
    fw.close()
    filepath.toString
  }


  def streamInstructions(instructions: Seq[Long], packet_stream: BareNoCBundle, clock: Clock)
                        (stream_cond: => Boolean): Unit = {
    val packets = instructions.flatMap { inst =>
      Seq(
        inst & 0x0000FFFF,
        (inst >> 16) & 0x0000FFFF,
        (inst >> 32) & 0x0000FFFF,
        (inst >> 48) & 0x0000FFFF
      ).map { d =>
        new BareNoCBundle(ManticoreBaseISA).Lit(
          _.data -> d.U,
          _.address -> 0.U,
          _.valid -> true.B
        )
      }
    }

    val empty_packet = new BareNoCBundle(ManticoreBaseISA).Lit(
      _.data -> 0.U,
      _.address -> 0.U,
      _.valid -> false.B
    )

    @tailrec
    def streamLoop(left: Seq[BareNoCBundle]): Unit = {
      if (left.nonEmpty) {
        if (stream_cond) {
          packet_stream.poke(left.head)
          clock.step()
          streamLoop(left.tail)
        } else {
          packet_stream.poke(empty_packet)
          clock.step()
          streamLoop(left)
        }
      } else {
        packet_stream.poke(empty_packet)
      }
    }

    streamLoop(packets)
  }

  def streamInstructions(instructions: Seq[Long], dut: Processor)(stream_cond: => Boolean): Unit = {
    streamInstructions(instructions, dut.io.packet_in, dut.clock)(stream_cond)
  }


  /**
   * Program the uniprocessor given some streaming condition (e.g., with some probability)
   *
   * @param instructions the sequence of assembed instructions
   * @param epilogue_length
   * @param sleep_length
   * @param countdown
   * @param dut          the processor
   * @param stream_cond  a of nothing to Boolean that defines whether packet should be enqueued to the processor
   */
  def programProcessor(instructions: Seq[Long], epilogue_length: Int, sleep_length: Int,
                       countdown: Int, dut: Processor)(stream_cond: => Boolean): Unit = {
    programProcessor(instructions, epilogue_length, sleep_length, countdown, dut.io.packet_in, dut.clock)(stream_cond)
  }

  def programProcessor(instructions: Seq[Long], epilogue_length: Int, sleep_length: Int,
                       countdown: Int, packet_stream: BareNoCBundle, clock: Clock)(stream_cond: => Boolean): Unit = {
    val empty_packet = new BareNoCBundle(ManticoreBaseISA).Lit(
      _.data -> 0.U,
      _.address -> 0.U,
      _.valid -> false.B
    )

    @tailrec
    def sendPacketLoop(value: Int): Unit = {
      if (stream_cond) {
        packet_stream.poke(new BareNoCBundle(ManticoreBaseISA).Lit(
          _.data -> value.U,
          _.address -> 0.U,
          _.valid -> true.B
        ))
        clock.step()
        packet_stream.poke(empty_packet)
      } else {
        packet_stream.poke(empty_packet)
        clock.step()
        sendPacketLoop(value)
      }
    }

    sendPacketLoop(instructions.length)

    streamInstructions(instructions, packet_stream, clock)(stream_cond)

    sendPacketLoop(epilogue_length)

    sendPacketLoop(sleep_length)

    sendPacketLoop(countdown)

  }


  // wrap the processor to have a clock enable
  class ClockedProcessor(config: ISA,
                         DimX: Int,
                         DimY: Int,
                         equations: Seq[Seq[Int]],
                         initial_registers: String = "",
                         initial_array: String = "") extends Module {
    val io = IO(new ProcessorInterface(config, DimX, DimY) {
      val clock_enable: Bool = Input(Bool())
      val rwb: Bool = Output(Bool())
      //  val clock: Clock = IO(Input(Clock()))

    })

    val gated_clock: Clock = Wire(Clock())
    val clock_buffer = Module(new ClockBuffer())
    clock_buffer.io.I := clock
    clock_buffer.io.CE := io.clock_enable
    gated_clock := clock_buffer.io.O

    withClockAndReset(clock = gated_clock, reset = 0.B) {
      val impl: Processor = Module(new Processor(config, DimX, DimY,
        equations, initial_registers, initial_array))
      io <> impl.io

      io.rwb := io.periphery.cache.cmd === CacheCommand.Read
    }

  }
}
