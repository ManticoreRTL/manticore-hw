package thyrio

import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.tester.experimental.sanitizeFileName
import thyrio.core.{BareNoCBundle, Processor}

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path, Paths}
import scala.util.Random
import Chisel._
import chisel3.tester.{parallel, testableClock, testableData}

import scala.annotation.tailrec

object UniProcessorTestUtils {

  def createMemoryDataFiles(data: Seq[Int])(filepath: Path): String = {
    Files.createDirectories(filepath.getParent)
    val fw = new PrintWriter(filepath.toFile)
    data.foreach{ v =>
      fw.println(s"%016d".format(v.toBinaryString.toLong))
    }
    fw.close()
    filepath.toString
  }

  def streamInstructions(instructions: Seq[Long], dut: Processor)(stream_cond: => Boolean): Unit = {


    val packets = instructions.flatMap{inst =>
      Seq(
        inst & 0x0000FFFF,
        (inst >> 16) & 0x0000FFFF,
        (inst >> 32) & 0x0000FFFF,
        (inst >> 48) & 0x0000FFFF
      ).map{d =>
        new BareNoCBundle(ThyrioISA).Lit(
          _.data -> d.U,
          _.address -> 0.U,
          _.valid -> true.B
        )
      }
    }

    val empty_packet =  new BareNoCBundle(ThyrioISA).Lit(
      _.data -> 0.U,
      _.address -> 0.U,
      _.valid -> false.B
    )

    @tailrec
    def streamLoop(left: Seq[BareNoCBundle]): Unit = {
      if (left.nonEmpty) {
        if (stream_cond) {
          dut.io.packet_in.poke(left.head)
          dut.clock.step()
          streamLoop(left.tail)
        } else {
          dut.io.packet_in.poke(empty_packet)
          dut.clock.step()
          streamLoop(left)
        }
      } else {
        dut.io.packet_in.poke(empty_packet)
      }
    }
    streamLoop(packets)
  }


  /**
   * Program the uniprocessor given some streaming condition (e.g., with some probability)
   * @param instructions the sequence of assembed instructions
   * @param epilogue_length
   * @param sleep_length
   * @param countdown
   * @param dut the processor
   * @param stream_cond a of nothing to Boolean that defines whether packet should be enqueued to the processor
   */
  def programProcessor(instructions: Seq[Long], epilogue_length: Int, sleep_length: Int,
                       countdown: Int, dut: Processor)(stream_cond: => Boolean): Unit = {
    val empty_packet =  new BareNoCBundle(ThyrioISA).Lit(
      _.data -> 0.U,
      _.address -> 0.U,
      _.valid -> false.B
    )
    @tailrec
    def sendPacketLoop(value: Int): Unit = {
      if (stream_cond) {
        dut.io.packet_in.poke(new BareNoCBundle(ThyrioISA).Lit(
          _.data -> value.U,
          _.address -> 0.U,
          _.valid -> true.B
        ))
        dut.clock.step()
        dut.io.packet_in.poke(empty_packet)
      } else {
        dut.io.packet_in.poke(empty_packet)
        dut.clock.step()
        sendPacketLoop(value)
      }
    }

    sendPacketLoop(instructions.length)

    streamInstructions(instructions, dut)(stream_cond)

    sendPacketLoop(epilogue_length)

    sendPacketLoop(sleep_length)

    sendPacketLoop(countdown)

  }
}
