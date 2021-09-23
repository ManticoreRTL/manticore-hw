package thyrio

import chisel3.tester.{ChiselScalatestTester, testableClock, testableData}
import chiseltest.internal.VerilatorBackendAnnotation
import org.scalatest.{FlatSpec, Matchers}
import thyrio.core.{NoCBundle, Programmer}
import Chisel._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.tester.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chiseltest.internal.WriteVcdAnnotation

import scala.annotation.tailrec

class ProgrammerTester extends FlatSpec with ChiselScalatestTester with Matchers {


  val rdgen = new scala.util.Random(0)
  val DimX = 3
  val DimY = 3
  val isa = ThyrioISA
  behavior of "Programmer"

  it should "correctly read instruction stream from the cache and stream it out" in {


    test(new Programmer(config = isa, DIMX = DimX, DIMY = DimY)).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ){ dut =>

      val memory_spec = new ProgrammerTestUtils.MemoryStreamSpec(DimX, DimY, isa, rdgen)

      def validateStreaming(): Unit = {
        case class MemAccess(counter: Int, value: Int)
        @tailrec
        def loop(check_index: Int, access: Option[MemAccess], expected_stream: Seq[(Int, Int, Int)]): Unit = {
          if (memory_spec.checkAddress(check_index) && expected_stream.nonEmpty) {
            val next_access = access match {
              case Some(MemAccess(counter, value)) =>
                if (counter == 0) {
                  dut.io.cache_frontend.done.poke(true.B)
                  dut.io.cache_frontend.rdata.poke(value.U)
                  None
                } else {
                  dut.io.cache_frontend.rdata.poke(0.U)
                  dut.io.cache_frontend.done.poke(false.B)
                  Some(MemAccess(counter - 1, value))
                }
              case None =>
                dut.io.cache_frontend.rdata.poke(0.U)
                dut.io.cache_frontend.done.poke(false.B)
                None
            }

            val new_access = if (dut.io.cache_frontend.start.peek().litToBoolean) {
              /// read from the cache/memory
              if (next_access.nonEmpty) {
                fail("Can not handle multiple cache accesses")
              }
              val latency = 2 max rdgen.nextInt(2)
              val address = dut.io.cache_frontend.addr.peek().litValue().toInt
              val rdata = memory_spec(address)
              Some(MemAccess(latency, rdata))
            } else {
              next_access
            }

            val (next_index: Int, next_expected_stream: Seq[(Int, Int, Int)]) =
              if (dut.io.packet_out.valid.peek().litToBoolean) {
                dut.io.packet_out.xHops.expect(expected_stream.head._2.U)
                dut.io.packet_out.yHops.expect(expected_stream.head._3.U)
                dut.io.packet_out.data.expect(expected_stream.head._1.U)
                (check_index + 1, expected_stream.tail)
              } else {
                (check_index, expected_stream)
              }
            dut.io.idle.expect(false.B)
            dut.io.done.expect(false.B)
            dut.clock.step()
            loop(next_index, new_access, next_expected_stream)
          }
        }

        dut.io.start.poke(true.B)
        dut.io.instruction_stream_base.poke(memory_spec.base_address.U)
        dut.clock.step()
        dut.clock.setTimeout(DimX * DimY * 1024)
        dut.io.start.poke(false.B)
        dut.io.core_active.foreach{v => v.poke(false.B)}
        loop(memory_spec.base_address, None, memory_spec.expected_stream)
      }

      println("Starting stream validation, this may take a while...")
      validateStreaming()

      def checkSyn(exec_times: Seq[Int]): Unit = {
        @tailrec
        def loop(counters: Seq[Int]): Unit = {
          if (counters.exists(_ != 0)) {

            counters zip dut.io.core_active foreach { case (v, port) => port.poke((v > 0).B) }
            val next_counters = counters.map(x => 0 max (x - 1))
            dut.clock.step()
            dut.io.global_synch.expect(false.B)
            loop(next_counters)
          }
        }
        loop(exec_times)
        dut.io.core_active.foreach(port => port.poke(false.B))
        dut.clock.step()
        dut.io.global_synch.expect(true.B)
        dut.io.core_active.foreach(p => p.poke(false.B))
        dut.clock.step()
      }

      checkSyn(Seq.fill(DimX * DimY){rdgen.nextInt(300)})




    }

  }

}
