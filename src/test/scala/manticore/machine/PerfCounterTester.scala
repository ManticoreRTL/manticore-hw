package manticore.machine

import chisel3._
import chisel3.util._

// import chisel3.tester.testableData
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class PerfCounterTester extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "PerfCounter"

  class Wrapper(bits: Int, levels: Int) extends Module {
    val io = IO(new Bundle {
      val value = Output(UInt((bits * levels).W))
      val en    = Input(Bool())
      val clr   = Input(Bool())
    })
    val cnt      = PerfCounter(bits, levels)
    def maxValue = cnt.maxValue
    io.value := cnt.value
    when(io.clr) { cnt.clear() }
      .elsewhen(io.en) { cnt.inc() }

  }

  def mkTest(bits: Int, levels: Int, upTo: Int): Unit =
    s"PerfCounter ${bits}-bits ${levels}-deep" should s"count up correctly to ${upTo}" in {

      test(new Wrapper(bits, levels)).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
        dut.io.en.poke(false.B)
        dut.io.clr.poke(true.B)

        dut.clock.step()
        dut.io.clr.poke(false.B)

        for (i <- 0 until upTo) {
          dut.io.en.poke(true.B)
          dut.clock.step()
        }

        dut.io.en.poke(false.B)

        dut.clock.step(levels + 1)
        val maxValue =
          if (upTo <= dut.maxValue) {
            dut.io.value.expect(upTo)
          } else {
            fail("counter overflow")
          }

      }

    }

  mkTest(2, 4, 12)  // 8-bit counter 4-level deep
  mkTest(2, 4, 255) // 8-bit counter 4-level deep
//   mkTest(2, 4, 256) // overflows
  mkTest(8, 4, 12)

}
