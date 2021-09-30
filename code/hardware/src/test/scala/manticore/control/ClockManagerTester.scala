package manticore.control

import Chisel._
import chisel3.tester.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chisel3.tester.{ChiselScalatestTester, testableClock}
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import chiseltest.testableData
import manticore.TestsCommon.RequiresVerilator
import manticore.core.{CacheRequestIntercept, ClockManager, ExceptionHandler}
import org.scalatest.{FlatSpec, Matchers, Tag}


object ClockManagerTester {


  class ClockControlModules(NameBits: Int) extends Module {

    val io = IO(
      new Bundle {

      }
    )


    val intercept = Seq.fill(4) {
      Module(new CacheRequestIntercept)
    }

    val handler = Seq.fill(4) {
      Module(new ExceptionHandler(NameBits))
    }


  }


}
class ClockManagerTester extends FlatSpec with Matchers with ChiselScalatestTester {

  val rdgen = new scala.util.Random(0)
  val NumUsers = 4
  behavior of "Clock manager"


  implicit class TestableVec[T <: Data](orig: Vec[T]) {
    def pokeWithSeq(literals: Seq[T]): Unit = {
      orig.zip(literals).foreach{ case (wire, value) =>
        wire.poke(value)
      }
    }
  }

  it should "correctly capture a single request" taggedAs RequiresVerilator in {
    test(new ClockManager(NumUsers = NumUsers)).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>

      dut.clock.step()
      dut.io.start_request.pokeWithSeq(Seq.fill(NumUsers)(false.B))
      dut.io.done_request.pokeWithSeq(Seq.fill(NumUsers)(false.B))
      dut.io.clock_enable_n.expect(false.B)
      dut.clock.step()
      // request clock gating by a single entity
      dut.io.start_request.pokeWithSeq{
        true.B +: Seq.fill(NumUsers - 1)(false.B)
      }
      dut.clock.step()
      dut.io.clock_enable_n.expect(true.B)
      dut.io.start_request.pokeWithSeq(Seq.fill(NumUsers)(false.B))
      dut.clock.step(rdgen.nextInt(20) + 1)
      dut.io.done_request.pokeWithSeq(true.B +: Seq.fill(NumUsers - 1)(false.B))
      dut.clock.step(2)
      dut.io.clock_enable_n.expect(false.B)
      dut.clock.step()

    }
  }

}
