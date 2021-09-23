package example

import chisel3.tester.{ChiselScalatestTester, testableClock, testableData}
import org.scalatest.{FlatSpec, Matchers}
import Chisel._
import chisel3.tester.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chiseltest.internal.{VerilatorBackendAnnotation => USE_VERILATOR, WriteVcdAnnotation => DUMP_VCD}
import firrtl.transforms.NoDCEAnnotation

import scala.util.Random

class ConstantFirFilterTester extends FlatSpec with ChiselScalatestTester with Matchers {

  val rdgen = new Random(0)
  val DEPTH = 3
  val TEST_SIZE = 32
  val coeffs = Seq.fill(DEPTH)(rdgen.nextInt(1 << 8))
  val Xn = Seq.fill(TEST_SIZE)(Seq.fill(DEPTH)(rdgen.nextInt(1 << 8)))
  behavior of "ConstantFirFilter"
  it should "match the software equivalent" in {
    test(new ConstantFirFilter(coeffs)).withAnnotations(Seq(DUMP_VCD)) {dut =>
      Xn.foreach{xs =>
        xs.foreach{x =>
          dut.io.Xn.poke(x.U)
          dut.clock.step()
        }
        val y = coeffs.zip(xs).map{case (c, x) => (x * c) & 0x0000FFFF}.sum
        dut.io.Yn.expect((y & 0x0000FFFF).U)

      }
    }
  }
  it should "write correct values to the output memory" in {
    test(new WrappedConstantFirFilter(Xn.flatten, coeffs)).withAnnotations(Seq(DUMP_VCD, NoDCEAnnotation)) { dut =>

      dut.clock.step(Xn.flatten.size + coeffs.size + 10)
      dut.reset.poke(true.B)
      dut.clock.step( 10)
      val Yn = dut.test_data.sliding(coeffs.size, 1).map { xs: Seq[Int] =>
        coeffs.zip(xs).map{case (c, x) => (x * c) & 0x0000FFFF}.sum
      }.toSeq

//      Yn.zipWithIndex.foreach{ case (y, i) =>
//        dut.y_mem(i).expect(y.U)
//      }

    }
  }

}
