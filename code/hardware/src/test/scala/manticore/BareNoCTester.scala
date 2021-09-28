package manticore

import chisel3.tester.{ChiselScalatestTester, testableClock, testableData}
import org.scalatest.{FlatSpec, Matchers}
import manticore.core.{BareNoC, NoCBundle}

import scala.util.Random
import Chisel._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.tester.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}

import scala.annotation.tailrec

class BareNoCTester extends FlatSpec with ChiselScalatestTester with Matchers {


  val rdgen = new Random(0)

  val config = ManticoreBaseISA



  def checkRoutable(implicit dut: BareNoC): Unit = {


    val DimX = dut.io.corePacketInput.length
    val DimY = dut.io.corePacketInput.head.length

    case class ExpectedPacket(data: Int, address: Int, target: Position, latency: Int)
    case class Position(x: Int, y: Int)
    case class TestCase(source: Position, target: Position)

    @tailrec
    def waitResponse(expected: ExpectedPacket, cycles: Int = 0): Unit = {
      if (cycles < DimX * DimY) {
        val x = expected.target.x
        val y = expected.target.y
        if (dut.io.corePacketOutput(x)(y).valid.peek().litToBoolean) {
          // check if the data is correct
          dut.io.corePacketOutput(x)(y).data.expect(expected.data.U)
          dut.io.corePacketOutput(x)(y).data.expect(expected.address.U)
          if (cycles != expected.latency) {
            fail(s"Packet did not meet timing requirements, " +
              s"expected latency is ${expected.latency} but packet received after ${cycles}")
          }
        } else {
          dut.clock.step()
          waitResponse(expected, cycles + 1)
        }
      } else {
        fail(s"Packet did not meet timing requirements, " +
          s"expected latency is ${expected.latency} but packet not received after ${cycles}")

      }
    }


    @tailrec
    def singleTest(coverage: Set[TestCase]): TestCase = {

      val source = Position(rdgen.nextInt(DimX), rdgen.nextInt(DimY))
      val packet: NoCBundle = rdgen.nextInt(3) match {
        case 0 => new NoCBundle(DimX, DimY, config).Lit(
          _.data -> SwitchTestUtils.genData(config)(rdgen),
          _.address -> SwitchTestUtils.genAddress(config)(rdgen),
          _.valid -> true.B,
          _.xHops -> (rdgen.nextInt(DimX - 1) + 1).U,
          _.yHops -> (rdgen.nextInt(DimY - 1) + 1).U
        )
        case 1 => new NoCBundle(DimX, DimY, config).Lit(
          _.data -> SwitchTestUtils.genData(config)(rdgen),
          _.address -> SwitchTestUtils.genAddress(config)(rdgen),
          _.valid -> true.B,
          _.xHops -> (rdgen.nextInt(DimX - 1) + 1).U,
          _.yHops -> 0.U
        )
        case 2 => new NoCBundle(DimX, DimY, config).Lit(
          _.data -> SwitchTestUtils.genData(config)(rdgen),
          _.address -> SwitchTestUtils.genAddress(config)(rdgen),
          _.valid -> true.B,
          _.xHops -> 0.U,
          _.yHops -> (rdgen.nextInt(DimY - 1) + 1).U
        )
        case _ => fail("Invalid random number!")
      }

      val target =
        Position(
          (packet.xHops.litValue().toInt + source.x) % DimX,
          (packet.yHops.litValue().toInt + source.y) % DimY)

      if (!coverage.contains(TestCase(source, target))) {
        val latency = (packet.xHops.litValue().toInt + packet.yHops.litValue().toInt)
        val expected = ExpectedPacket(
          data = packet.data.litValue().toInt,
          address = packet.data.litValue().toInt,
          target = target,
          latency = latency
        )

        println(s"Sending ${packet} from ${source} to ${target}")
        dut.io.corePacketInput(source.x)(source.y).poke(packet)
        dut.clock.step()
        dut.io.corePacketInput(source.x)(source.y).poke(
          new NoCBundle(DimX, DimY, config).Lit(
            _.data -> 0.U,
            _.address -> 0.U,
            _.valid -> false.B,
            _.xHops -> 0.U,
            _.yHops -> 0.U
          ))
        waitResponse(expected)
        TestCase(source, target)
      } else {
        singleTest(coverage)
      }
    }

    def testUntilCoverage(percent: Double) = {
      val test_space = (DimX * DimY) * (DimX * DimY - 1)
      @tailrec
      def loop(coverage: Set[TestCase]): Double = {
        val cov = (coverage.size.toFloat / test_space.toFloat) * 100.0
        if (cov < percent) {
          val new_case = singleTest(coverage)
          loop(coverage.union(Set(new_case)))
        } else {
          println(s"Reached coverage ${cov} %")
          cov
        }
      }
      loop(Set())
    }
    dut.io.configEnable.poke(false.B)
    testUntilCoverage(99.9)
  }

  behavior of "BareNoC"

  it should "always deliver packets in absence of congestion" taggedAs RequiresVerilator in {

    test(new BareNoC(6, 7, ManticoreBaseISA))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { implicit dut =>
        checkRoutable
      }

  }

}
