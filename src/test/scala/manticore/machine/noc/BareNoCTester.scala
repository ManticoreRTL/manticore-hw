package manticore.machine.noc

import Chisel._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import manticore.machine.ManticoreBaseISA
import manticore.machine.TestsCommon.RequiresVerilator
import manticore.machine.core.BareNoC
import manticore.machine.core.NoCBundle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import scala.util.Random

class BareNoCTester extends AnyFlatSpec with ChiselScalatestTester with Matchers {


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

        Range(0, DimX).foreach { x =>
          Range(0, DimY).foreach { y =>
            if (x != expected.target.x || y != expected.target.y) {
              dut.io.corePacketOutput(x)(y).valid.expect(false.B)
            }
          }
        }

        val x = expected.target.x
        val y = expected.target.y
        if (dut.io.corePacketOutput(x)(y).valid.peek().litToBoolean) {
          // check if the data is correct
          if (dut.n_hop == 2) {
            dut.clock.step()
          }
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
          (packet.xHops.litValue.toInt + source.x) % DimX,
          (packet.yHops.litValue.toInt + source.y) % DimY)

      if (!coverage.contains(TestCase(source, target))) {
        val latency = dut.n_hop * (packet.xHops.litValue.toInt + packet.yHops.litValue.toInt)
        val expected = ExpectedPacket(
          data = packet.data.litValue.toInt,
          address = packet.data.litValue.toInt,
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


    test(new BareNoC(6, 7, ManticoreBaseISA, n_hop = 2))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { implicit dut =>
        checkRoutable
      }
    
    test(new BareNoC(6, 7, ManticoreBaseISA, n_hop = 1))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { implicit dut =>
        checkRoutable
      }

  }

}
