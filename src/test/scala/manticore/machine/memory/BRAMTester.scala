package manticore.machine.memory

import chisel3._
import chiseltest._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class BRAMTester extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "BRAM"
  it should "be able to read or write a value in a single clock edge" in {
    test(BRAM2Kx16()).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
//      dut.clock.step(2)
      val rgen = Random
      val test_size = 256
      val test_values = for (i <- Range(0, test_size)) yield rgen.nextInt(255)

      dut.io.wea.poke(false.B)
      dut.io.web.poke(false.B)
      dut.io.addra.poke(0.U)
      dut.io.addrb.poke(0.U)
      dut.io.dina.poke(0.U)
      dut.io.dinb.poke(0.U)

      dut.clock.step()
      for(address <- Range(0, test_size)) {
        dut.io.dina.poke(test_values(address).U)
        dut.io.addra.poke(address.U)
        dut.io.wea.poke(true.B)
        dut.clock.step()
      }

      for(address <- Range(0, test_size)) {
        dut.io.addra.poke(address.U)
        dut.io.wea.poke(false.B)
        dut.clock.step()
        dut.io.douta.expect(test_values(address).U)
      }
      dut.clock.step()
    }
  }
}

