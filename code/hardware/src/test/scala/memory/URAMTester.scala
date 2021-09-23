package memory
import chisel3._
import chiseltest._
import chiseltest.internal.
{WriteVcdAnnotation => DUMP_VCD,
  VerilatorBackendAnnotation => USE_VERILATOR}

import org.scalatest.{FlatSpec, FreeSpec, Matchers}
import chiseltest.experimental.TestOptionBuilder._
import scala.util.Random

class URAMTester extends FlatSpec with Matchers with ChiselScalatestTester {

  val rdgen = Random
  def randomValue: Long = Math.abs(rdgen.nextLong())
  def randomAddress: Long = Math.abs(rdgen.nextInt(4096)).toLong
  def randomByteEnable: Long = Math.abs(rdgen.nextInt(256)).toLong
  val TEST_SIZE = 100
  def enableByte(byteEn: Int, index: Int): Int = byteEn | (1 << index)
//  def isEnabled
  behavior of "URAM"
  it should "have synchronous read and write" in {
    test(new URAM4kx64()){ dut =>
      val test_values = for (i <- Range(0, TEST_SIZE)) yield randomValue
      val address_values = for (i <- Range(0, TEST_SIZE)) yield randomAddress
      dut.io.web.poke(false.B)
      dut.io.bweb.poke(255.U) // enable all bytes
      dut.io.addra.poke(0.U)
      dut.io.addrb.poke(0.U)
      dut.io.dinb.poke(0.U)
      dut.clock.step()
      for((address, value) <- address_values.zip(test_values)) {
        dut.io.dinb.poke(value.U)
        dut.io.addrb.poke(address.U)
        dut.io.web.poke(true.B)
        dut.clock.step()
        dut.io.addra.poke(address.U)
        dut.io.web.poke(false.B)
        dut.clock.step()
        dut.io.douta.expect(value.U)
      }
    }
  }
  it should "partially write a word based on the byte enable" in {
    test(new URAM4kx64()).withAnnotations(Seq(USE_VERILATOR)) { dut =>

      val test_value = Seq.fill(TEST_SIZE){randomValue}
      val test_value_masked = Seq.fill(TEST_SIZE){randomValue}
      val address_values = Seq.fill(TEST_SIZE){randomAddress}
      val byte_enable = Seq.fill(TEST_SIZE){randomByteEnable}
      dut.io.web.poke(false.B)
      dut.io.bweb.poke(255.U) // enable all bytes
      dut.io.addra.poke(0.U)
      dut.io.addrb.poke(0.U)
      dut.io.dinb.poke(0.U)
      dut.clock.step()


      def expectedWord(old_value: Long, new_value: Long, byte_mask: Long) = {
        def extractByte(word: Long, index: Int) = {
          val m : Long = 0x00000000000000FF
          val mask: Long = (m << (index * 8))
          word & mask
        }
        val bytes = Range(0, 8).map { byte_index =>
          val enabled: Long = (byte_mask >> byte_index) % 2
          if (enabled == 1)
            extractByte(new_value, byte_index)
          else
            extractByte(old_value, byte_index)
        }
        bytes.reduce(_|_)
      }
      for(ls <- Seq(test_value, test_value_masked, address_values, byte_enable).transpose) {
        ls match {
          case Seq(value, value_masked, address, byte_en) =>
            // write a randome value
            dut.io.web.poke(true.B)
            dut.io.addrb.poke(address.U)
            dut.io.dinb.poke(value.U)
            dut.io.bweb.poke(255.U)
            dut.clock.step()
            // write a masked value
            dut.io.bweb.poke(byte_en.U)
            dut.io.dinb.poke(value_masked.U)
            dut.clock.step()
            // disable writing
            dut.io.web.poke(false.B)
            dut.io.addra.poke(address.U)
            dut.clock.step()
            // check the value
            val expected = expectedWord(value, value_masked, byte_en)
            dut.io.douta.expect(expected.U)

        }
      }

    }
  }

}
