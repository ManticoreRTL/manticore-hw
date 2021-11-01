package manticore.xrt

import chisel3._
import chisel3.tester.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import manticore.ManticoreFullISA
import chisel3.tester.{ChiselScalatestTester, testableClock, testableData}
import org.scalatest.{FlatSpec, Matchers}
import scala.annotation.tailrec

class AxiSlaveTester extends FlatSpec with ChiselScalatestTester with Matchers {

  val rdgen = new scala.util.Random(1)

  @tailrec
  private def waitOnCondition(
      cond: => Boolean
  )(implicit dut: AxiSlave): Unit = {
    if (!cond) {
      dut.clock.step()
      waitOnCondition(cond)
    }
  }
  def writeWord(word: Long, address: Int)(implicit dut: AxiSlave) = {

    dut.io.core.AWADDR.poke(address.U)
    dut.io.core.AWVALID.poke(true.B)
    dut.io.core.WVALID.poke(false.B)
    dut.io.core.BREADY.poke(false.B)

    waitOnCondition {
      dut.io.core.AWREADY.peek().litToBoolean
    }

    dut.clock.step()
    dut.io.core.AWVALID.poke(false.B)
    dut.io.core.WVALID.poke(true.B)
    dut.io.core.WSTRB.poke(0xf.U)
    dut.io.core.WDATA.poke(word.U)
    waitOnCondition {
      dut.io.core.WREADY.peek().litToBoolean
    }

    dut.clock.step()
    dut.io.core.BREADY.poke(true.B)
    waitOnCondition {
      dut.io.core.BVALID.peek.litToBoolean
    }
    dut.io.core.BRESP.expect(0.U)
    dut.clock.step()
  }
  def readWord(address: Int)(implicit dut: AxiSlave): Long = {

    dut.io.core.ARADDR.poke(address.U)
    dut.io.core.ARVALID.poke(true.B)
    dut.io.core.RREADY.poke(false.B)
    waitOnCondition {
      dut.io.core.ARREADY.peek.litToBoolean
    }
    dut.clock.step()
    dut.io.core.ARVALID.poke(false.B)
    dut.io.core.RREADY.poke(true.B)
    waitOnCondition {
      dut.io.core.RVALID.peek.litToBoolean
    }
    val read_word = dut.io.core.RDATA.peek().litValue().toLong
    dut.io.core.RRESP.expect(0.U)
    dut.clock.step()
    dut.io.core.RREADY.poke(false.B)

    read_word
  }

  def randomWord = rdgen
    .nextLong() & ((1.toLong << 32.toLong) - 1) // chisel fails if nextInt is used
  behavior of "AxiSlave"

  it should "handle host register writes" in {
    test(new AxiSlave(ManticoreFullISA))
      .withAnnotations(Seq(WriteVcdAnnotation)) { implicit dut =>
        dut.clock.step(1)
        val addresses =
          (dut.io.host_regs.elements ++ dut.io.pointer_regs.elements).map {
            elem =>
              dut.AddressMap(elem)
          }.toSeq

        val expected_data = addresses.map { addr =>
          val word = randomWord
          writeWord(word, addr)
          word
        }
        expected_data.zip(addresses).foreach { case (expected, addr) =>
          val read_word = readWord(addr)
          // println(s"expected: ${expected} read: ${read_word}")
          read_word should equal(expected)
        }
      }
  }
  it should "handle host register reads" in {
    test(new AxiSlave(ManticoreFullISA))
      .withAnnotations(Seq(WriteVcdAnnotation)) { implicit dut =>
        val expected_words = dut.io.dev_regs.elements.map { case (name, data) =>
          val word = randomWord
          data.poke(word.U)
          word
        }
        dut.clock.step()

        expected_words.zip(dut.io.dev_regs.elements).foreach {
          case (expected_value, port) =>
            val addr = dut.AddressMap(port)
            println(s"${port._1} => ${addr}")
            val read_word = readWord(addr)
            read_word should equal(expected_value)
        }
      }
  }

  it should "be able to issue ap_start" in {
    test(new AxiSlave(ManticoreFullISA))
      .withAnnotations(Seq(WriteVcdAnnotation)) { implicit dut =>
        dut.clock.step(1)
        val start_command = 0x1
        writeWord(start_command, AxiSlave.ApControlAddress)
        Range(0, 10).foreach { _ =>
          dut.io.control.ap_start.expect(true.B)
          val ctrl_word = readWord(0)
          ctrl_word should equal(1)
        }

        dut.io.control.ap_ready.poke(true.B)
        dut.io.control.ap_done.poke(true.B)
        
        dut.clock.step()
        dut.io.control.ap_ready.poke(false.B)
        dut.io.control.ap_done.poke(false.B)
        dut.io.control.ap_idle.poke(true.B)
        dut.clock.step(3)
        val ctrl_word = readWord(0)
        ctrl_word should equal(0x6)
        Range(0, 10).foreach { _ =>
          // dut.io.control.ap_start.expect(true.B)
          val ctrl_word = readWord(0)
          ctrl_word should equal(0x4) // done is cleared on read
        }
        dut.clock.step(10)

      }
  }

  it should "be able to generate and interrupt on ap_done" in {
    test(new AxiSlave(ManticoreFullISA))
      .withAnnotations(Seq(WriteVcdAnnotation)) { implicit dut =>
        // enable interrupts
        writeWord(0x1, AxiSlave.GlobalInterruptEnableAddress)
        writeWord(0x1, AxiSlave.IpInterruptEnableRegister)
        dut.clock.step(10)
        // start the axi slave
        writeWord(0x1, AxiSlave.ApControlAddress)
        // wait for some cycles
        Range(0, 20).foreach { _ =>
          dut.io.control.ap_start.expect(true.B)
          dut.clock.step()
        }
        dut.io.control.ap_ready.poke(true.B)
        dut.io.control.ap_done.poke(true.B)
        dut.clock.step()
        dut.io.control.ap_ready.poke(false.B)
        dut.io.control.ap_done.poke(false.B)
        Range(0, 10).foreach { _ =>
          dut.io.control.interrupt.expect(true.B)
          dut.clock.step()
        }

        writeWord(0x3, AxiSlave.IpInterruptStatusRegister) // clear interrupt on ap_ready and ap_done
        Range(0, 5).foreach { _ =>
          dut.io.control.interrupt.expect(false.B)
          dut.clock.step()
        }
      }
  }
}
