package manticore.machine.xrt

import chisel3._
import chiseltest._
import manticore.machine.ManticoreFullISA
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import manticore.machine.xrt.AxiSlave.DramBank0Base
import manticore.machine.xrt.AxiSlave.DramBank1Base
import manticore.machine.xrt.AxiSlave.ScheduleConfig
import manticore.machine.xrt.AxiSlave.TraceDumpBase
import manticore.machine.xrt.AxiSlave.GlobalMemoryInstructionBase
import manticore.machine.xrt.AxiSlave.BootloaderCycleCount
import manticore.machine.xrt.AxiSlave.DeviceInfo
import manticore.machine.xrt.AxiSlave.ExceptionId
import manticore.machine.xrt.AxiSlave.CycleCount
import manticore.machine.xrt.AxiSlave.TraceDumpHead
import manticore.machine.xrt.AxiSlave.VirtualCycleCount

class AxiSlaveTester extends AnyFlatSpec with ChiselScalatestTester with Matchers {

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
      dut.io.core.BVALID.peek().litToBoolean
    }
    dut.io.core.BRESP.expect(0.U)
    dut.clock.step()
  }
  def readWord(address: Int)(implicit dut: AxiSlave): Long = {

    dut.io.core.ARADDR.poke(address.U)
    dut.io.core.ARVALID.poke(true.B)
    dut.io.core.RREADY.poke(false.B)
    waitOnCondition {
      dut.io.core.ARREADY.peek().litToBoolean
    }
    dut.clock.step()
    dut.io.core.ARVALID.poke(false.B)
    dut.io.core.RREADY.poke(true.B)
    waitOnCondition {
      dut.io.core.RVALID.peek().litToBoolean
    }
    val read_word = dut.io.core.RDATA.peek().litValue.toLong
    dut.io.core.RRESP.expect(0.U)
    dut.clock.step()
    dut.io.core.RREADY.poke(false.B)

    read_word
  }

  def randomWord = rdgen
    .nextLong() & ((1.toLong << 32.toLong) - 1) // chisel fails if nextInt is used
  behavior of "AxiSlave"

  it should "handle host register read/writes" in {
    test(new AxiSlave(ManticoreFullISA))
      .withAnnotations(Seq(WriteVcdAnnotation)) { implicit dut =>
        dut.clock.step(1)
        val hostRegs = AxiSlave.listRegs.collect { case r: AxiSlave.HostReg => r }
        hostRegs.foreach { hr =>
          val addr = AxiSlave.addressOf(hr)
          val expected = if (hr.width == 64) {
            val w1 = randomWord
            writeWord(w1, addr)
            readWord(addr) shouldEqual w1
            val w2 = randomWord
            writeWord(w2, addr + 4)
            readWord(addr + 4) shouldEqual w2
            BigInt(w1) | (BigInt(w2) << 32)
          } else if (hr.width == 32) {
            val w = randomWord
            writeWord(w, addr)
            readWord(addr) shouldEqual w
            BigInt(w)
          } else {
            fail("can only test 32- or 64-bit registers")
          }
          (hr: @unchecked) match {
            case DramBank0Base               => dut.io.pointer_regs.pointer_0.expect(expected)
            // case DramBank1Base               => dut.io.pointer_regs.pointer_1.expect(expected)
            case GlobalMemoryInstructionBase => dut.io.host_regs.global_memory_instruction_base.expect(expected)
            case ScheduleConfig              => dut.io.host_regs.schedule_config.expect(expected)
            case TraceDumpBase               => dut.io.host_regs.trace_dump_base.expect(expected)
          }

        }
      }
  }
  it should "handle dev registers " in {
    test(new AxiSlave(ManticoreFullISA)).withAnnotations(Seq(WriteVcdAnnotation)) { implicit dut =>
      dut.clock.step(1)
      val devRegs = AxiSlave.listRegs.collect { case r: AxiSlave.DevReg => r }
      def read64(addr: Int) = {
        val w1 = readWord(addr)
        val w2 = readWord(addr + 4)
        (BigInt(w2) << 32) | BigInt(w1)
      }
      def read32(addr: Int) = readWord(addr)
      devRegs.foreach { dr =>
        val w64  = (BigInt(randomWord) << 32) | BigInt(randomWord)
        val w32  = BigInt(randomWord)
        val addr = AxiSlave.addressOf(dr)
        dr match {
          case BootloaderCycleCount =>
            dut.io.dev_regs.bootloader_cycles.poke(w32)
            dut.clock.step()
            read32(addr) shouldEqual w32
          case DeviceInfo =>
            dut.io.dev_regs.device_info.poke(w32)
            dut.clock.step()
            read32(addr) shouldEqual w32
          case ExceptionId =>
            dut.io.dev_regs.exception_id.poke(w32)
            dut.clock.step()
            read32(addr) shouldEqual w32
          case CycleCount        =>
            dut.io.dev_regs.execution_cycles.poke(w64)
            dut.clock.step()
            read64(addr) shouldEqual w64
          case TraceDumpHead     =>
            dut.io.dev_regs.trace_dump_head.poke(w64)
            dut.clock.step()
            read64(addr) shouldEqual w64
          case VirtualCycleCount =>
            dut.io.dev_regs.virtual_cycles.poke(w64)
            dut.clock.step()
            read64(addr) shouldEqual w64
        }

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
