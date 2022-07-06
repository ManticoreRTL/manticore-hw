package manticore.machine.memory

import chisel3._
import chisel3.util._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.ChiselScalatestTester
import org.scalatest.matchers.should.Matchers
import manticore.machine.xrt.AxiSlave
import manticore.machine.xrt.axislave_vip
import manticore.machine.xrt.CacheSubsystem
import manticore.machine.xrt.AxiMemoryModel
import manticore.machine.xrt.AxiCacheAdapter
import manticore.machine.xrt.AxiMemoryModelSimInterface
import chiseltest.simulator.VerilatorBackendAnnotation
import chiseltest._
class CacheAxiTester extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  val memorySize = 1 << 16 // 128 KiB
  class CacheSubsystemWithMemory extends Module {

    val io = IO(new Bundle {
      val sim  = new AxiMemoryModelSimInterface(CacheConfig.DataBits)
      val core = CacheConfig.frontInterface()
    })
    val memory = Module(
      new AxiMemoryModel(
        axiParams = AxiCacheAdapter.CacheAxiParameters,
        memorySize = memorySize,
        dataWidth = CacheConfig.DataBits
      )
    )
    val cache = Module(new CacheSubsystem)

    cache.io.base := 0.U
    io.core <> cache.io.core
    cache.io.bus <> memory.io.axi
    io.sim <> memory.io.sim

  }

  behavior of "Cache connected to an Axi master port"

  it should "be able to perform read operations" in {

    test(new CacheSubsystemWithMemory).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      val randGen = new scala.util.Random(3123)

      dut.clock.setTimeout(0)
      val initMemory = Array.tabulate(memorySize) { i =>
        i & 0xffff
      }

      def cachedWrite(value: Int, address: Int): Unit = {
        dut.io.core.addr.poke(address.U)
        dut.io.core.wdata.poke(value.U)
        dut.io.core.start.poke(true.B)
        dut.io.core.cmd.poke(CacheCommand.Write)
        initMemory(address) = value
        dut.clock.step()
        dut.io.core.start.poke(false.B)
        while (!dut.io.core.done.peekBoolean()) { dut.clock.step() }
        dut.clock.step()
      }

      def cachedRead(address: Int): Int = {
        dut.io.core.addr.poke(address.U)
        dut.io.core.start.poke(true.B)
        dut.io.core.cmd.poke(CacheCommand.Read)
        dut.clock.step()
        dut.io.core.start.poke(false.B)
        while (!dut.io.core.done.peekBoolean()) { dut.clock.step() }
        val result = dut.io.core.rdata.peek().litValue.toInt
        dut.clock.step()
        result
      }

      def checkConsistent() = {
        for (addr <- 0 until memorySize) {
          dut.io.sim.raddr.poke(addr.U)
          dut.clock.step()
          dut.io.sim.rdata.expect(initMemory(addr).U)
        }
      }

      dut.clock.step(10)
      dut.io.sim.wen.poke(false.B)
      println("Filling up memory")
      for (addr <- 0 until memorySize) {
        dut.io.sim.wen.poke(1.B)
        dut.io.sim.waddr.poke(addr.U)
        dut.io.sim.wdata.poke(initMemory(addr))
        dut.clock.step()
      }
      dut.io.sim.wen.poke(false.B)

      println("Ensuring memory model is correct")

      checkConsistent()

      println("Filling up cache lines with writes")
      // for (idx <- 0 until (randGen.nextInt(memorySize) + 1)) {
      for (idx <- 0 until 4096) {

        cachedWrite(randGen.nextInt(1 << CacheConfig.DataBits), randGen.nextInt(memorySize))
      }

      println("Reading memory to ensure cache writes work fine")

      // need to flush
      dut.io.core.cmd.poke(CacheCommand.Flush)
      dut.io.core.start.poke(true.B)
      dut.clock.step()
      dut.io.core.start.poke(false.B)

      while (!dut.io.core.done.peekBoolean()) { dut.clock.step() }

      checkConsistent()


      println("Checking cache reads")
      for (idx <- 0 until 4096) {

        val raddr = randGen.nextInt(memorySize)
        val rdata = cachedRead(raddr)
        rdata shouldBe initMemory(raddr)

      }

    }

  }

}
