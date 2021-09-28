package manticore


import Chisel._
import chisel3.tester.{ChiselScalatestTester, testableClock, testableData, timescope}
import chisel3.tester.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chisel3.tester.experimental.sanitizeFileName
import chisel3.testers.TesterDriver.VerilatorBackend
import chisel3.withClockAndReset
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import memory.CacheCommand
import org.scalatest.Matchers.fail
import org.scalatest.{FlatSpec, Matchers}
import manticore.assembly.Assembler
import manticore.assembly.Instruction.{Add2, GlobalLoad, GlobalStore, Instruction, Nop, R}
import manticore.core.{ClockBuffer, Processor, ProcessorInterface}

import java.io.File
import java.nio.file.Paths
import scala.annotation.tailrec


/// wrap the processor to have a clock enable
class ClockedProcessor(config: ISA,
                       DimX: Int,
                       DimY: Int,
                       equations: Seq[Seq[Int]],
                       initial_registers: String = "",
                       initial_array: String = "") extends Module {
  val io = IO(new ProcessorInterface(config, DimX, DimY) {
    val clock_enable: Bool = Input(Bool())
    val rwb: Bool = Output(Bool())
    //  val clock: Clock = IO(Input(Clock()))

  })

  val gated_clock: Clock = Wire(Clock())
  val clock_buffer = Module(new ClockBuffer())
  clock_buffer.io.I := clock
  clock_buffer.io.CE := io.clock_enable
  gated_clock := clock_buffer.io.O

  withClockAndReset(clock = gated_clock, reset = 0.B) {
    val impl: Processor = Module(new Processor(config, DimX, DimY,
      equations, initial_registers, initial_array))
    io <> impl.io

    io.rwb := io.periphery.cache.cmd === CacheCommand.Read
  }

}

class UniProcessorGlobalMemoryTester extends FlatSpec with Matchers with ChiselScalatestTester {

  val rdgen = new scala.util.Random(0)

  val base_addr = rdgen.nextLong() % (1 << (3 * ManticoreBaseISA.IdBits * 3))
  val addr_lo_init = rdgen.nextInt((1 << ManticoreBaseISA.DataBits) - 1024)
  val addr_mid_init = rdgen.nextInt(1 << ManticoreBaseISA.DataBits)
  val addr_hi_init = rdgen.nextInt(1 << ManticoreBaseISA.DataBits)
  val addr_lo = R(2)
  val addr_mi = R(0)
  val addr_hi = R(0)
  val const_1 = R(1)
  val const_0 = R(2)
  val val_reg = R(3) // initially zero
  val program = Array[Instruction](
    GlobalLoad(val_reg, addr_hi, addr_mi, addr_lo),
    Nop(),
    Nop(),
    Add2(val_reg, val_reg, const_1),
    Nop(),
    Nop(),
    GlobalStore(val_reg, addr_hi, addr_mi, addr_lo),
    Add2(val_reg, const_0, const_0),
    Nop(),
    Nop()
  )

  val equations = Seq.fill(1 << ManticoreFullISA.FunctBits) {
    Seq.fill(ManticoreFullISA.DataBits) {
      rdgen.nextInt(1 << 16)
    }
  }

  def makeProcessor(): ClockedProcessor =
    new ClockedProcessor(
      ManticoreFullISA,
      2, 2,
      equations,
      UniProcessorTestUtils.createMemoryDataFiles(
        Seq.tabulate(1 << ManticoreFullISA.IdBits) {
          i =>
            if (i == val_reg.index) {
              0
            } else if (i == addr_lo.index) {
              addr_lo_init
            } else if (i == addr_mi.index) {
              addr_mid_init
            } else if (i == addr_hi.index) {
              addr_hi_init
            } else {
              i
            }
        }
      ) {
        Paths.get("test_data_dir" + File.separator +
          sanitizeFileName(scalaTestContext.value.get.name) + File.separator + "rf.data").toAbsolutePath
      },
      UniProcessorTestUtils.createMemoryDataFiles(
        Seq.fill(1 << ManticoreFullISA.IdBits)(0)
      ) {
        Paths.get("test_data_dir" + File.separator +
          sanitizeFileName(scalaTestContext.value.get.name) + File.separator + "ra.data").toAbsolutePath
      }
    )

  behavior of
    "Processor accessing global memory"

  it should "should correctly handle register spilling to global" +
    " memory when the clock gating works correctly" in {

    test {
      makeProcessor()
    }.withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>


      type MemoryStorage = Map[Long, Int]

      case class Request(value: Int, address: Long, latency: Int) {
        def finished(): Boolean = latency == 0

        def tick(): Request =
          if (finished())
            fail("request already finished!")
          else
            Request(value, address, latency - 1)
      }

      case class TickState(store: Option[Request], load: Option[Request], mem: MemoryStorage)

      def tick(state: TickState): TickState = {
        val store = state.store
        val load = state.load
        val mem = state.mem
        val new_state: TickState = (store, load) match {
          case (None, None) =>
            if (dut.io.periphery.cache.start.peek().litToBoolean) {

              if (!dut.io.rwb.peek().litToBoolean) {
                // store

                val new_req = Request(
                  dut.io.periphery.cache.wdata.peek().litValue().toInt,
                  dut.io.periphery.cache.addr.peek().litValue().toLong,
                  10 max rdgen.nextInt(20)
                )
                if (mem.getOrElse(new_req.address, 0) + 1 != new_req.value) {
                  fail(s"Expected Mem[${new_req.address}] = " +
                    s"${mem.getOrElse(new_req.address, 0) + 1} but got ${new_req.value}")
                }
                println(s"GlobalStore will take ${new_req.latency} cycles")
                val new_mem = mem + (new_req.address -> new_req.value)
                // disable the clock
                dut.io.clock_enable.poke(false.B)
                dut.io.periphery.cache.done.poke(true.B)
                dut.clock.step()
                TickState(Some(new_req), None, new_mem)
              } else { // load
                val address = dut.io.periphery.cache.addr.peek().litValue().toLong
                if (!mem.contains(address)) {
                  println(s"Loading 0 from an uninitialized memory location ${address}")
                }
                val new_req = Request(
                  mem.getOrElse(address, 0),
                  address,
                  10 max rdgen.nextInt(20)
                )
                println(s"GlobalLoad will take ${new_req.latency} cycles")
                dut.io.clock_enable.poke(false.B)
                dut.io.periphery.cache.done.poke(false.B)
                dut.clock.step()
                TickState(None, Some(new_req), mem)
              }

            } else {
              dut.clock.step()
              dut.io.periphery.cache.done.poke(false.B)
              dut.io.clock_enable.poke(true.B)
              TickState(None, None, mem)
            }
          case (Some(s), None) =>
            if (s.finished()) {
              dut.io.periphery.cache.done.poke(true.B)
              dut.io.clock_enable.poke(true.B)
              dut.clock.step()
              TickState(None, None, mem)
            } else {
              dut.io.periphery.cache.done.poke(false.B)
              dut.clock.step()
              TickState(Some(s.tick()), None, mem)
            }
          case (None, Some(l)) =>
            if (l.finished()) {
              dut.io.periphery.cache.done.poke(true.B)
              dut.io.periphery.cache.rdata.poke(l.value.U)
              dut.io.clock_enable.poke(true.B)
              dut.clock.step()
              TickState(None, None, mem)
            } else {
              dut.io.periphery.cache.done.poke(false.B)
              dut.io.clock_enable.poke(false.B)
              dut.clock.step()
              TickState(None, Some(l.tick()), mem)
            }
          case (Some(_), Some(_)) => fail("Can not have 2 inflight requests!")
        }
        new_state
      }

      @tailrec
      def simulate(num_tick: Int)(implicit tickState: TickState): TickState = {
        if (num_tick != 0) {
          simulate(num_tick - 1)(tick(tickState))
        } else {
          tickState
        }
      }

      dut.io.clock_enable.poke(true.B)
      dut.clock.step()
      UniProcessorTestUtils.programProcessor(
        program.map(inst => Assembler.assemble(inst)(equations)),
        10, 10, 5, dut.io.packet_in, dut.clock
      ) {
        true
      }
      simulate(1000)(TickState(None, None, Map()))

    }

  }
}
