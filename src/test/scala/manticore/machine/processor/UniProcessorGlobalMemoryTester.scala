package manticore.machine.processor

import chisel3._
import chiseltest._
import manticore.machine.ISA
import manticore.machine.ManticoreBaseISA
import manticore.machine.ManticoreFullISA
import manticore.machine.assembly
import manticore.machine.assembly.Assembler
import manticore.machine.assembly.Instruction.Add2
import manticore.machine.assembly.Instruction.GlobalLoad
import manticore.machine.assembly.Instruction.GlobalStore
import manticore.machine.assembly.Instruction.Instruction
import manticore.machine.assembly.Instruction.Nop
import manticore.machine.assembly.Instruction.Predicate
import manticore.machine.assembly.Instruction.R
import manticore.machine.core.ClockBuffer
import manticore.machine.core.Processor
import manticore.machine.core.ProcessorInterface
import manticore.machine.memory.CacheCommand
import manticore.machine.processor.UniProcessorTestUtils.ClockedProcessor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.file.Paths
import scala.annotation.tailrec




class UniProcessorGlobalMemoryTester extends AnyFlatSpec with Matchers with ChiselScalatestTester {

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
    Nop(),
    Add2(val_reg, val_reg, const_1),
    Nop(),
    Nop(),
    Predicate(const_1),
    GlobalStore(val_reg, addr_hi, addr_mi, addr_lo),
    Add2(val_reg, const_0, const_0),
    Nop(),
    Nop()
  )

  val equations = Seq.fill(1 << ManticoreFullISA.FunctBits) {
    Seq.fill(ManticoreFullISA.DataBits) {
      BigInt(rdgen.nextInt(1 << 16))
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
          getTestName + File.separator + "rf.data").toAbsolutePath
      },
      UniProcessorTestUtils.createMemoryDataFiles(
        Seq.fill(1 << ManticoreFullISA.IdBits)(0)
      ) {
        Paths.get("test_data_dir" + File.separator +
          getTestName + File.separator + "ra.data").toAbsolutePath
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
                  dut.io.periphery.cache.wdata.peek().litValue.toInt,
                  dut.io.periphery.cache.addr.peek().litValue.toLong,
                  10 max rdgen.nextInt(20)
                )
                if (mem.getOrElse(new_req.address, 0) + 1 != new_req.value) {
                  fail(s"Expected Mem[${new_req.address}] = " +
                    s"${mem.getOrElse(new_req.address, 0) + 1} but got ${new_req.value}")
                }
                println(s"GlobalStore will take ${new_req.latency} cycles")
                val new_mem = mem + (new_req.address -> new_req.value)
                // disable the clock
                dut.io.clock_enable_n.poke(true.B)
                dut.io.periphery.cache.done.poke(false.B)
                dut.clock.step()
                TickState(Some(new_req), None, new_mem)
              } else { // load
                val address = dut.io.periphery.cache.addr.peek().litValue.toLong
                if (!mem.contains(address)) {
                  println(s"Loading 0 from an uninitialized memory location ${address}")
                }
                val new_req = Request(
                  mem.getOrElse(address, 0),
                  address,
                  10 max rdgen.nextInt(20)
                )
                println(s"GlobalLoad will take ${new_req.latency} cycles")
                dut.io.clock_enable_n.poke(true.B)
                dut.io.periphery.cache.done.poke(false.B)
                dut.clock.step()
                TickState(None, Some(new_req), mem)
              }

            } else {
              dut.clock.step()
              dut.io.periphery.cache.done.poke(false.B)
              dut.io.clock_enable_n.poke(false.B)
              TickState(None, None, mem)
            }
          case (Some(s), None) =>
            if (s.finished()) {
              dut.io.periphery.cache.done.poke(true.B)
              dut.io.clock_enable_n.poke(false.B)
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
              dut.io.clock_enable_n.poke(false.B)
              dut.clock.step()
              TickState(None, None, mem)
            } else {
              dut.io.periphery.cache.done.poke(false.B)
              dut.io.clock_enable_n.poke(true.B)
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

      dut.io.clock_enable_n.poke(false.B)
      dut.clock.step()
      UniProcessorTestUtils.programProcessor(
        program.map(inst => Assembler.assemble(inst)(equations)).toIndexedSeq,
        10, 10, 5, dut.io.packet_in, dut.clock
      ) {
        true
      }
      simulate(10000)(TickState(None, None, Map()))

    }

  }
}
