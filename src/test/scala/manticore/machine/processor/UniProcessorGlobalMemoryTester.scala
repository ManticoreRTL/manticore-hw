package manticore.machine.processor

import chisel3._
import chiseltest._
import manticore.machine.ISA
import manticore.machine.ManticoreBaseISA
import manticore.machine.ManticoreFullISA
import manticore.machine.UIntWide
import manticore.machine.assembly
import manticore.machine.assembly.Assembler
import manticore.machine.assembly.Instruction
import manticore.machine.assembly.Instruction.Add2
import manticore.machine.assembly.Instruction.GlobalLoad
import manticore.machine.assembly.Instruction.GlobalStore
import manticore.machine.assembly.Instruction.Instruction
import manticore.machine.assembly.Instruction.Nop
import manticore.machine.assembly.Instruction.Predicate
import manticore.machine.assembly.Instruction.R
import manticore.machine.core.Processor
import manticore.machine.core.ProcessorInterface
import manticore.machine.memory.CacheCommand
import manticore.machine.processor.UniProcessorTestUtils.ClockedProcessor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.file.Paths
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

class UniProcessorGlobalMemoryTester extends AnyFlatSpec with Matchers with ChiselScalatestTester {

  val rdgen    = new scala.util.Random(0)
  val numTests = 10

  val initialRegs = ArrayBuffer.fill(ManticoreBaseISA.numRegs)(0)
  val reg_lo      = ArrayBuffer.fill(numTests)(R(0))
  val reg_mid     = ArrayBuffer.fill(numTests)(R(0))
  val reg_hi      = ArrayBuffer.fill(numTests)(R(0))
  val reg_store   = ArrayBuffer.fill(numTests)(R(0))
  val val_store   = ArrayBuffer.fill(numTests)(0)
  val addr_store  = ArrayBuffer.fill(numTests)(0.toLong)
  val reg_load    = ArrayBuffer.fill(numTests)(R(0))
  val mem         = Map[Long, Int]()
  Range(0, numTests).foreach { i =>
    // addr_lo
    initialRegs(i) = rdgen.nextInt(2)
    reg_lo(i) = R(i)
  }
  Range(numTests, 2 * numTests).foreach { i =>
    // addr_mid
    initialRegs(i) = rdgen.nextInt(1 << ManticoreBaseISA.DataBits)
    reg_mid(i - numTests) = R(i)
  }
  Range(2 * numTests, 3 * numTests).foreach { i =>
    // addr_hi
    initialRegs(i) = rdgen.nextInt(1 << ManticoreBaseISA.DataBits)
    reg_hi(i - 2 * numTests) = R(i)
  }
  Range(3 * numTests, 4 * numTests).foreach { i =>
    // store value
    initialRegs(i) = rdgen.nextInt(1 << ManticoreBaseISA.DataBits)
    reg_store(i - 3 * numTests) = R(i)
    val_store(i - 3 * numTests) = initialRegs(i)
  }
  Range(4 * numTests, 5 * numTests).foreach { i =>
    // load value
    reg_load(i - 4 * numTests) = R(i)
  }

  Range(0, numTests).foreach { i =>
    val_store(i) = initialRegs(i + 3 * numTests)
    addr_store(i) = (initialRegs(i).toLong << (2 * ManticoreBaseISA.DataBits)) +
      (initialRegs(i + numTests).toLong << ManticoreBaseISA.DataBits) +
      initialRegs(i + 2 * numTests).toLong
  }
  // const
  initialRegs(5 * numTests) = 0
  initialRegs(5 * numTests + 1) = 1
  val reg_const_0 = R(5 * numTests)
  val reg_const_1 = R(5 * numTests + 1)

  val program       = ArrayBuffer.empty[Instruction.Instruction]
  val expectedSends = ArrayBuffer.empty[Int]
  Range(0, numTests).foreach { i =>
    program += Predicate(reg_const_1)
    program += GlobalStore(reg_store(i), reg_hi(i), reg_mid(i), reg_lo(i))
  }
  Range(0, numTests).foreach { i =>
    program += GlobalLoad(reg_load(i), reg_hi(i), reg_mid(i), reg_lo(i))
  }
  Range(0, numTests).foreach { i =>
    // program += Instruction.Send(reg_load(i), reg_load(i), 1, 1)
    program += Instruction.Nop()
    mem += (addr_store(i) -> val_store(i))
    expectedSends += val_store(i)
  }

  val equations = Seq.fill(1 << ManticoreFullISA.FunctBits) {
    Seq.fill(ManticoreFullISA.DataBits) {
      BigInt(rdgen.nextInt(1 << 16))
    }
  }

  def makeProcessor(): ClockedProcessor =
    new ClockedProcessor(
      ManticoreFullISA,
      2,
      2,
      equations,
      UniProcessorTestUtils.createMemoryDataFiles(
        Seq.tabulate(1 << ManticoreFullISA.IdBits) { i =>
          initialRegs(i)
        }
      ) {
        Paths
          .get(
            "test_data_dir" + File.separator +
              getTestName + File.separator + "rf.data"
          )
          .toAbsolutePath
      },
      UniProcessorTestUtils.createMemoryDataFiles(
        Seq.fill(1 << ManticoreFullISA.IdBits)(0)
      ) {
        Paths
          .get(
            "test_data_dir" + File.separator +
              getTestName + File.separator + "ra.data"
          )
          .toAbsolutePath
      }
    )

  behavior of
    "Processor accessing global memory"

  it should "correctly handle register spilling to global" +
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
          val load  = state.load
          val mem   = state.mem
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
                  if (mem.getOrElse(new_req.address, 0) != new_req.value) {
                    fail(
                      s"Expected Mem[${new_req.address}] = " +
                        s"${mem.getOrElse(new_req.address, 0)} but got ${new_req.value}"
                    )
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
          10,
          10,
          5,
          dut.io.packet_in,
          dut.clock
        ) {
          true
        }
        simulate(10000)(TickState(None, None, mem))

      }

    }
}
