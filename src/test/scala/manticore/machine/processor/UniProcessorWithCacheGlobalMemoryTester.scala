package manticore.machine.processor

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
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
import manticore.machine.core.Processor
import manticore.machine.core.ProcessorInterface
import manticore.machine.memory.CacheCommand
import manticore.machine.processor.UniProcessorTestUtils.ClockedProcessor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import manticore.machine.core.BareNoCBundle
import java.io.File
import java.nio.file.Paths
import scala.annotation.tailrec
import manticore.machine.xrt.CacheKernelWithSlave




class UniProcessorWithCacheGlobalMemoryTester extends AnyFlatSpec with Matchers with ChiselScalatestTester {

  val rdgen = new scala.util.Random(0)

  val base_addr = rdgen.nextLong() % (1 << (3 * ManticoreFullISA.IdBits * 3))
//   val addr_lo_init = rdgen.nextInt((1 << ManticoreFullISA.DataBits) - 1024)
//   val addr_mid_init = rdgen.nextInt(1 << ManticoreFullISA.DataBits)
//   val addr_hi_init = rdgen.nextInt(1 << ManticoreFullISA.DataBits)

  val addr_lo_init = 10
  val addr_mid_init = 0
  val addr_hi_init = 0

  val const_0_init = 0
  val const_1_init = 1
  val addr_lo = R(2)
  val addr_mi = R(0)
  val addr_hi = R(0)
  val const_1 = R(1)
  val const_0 = R(0)
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
    Nop(),
    Nop(),
    Nop(),
    Add2(val_reg, const_0, const_0),
    Nop(),
    Nop(),
    Nop()
  )
  val zeroFunct = Seq.fill(ManticoreFullISA.DataBits)(BigInt(0))
  val zeroFuncts = Seq.fill(ManticoreFullISA.numFuncts)(zeroFunct)

  val assembledInstructions = program.map(instr => Assembler.assemble(instr)(zeroFuncts))
  print(assembledInstructions)
  val equations = Seq.fill(1 << ManticoreFullISA.FunctBits) {
    Seq.fill(ManticoreFullISA.DataBits) {
      BigInt(rdgen.nextInt(1 << 16))
    }
  }

  val config = ISA

  UniProcessorTestUtils.createMemoryDataFiles(
    Seq.tabulate(1 << ManticoreFullISA.IdBits) {
      i =>
        if (i == val_reg.index) {
          2
        } else if (i == const_0.index) {
          const_0_init
        } else if (i == const_1.index) {
          const_1_init
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
      "CacheKernelWithSlave" + File.separator + "rf.data").toAbsolutePath
  }

  behavior of "CacheKernelWithSlave"

  it should "Execute simple instruction" in {
    test {new CacheKernelWithSlave(config= ManticoreFullISA)

    }.withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)){
        dut =>
     def streamInstructionsTemp(instructions: Seq[Long], dut: Processor)(stream_cond: => Boolean): Unit = {
    streamInstructions(instructions, dut.io.packet_in, dut.clock)(stream_cond)
  }
  def streamInstructions(instructions: Seq[Long], packet_stream: BareNoCBundle, clock: Clock)
                        (stream_cond: => Boolean): Unit = {
    val packets = instructions.flatMap { inst =>
      Seq(
        inst & 0x0000FFFF,
        (inst >> 16) & 0x0000FFFF,
        (inst >> 32) & 0x0000FFFF,
        (inst >> 48) & 0x0000FFFF
      ).map { d =>
        new BareNoCBundle(ManticoreFullISA).Lit(
          _.data -> d.U,
          _.address -> 0.U,
          _.valid -> true.B
        )
      }
    }

    val empty_packet = new BareNoCBundle(ManticoreFullISA).Lit(
      _.data -> 0.U,
      _.address -> 0.U,
      _.valid -> false.B
    )

    @tailrec
    def streamLoop(left: Seq[BareNoCBundle]): Unit = {
      if (left.nonEmpty) {
        if (stream_cond) {
          packet_stream.poke(left.head)
          clock.step()
          streamLoop(left.tail)
        } else {
          packet_stream.poke(empty_packet)
          clock.step()
          streamLoop(left)
        }
      } else {
        packet_stream.poke(empty_packet)
      }
    }

    streamLoop(packets)
  }
    def programProcessorWithCacheTemp(instructions: Seq[Long], epilogue_length: Int, sleep_length: Int,
                       countdown: Int, dut: CacheKernelWithSlave)(stream_cond: => Boolean): Unit = {
    programProcessorWithCache(instructions, epilogue_length, sleep_length, countdown, dut.io.packet_in, dut.clock)(stream_cond)
  }



  def programProcessorWithCache(instructions: Seq[Long], epilogue_length: Int, sleep_length: Int,
                       countdown: Int, packet_stream: BareNoCBundle, clock: Clock)(stream_cond: => Boolean): Unit = {
    val empty_packet = new BareNoCBundle(ManticoreFullISA).Lit(
      _.data -> 0.U,
      _.address -> 0.U,
      _.valid -> false.B
    )

    @tailrec
    def sendPacketLoop(value: Int): Unit = {
      if (stream_cond) {
        packet_stream.poke(new BareNoCBundle(ManticoreFullISA).Lit(
          _.data -> value.U,
          _.address -> 0.U,
          _.valid -> true.B
        ))
        clock.step()
        packet_stream.poke(empty_packet)
      } else {
        packet_stream.poke(empty_packet)
        clock.step()
        sendPacketLoop(value)
      }
    }

    sendPacketLoop(instructions.length)

    streamInstructions(instructions, packet_stream, clock)(stream_cond)

    sendPacketLoop(epilogue_length)

    sendPacketLoop(sleep_length)

    sendPacketLoop(countdown)

  }


      val sleep_length = 0
      val epilogue_length = 12
      val countdown = 0
    programProcessorWithCacheTemp(
        assembledInstructions.toIndexedSeq, epilogue_length, sleep_length, countdown, dut
      ){
        rdgen.nextInt(10) == 0
      }
      for (i<-0 until(1000)){
      dut.clock.step()
      }
    }

  }


}