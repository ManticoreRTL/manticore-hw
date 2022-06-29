package manticore.machine.processor


import chisel3.VecInit
import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.ChiselEnum
import chisel3.util.Cat
import chisel3.util.is
import chisel3.util.switch
import chiseltest._
import manticore.machine.ISA
import manticore.machine.ManticoreBaseISA
import manticore.machine.ManticoreFullISA
import manticore.machine.core.BareNoCBundle
import manticore.machine.core.Processor
import manticore.machine.core.ProcessorInterface
import manticore.machine.memory.CacheBackInterface
import manticore.machine.memory.CacheBackendCommand
import manticore.machine.memory.CacheCommand
import manticore.machine.memory.CacheConfig
import manticore.machine.memory.SimpleDualPortMemory

import java.io.PrintWriter
import java.nio.file.Files
import java.nio.file.Path
import scala.annotation.tailrec
import manticore.machine.core.ClockDistribution

object UniProcessorTestUtils {

  def createMemoryDataFiles(data: Seq[Int])(filepath: Path): String = {
    Files.createDirectories(filepath.getParent)
    val fw = new PrintWriter(filepath.toFile)
    data.foreach { v =>
      fw.println(s"%016d".format(v.toBinaryString.toLong))
    }
    fw.close()
    filepath.toString
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
        new BareNoCBundle(ManticoreBaseISA).Lit(
          _.data -> d.U,
          _.address -> 0.U,
          _.valid -> true.B
        )
      }
    }

    val empty_packet = new BareNoCBundle(ManticoreBaseISA).Lit(
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

  def streamInstructions(instructions: Seq[Long], dut: Processor)(stream_cond: => Boolean): Unit = {
    streamInstructions(instructions, dut.io.packet_in, dut.clock)(stream_cond)
  }


  /**
   * Program the uniprocessor given some streaming condition (e.g., with some probability)
   *
   * @param instructions the sequence of assembed instructions
   * @param epilogue_length
   * @param sleep_length
   * @param countdown
   * @param dut          the processor
   * @param stream_cond  a of nothing to Boolean that defines whether packet should be enqueued to the processor
   */
  def programProcessor(instructions: Seq[Long], epilogue_length: Int, sleep_length: Int,
                       countdown: Int, dut: Processor)(stream_cond: => Boolean): Unit = {
    programProcessor(instructions, epilogue_length, sleep_length, countdown, dut.io.packet_in, dut.clock)(stream_cond)
  }

  def programProcessor(instructions: Seq[Long], epilogue_length: Int, sleep_length: Int,
                       countdown: Int, packet_stream: BareNoCBundle, clock: Clock)(stream_cond: => Boolean): Unit = {
    val empty_packet = new BareNoCBundle(ManticoreBaseISA).Lit(
      _.data -> 0.U,
      _.address -> 0.U,
      _.valid -> false.B
    )

    @tailrec
    def sendPacketLoop(value: Int): Unit = {
      if (stream_cond) {
        packet_stream.poke(new BareNoCBundle(ManticoreBaseISA).Lit(
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


  // wrap the processor to have a clock enable
  class ClockedProcessor(config: ISA,
                         DimX: Int,
                         DimY: Int,
                         equations: Seq[Seq[BigInt]],
                         initial_registers: String = "",
                         initial_array: String = "") extends Module {
    val io = IO(new ProcessorInterface(config, DimX, DimY) {
      val clock_enable_n: Bool = Input(Bool())
      val rwb: Bool = Output(Bool())
      //  val clock: Clock = IO(Input(Clock()))

    })

    val gated_clock: Clock = Wire(Clock())
    val clock_buffer = Module(new ClockDistribution())
    clock_buffer.io.root_clock := clock
    clock_buffer.io.compute_clock_en := !io.clock_enable_n
    gated_clock := clock_buffer.io.compute_clock

    withClockAndReset(clock = gated_clock, reset = reset) {
      val impl: Processor = Module(new Processor(config, DimX, DimY,
        equations, initial_registers, initial_array))
      io <> impl.io
      io.rwb := io.periphery.cache.cmd === CacheCommand.Read

    }

  }

  class MemoryError extends Bundle {
    val segfault: Bool      = Bool()
    val none_aligned: Bool  = Bool()
    val bad_writeback: Bool = Bool()
  }

  class MemoryModel(val rom_values: String, val memory_size: Int)
      extends Module {


    val io = IO(new Bundle {
      val memory: CacheBackInterface = Flipped(CacheConfig.backInterface())
      val errors: MemoryError        = Output(new MemoryError)
    })

    private val rdgen =
      new scala.util.Random(0) // constant seed make the results reproducible
    val NumDifferentDelays = 20

    val Config = ManticoreFullISA

    def createDelays = VecInit(
      Seq.fill(NumDifferentDelays) {
        (5 max rdgen.nextInt(20)).U
      }
    )

    val write_delays = createDelays
    val read_delays  = createDelays

    val DataBits      = CacheConfig.DataBits
    val CacheLineBits = CacheConfig.CacheLineBits

    val storage = Range(0, CacheLineBits / DataBits).map { _ =>
      Module(
        new SimpleDualPortMemory(
          ADDRESS_WIDTH = util.log2Ceil(memory_size),
          READ_LATENCY = 2,
          DATA_WIDTH = DataBits,
          INIT = rom_values
        )
      )
    }

    val raddr_reg     = Reg(UInt(CacheConfig.UsedAddressBits.W))
    val waddr_reg     = Reg(UInt(CacheConfig.UsedAddressBits.W))
    val aligned_raddr = Wire(Bool())
    aligned_raddr := io.memory.raddr(
      util.log2Ceil(CacheLineBits / DataBits) - 1,
      0
    ) === 0.U
    val aligned_waddr = Wire(Bool())
    aligned_waddr := io.memory.waddr(
      util.log2Ceil(CacheLineBits / DataBits) - 1,
      0
    ) === 0.U

    val wline_reg = Reg(UInt(CacheLineBits.W))

    object State extends ChiselEnum {
      val Idle, ServingRead, ServingWrite, ServingWriteBack, Done, SegFault,
          BadAlign, BadWriteBack = Value
    }

    val state         = RegInit(State.Type(), State.Idle)
    val delay_counter = Reg(UInt())

    io.memory.done := false.B

    storage.zipWithIndex.foreach { case (bank, i) =>
      bank.io.wen   := false.B
      bank.io.raddr := raddr_reg + i.U
      bank.io.waddr := waddr_reg + i.U
      bank.io.din   := wline_reg((i + 1) * DataBits - 1, i * DataBits)
    }

    io.errors.segfault      := false.B
    io.errors.none_aligned  := false.B
    io.errors.bad_writeback := false.B

    switch(state) {
      is(State.Idle) {
        when(io.memory.start) {
          raddr_reg := io.memory.raddr
          waddr_reg := io.memory.waddr
          when(io.memory.cmd === CacheBackendCommand.Read.id.U) {
            when(aligned_raddr) {
              state := State.ServingRead
            } otherwise {
              state := State.BadAlign
            }
            delay_counter := read_delays(io.memory.raddr)
          }.elsewhen(io.memory.cmd === CacheBackendCommand.Write.id.U) {
            when(aligned_waddr) {
              state     := State.ServingWrite
              wline_reg := io.memory.wline
            } otherwise {
              state := State.BadAlign
            }
            delay_counter := write_delays(io.memory.waddr)
          } otherwise {
            when(aligned_raddr && aligned_waddr) {
              state     := State.ServingWriteBack
              wline_reg := io.memory.wline
            } otherwise {
              state := State.BadAlign
            }
            delay_counter := write_delays(io.memory.waddr) + read_delays(
              io.memory.raddr
            )
          }
        }
      }
      is(State.ServingRead) {

        delay_counter := delay_counter - 1.U
        when(delay_counter === 1.U) {
          io.memory.rline := Cat(
            storage.map(_.io.dout).reverse
          ) // little-endian
          state := State.Done

        }

      }
      is(State.ServingWrite) {
        delay_counter := delay_counter - 1.U
        when(delay_counter === 1.U) {
          storage.foreach { _.io.wen := true.B }

          state := State.Done
        }
      }

      is(State.ServingWriteBack) {
        delay_counter := delay_counter - 1.U

        when(delay_counter === 1.U) {
          when(raddr_reg === waddr_reg) {
            state := State.BadWriteBack
          } otherwise {
            storage.foreach { _.io.wen := true.B }
            io.memory.rline := Cat(
              storage.map(_.io.dout).reverse
            ) // little-endian
            state := State.Done

            state := State.Done

          }
        }
      }

      is(State.Done) {
        io.memory.done := true.B
        state          := State.Idle
      }

      is(State.BadAlign) {
        io.errors.none_aligned := true.B
      }
      is(State.SegFault) {
        io.errors.segfault := true.B
      }
      is(State.BadWriteBack) {
        io.errors.bad_writeback := true.B
      }

    }
  }
}
