package manticore.processor


import Chisel._
import chisel3.VecInit
import chisel3.experimental.ChiselEnum
import chisel3.tester.ChiselScalatestTester
import chisel3.tester.experimental.sanitizeFileName
import manticore.ManticoreFullISA
import manticore.core.{ComputeGrid, ComputeGridOrchestrator, DeviceRegisters, HostRegisters}
import memory.{CacheBackInterface, CacheBackendCommand, CacheConfig}
import org.scalatest.{FlatSpec, Matchers, stats}

import java.io.File
import java.nio.file.Paths

object ComputeGridParallelCountersTest {


  class MemoryError extends Bundle {
    val segfault: Bool = Bool()
    val none_aligned: Bool = Bool()
    val bad_writeback: Bool = Bool()
  }

  class MemoryModel(val rom_values: Seq[Int], val memory_size: Int) extends Module {


    val io = IO(new Bundle {
      val memory: CacheBackInterface = Flipped(CacheConfig.backInterface())
      val errors: MemoryError = Output(new MemoryError)
    })

    private val rdgen = new scala.util.Random(0) // constant seed make the results reproducible
    val NumDifferentDelays = 20

    def createDelays = VecInit(
      Seq.fill(NumDifferentDelays) {
        (5 max rdgen.nextInt(20)).U
      }
    )

    val write_delays = createDelays
    val read_delays = createDelays

    val RomBitWidth = 64
    val DataBits = CacheConfig.DataBits
    val CacheLineBits = CacheConfig.CacheLineBits

    val rom = VecInit(
      rom_values.map(_.U(RomBitWidth.W))
    )

    val RomAddressEnd = scala.math.ceil((RomBitWidth * rom.size) / CacheLineBits).toInt.U(CacheConfig.UsedAddressBits.W)

    val mem = Mem(memory_size, UInt(DataBits.W))
    val raddr_reg = Reg(UInt(CacheConfig.UsedAddressBits.W))
    val waddr_reg = Reg(UInt(CacheConfig.UsedAddressBits.W))
    val aligned_raddr = Wire(Bool())
    aligned_raddr := io.memory.raddr(log2Ceil(CacheLineBits / DataBits) - 1, 0) === 0.U
    val aligned_waddr = Wire(Bool())
    aligned_waddr := io.memory.waddr(log2Ceil(CacheLineBits / DataBits) - 1, 0) === 0.U

    val wline_reg = Reg(UInt(CacheLineBits.W))


    object State extends ChiselEnum {
      val Idle, ServingRead, ServingWrite, ServingWriteBack, Done, SegFault, BadAlign, BadWriteBack = Value
    }

    val state = RegInit(State.Type(), State.Idle)
    val delay_counter = Reg(UInt())


    io.memory.done := false.B

    io.errors.segfault := false.B
    io.errors.none_aligned := false.B
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
              state := State.ServingWrite
              wline_reg := io.memory.wline
            } otherwise {
              state := State.BadAlign
            }
            delay_counter := write_delays(io.memory.waddr)
          } otherwise {
            when(aligned_raddr && aligned_waddr) {
              state := State.ServingWriteBack
              wline_reg := io.memory.wline
            } otherwise {
              state := State.BadAlign
            }
            delay_counter := write_delays(io.memory.waddr) + read_delays(io.memory.raddr)
          }
        }
      }
      is(State.ServingRead) {

        delay_counter := delay_counter - 1.U
        when(delay_counter === 1.U) {

          when(raddr_reg > RomAddressEnd) {
            io.memory.rline := Cat(
              Range(0, CacheLineBits / DataBits).reverse.map { offset =>
                mem(raddr_reg + offset.U)
              }
            )
          } otherwise {
            io.memory.rline := Cat(
              Range(0, CacheLineBits / RomBitWidth).reverse.map { offset =>
                mem(raddr_reg + offset.U)
              }
            )
          }

          state := State.Done

        }

      }
      is(State.ServingWrite) {
        delay_counter := delay_counter - 1.U
        when(delay_counter === 1.U) {
          when(waddr_reg > RomAddressEnd) {
            Range(0, CacheLineBits / DataBits).foreach { offset =>
              mem(waddr_reg + offset.U) := wline_reg((offset + 1) * DataBits - 1, offset * DataBits)
            }
            state := State.Done
          } otherwise {
            state := State.SegFault
          }
        }
      }

      is(State.ServingWriteBack) {
        delay_counter := delay_counter - 1.U

        when(delay_counter === 1.U) {
          when(raddr_reg === waddr_reg) {
            state := State.BadWriteBack
          } otherwise {

            // handle write
            when(waddr_reg > RomAddressEnd) {
              Range(0, CacheLineBits / DataBits).foreach { offset =>
                mem(waddr_reg + offset.U) := wline_reg((offset + 1) * DataBits - 1, offset * DataBits)
              }
              state := State.Done
            } otherwise {
              state := State.SegFault
            }

            // handle read
            when(raddr_reg > RomAddressEnd) {
              io.memory.rline := Cat(
                Range(0, CacheLineBits / DataBits).reverse.map { offset =>
                  mem(raddr_reg + offset.U)
                }
              )
            } otherwise {
              io.memory.rline := Cat(
                Range(0, CacheLineBits / RomBitWidth).reverse.map { offset =>
                  mem(raddr_reg + offset.U)
                }
              )
            }

            state := State.Done
          }
        }
      }
      is(State.Done) {
        io.memory.done := true.B
        state := State.Idle
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

class ComputeGridParallelTester extends FlatSpec with ChiselScalatestTester with Matchers {


  val rdgen = new scala.util.Random(1)

  val equations = Seq.fill(1 << ManticoreFullISA.FunctBits) {
    Seq.fill(ManticoreFullISA.DataBits) {
      rdgen.nextInt(1 << 16)
    }
  }
  val DimX = 2
  val DimY = 3

  class ComputeGridParallelCountersWithMemories(val rom_values: Seq[Int]) extends Module {

    import ComputeGridParallelCountersTest._


    val io = IO(new Bundle {

      val mem_errors: Vec[MemoryError] = Output(Vec(4, new MemoryError))
      val host_registers = Input(new HostRegisters(ManticoreFullISA))
      val device_registers = Output(new DeviceRegisters(ManticoreFullISA))

      val start: Bool = Input(Bool())
      val idle: Bool = Output(Bool())

    })


    val rf = UniProcessorTestUtils.createMemoryDataFiles {
      Seq.tabulate(2048) { i =>
        if (i >= 500)
          0
        else
          i
      }
    } {
      Paths.get("test_data_dir" + File.separator +
        sanitizeFileName(scalaTestContext.value.get.name) + File.separator + "rf.data").toAbsolutePath
    }
    val ra = UniProcessorTestUtils.createMemoryDataFiles {
      Seq.fill(2048)(0)
    } {
      Paths.get("test_data_dir" + File.separator +
        sanitizeFileName(scalaTestContext.value.get.name) + File.separator + "ra.data").toAbsolutePath
    }

    import ComputeGrid.InitialState

    val compute_grid = Module(new ComputeGrid(DimX, DimY,
      new InitialState(
        lut_configs = Seq.fill(DimX) {
          Seq.fill(DimY) {
            equations
          }
        },
        regfile_files = Seq.fill(DimX) {
          Seq.fill(DimY) {
            rf
          }
        },
        regarray_files = Seq.fill(DimX) {
          Seq.fill(DimY) {
            ra
          }
        }
      )
    ))

    val controller = Module(new ComputeGridOrchestrator(
      DimX, DimY, ManticoreFullISA
    ))

    // 4 banks, each 8 MiB
    val dram_bank = Seq.fill(4){
      Module(new MemoryModel(
        rom_values, 1 << 12
      ))
    }

    compute_grid.io.cores.zip(controller.io.periphery_core).foreach { case (core, control) =>
      core <> control
    }

    controller.io.registers.from_host := io.host_registers
    io.device_registers := controller.io.registers.to_host

    controller.io.start := io.start
    io.idle := controller.io.idle


    dram_bank.zip(controller.io.cache_backend).foreach { case (dram, cache) =>
      cache <> dram.io.memory
    }

    io.mem_errors := dram_bank.map(_.io.errors)

  }



  behavior of "Parallel Counters "
  it should "correctly increment counters" in {




  }

}

