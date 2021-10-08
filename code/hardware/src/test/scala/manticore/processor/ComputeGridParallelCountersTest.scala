package manticore.processor

import Chisel._
import chisel3.VecInit
import chisel3.experimental.ChiselEnum
import chisel3.tester.ChiselScalatestTester
import chisel3.tester.experimental.sanitizeFileName
import manticore.ManticoreFullISA
import manticore.core.{
  ComputeGrid,
  ComputeGridOrchestrator,
  DeviceRegisters,
  HostRegisters
}
import memory.{CacheBackInterface, CacheBackendCommand, CacheConfig}
import org.scalatest.{FlatSpec, Matchers, stats}

import java.io.File
import java.nio.file.Paths
import manticore.assembly.Instruction

object ComputeGridParallelCountersTest {

  class MemoryError extends Bundle {
    val segfault: Bool      = Bool()
    val none_aligned: Bool  = Bool()
    val bad_writeback: Bool = Bool()
  }

  class MemoryModel(val rom_values: Seq[Int], val memory_size: Int)
      extends Module {

    val io = IO(new Bundle {
      val memory: CacheBackInterface = Flipped(CacheConfig.backInterface())
      val errors: MemoryError        = Output(new MemoryError)
    })

    private val rdgen =
      new scala.util.Random(0) // constant seed make the results reproducible
    val NumDifferentDelays = 20

    def createDelays = VecInit(
      Seq.fill(NumDifferentDelays) {
        (5 max rdgen.nextInt(20)).U
      }
    )

    val write_delays = createDelays
    val read_delays  = createDelays

    val RomBitWidth   = 64
    val DataBits      = CacheConfig.DataBits
    val CacheLineBits = CacheConfig.CacheLineBits

    val rom = VecInit(
      rom_values.map(_.U(RomBitWidth.W))
    )

    val RomAddressEnd = scala.math
      .ceil((RomBitWidth * rom.size) / CacheLineBits)
      .toInt
      .U(CacheConfig.UsedAddressBits.W)

    val mem           = Mem(memory_size, UInt(DataBits.W))
    val raddr_reg     = Reg(UInt(CacheConfig.UsedAddressBits.W))
    val waddr_reg     = Reg(UInt(CacheConfig.UsedAddressBits.W))
    val aligned_raddr = Wire(Bool())
    aligned_raddr := io.memory.raddr(
      log2Ceil(CacheLineBits / DataBits) - 1,
      0
    ) === 0.U
    val aligned_waddr = Wire(Bool())
    aligned_waddr := io.memory.waddr(
      log2Ceil(CacheLineBits / DataBits) - 1,
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
              mem(waddr_reg + offset.U) := wline_reg(
                (offset + 1) * DataBits - 1,
                offset * DataBits
              )
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
                mem(waddr_reg + offset.U) := wline_reg(
                  (offset + 1) * DataBits - 1,
                  offset * DataBits
                )
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

class ComputeGridParallelTester
    extends FlatSpec
    with ChiselScalatestTester
    with Matchers {

  val rdgen = new scala.util.Random(1)

  val equations = Seq.fill(1 << ManticoreFullISA.FunctBits) {
    Seq.fill(ManticoreFullISA.DataBits) {
      rdgen.nextInt(1 << 16)
    }
  }
  val DimX = 2
  val DimY = 3

  class ComputeGridParallelCountersWithMemories(val rom_values: Seq[Seq[Int]], ra: String, rf: String)
      extends Module {

    import ComputeGridParallelCountersTest._

    val io = IO(new Bundle {

      val mem_errors: Vec[MemoryError] = Output(Vec(4, new MemoryError))
      val host_registers   = Input(new HostRegisters(ManticoreFullISA))
      val device_registers = Output(new DeviceRegisters(ManticoreFullISA))

      val start: Bool = Input(Bool())
      val idle: Bool  = Output(Bool())

    })

    
    import ComputeGrid.InitialState

    val compute_grid = Module(
      new ComputeGrid(
        DimX,
        DimY,
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
      )
    )

    val controller = Module(
      new ComputeGridOrchestrator(
        DimX,
        DimY,
        ManticoreFullISA
      )
    )

    require(rom_values.size == 4)
    // 4 banks, each 8 MiB
    val dram_bank = rom_values.map { init =>
      Module(
        new MemoryModel(
          init,
          1 << 12
        )
      )
    }

    compute_grid.io.cores.zip(controller.io.periphery_core).foreach {
      case (core, control) =>
        core <> control
    }

    controller.io.registers.from_host := io.host_registers
    io.device_registers               := controller.io.registers.to_host

    controller.io.start := io.start
    io.idle             := controller.io.idle

    dram_bank.zip(controller.io.cache_backend).foreach { case (dram, cache) =>
      cache <> dram.io.memory
    }

    io.mem_errors := dram_bank.map(_.io.errors)

  }

  def shiftRegisterProcedure(
      size: Int,
      shift_in: Instruction.Register,
      shift_en: Instruction.Register,
      base: Instruction.Register,
      temps: Seq[Instruction.Register],
      const_1: Instruction.Register,
      const_0: Instruction.Register
  ): Seq[Instruction.Instruction] = {

    import manticore.assembly.Instruction._

    require(temps.size >= size)

    Seq(
      SetEqual(shift_en, shift_en, const_1)
    ) ++ // configure the mux, has side-effects
      temps.slice(0, size).zipWithIndex.reverse.map { case (out, i) =>
        LocalLoad(out, base, i) // load all the old values
      } ++
      Seq(
        SetEqual(shift_en, shift_en, R(1))
      ) ++
      (shift_in +: temps.slice(0, size - 1)).zip(temps.slice(0, size)).map {
        case (new_val, old_val) =>
          Mux2(old_val, new_val, old_val) // shift in the new values
      } ++ temps.slice(0, size).zipWithIndex.map { case (v, i) =>
        LocalStore(v, base, i) // store them back to the array memory
      }

  }

  def shifRegisterFillerProcess(
      i: Int,
      rows: Int,
      cols: Int,
      counter: Instruction.Register,
      from_mem: Instruction.Register,
      consts: Map[Int, Instruction.Register],
      free_regs: Seq[Instruction.Register],
      dest_proc: (Int, Int),
      dest_reg: Instruction.Register,
      num_nops: Int
  ): Seq[Instruction.Instruction] = {

    var next_id = 0
    def freshReg() = {
      next_id = next_id + 1
      free_regs(next_id - 1)
    }

    // enable shifting: (count >= i * cols && count < (i + 1) * cols) | count >= (row * cols)
    // shift in value: (count >= i * cols && count < (i + 1) * cols) ? from_mem : 0
    // val counter  = freshReg() // read-only
    // val from_mem = freshReg() // read-only
  

    val shift_in = freshReg() // read-write
    val shift_en = freshReg() // boolean read-write

    // shift_en condition
    val c0 = freshReg() // count < (i + 1) * cols
    val c1 = freshReg() // count < 2 * rows * cols
    val c2 = freshReg() // rows * cols - 1 < count
    // val shift_en = c0 | (c1 & c2)
    val bram_base = consts(0)
    val tail      = freshReg()

    import Instruction._
    Seq(
      SetLessThanUnsigned(c0, counter, consts(i * cols)),
      SetLessThanUnsigned(c1, counter, consts(2 * rows * cols)),
      SetLessThanUnsigned(c2, consts(rows * cols - 1), counter),
      LocalLoad(tail, bram_base, (cols - 1)),
      Nop(),
      And2(c2, c2, c1),
      Nop(),
      Nop(),
      Or2(shift_en, c0, c2),
      Nop(),
      Nop()
    ) ++ Seq.fill(num_nops){
      Nop()
    } ++ Seq(
      Send(dest_reg, tail, dest_proc._1, dest_proc._2)
    ) ++ shiftRegisterProcedure(
      size = cols,
      shift_in = shift_in,
      shift_en = shift_en,
      base = bram_base,
      temps = Seq.fill(rows + cols - 1)(freshReg()),
      const_1 = consts(1),
      const_0 = consts(0)
    )

  }

  def matrixLoaderProcess(
      rows: Int,
      cols: Int,
      addr: Instruction.Register,
      consts: Map[Int, Instruction.Register],
      free_regs: Seq[Instruction.Register],
      counter_receivers: Seq[(Instruction.Register, Int, Int)],
      word_receivers: Seq[(Instruction.Register, Int, Int)],
      state_receiver: (Instruction.Register, Int, Int)
  ) = {

    import manticore.assembly.Instruction._

    var next_id = 0
    def freshReg() = {
      next_id = next_id + 1
      free_regs(next_id - 1)
    }

    val counter = freshReg()
    val counter_next = freshReg()
    val state = freshReg()
   
    val addr_next = freshReg()
    val data = freshReg()
    val c0 = freshReg()
    val c1 = freshReg()
    val c2 = freshReg()
    val c3 = freshReg()
    val idle = freshReg()
    val t0 = freshReg()
    val t1 = freshReg()
    val t2 = freshReg()
    val t3 = freshReg()

    val word = freshReg()

    Seq(

      Add2(addr_next, addr, consts(1)),
      Add2(counter_next, counter, consts(1)),
      SetEqual(c0, state, consts(0)),
      Mux2(t0, consts(1), state),
      SetEqual(c0, state, consts(1)),
      Mux2(addr_next, addr_next, consts(0)),
      SetEqual(c1, counter, consts(rows * cols - 1)),
      SetEqual(c2, state, consts(2)),
      SetEqual(c3, counter, consts(2 * rows * cols - 1)),
      SetEqual(idle, state, consts(4)),
      Mux2(counter_next, consts(0), counter_next),
      And2(c0, c1, c0),
      GlobalLoad(word, addr, consts(0), consts(0)),
      And2(c2, c2, c3),
      SetEqual(c0, c0, consts(1)),
      Mux2(t1, consts(2), t0),
      SetEqual(c2, c2, consts(1)),
      Mux2(t2, consts(3), t1), // t2 is the next state
      Add2(counter, counter_next, consts(0)), 
      Add2(state, t2, consts(0))
    ) ++ counter_receivers.map { case (rd, x, y) => 
      Send(rd, counter_next, x, y)  
    } ++ word_receivers.map { case (rd, x, y) =>
      Send(rd, word, x, y)
    } :+ Send(
      state_receiver._1, state, state_receiver._2, state_receiver._2
    )



  }


  def matrixWriterAndCheckerProcess(
      rows: Int,
      cols: Int,
      gold_addr: Instruction.Register,
      write_addr: Instruction.Register,
      consts: Map[Int, Instruction.Register],
      free_regs: Seq[Instruction.Register],
      inbound_word: Seq[Instruction.Register],
      inbound_state: Instruction.Register
  ) = {

    var next_id = 0
    def freshReg() = {
      next_id = next_id + 1
      free_regs(next_id - 1)
    }

    require(inbound_word.size == rows)
    
    import manticore.assembly.Instruction._

    val expected_words = inbound_word.map(_ => freshReg())
    val enable_expect = freshReg()
    val next_gold_addr = freshReg()

    expected_words.zipWithIndex.map { case (lw, i) => 
      LocalLoad(lw, gold_addr, i)  
    } ++ Seq(
      Add2(next_gold_addr, gold_addr, consts(rows)),
      SetEqual(enable_expect, inbound_state, consts(2)),
    ) ++ expected_words.zip(inbound_word).map { case (expected, got) =>
      Mux2(expected, expected, got)  
    } ++ expected_words.zip(inbound_word.zipWithIndex).map { case (expected, (got, i)) =>
      Expect(got, expected, i)  
    } ++ Seq(
      Mux2(gold_addr, next_gold_addr, gold_addr),
      Predicate(enable_expect)
    ) ++ inbound_word.zipWithIndex.map { case (w, i) =>
      GlobalStore(w, write_addr, consts(0), consts(0))  
    }



  }

  def makeProgram() = {
    import manticore.assembly.Instruction.R

    val rows = 2 // only 2 is valid
    val cols = 4

    val const_range = 1024
    val consts = Seq.tabulate(const_range)(i => R(i)).zipWithIndex.map { case (r, i) =>
      (i, r)
    }.toMap

    val shared_regs = Map(
      "load_addr_base" -> R(const_range + 1),
      "counter" -> R(const_range + 2),
      "from_mem" -> R(const_range + 3),
      "state" -> R(const_range + 4),
      "store_addr_base" -> R(const_range + 5),
      "gold_addr_base" -> R(const_range + 6)
    ) ++ Seq.tabulate(rows){ i => 
      ("word_" + i, R(const_range + 7 + i))
    }.toMap
    

    val free_regs = Seq.tabulate(2048 - 512){ i => R(i) }

    val rf = UniProcessorTestUtils.createMemoryDataFiles {
      Seq.tabulate(2048) { i =>
        if (i < const_range) {
          i
        } else if (i == shared_regs("load_addr_base").index) {
          0 // base address of the input matrix
        } else if (i == shared_regs("store_addr_base").index) {
          0 // base address of the output
        } else if (i == shared_regs("gold_addr_base").index) {
          4000
        } else {
          0
        }
      }
    } {
      Paths
        .get(
          "test_data_dir" + File.separator +
            sanitizeFileName(
              scalaTestContext.value.get.name
            ) + File.separator + "rf.data"
        )
        .toAbsolutePath
    }
    val ra = UniProcessorTestUtils.createMemoryDataFiles {
      Seq.fill(2048)(0)
    } {
      Paths
        .get(
          "test_data_dir" + File.separator +
            sanitizeFileName(
              scalaTestContext.value.get.name
            ) + File.separator + "ra.data"
        )
        .toAbsolutePath
    }

   
    
    val row_0 = shifRegisterFillerProcess(
      0, rows, cols, shared_regs("counter"), shared_regs("from_mem"),
      consts, free_regs, (1, 1), shared_regs("word_0"), 0
    ) 
    val row_1 = shifRegisterFillerProcess(
      1, rows, cols, shared_regs("counter"), shared_regs("from_mem"),
      consts, free_regs, (1, 1), shared_regs("word_1"), 1
    )


  }
  def mainProgram = {
    import manticore.assembly.Instruction._

   

  }
  behavior of "Parallel Counters "
  it should "correctly increment counters" in {}

}
