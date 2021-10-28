package manticore.processor

import Chisel._
import chisel3.VecInit
import chisel3.experimental.ChiselEnum

import chisel3.tester.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
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
import manticore.assembly.Assembler

import chisel3.tester.{ChiselScalatestTester, testableClock, testableData}
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import chisel3.tester.experimental.sanitizeFileName

import chisel3.util.HasBlackBoxResource
import _root_.chisel3.Data
import memory.SimpleDualPortMemory
import scala.annotation.tailrec

class ComputeGridShifterTester
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
  val DimY = 2

  class ComputeGridWithSimulatedMemory(
      val rom_values: Seq[String],
      rf: String,
      ra: String
  ) extends Module {

    import UniProcessorTestUtils.MemoryModel
    import UniProcessorTestUtils.MemoryError

    val io = IO(new Bundle {

      val mem_errors: Vec[MemoryError] = Output(Vec(4, new MemoryError))
      val host_registers   = Input(new HostRegisters(ManticoreFullISA))
      val device_registers = Output(new DeviceRegisters(ManticoreFullISA))
      val core_active      = Output(Vec(4, Bool()))
      val start: Bool      = Input(Bool())
      val idle: Bool       = Output(Bool())

    })

    import ComputeGrid.InitialState

    val initial_state = new InitialState(
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

    val compute_grid = Module(
      new ComputeGrid(
        DimX = DimX,
        DimY = DimY,
        power_on_state = initial_state,
        debug_enable = true
      )
    )

    val controller = Module(
      new ComputeGridOrchestrator(
        DimX,
        DimY,
        ManticoreFullISA,
        true
      )
    )

    require(rom_values.size == 4)
    // 4 banks, each 8 MiB
    val dram_bank = rom_values.map { init =>
      Module(
        new MemoryModel(
          init,
          1 << 15
        )
      )
    }

    compute_grid.io.cores.zip(controller.io.periphery_core).foreach {
      case (core, control) =>
        control <> core
    }

    controller.io.registers.from_host := io.host_registers
    io.device_registers               := controller.io.registers.to_host

    controller.io.start := io.start
    io.idle             := controller.io.idle

    dram_bank.zip(controller.io.cache_backend).foreach { case (dram, cache) =>
      dram.io.memory <> cache
    }

    io.mem_errors := dram_bank.map(_.io.errors)

    io.core_active := compute_grid.io.cores.map(_.active)

    compute_grid.io.external_packet        := controller.io.packet_out
    compute_grid.io.external_packet_enable := controller.io.packet_out.valid
    compute_grid.io.clock_enable_n         := controller.io.clock_enable_n
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
      Predicate(const_1),
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
          Mux2(old_val, old_val, new_val) // shift in the new values
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
      SetLessThanUnsigned(c0, counter, consts((i + 1) * cols + 1)),
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
    ) ++ Seq.fill(num_nops) {
      Nop()
    } ++ Seq(
      Send(dest_reg, tail, dest_proc._1, dest_proc._2)
    ) ++ shiftRegisterProcedure(
      size = cols,
      shift_in = from_mem,
      shift_en = shift_en,
      base = bram_base,
      temps = Seq.fill(rows + cols - 1)(freshReg()),
      const_1 = consts(1),
      const_0 = consts(0)
    ) ++ Seq.fill(20)(Nop()) // to receive the from_mem and counter value

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

    val counter      = freshReg()
    val counter_next = freshReg()
    val state        = freshReg()

    val addr_next = freshReg()
    val data      = freshReg()
    val c0        = freshReg()
    val c1        = freshReg()
    val c2        = freshReg()
    val c3        = freshReg()
    val t0        = freshReg()
    val t1        = freshReg()
    val t2        = freshReg()
    val t3        = freshReg()
    val count_en  = freshReg()
    val word      = freshReg()

    Seq(
      Add2(addr_next, addr, consts(1)),
      Add2(counter_next, counter, consts(1)),
      SetEqual(c0, state, consts(0)),
      Mux2(t0, state, consts(1)),
      SetEqual(c0, state, consts(1)),
      Mux2(addr_next, addr, addr_next),
      SetEqual(c2, state, consts(2)),
      SetEqual(c1, counter, consts(rows * cols - 1)),
      SetEqual(c3, counter, consts(2 * rows * cols - 1)),
      Or2(count_en, c0, c2),
      Nop(),
      Nop(),
      SetEqual(count_en, count_en, consts(1)),
      Mux2(counter_next, consts(0), counter_next),
      And2(c0, c1, c0),
      GlobalLoad(word, addr, consts(0), consts(0)),
      // And2(c2, c2, c3),
      // Nop(),
      SetEqual(c0, c0, consts(1)),
      Mux2(t1, t0, consts(2)),
      SetEqual(c2, c2, consts(1)),
      Nop(),
      Mux2(t2, t1, consts(3)),
      Nop(),
      SetEqual(c0, state, consts(3)),
      Mux2(t3, t2, consts(4)),
      SetEqual(c0, state, consts(4)),
      Nop(),
      Nop(),
      And2(c0, c0, c3),
      Nop(),
      Nop(),
      SetEqual(c0, c0, consts(1)),
      Mux2(t3, t3, consts(5)), // t3 is the next state
      Add2(counter, counter_next, consts(0)),
      Add2(addr, addr_next, consts(0)),
      Add2(state, t3, consts(0))
    ) ++ counter_receivers.map { case (rd, x, y) =>
      Send(rd, counter, x, y)
    } ++ word_receivers.map { case (rd, x, y) =>
      Send(rd, word, x, y)
    } :+ Send(
      state_receiver._1,
      state,
      state_receiver._2,
      state_receiver._2
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
    val enable_expect  = freshReg()
    val next_gold_addr = freshReg()

    Seq(
      Add2(next_gold_addr, consts(0), gold_addr),
      Nop(),
      Nop()
    ) ++
      expected_words.zipWithIndex.flatMap { case (lw, i) =>
        Seq(
          GlobalLoad(lw, next_gold_addr, consts(0), consts(0)),
          Add2(next_gold_addr, next_gold_addr, consts(1)),
          Nop(),
          Nop()
        )
      } ++ Seq(
        SetEqual(enable_expect, inbound_state, consts(5))
      ) ++ expected_words.zip(inbound_word).map { case (expected, got) =>
        Mux2(expected, got, expected)
      } ++ Seq(
        Nop(),
        Nop()
      ) ++ expected_words
        .zip(inbound_word.zipWithIndex)
        .map { case (expected, (got, i)) =>
          Expect(got, expected, i)
        } ++ Seq(
        Mux2(gold_addr, gold_addr, next_gold_addr),
        Predicate(enable_expect)
      ) ++ inbound_word.zipWithIndex.flatMap { case (w, i) =>
        Seq(
          GlobalStore(w, write_addr, consts(0), consts(0)),
          Add2(write_addr, write_addr, consts(1)),
          Nop(),
          Nop()
        )
      } ++ Seq.fill(20)(Nop())

  }

  case class TheProgram(
      binary: Seq[Int],
      schedule_length: Int,
      sleep_length: Map[(Int, Int), Int],
      epilogue_length: Map[(Int, Int), Int],
      process: Map[(Int, Int), Seq[Instruction.Instruction]],
      shared_regs: Map[String, Instruction.Register],
      rf_path: String,
      ra_path: String,
      rows: Int,
      cols: Int
  )
  def makeProgram(
      memory_base_addr: Int = 4096,
      max_matrix_size: Int = 4096
  ) = {
    import manticore.assembly.Instruction.R
    import manticore.assembly.Instruction.Instruction
    val rows = 2 // only 2 is valid
    val cols = 4

    val const_range = 1024
    val consts = Seq
      .tabulate(const_range)(i => R(i))
      .zipWithIndex
      .map { case (r, i) =>
        (i, r)
      }
      .toMap

    val shared_regs = Map(
      "load_addr_base"  -> R(const_range + 1),
      "counter"         -> R(const_range + 2),
      "from_mem"        -> R(const_range + 3),
      "state"           -> R(const_range + 4),
      "store_addr_base" -> R(const_range + 5),
      "gold_addr_base"  -> R(const_range + 6)
    ) ++ Seq
      .tabulate(rows) { i =>
        ("word_" + i, R(const_range + 7 + i))
      }
      .toMap

    val free_regs = Seq.tabulate(512) { i => R(2047 - i) }

    shared_regs.map { case (k, v) =>
      println(s"${k} -> R(${v.index})")
    }
    val rf = UniProcessorTestUtils.createMemoryDataFiles {
      Seq.tabulate(2048) { i =>
        if (i < const_range) {
          i
        } else if (i == shared_regs("load_addr_base").index) {
          memory_base_addr // base address of the input matrix
        } else if (i == shared_regs("store_addr_base").index) {
          max_matrix_size // base address of the output
        } else if (i == shared_regs("gold_addr_base").index) {
          0
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
      0,
      rows,
      cols,
      shared_regs("counter"),
      shared_regs("from_mem"),
      consts,
      free_regs,
      (0, 1),
      shared_regs("word_0"),
      0
    ) // placed at (1, 0)

    val row_1 = shifRegisterFillerProcess(
      1,
      rows,
      cols,
      shared_regs("counter"),
      shared_regs("from_mem"),
      consts,
      free_regs,
      (1, 0),
      shared_regs("word_1"),
      1
    ) // placed at (0, 1)

    val matrix_loader = matrixLoaderProcess(
      rows,
      cols,
      shared_regs("load_addr_base"),
      consts,
      free_regs,
      Seq(
        (shared_regs("counter"), 1, 0),
        (shared_regs("counter"), 0, 1)
      ),
      Seq(
        (shared_regs("from_mem"), 1, 0),
        (shared_regs("from_mem"), 0, 1)
      ),
      (shared_regs("state"), 1, 1)
    ) // placed at  (0, 0)

    val checker = matrixWriterAndCheckerProcess(
      rows,
      cols,
      shared_regs("gold_addr_base"),
      shared_regs("store_addr_base"),
      consts,
      free_regs,
      Seq(
        shared_regs("word_0"),
        shared_regs("word_1")
      ),
      shared_regs("state")
    )

    val process = Map(
      (0, 0) -> matrix_loader,
      (1, 0) -> row_0,
      (0, 1) -> row_1,
      (1, 1) -> checker
    )
    val epilogue_length = Map( // number of expected receives
      (0, 0) -> 0, // does not receive anything
      (1, 0) -> 2, // recieves the from_mem and the counter
      (0, 1) -> 2, // recieves from_mem and the counter
      (1, 1) -> 3 // recieves word_0, word_1, and state
    )

    val body_length = process.mapValues(_.length)
    val schedule_length = process.map { case ((x, y), p) =>
      epilogue_length((x, y)) + p.length
    }.max + 4 // 4 is the pipeline depth

    val sleep_length = process.map { case ((x, y), p) =>
      (x, y) -> (schedule_length - (p.length + epilogue_length((x, y))))
    }.toMap

    require((sleep_length.forall { case ((_, _), v) => v >= 4 }))

    def assemble(
        instructions: Seq[Instruction],
        x: Int,
        y: Int
    ): Seq[Long] = {
      ((y.toLong << 8) | x.toLong) +:
        instructions.map(inst => Assembler.assemble(inst)(equations))
    }

    val binary: Seq[Int] = process.keys.toSeq.sorted.flatMap { case (x, y) =>
      Seq(
        ((y << 8) | x),
        body_length((x, y))
      ) ++ process((x, y)).flatMap { inst =>
        val bin = Assembler.assemble(inst)(equations)
        Seq(
          bin & 0x0000ffff,
          (bin >> 16) & 0x0000ffff,
          (bin >> 32) & 0x0000ffff,
          (bin >> 48) & 0x0000ffff
        ).map(_.toInt)
      } ++ Seq(
        epilogue_length((x, y)),
        sleep_length((x, y))
      )
    }

    require(binary.size < memory_base_addr)

    TheProgram(
      binary,
      schedule_length,
      sleep_length,
      epilogue_length,
      process,
      shared_regs,
      rf,
      ra,
      rows,
      cols
    )

  }

  behavior of "ComputeGrid "
  it should "correctly shift entries read from external memory" in {

    val the_program = makeProgram(4096, 4096)

    println(s"Schedule length: ${the_program.schedule_length}")
    println(s"Binary length: ${the_program.binary.length}")

    val input_matrix =
      Seq.tabulate(the_program.rows * the_program.cols)(i => i + 100)
    println(input_matrix)
    val output_matrix =
      input_matrix.grouped(the_program.cols).toSeq.transpose.flatten
    println(output_matrix.map(_.toString()).reduce { (x: String, y: String) =>
      x + ", " + y
    })
    val main_memory_initial_values =
      the_program.binary ++
        Seq.fill(4096 - the_program.binary.length)(0) ++ // zero fill
        input_matrix

    val rom_values = Seq(
      main_memory_initial_values, // program binary and the input matrix
      Seq.fill(4096)(0),
      Seq.fill(4096)(0),
      output_matrix // gold values
    ).zipWithIndex.map { case (data, i) =>
      UniProcessorTestUtils.createMemoryDataFiles(data) {
        Paths
          .get(
            "test_data_dir" + File.separator +
              sanitizeFileName(
                scalaTestContext.value.get.name
              ) + File.separator + "dram_" + i + ".data"
          )
          .toAbsolutePath
      }
    }

    test(
      new ComputeGridWithSimulatedMemory(
        rom_values,
        the_program.rf_path,
        the_program.ra_path
      )
    ).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) {
      dut =>
        dut.io.host_registers.global_memory_instruction_base_low.poke(0.U)
        dut.io.host_registers.global_memory_instruction_base_high.poke(0.U)
        dut.io.host_registers.schedule_length
          .poke(the_program.schedule_length.U)
        dut.clock.step()
        dut.io.start.poke(true.B)

        dut.clock.step()

        dut.io.start.poke(false.B)

        @tailrec
        def waitForActivation(): Unit = {
          if (
            dut.io.core_active
              .map(active => active.peek().litToBoolean)
              .exists(_ == false)
          ) {
            dut.clock.step()
            waitForActivation()
          }
        }

        dut.clock.setTimeout(5000)
        waitForActivation()
        println("Execution has started")
        dut.clock.step(300)

    }

  }

}
