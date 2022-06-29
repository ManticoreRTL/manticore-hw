package manticore.machine.core

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import manticore.machine.ManticoreBaseISA
import manticore.machine.ManticoreFullISA
import manticore.machine.memory.CacheCommand
import manticore.machine.memory.CacheConfig

import scala.annotation.tailrec
import manticore.machine.ISA

/// registers written by the host
class HostRegisters(config: ISA) extends Bundle {
  val global_memory_instruction_base: UInt = UInt(64.W)
  val value_change_symbol_table_base: UInt = UInt(64.W)
  val value_change_log_base: UInt          = UInt(64.W)
  val schedule_config: UInt                = UInt(64.W)
  // |               schedule_config                  |
  // +------------------------------------------------+
  // |63            56|55                            0|
  // +----------------+-------------------------------+
  // |      CMD       |           CMD DATA            |
  // -----------------+--------------------------------

}

/// registers written by the device, i.e., the compute grid
class DeviceRegisters(config: ISA) extends Bundle {
  val virtual_cycles: UInt    = UInt(64.W)
  val bootloader_cycles: UInt = UInt(32.W) // for profiling
  // val exception_id: Vec[UInt] = Vec(4, UInt(config.DataBits.W))
  val exception_id: UInt     = UInt(32.W)
  val execution_cycles: UInt = UInt(64.W)
}

class ManticoreFlatArrayInterface extends Bundle {

  val host_registers   = Input(new HostRegisters(ManticoreFullISA))
  val device_registers = Output(new DeviceRegisters(ManticoreFullISA))

  val start: Bool = Input(Bool())
  val done: Bool  = Output(Bool())
  val idle: Bool  = Output(Bool())
  val memory_backend: MemoryReadWriteInterface = new MemoryReadWriteInterface(
    ManticoreFullISA
  )
  val clock_active  = Output(Bool())
  val compute_clock = Input(Clock())
  val control_clock = Input(Clock())
  val clock_stabled = Input(Bool())
  val reset         = Input(Reset())
}

object ClockDistribution {

  /** Give a frequency in MHz, compute the MMCM_ADV parameter
    *
    * @param freqMhz
    * @return
    */
  def frequencyToParam(freqMhz: Double): Double = {

    require(freqMhz >= 10.0, "Minimum frequency is 10MHz")
    require(freqMhz <= 775.0, "Maximum frequency is 775MHz")
    // we need to compute the CLKFBOUT_MULT_F (M) parameter of the MMCM_ADV (see
    // ug572, UltraScale Architecture Clocking resources) according to (Fin =
    // FreqMhz):
    // * Fvco = Fin * M / D
    // * Fout = Fin * M / (D * O)
    // where D is set to 1.0 and Fin = Fout and M = O is a multiple of 0.125 and also 2 <= M <= 128
    // therefore we have:
    // Fvco = Fin * M = Fin * 0.125 * n, where n is an integer 16 <= n <= 1024
    // based on "75237 - What are the allowed MMCM input frequencies for secondary clock CLKIN2?"
    // we know that 800Mhz <= Fvco <= 1600MHz

    @tailrec
    def findSolutions(
        n: Int = 16,
        sols: Seq[Int] = Seq.empty[Int]
    ): Seq[Int] = {
      // check if a solution exists
      val vco = n.toDouble * 0.125 * freqMhz
      if (n > 1024) {
        sols
      } else {
        val next_sols = if (vco <= 1600.0 && vco >= 800.0) {
          sols :+ n
        } else {
          sols
        }
        findSolutions(n + 1, next_sols)
      }
    }
    val sols = findSolutions()
    // println(sols.map { _.toDouble * 0.125 })
    val lookup = Map(
      200.0 -> 6.0,
      225.0 -> 5.0,
      250.0 -> 5.0,
      280.0 -> 4.0,
      300.0 -> 4.0,
      350.0 -> 3.0,
      400.0 -> 3.0,
      450.0 -> 3.0,
      500.0 -> 2.0
    )
    lookup.get(freqMhz) match {
      case None =>
        throw new UnsupportedOperationException(
          s"Invalid frequency ${freqMhz} MHz"
        )
      case Some(value) => value
    }
  }

}
class ClockDistribution extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val root_clock       = Input(Clock())
    val root_rst_n       = Input(Bool())  // rst_n coming from the root clock domain
    val compute_clock    = Output(Clock())
    val control_clock    = Output(Clock())
    val compute_clock_en = Input(Bool())
    val locked           = Output(Bool())
    val sync_rst_n       = Output(Bool()) // reset that can be used in the output clock domains
  })
  addResource("/verilog/ClockDistribution.v")
}

class KernelControl extends Module {
  val io = IO(new Bundle {

    val start              = Input(Bool())
    val done               = Output(Bool())
    val idle               = Output(Bool())
    val boot_start         = Output(Bool())
    val boot_finished      = Input(Bool())
    val device_registers   = Output(new DeviceRegisters(ManticoreFullISA))
    val kill_clock         = Input(Bool())
    val resume_clock       = Input(Bool())
    val exception_occurred = Input(Bool())
    val schedule_config    = Input(UInt(64.W))
    val exception_id       = Input(UInt(32.W))
    val config_enable      = Output(Bool())

    val clock_active     = Output(Bool())
    val execution_active = Input(Bool())
    val soft_reset       = Output(Bool())

    val clock_stabled = Input(Bool())
  })

  val clock_active = RegInit(Bool(), true.B)

  io.clock_active := clock_active

  val start_reg: Bool = RegInit(Bool(), false.B)
  start_reg := io.start
  val start_pulse: Bool = Wire(Bool())
  start_pulse := (!start_reg) & io.start

  val dev_regs = Reg(new DeviceRegisters(ManticoreFullISA))

  val command      = Wire(UInt(8.W))
  val command_data = Wire(UInt(56.W))

  command      := io.schedule_config.head(8)
  command_data := io.schedule_config.tail(8)

  object State extends ChiselEnum {
    val Idle, SoftReset, BootLoading, VirtualCycle, SignalDone = Value
  }

  val state = RegInit(State.Type(), State.Idle)

  val done_out = Reg(Bool())
  val done_int = Wire(Bool())
  val idle_out = Reg(Bool())
  val idle_int = Wire(Bool())

  done_out := done_int // register for timing closure because the axi_slave might be far on the chip
  idle_out := idle_int

  done_int := (state === State.SignalDone)
  idle_int := (State.Idle === state)

  val config_enable_reg = Reg(Bool())
  config_enable_reg := true.B
  io.config_enable  := config_enable_reg // registering this signal means that
  // the master processor can not receive messages in the first cycle when
  // it becomes active, but that is fine, because the pipeline depth is larger
  // than 1 by design.

  // to count the virtual cycles, we use the execution active signal
  // a 1 to 0 means a virtual cycles has finished and a 0 to 1 means
  // a virtual cycle has started, we can use either edges to count up
  val execution_active_old = Reg(Bool())
  execution_active_old := io.execution_active

  io.boot_start := false.B
  val ResetLatency     = 16
  val reset_counter    = Reg(UInt(8.W))
  val soft_rest_source = Wire(Bool())
  soft_rest_source := false.B

  // create a pipe of registers to allow re-timing of the reset signal
  io.soft_reset := Seq
    .fill(ResetLatency) { Reg(Bool()) }
    .foldLeft(soft_rest_source) { case (prev, next) =>
      next := prev
      next
    }

  switch(state) {
    is(State.Idle) {
      when(start_pulse && io.clock_stabled) {
        state            := State.SoftReset
        reset_counter    := 0.U
        soft_rest_source := true.B
        clock_active     := true.B

        dev_regs.bootloader_cycles := 0.U
        dev_regs.virtual_cycles    := 0.U
        dev_regs.execution_cycles  := 0.U
        execution_active_old       := false.B

      }
    }
    is(State.SoftReset) {
      reset_counter := reset_counter + 1.U
      clock_active  := true.B
      // since the reset takes many cycles, we have to keep the clock active.
      // Note that it is possible that while the reset is taking place some
      // programming is running on the master core and triggers an exception
      // which should normally kill the clock. To avoid this, we only let the
      // master core issue any clock kill request if the controller is in the
      // VirtualCycle state. This could easily happen when we are running
      // initialization programs with very few initialization instructions.
      // In other words, while we are trying to reset the manticore array to
      // start the actual program, the initializer runs again and tries to kill
      // the clock before reset reaches the processor and hence the processor
      // may not properly reset!
      when(reset_counter === ResetLatency.U) {
        state         := State.BootLoading
        io.boot_start := true.B
      }
    }
    is(State.BootLoading) {
      clock_active               := true.B
      dev_regs.bootloader_cycles := dev_regs.bootloader_cycles + 1.U
      when(io.boot_finished) {
        state := State.VirtualCycle
      }
    }
    is(State.VirtualCycle) {
      dev_regs.execution_cycles := dev_regs.execution_cycles + 1.U
      // a program could only kill the clock if the controller is in the virtual
      // cycle state.
      when(clock_active === true.B) {
        clock_active := !(io.kill_clock | io.exception_occurred)
      } otherwise {
        // when the clock is inactive, resume the clock on command
        clock_active := io.resume_clock
      }
      config_enable_reg := false.B
      when(clock_active) {
        // either the application has finished
        when(io.exception_occurred) {
          state                 := State.SignalDone
          dev_regs.exception_id := io.exception_id
        }.elsewhen(command === 1.U && dev_regs.virtual_cycles === command_data) {
          // or we have run out of time
          state                 := State.SignalDone
          dev_regs.exception_id := 0x10000.U // give some id that users can not
          // make (they are restricted to 16 bits, i.e., up to 0xFFFF)
          // NOTE: max time out  is 1 << 56 (more than enough)
        }
      }
      when(execution_active_old === false.B & io.execution_active === true.B) {
        dev_regs.virtual_cycles := dev_regs.virtual_cycles + 1.U
      }
    }

    is(State.SignalDone) {
      state := State.Idle
    }

  }

  io.device_registers := dev_regs
  io.idle             := idle_out
  io.done             := done_out

}

class LoadStoreIssue extends Module {
  val io = IO(
    new Bundle {
      val inbound      = CacheConfig.frontInterface()
      val outbound     = new MemoryReadWriteInterface(ManticoreFullISA)
      val kill_clock   = Output(Bool())
      val resume_clock = Output(Bool())
    }
  )

  val rdata_reg: UInt = Reg(UInt(ManticoreFullISA.DataBits.W))

  val addr_reg: UInt  = Reg(UInt(64.W))
  val wdata_reg: UInt = Reg(UInt(ManticoreFullISA.DataBits.W))
  val mem_done: Bool  = Reg(Bool())

  mem_done := io.outbound.done

  when(io.outbound.done) { rdata_reg := io.outbound.rdata }

  when(io.inbound.start) {
    addr_reg  := io.inbound.addr
    wdata_reg := io.inbound.wdata
  }

  io.outbound.wdata := wdata_reg
  io.outbound.addr  := addr_reg

  io.inbound.rdata := rdata_reg

  val wen_int       = Reg(Bool())
  val mem_start_int = Reg(Bool())
  val done_int      = Reg(Bool())
  val idle_int      = Reg(Bool())
  io.outbound.wen   := wen_int
  io.outbound.start := mem_start_int

  when(io.inbound.start) {

    when(io.inbound.cmd === CacheCommand.Read) {
      wen_int := false.B
    }.elsewhen(io.inbound.cmd === CacheCommand.Write) {
      wen_int := true.B
    }.elsewhen(io.inbound.cmd === CacheCommand.Flush) {
      printf(
        "Invalid memory operation! Only the the controller can issue a cache flush!"
      )
    }.elsewhen(io.inbound.cmd === CacheCommand.Reset) {
      printf(
        "Invalid memory operation! Only the controller can issue a cache reset!"
      )
    }
  }

  object State extends ChiselEnum {
    val Idle, StartMemoryRequest, WaitForMemoryResponse, ResumeClock, SignalDone =
      Value
  }
  val state = RegInit(State.Type(), State.Idle)

  io.inbound.done := done_int
  io.inbound.idle := idle_int
  io.kill_clock   := false.B
  mem_start_int   := false.B
  done_int        := false.B
  idle_int        := (State.Idle === state)
  io.resume_clock := false.B
  switch(state) {
    is(State.Idle) {
      idle_int := true.B
      when(io.inbound.start) {
        state         := State.StartMemoryRequest
        io.kill_clock := true.B
      }
    }
    is(State.StartMemoryRequest) {
      state         := State.WaitForMemoryResponse
      mem_start_int := true.B
    }
    is(State.WaitForMemoryResponse) {
      when(mem_done) {
        io.resume_clock := true.B
        state           := State.SignalDone
        done_int        := true.B
      }
    }
    is(State.SignalDone) {
      idle_int := true.B
      state    := State.Idle
    }
  }
}

class ComputeArray(
    dimx: Int,
    dimy: Int,
    debug_enable: Boolean = false,
    enable_custom_alu: Boolean = true,
    prefix_path: String = "./"
) extends Module {

  val io = IO(new Bundle {
    val mem_access         = Flipped(CacheConfig.frontInterface())
    val config_packet      = Input(new NoCBundle(dimx, dimy, ManticoreFullISA))
    val config_enable      = Input(Bool())
    val exception_id       = Output(UInt(32.W))
    val exception_occurred = Output(Bool())
    val dynamic_cycle      = Output(Bool())
    val execution_active   = Output(Bool())
  })

  val equations = Seq.fill(1 << ManticoreBaseISA.FunctBits) {
    Seq.tabulate(ManticoreBaseISA.DataBits) { i => BigInt(1) << i }
  }

  def makeConfigData[T](gen: (Int, Int) => T): Seq[Seq[T]] =
    Seq.tabulate(dimx) { x =>
      Seq.tabulate(dimy) { y =>
        gen(x, y)
      }
    }

  def hasMemory(x: Int, y: Int): Boolean = (x == 0) && (y == 0)
  case class FatCore(core: Processor, switch: Switch, x: Int, y: Int)

  val cores: Seq[Seq[FatCore]] = Seq.tabulate(dimx) { x =>
    Seq.tabulate(dimy) { y =>
      val core_conf =
        if (hasMemory(x, y)) ManticoreFullISA else ManticoreBaseISA

      val core = Module(
        new Processor(
          config = core_conf,
          DimX = dimx,
          DimY = dimy,
          equations = equations,
          initial_registers = s"${prefix_path}/rf_${x}_${y}.dat",
          initial_array = s"${prefix_path}/ra_${x}_${y}.dat",
          debug_enable = debug_enable,
          name_tag = s"CoreX${x}Y${y}",
          enable_custom_alu = enable_custom_alu
        )
      )
      core.suggestName(s"core_${x}_${y}")
      val switch = Module(
        new Switch(dimx, dimy, core_conf)
      )
      switch.suggestName(s"switch_${x}_${y}")
      FatCore(core, switch, x, y)
    }
  }

  // connect the cores via switches
  Range(0, dimx).foreach { x =>
    Range(0, dimy).foreach { y =>
      cores(x)(y).switch.io.xInput := {
        if (x == 0)
          cores(dimx - 1)(y).switch.io.xOutput
        else cores(x - 1)(y).switch.io.xOutput
      }
      cores(x)(y).switch.io.yInput := {
        if (y == 0)
          cores(x)(dimy - 1).switch.io.yOutput
        else
          cores(x)(y - 1).switch.io.yOutput
      }

      if (debug_enable) {
        val switch_watcher = Module(
          new SwitchPacketInspector(
            DimX = dimx,
            DimY = dimy,
            config = ManticoreBaseISA,
            pos = (x, y),
            fatal = true
          )
        )
        val debug_time = RegInit(UInt(64.W), 0.U)
        debug_time                               := debug_time + 1.U
        cores(x)(y).core.io.periphery.debug_time := debug_time

        switch_watcher.io.xInput := {
          if (x == 0) cores(dimx - 1)(y).switch.io.xOutput
          else cores(x - 1)(y).switch.io.xOutput
        }
        switch_watcher.io.yInput := {
          if (y == 0) cores(x)(dimy - 1).switch.io.yOutput
          else cores(x)(y - 1).switch.io.yOutput
        }
        switch_watcher.io.lInput := cores(x)(y).core.io.packet_out

      } else {
        cores(x)(y).core.io.periphery.debug_time := 0.U
      }

      // connect the switches to the cores
      cores(x)(y).core.io.packet_in       := cores(x)(y).switch.io.yOutput
      cores(x)(y).core.io.packet_in.valid := cores(x)(y).switch.io.terminal
      cores(x)(y).switch.io.lInput        := cores(x)(y).core.io.packet_out

      cores(x)(y).core.io.periphery.cache.done  := false.B
      cores(x)(y).core.io.periphery.cache.idle  := false.B
      cores(x)(y).core.io.periphery.cache.rdata := 0.U
    }

    val master_core = cores.flatten.filter(c => hasMemory(c.x, c.y)).head

    master_core.core.io.periphery.cache <> io.mem_access

    // connect the configuration packet to the master core switch
    when(io.config_enable) {
      master_core.switch.io.xInput := io.config_packet

    }

    io.exception_id       := master_core.core.io.periphery.exception.id
    io.exception_occurred := master_core.core.io.periphery.exception.error
    io.dynamic_cycle      := master_core.core.io.periphery.dynamic_cycle
    io.execution_active   := master_core.core.io.periphery.active
  }

}
class ManticoreFlatArray(
    dimx: Int,
    dimy: Int,
    debug_enable: Boolean = false,
    enable_custom_alu: Boolean = true,
    prefix_path: String = "./"
) extends RawModule {

  val io = IO(new ManticoreFlatArrayInterface)

  // val clock_distribution = Module(new ClockDistribution())

  // clock_distribution.io.root_clock := clock

  val controller =
    withClockAndReset(reset = io.reset, clock = io.control_clock) {
      Module(new KernelControl)
    }

  io.clock_active := controller.io.clock_active

  val bootloader =
    withClockAndReset(
      clock = io.control_clock,
      reset = controller.io.soft_reset
    ) {
      Module(
        new Programmer(ManticoreFullISA, dimx, dimy)
      )
    }

  controller.io.start := io.start
  io.done             := controller.io.done
  io.idle             := controller.io.idle
  io.device_registers := controller.io.device_registers

  controller.io.boot_finished := bootloader.io.running
  bootloader.io.start         := controller.io.boot_start
  bootloader.io.instruction_stream_base := io.host_registers.global_memory_instruction_base
    .pad(bootloader.io.instruction_stream_base.getWidth)
  bootloader.io.finish := false.B
  val memory_intercept =
    withClockAndReset(clock = io.control_clock, reset = io.reset) {
      Module(new LoadStoreIssue())
    }

  // controller.io.kill_clock   := memory_intercept.io.kill_clock
  controller.io.resume_clock := memory_intercept.io.resume_clock

  val debug_time =
    withClockAndReset(clock = io.control_clock, reset = io.reset) {
      RegInit(UInt(64.W), 0.U)
    }
  debug_time := debug_time + 1.U

  val compute_array =
    withClockAndReset(
      clock = io.compute_clock,
      reset = controller.io.soft_reset
    ) {
      Module(new ComputeArray(dimx, dimy, debug_enable, enable_custom_alu, prefix_path))
    }

  controller.io.kill_clock := compute_array.io.dynamic_cycle

  compute_array.io.config_enable := controller.io.config_enable
  compute_array.io.config_packet := bootloader.io.packet_out

  compute_array.io.mem_access <> memory_intercept.io.inbound
  io.memory_backend <> memory_intercept.io.outbound

  when(controller.io.config_enable) {
    io.memory_backend.wen              := bootloader.io.memory_backend.wen
    io.memory_backend.wdata            := bootloader.io.memory_backend.wdata
    io.memory_backend.addr             := bootloader.io.memory_backend.addr
    io.memory_backend.start            := bootloader.io.memory_backend.start
    bootloader.io.memory_backend.done  := io.memory_backend.done
    bootloader.io.memory_backend.idle  := io.memory_backend.idle
    bootloader.io.memory_backend.rdata := io.memory_backend.rdata

    memory_intercept.io.outbound.done  := false.B
    memory_intercept.io.outbound.idle  := false.B
    memory_intercept.io.outbound.rdata := DontCare

  } otherwise {
    io.memory_backend.wen              := memory_intercept.io.outbound.wen
    io.memory_backend.wdata            := memory_intercept.io.outbound.wdata
    io.memory_backend.addr             := memory_intercept.io.outbound.addr
    io.memory_backend.start            := memory_intercept.io.outbound.start
    memory_intercept.io.outbound.done  := io.memory_backend.done
    memory_intercept.io.outbound.idle  := io.memory_backend.idle
    memory_intercept.io.outbound.rdata := io.memory_backend.rdata

    bootloader.io.memory_backend.done  := false.B
    bootloader.io.memory_backend.idle  := false.B
    bootloader.io.memory_backend.rdata := DontCare
  }

  controller.io.exception_id       := compute_array.io.exception_id
  controller.io.exception_occurred := compute_array.io.exception_occurred
  controller.io.execution_active   := compute_array.io.execution_active
  controller.io.schedule_config    := io.host_registers.schedule_config
  controller.io.clock_stabled      := io.clock_stabled

}

object Gentest extends App {

  // // new ChiselStage()
  // //   .emitVerilog(new ManticoreFlatArray(2, 2), Array("-td", "gen-dir/flat"))
  // new ChiselStage().emitVerilog(
  //   new GatedPipeReg(new Bundle {
  //     val x = UInt(32.W)
  //     val y = UInt(32.W)
  //   }),
  //   Array("-td", "gen-dir/pipereg")
  // )
}
