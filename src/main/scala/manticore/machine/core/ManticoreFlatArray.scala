package manticore.machine.core

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.stage.ChiselStage
import chisel3.util._
import manticore.machine.ISA
import manticore.machine.ManticoreBaseISA
import manticore.machine.ManticoreFullISA
import manticore.machine.memory.CacheCommand
import manticore.machine.memory.CacheConfig

import scala.annotation.tailrec

/// registers written by the host
class HostRegisters extends Bundle {
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
class DeviceRegisters extends Bundle {
  val virtual_cycles: UInt    = UInt(64.W)
  val bootloader_cycles: UInt = UInt(32.W) // for profiling
  // val exception_id: Vec[UInt] = Vec(4, UInt(config.DataBits.W))
  val exception_id: UInt     = UInt(32.W)
  val execution_cycles: UInt = UInt(64.W)
}

class ManticoreFlatArrayInterface extends Bundle {

  val host_registers   = Input(new HostRegisters)
  val device_registers = Output(new DeviceRegisters)

  val start: Bool    = Input(Bool())
  val done: Bool     = Output(Bool())
  val idle: Bool     = Output(Bool())
  val memory_backend = Flipped(CacheConfig.frontInterface())
  val clock_active   = Output(Bool())
  val compute_clock  = Input(Clock())
  val control_clock  = Input(Clock())
  val clock_stabled  = Input(Bool())
  val reset          = Input(Bool())
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
    val device_registers   = Output(new DeviceRegisters)
    val kill_clock         = Input(Bool())
    val resume_clock       = Input(Bool())
    val exception_occurred = Input(Bool())
    val schedule_config    = Input(UInt(64.W))
    val exception_id       = Input(UInt(32.W))
    val config_enable      = Output(Bool())
    val cache_reset_start  = Output(Bool())
    val cache_reset_done   = Input(Bool())
    val clock_active       = Output(Bool())
    val execution_active   = Input(Bool())
    val soft_reset         = Output(Bool())

    val clock_stabled = Input(Bool())
  })

  val clock_active = RegInit(Bool(), true.B)

  io.clock_active := clock_active

  val start_reg: Bool = RegInit(Bool(), false.B)
  start_reg := io.start
  val start_pulse: Bool = Wire(Bool())
  start_pulse := (!start_reg) & io.start

  val dev_regs = Reg(new DeviceRegisters)

  val command      = Wire(UInt(8.W))
  val command_data = Wire(UInt(56.W))

  command      := io.schedule_config.head(8)
  command_data := io.schedule_config.tail(8)

  object State extends ChiselEnum {
    val Idle, SoftReset, CacheReset, BootLoading, VirtualCycle, SignalDone = Value
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

  io.cache_reset_start := false.B
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
      // program is running on the master core and triggers an exception which
      // should normally kill the clock. To avoid this, we only let the master
      // core issue any clock kill request if the controller is in the
      // VirtualCycle state. This could easily happen when we are running
      // initialization programs with very few initialization instructions. In
      // other words, while we are trying to reset the manticore array to start
      // the actual program, the initializer runs again and tries to kill the
      // clock before reset reaches the processor and hence the processor may
      // not properly reset!
      when(reset_counter === ResetLatency.U) {
        state := State.CacheReset

      }
    }
    is(State.CacheReset) {
      io.cache_reset_start := true.B
      clock_active         := true.B // keep the clock active
      when(io.cache_reset_done) {
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
    prefix_path: String = "./",
    n_hop: Int = 2
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
        new Switch(dimx, dimy, core_conf, n_hop)
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
    prefix_path: String = "./",
    n_hop: Int = 2
) extends RawModule {

  val io = IO(new ManticoreFlatArrayInterface)

  val controller =
    withClockAndReset(reset = io.reset, clock = io.control_clock) {
      Module(new Management)
    }

  val memory_intercept = withClockAndReset(clock = io.control_clock, reset = io.reset) {
    Module(new MemoryIntercept)
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

  controller.io.core_revive_clock := memory_intercept.io.core_revive_clock

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
      Module(
        new ComputeArray(
          dimx = dimx,
          dimy = dimy,
          debug_enable = debug_enable,
          enable_custom_alu = enable_custom_alu,
          prefix_path = prefix_path,
          n_hop = n_hop
        )
      )
    }

  controller.io.core_kill_clock := compute_array.io.dynamic_cycle
  controller.io.cache_done      := io.memory_backend.done

  memory_intercept.io.cache_flush := controller.io.cache_flush_start
  memory_intercept.io.cache_reset := controller.io.cache_reset_start

  compute_array.io.config_enable := controller.io.config_enable
  compute_array.io.config_packet := bootloader.io.packet_out

  compute_array.io.mem_access <> memory_intercept.io.core
  bootloader.io.memory_backend <> memory_intercept.io.boot

  io.memory_backend <> memory_intercept.io.cache

  memory_intercept.io.core_clock := io.compute_clock

  memory_intercept.io.config_enable := controller.io.config_enable

  controller.io.exception_id            := compute_array.io.exception_id
  controller.io.core_exception_occurred := compute_array.io.exception_occurred
  controller.io.execution_active        := compute_array.io.execution_active
  controller.io.schedule_config         := io.host_registers.schedule_config
  controller.io.clock_locked            := io.clock_stabled

}


