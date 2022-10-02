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

import manticore.machine.Helpers

/// registers written by the host
class HostRegisters extends Bundle {
  val schedule_config: UInt                = UInt(64.W)
  val global_memory_instruction_base: UInt = UInt(64.W)
  val trace_dump_base: UInt                = UInt(64.W)
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
  val exception_id: UInt      = UInt(32.W)
  val execution_cycles: UInt  = UInt(64.W)
  val trace_dump_head: UInt   = UInt(64.W)
  var device_info: UInt       = UInt(32.W)
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

class ComputeArray(
    dimx: Int,
    dimy: Int,
    debug_enable: Boolean = false,
    enable_custom_alu: Boolean = true,
    prefix_path: String = ".",
    n_hop: Int = 1
) extends Module {

  val io = IO(new Bundle {
    val mem_access         = Flipped(CacheConfig.frontInterface())
    val config_packet      = Input(new NoCBundle(dimx, dimy, ManticoreFullISA))
    val config_enable      = Input(Bool())
    val exception_id       = Output(UInt(32.W))
    val exception_occurred = Output(Bool())
    val dynamic_cycle      = Output(Bool())
    val execution_active   = Output(Bool())
    // val core_reset_done    = Output(Bool())
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
  case class FatCore(core: ProcessorWithSendRecvPipe, switch: Switch, x: Int, y: Int)

  // Allow placement flexibility between cores and switches by using a dedicated reset tree for each.
  val core_reset_tree   = Module(new CoreResetTree(dimx, dimy))
  val switch_reset_tree = Module(new SwitchResetTree(dimx, dimy))

  // io.core_reset_done := core_reset_tree.io.last
  // val reset_tree = Module(new SoftResetTree(dimx, dimy))
  // io.core_reset_done := reset_tree.io.last

  val cores: Seq[Seq[FatCore]] = Seq.tabulate(dimx) { x =>
    Seq.tabulate(dimy) { y =>
      val core_conf = if (hasMemory(x, y)) ManticoreFullISA else ManticoreBaseISA

      val core = withReset(core_reset_tree.io.taps(x)(y)) {
        Module(
          new ProcessorWithSendRecvPipe(
            config = core_conf,
            DimX = dimx,
            DimY = dimy,
            x = x,
            y = y,
            equations = equations,
            initial_registers = s"${prefix_path}/rf_${x}_${y}.dat",
            initial_array = s"${prefix_path}/ra_${x}_${y}.dat",
            debug_enable = debug_enable,
            name_tag = s"CoreX${x}Y${y}",
            enable_custom_alu = enable_custom_alu
          )
        )
      }
      core.suggestName(s"core_${x}_${y}")

      val switch = withReset(switch_reset_tree.io.taps(x)(y)) {
        Module(
          new Switch(dimx, dimy, core_conf, n_hop)
        )
      }
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
    prefix_path: String = ".",
    n_hop: Int = 1
) extends RawModule {

  val io = IO(new ManticoreFlatArrayInterface)

  val controller = withClockAndReset(
    reset = io.reset,
    clock = io.control_clock
  ) {
    Module(new Management(dimx, dimy))
  }

  val memory_intercept = withClockAndReset(
    clock = io.control_clock,
    reset = io.reset
  ) {
    Module(new MemoryIntercept)
  }

  io.clock_active := controller.io.clock_active

  val bootloader = withClockAndReset(
    clock = io.control_clock,
    reset = controller.io.soft_reset
  ) {
    Module(
      new Programmer(ManticoreFullISA, dimx, dimy)
    )
  }
  controller.io.compute_clock := io.compute_clock
  controller.io.start         := io.start
  io.done                     := controller.io.done
  io.idle                     := controller.io.idle
  io.device_registers         := controller.io.device_registers

  controller.io.boot_finished := bootloader.io.running
  bootloader.io.start         := controller.io.boot_start
  bootloader.io.instruction_stream_base := io.host_registers.global_memory_instruction_base
    .pad(bootloader.io.instruction_stream_base.getWidth)
  bootloader.io.finish := false.B

  controller.io.core_revive_clock := memory_intercept.io.core_revive_clock

  val debug_time = withClockAndReset(
    clock = io.control_clock,
    reset = io.reset
  ) {
    RegInit(UInt(64.W), 0.U)
  }
  debug_time := debug_time + 1.U

  val compute_array = withClockAndReset(
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

  // controller.io.soft_reset_done := compute_array.io.core_reset_done

  memory_intercept.io.cache_flush := controller.io.cache_flush_start
  memory_intercept.io.cache_reset := controller.io.cache_reset_start

  withClockAndReset(
    clock = io.control_clock,
    reset = controller.io.soft_reset
  ) {
    compute_array.io.config_enable := controller.io.config_enable
    compute_array.io.config_packet := bootloader.io.packet_out
  }

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
