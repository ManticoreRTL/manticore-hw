package manticore.core

import chisel3._
import manticore.ManticoreFullISA
import chisel3.experimental.ChiselEnum
import chisel3.util._
import manticore.ManticoreBaseISA
import memory.CacheConfig
import org.scalatest.selenium.WebBrowser
import memory.CacheCommand
import manticore.xrt.ManticoreKernel
import chisel3.stage.ChiselStage
import chisel3.internal.sourceinfo.SourceInfo

class ManticoreFlatArrayInterface extends Bundle {

  val host_registers   = Input(new HostRegisters(ManticoreFullISA))
  val device_registers = Output(new DeviceRegisters(ManticoreFullISA))

  val start: Bool = Input(Bool())
  val done: Bool  = Output(Bool())
  val idle: Bool  = Output(Bool())
  val memory_backend: MemoryReadWriteInterface = new MemoryReadWriteInterface(
    ManticoreFullISA
  )
  val clock_inactive = Output(Bool())
  val compute_clock  = Input(Clock())
  val control_clock  = Input(Clock())
  val reset          = Input(Reset())
}

class ClockDistribution extends BlackBox with HasBlackBoxResource {

  val io = IO(new Bundle {
    val root_clock         = Input(Clock())
    val compute_clock      = Output(Clock())
    val control_clock      = Output(Clock())
    val compute_clock_en_n = Input(Bool())
  })
  addResource("/verilog/ClockDistribution.v")
}
class KernelControl extends Module {
  val io = IO(new Bundle {

    val start             = Input(Bool())
    val done              = Output(Bool())
    val idle              = Output(Bool())
    val boot_start        = Output(Bool())
    val boot_finished     = Input(Bool())
    val device_registers  = Output(new DeviceRegisters(ManticoreFullISA))
    val kill_clock        = Input(Bool())
    val resume_clock      = Input(Bool())
    val exception_occured = Input(Bool())
    val exception_id      = Input(UInt(32.W))
    val config_enable     = Output(Bool())
    val clock_inactive    = Output(Bool())
  })

  val clock_inactive = RegInit(Bool(), false.B)
  when(clock_inactive === false.B) {
    clock_inactive := io.kill_clock | io.exception_occured
  } otherwise {
    // when the clock is inactive, resume the clock on command
    clock_inactive := !io.resume_clock
  }

  io.clock_inactive := clock_inactive

  val start_reg: Bool = RegInit(Bool(), false.B)
  start_reg := io.start
  val start_pulse: Bool = Wire(Bool())
  start_pulse := (!start_reg) & io.start

  val dev_regs = Reg(new DeviceRegisters(ManticoreFullISA))

  object State extends ChiselEnum {
    val Idle, BootLoading, VirtualCycle, SignalDone = Value
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

  io.boot_start := false.B

  switch(state) {
    is(State.Idle) {
      when(start_pulse) {
        state                      := State.BootLoading
        io.boot_start              := true.B
        dev_regs.bootloader_cycles := 0.U
        dev_regs.virtual_cycles    := 0.U
      }
    }
    is(State.BootLoading) {
      dev_regs.bootloader_cycles := dev_regs.bootloader_cycles + 1.U
      when(io.boot_finished) {
        state := State.VirtualCycle
      }
    }
    is(State.VirtualCycle) {
      config_enable_reg := false.B
      when(!clock_inactive) {
        when(io.exception_occured) {
          state                   := State.SignalDone
          dev_regs.exception_id_0 := io.exception_id
        }
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
    val Idle, StartMemoryRequest, WaitForMemoryResponse, ResumeClock,
        SignalDone =
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

class ComputeArray(dimx: Int, dimy: Int, debug_enable: Boolean = false)
    extends Module {

  val io = IO(new Bundle {
    val mem_access        = Flipped(CacheConfig.frontInterface())
    val config_packet     = Input(new NoCBundle(dimx, dimy, ManticoreFullISA))
    val config_enable     = Input(Bool())
    val exception_id      = Output(UInt(32.W))
    val exception_occured = Output(Bool())
    val dynamic_cycle     = Output(Bool())
  })
  val equations = Seq.fill(1 << ManticoreFullISA.FunctBits) {
    Seq.tabulate(ManticoreFullISA.DataBits) { i => (1 << i) }
  }

  def makeConfigData[T](gen: (Int, Int) => T): Seq[Seq[T]] =
    Seq.tabulate(dimx) { x =>
      Seq.tabulate(dimy) { y =>
        gen(x, y)
      }
    }
  val initial_state = new ComputeGrid.InitialState(
    lut_configs = makeConfigData((x, y) => equations),
    regfile_files = makeConfigData((x, y) => s"rf_${x}_${y}.dat"),
    regarray_files = makeConfigData((x, y) => s"ra_${x}_${y}.dat")
  )

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
          initial_registers = s"rf_${x}_${y}.dat",
          initial_array = s"ra_${x}_${y}.dat",
          debug_enable = debug_enable,
          debug_tag = s"CoreX${x}Y${y}"
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

      cores(x)(y).core.io.periphery.debug_time := 0.U

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

    io.exception_id      := master_core.core.io.periphery.exception.id
    io.exception_occured := master_core.core.io.periphery.exception.error
    io.dynamic_cycle     := master_core.core.io.periphery.dynamic_cycle
  }

}
class ManticoreFlatArray(dimx: Int, dimy: Int, debug_enable: Boolean = false)
    extends RawModule {

  val io = IO(new ManticoreFlatArrayInterface)

  // val clock_distribution = Module(new ClockDistribution())

  // clock_distribution.io.root_clock := clock

  val controller =
    withClockAndReset(reset = io.reset, clock = io.control_clock) {
      Module(new KernelControl)
    }

  io.clock_inactive := controller.io.clock_inactive

  val bootloader =
    withClockAndReset(clock = io.control_clock, reset = io.reset) {
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
    withClockAndReset(clock = io.compute_clock, reset = io.reset) {
      Module(new ComputeArray(dimx, dimy, debug_enable))
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
    memory_intercept.io.outbound.done  := io.memory_backend.done
    memory_intercept.io.outbound.idle  := io.memory_backend.idle
    memory_intercept.io.outbound.rdata := io.memory_backend.rdata

    bootloader.io.memory_backend.done  := false.B
    bootloader.io.memory_backend.idle  := false.B
    bootloader.io.memory_backend.rdata := DontCare
  }

  controller.io.exception_id      := compute_array.io.exception_id
  controller.io.exception_occured := compute_array.io.exception_occured

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
