package manticore.core

import Chisel._
import chisel3.experimental.ChiselEnum
import chisel3.stage.ChiselStage
import memory.{Cache, CacheBackInterface, CacheConfig, CacheFrontInterface}
import manticore.{ISA, ManticoreFullISA}

/// registers written by the host
class HostRegisters(config: ISA) extends Bundle {
  val global_memory_instruction_base: UInt = UInt(64.W)
  val value_change_symbol_table_base: UInt = UInt(64.W)
  val value_change_log_base: UInt          = UInt(64.W)
  val schedule_length: UInt                = UInt((config.NumPcBits + 1).W)
  val clear_exception: Bool                = Bool()
}

/// registers written by the device, i.e., the compute grid
class DeviceRegisters(config: ISA) extends Bundle {
  val virtual_cycles: UInt = UInt(
    48.W
  ) // notice we can simulate up to 2^48 - 1 cycles
  val bootloader_cycles: UInt = UInt(32.W) // for profiling
  val exception_id: Vec[UInt] = Vec(4, UInt(config.DataBits.W))
  val status: UInt            = UInt(32.W)

}

class ComputeGridOrchestratorInterface(DimX: Int, DimY: Int, config: ISA)
    extends Bundle {

  val periphery_core: Vec[PeripheryProcessorInterface] = Flipped(
    Vec(4, new PeripheryProcessorInterface(config))
  )
  val cache_backend: Vec[CacheBackInterface] =
    Vec(4, CacheConfig.backInterface())
  val start: Bool = Input(Bool())
  val registers = new Bundle {
    val from_host = Input(new HostRegisters(config))
    val to_host   = Output(new DeviceRegisters(config))
  }
  val idle: Bool            = Output(Bool())
  val done: Bool            = Output(Bool())
  val clock_enable_n: Bool  = Output(Bool())
  val packet_out: NoCBundle = Output(new NoCBundle(DimX, DimY, config))

}

class ComputeGridOrchestrator(DimX: Int, DimY: Int, config: ISA)
    extends Module {

  val io: ComputeGridOrchestratorInterface = IO(
    new ComputeGridOrchestratorInterface(DimX, DimY, config)
  )

  val master_cache = Module(new Cache)

  val storage_cache: Seq[Cache] = Seq.fill(3) {
    Module(new Cache)
  }

  val program_loader: Programmer = Module(new Programmer(config, DimX, DimY))

  io.packet_out := program_loader.io.packet_out

  val exception_handler: Seq[ExceptionHandler] = io.periphery_core.map { core =>
    val handler = Module(new ExceptionHandler(config.DataBits))
    handler.io.exception := core.exception
    handler
  }

  val master_cache_intercept: CacheRequestIntercept = Module(
    new CacheRequestIntercept
  )
  val storage_cache_intercept: Seq[CacheRequestIntercept] = storage_cache
    .map(_ => Module(new CacheRequestIntercept))

  val clock_manager: ClockManager = Module(new ClockManager(NumUsers = 4 * 2))
  io.clock_enable_n := clock_manager.io.clock_enable_n

  clock_manager.io.start_request
    .slice(0, 4)
    .zip(
      clock_manager.io.done_request
        .slice(0, 4)
    )
    .zip(exception_handler)
    .foreach { case ((_start, _end), handler) =>
      _start := handler.io.gate_request_start
      _end   := handler.io.gate_request_end
    }

  clock_manager.io.start_request
    .slice(4, 8)
    .zip(clock_manager.io.done_request.slice(4, 8))
    .zip(
      (master_cache_intercept +: storage_cache_intercept)
    )
    .foreach { case ((_start, _end), _intercept) =>
      _start := _intercept.io.clock_manager.gate_request_start
      _end   := _intercept.io.clock_manager.gate_request_end
      _intercept.io.clock_manager.clock_enable := clock_manager.io.clock_enable_n
    }

  // disable master cache access when the core is not active and let the programmer module
  // talk to the cache
  when(io.periphery_core.head.active === false.B) {
    program_loader.io.cache_frontend <> master_cache.io.front
  } otherwise {
    master_cache_intercept.io.front_side.cache <> master_cache.io.front
  }
  io.periphery_core.head.cache <> master_cache_intercept.io.front_side.core


  (storage_cache_intercept)
    .zip(
      storage_cache
        .zip(io.periphery_core.tail)
    )
    .foreach { case (_intercept, (_cache, _core)) =>
      _intercept.io.front_side.cache <> _cache.io.front
      _core.cache <> _intercept.io.front_side.core
    }


  (master_cache +: storage_cache)
    .zip(io.cache_backend)
    .foreach { case (_cache, _io) =>
      _io <> _cache.io.back
    }

  val schedule_counter: UInt = Reg(UInt((config.NumPcBits + 1).W))
  val virtual_cycles: UInt = Reg(UInt(48.W)) // can simulate up to 2^48 cycles,
  // which is about 3 days if the clock is 1ns

  when(clock_manager.io.clock_enable_n && program_loader.io.running) {
    schedule_counter := schedule_counter + 1.U
    when(schedule_counter === io.registers.from_host.schedule_length - 1.U) {
      schedule_counter := 0.U
      virtual_cycles   := virtual_cycles + 1.U
    }
  }

  val bootloader_counter: UInt = Reg(UInt(32.W))

  io.registers.to_host.bootloader_cycles := bootloader_counter
  io.registers.to_host.virtual_cycles    := virtual_cycles

  val exception: Bool = Wire(Bool())
  exception := io.periphery_core.exists(_.exception.error === true.B)
  io.registers.to_host.exception_id := io.periphery_core.map(_.exception.id)

  object Phase extends ChiselEnum {
    val WaitForStart, // initial state, requires boot-loading
    WaitForResume,    // state after went back to the host to handle exceptions
    BootLoading, VirtualCycle = Value
  }

  exception_handler.foreach { h =>
    h.io.clear := false.B
  }

  program_loader.io.start  := false.B
  program_loader.io.finish := false.B
  program_loader.io.instruction_stream_base :=
    io.registers.from_host.global_memory_instruction_base

  io.done := false.B
  io.idle := false.B
  val phase: Phase.Type = RegInit(Phase.Type(), Phase.WaitForStart)
  switch(phase) {
    is(Phase.WaitForStart) {
      io.idle := true.B
      when(io.start) {
        phase                   := Phase.BootLoading
        program_loader.io.start := true.B
        schedule_counter        := 0.U
        bootloader_counter      := 0.U
        virtual_cycles          := 0.U
      }
    }
    is(Phase.WaitForResume) {
      io.idle := true.B
      when(io.start) {
        exception_handler.foreach { h => h.io.clear := true.B }
        // clock will be enabled in the next cycle
        phase := Phase.VirtualCycle
      }
    }
    is(Phase.BootLoading) {
      bootloader_counter := bootloader_counter + 1.U
      when(program_loader.io.running) {
        phase := Phase.VirtualCycle
      }
    }

    is(Phase.VirtualCycle) {
      when(!clock_manager.io.clock_enable_n) {
        val any_exceptions = Wire(Vec(4, Bool()))
        any_exceptions := exception_handler.map(_.io.caught)
        when(any_exceptions.exists(_ === true.B)) {
          phase   := Phase.WaitForResume
          io.done := true.B
          io.registers.to_host.exception_id := io.periphery_core.map(
            _.exception.id
          )
        } // otherwise {
        //  handling cache requests, will resume automatically, no host interference
        // }
      }
    }
  }

}

object GenerateComputeGridOrchestrator extends App {

  new ChiselStage().emitVerilog(
    new ComputeGridOrchestrator(2, 2, ManticoreFullISA),
    Array("--target-dir", "gen-dir")
  )
}
