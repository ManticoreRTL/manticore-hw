package manticore.machine.core

import Chisel._
import chisel3.experimental.ChiselEnum
import chisel3.stage.ChiselStage

import manticore.machine.{ISA, ManticoreFullISA}


/// registers written by the host
class HostRegisters(config: ISA) extends Bundle {
  val global_memory_instruction_base: UInt = UInt(64.W)
  val value_change_symbol_table_base: UInt = UInt(64.W)
  val value_change_log_base: UInt          = UInt(64.W)
  val schedule_length: UInt                = UInt(32.W)
  // val clear_exception: UInt = UInt(32.W)
}

/// registers written by the device, i.e., the compute grid
class DeviceRegisters(config: ISA) extends Bundle {
  val virtual_cycles: UInt = UInt(64.W)

  val bootloader_cycles: UInt = UInt(32.W) // for profiling
  // val exception_id: Vec[UInt] = Vec(4, UInt(config.DataBits.W))
  val exception_id_0: UInt = UInt(32.W)
  val exception_id_1: UInt = UInt(32.W)
  val exception_id_2: UInt = UInt(32.W)
  val exception_id_3: UInt = UInt(32.W)
  val status: UInt         = UInt(32.W)

}

class ComputeGridOrchestratorInterface(DimX: Int, DimY: Int, config: ISA)
    extends Bundle {

  val periphery_core: Vec[PeripheryProcessorInterface] = Flipped(
    Vec(4, new PeripheryProcessorInterface(config))
  )
  val memory_backend: Vec[MemoryReadWriteInterface] =
    Vec(4, new MemoryReadWriteInterface(config))
  // val cache_backend: Vec[CacheBackInterface] =
  //   Vec(4, CacheConfig.backInterface())
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

class ComputeGridOrchestrator(
    DimX: Int,
    DimY: Int,
    config: ISA,
    debug_enable: Boolean = false
) extends Module {

  val io: ComputeGridOrchestratorInterface = IO(
    new ComputeGridOrchestratorInterface(DimX, DimY, config)
  )

  // val master_cache = Module(new Cache)

  // val storage_cache: Seq[Cache] = Seq.fill(3) {
  //   Module(new Cache)
  // }

  val program_loader: Programmer = Module(new Programmer(config, DimX, DimY))

  io.packet_out := program_loader.io.packet_out

  val exception_handler: Seq[ExceptionHandler] = io.periphery_core.map { core =>
    val handler = Module(new ExceptionHandler(config.DataBits))
    handler.io.exception := core.exception
    handler
  }

  val master_memory_intercept = Module(new MemoryRequestIntercept(config))
  val storage_memory_intercept = Seq.fill(3) {
    Module(new MemoryRequestIntercept(config))
  }

  // val master_cache_intercept: CacheRequestIntercept = Module(
  //   new CacheRequestIntercept
  // )
  // val storage_cache_intercept: Seq[CacheRequestIntercept] = storage_cache
  //   .map(_ => Module(new CacheRequestIntercept))

  val clock_manager: ClockManager = Module(
    new ClockManager(NumUsers = 4 * 2, debug_enable = debug_enable)
  )
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
      (master_memory_intercept +: storage_memory_intercept)
    )
    .foreach { case ((_start, _end), _intercept) =>
      _start := _intercept.io.clock_manager.gate_request_start
      _end   := _intercept.io.clock_manager.gate_request_end
      _intercept.io.clock_manager.clock_enable := clock_manager.io.clock_enable_n
    }

  // disable master cache access when the core is not active and let the programmer module
  // talk to the cache
  when(io.periphery_core.head.active === false.B) {
  
    io.memory_backend(0) <> program_loader.io.memory_backend  

  } otherwise {
    io.memory_backend(0) <> master_memory_intercept.io.memory
  }

  io.periphery_core.head.cache <> master_memory_intercept.io.core

  io.periphery_core.tail.zip(storage_memory_intercept).foreach {
    case (_core, _intercept) =>
      _core.cache <> _intercept.io.core
  }

  (master_memory_intercept +: storage_memory_intercept)
    .zip(io.memory_backend)
    .foreach { case (_intercept, _io) =>
      _io <> _intercept.io.memory
    }

  // (master_cache +: storage_cache)
  //   .zip(io.cache_backend)
  //   .foreach { case (_cache, _io) =>
  //     _io <> _cache.io.back
  //   }

  val dev_regs = Reg(new DeviceRegisters(config))

  val schedule_counter: UInt = Reg(UInt((config.NumPcBits + 1).W))
  val virtual_cycles: UInt = Reg(UInt(48.W)) // can simulate up to 2^48 cycles,
  // which is about 3 days if the clock is 1ns

  when(clock_manager.io.clock_enable_n && program_loader.io.running) {
    schedule_counter := schedule_counter + 1.U
    when(schedule_counter === io.registers.from_host.schedule_length - 1.U) {
      schedule_counter        := 0.U
      dev_regs.virtual_cycles := dev_regs.virtual_cycles + 1.U
    }
  }

  dev_regs.exception_id_0 := io.periphery_core(0).exception.id
  dev_regs.exception_id_1 := io.periphery_core(1).exception.id
  dev_regs.exception_id_2 := io.periphery_core(2).exception.id
  dev_regs.exception_id_3 := io.periphery_core(3).exception.id

  io.registers.to_host := dev_regs

  val exception: Bool = Wire(Bool())
  exception := io.periphery_core.exists(_.exception.error === true.B)

  object Phase extends ChiselEnum {
    val WaitForStart, // initial state, requires boot-loading
    WaitForResume,    // state after went back to the host to handle exceptions
    BootLoading, VirtualCycle, StartFlush, WaitFlush, SignalDone = Value
  }

  exception_handler.foreach { h =>
    h.io.clear := false.B
  }

  program_loader.io.start  := false.B
  program_loader.io.finish := false.B
  program_loader.io.instruction_stream_base := io.registers.from_host.global_memory_instruction_base

  io.done := false.B
  io.idle := false.B
  val phase: Phase.Type = RegInit(Phase.Type(), Phase.WaitForStart)
  switch(phase) {
    is(Phase.WaitForStart) {
      io.idle := true.B
      when(io.start) {
        phase                      := Phase.BootLoading
        program_loader.io.start    := true.B
        schedule_counter           := 0.U
        dev_regs.bootloader_cycles := 0.U
        virtual_cycles             := 0.U
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
      dev_regs.bootloader_cycles := dev_regs.bootloader_cycles + 1.U
      when(program_loader.io.running) {
        phase := Phase.VirtualCycle
      }
    }

    is(Phase.VirtualCycle) {
      when(!clock_manager.io.clock_enable_n) {
        val any_exceptions = Wire(Vec(4, Bool()))
        any_exceptions := exception_handler.map(_.io.caught)
        when(any_exceptions.exists(_ === true.B)) {
          phase := Phase.SignalDone
        } // otherwise {
        //  handling cache requests, will resume automatically, no host interference
        // }
      }
    }

    // is(Phase.StartFlush) {

    //   (master_cache +: storage_cache).foreach { cache =>
    //     cache.io.front.cmd   := CacheCommand.Flush
    //     cache.io.front.start := true.B
    //   }
    //   phase := Phase.WaitFlush
    // }

    // is(Phase.WaitFlush) {
    //   val all_idle   = Wire(Bool())
    //   val cache_idle = Wire(Vec(4, Bool()))
    //   cache_idle := (master_cache +: storage_cache).map(c => c.io.front.idle)

    //   when(cache_idle.forall(_ === true.B)) {
    //     phase   := Phase.WaitForResume
    //     io.done := true.B
    //   }
    // }
    is(Phase.SignalDone) {
      io.done := true.B
      phase   := Phase.WaitForResume
    }

  }

  val debug_time = RegInit(UInt(64.W), 0.U)

  if (debug_enable) {
    debug_time                  := debug_time + 1.U
    clock_manager.io.debug_time := debug_time
  }

}

object GenerateComputeGridOrchestrator extends App {

  new ChiselStage().emitVerilog(
    new ComputeGridOrchestrator(2, 2, ManticoreFullISA),
    Array("--target-dir", "gen-dir")
  )
}
