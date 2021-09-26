package thyrio.core


import Chisel._
import chisel3.RawModule
import chisel3.stage.ChiselStage
import memory.{Cache, CacheBackInterface, CacheConfig, CacheFrontInterface}
import thyrio.{ISA, ThyrioISAWithGlobalMemory}


class ProgrammableRegisters(config: ISA) extends Bundle {
  val global_mem_inst_base: UInt = UInt(64.W)
  val dynamic_cycle_count: UInt = UInt(64.W)
  val static_cycle_count: UInt = UInt(64.W)
  val schedule_length: UInt = UInt((config.NumPcBits + 1).W)

}

class ComputeGridOrchestratorInterface(DimX: Int, DimY: Int, config: ISA) extends Bundle {

  val periphery_core: Vec[PeripheryProcessorInterface] = Flipped(Vec(4, new PeripheryProcessorInterface(config)))
  val cache_backend: Vec[CacheBackInterface] = Vec(4, CacheConfig.backInterface())
  val registers: ProgrammableRegisters = Input(new ProgrammableRegisters(config))
  val start: Bool = Input(Bool())
  val idle: Bool = Output(Bool())
  val done: Bool = Output(Bool())
  val clock_enable: Bool = Output(Bool())

}

class ComputeGridOrchestrator(DimX: Int, DimY: Int, config: ISA) extends Module {

  val io: ComputeGridOrchestratorInterface = IO(new ComputeGridOrchestratorInterface(DimX, DimY, config))
  
  val master_cache = Module(new Cache)

  val storage_cache: Seq[Cache] = Seq.fill(3) {
    Module(new Cache)
  }


  val program_loader: Programmer = Module(new Programmer(config,
    DimX, DimY))

  val master_cache_intercept: CacheRequestIntercept = Module(new CacheRequestIntercept)
  val storage_cache_intercept: Seq[CacheRequestIntercept] = storage_cache
    .map(_ => Module(new CacheRequestIntercept))

  val clock_manager: ClockManager = Module(new ClockManager(NumUsers = 4))
  io.clock_enable := clock_manager.io.clock_enable

  clock_manager.io.start_request.zip(clock_manager.io.done_request)
    .zip((master_cache_intercept +: storage_cache_intercept))
    .foreach { case ((_start, _end), _intercept) =>
      _start := _intercept.io.clock_manager.gate_request_start
      _end := _intercept.io.clock_manager.gate_request_end
      _intercept.io.clock_manager.clock_enable := clock_manager.io.clock_enable
    }

  // disable master cache access when the core is not active and let the programmer module
  // talk to the cache
  when(io.periphery_core.head.active === false.B) {
    program_loader.io.cache_frontend <> master_cache.io.front
  } otherwise {
    master_cache_intercept.io.front_side.cache <> master_cache.io.front
  }


  (master_cache_intercept +: storage_cache_intercept)
    .zip((master_cache +: storage_cache)
      .zip(io.periphery_core))
    .foreach { case (_intercept, (_cache, _core)) =>
      _intercept.io.front_side.cache <> _cache.io.front
      _core.cache <> _intercept.io.front_side.core
    }


  (master_cache +: storage_cache).zip(io.cache_backend)
    .foreach { case (_cache, _io) =>
      _io <> _cache.io.back
    }

  val schedule_counter: UInt = Reg(UInt((config.NumPcBits + 1).W))

  when(program_loader.io.running) {
    when(clock_manager.io.clock_enable) {
      schedule_counter := schedule_counter + 1.U
      when(schedule_counter === io.registers.schedule_length) {
        schedule_counter := 0.U
      }
    }
  }

}


object GenerateComputeGridOrchestrator extends App {

  new ChiselStage().emitVerilog(
    new ComputeGridOrchestrator(2, 2, ThyrioISAWithGlobalMemory),
    Array("--target-dir", "gen-dir", "--no-dce")
  )
}
