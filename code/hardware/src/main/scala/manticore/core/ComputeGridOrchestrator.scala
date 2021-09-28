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
  val value_change_log_base: UInt = UInt(64.W)
  val schedule_length: UInt = UInt((config.NumPcBits + 1).W)
}


/// registers written by the device, i.e., the compute grid
class DeviceRegisters(config: ISA) extends Bundle {
  val virtual_cycles: UInt = UInt(48.W) // notice we can simulate up to 2^48 - 1 cycles
  val bootloader_cycles: UInt = UInt(32.W) // for profiling
  val exception_id: Vec[UInt] = Vec(4, UInt(config.DataBits.W))
}

class ComputeGridOrchestratorInterface(DimX: Int, DimY: Int, config: ISA) extends Bundle {

  val periphery_core: Vec[PeripheryProcessorInterface] = Flipped(Vec(4, new PeripheryProcessorInterface(config)))
  val cache_backend: Vec[CacheBackInterface] = Vec(4, CacheConfig.backInterface())
  val start: Bool = Input(Bool())
  val registers = new Bundle {
    val from_host = Input(new HostRegisters(config))
    val to_host = Output(new DeviceRegisters(config))
  }
  val idle: Bool = Output(Bool())
  val done: Bool = Output(Bool())
  val clock_enable: Bool = Output(Bool())
  val packet_out: NoCBundle = Output(new NoCBundle(DimX, DimY, config))

}

class ComputeGridOrchestrator(DimX: Int, DimY: Int, config: ISA) extends Module {

  val io: ComputeGridOrchestratorInterface = IO(new ComputeGridOrchestratorInterface(DimX, DimY, config))

  val master_cache = Module(new Cache)

  val storage_cache: Seq[Cache] = Seq.fill(3) {
    Module(new Cache)
  }


  val program_loader: Programmer = Module(new Programmer(config,
    DimX, DimY))

  io.packet_out := program_loader.io.packet_out

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
  val virtual_cycles: UInt = Reg(UInt(48.W)) // can simulate up to 2^48 cycles,
  // which is about 3 days if the clock is 1ns
  val bootloader_counter: UInt = Reg(UInt(32.W))

  io.registers.to_host.bootloader_cycles := bootloader_counter
  io.registers.to_host.virtual_cycles := virtual_cycles

  val exception: Bool = Wire(Bool())
  exception := io.periphery_core.exists(_.exception.error === true.B)
  io.registers.to_host.exception_id := io.periphery_core.map(_.exception.id)

  object Phase extends ChiselEnum {
    val WaitForStart, BootLoading, VirtualCycle, Terminate = Value
  }

  val phase: Phase.Type = RegInit(Phase.Type(), Phase.WaitForStart)
  switch(phase) {
    is(Phase.WaitForStart) {
      when(io.start) {
        phase := Phase.BootLoading
        program_loader.io.start := true.B
        schedule_counter := 0.U
        bootloader_counter := 0.U
        virtual_cycles := 0.U
      }
    }
    is(Phase.BootLoading) {
      bootloader_counter := bootloader_counter + 1.U
      when(program_loader.io.running) {
        phase := Phase.VirtualCycle
      }
    }

    is(Phase.VirtualCycle) {
      when(clock_manager.io.clock_enable) {
        schedule_counter := schedule_counter + 1.U
        when(schedule_counter === io.registers.from_host.schedule_length) {
          schedule_counter := 0.U
          virtual_cycles := virtual_cycles + 1.U
          when(exception) {
            phase := Phase.Terminate
            program_loader.io.finish := true.B
          } otherwise {
            phase := Phase.VirtualCycle
          }
        }
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
