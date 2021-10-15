package manticore.core

import chisel3._
import manticore.ManticoreFullISA
import memory.CacheBackInterface
import memory.CacheConfig
import chisel3.stage.ChiselStage

class ManticoreArrayInterface extends Bundle {

  // These wires are connected to an AXI-Slave module
  val host_registers   = Input(new HostRegisters(ManticoreFullISA))
  val device_registers = Output(new DeviceRegisters(ManticoreFullISA))

  // start signal comes from an AXI slave
  val start: Bool = Input(Bool())
  // Notify axi slave the execution is finished
  val idle: Bool = Output(Bool())
  val done: Bool = Output(Bool())

  // Interfaces to memory
  val cache_backend: Vec[CacheBackInterface] =
    Vec(4, CacheConfig.backInterface())

}
class ManticoreArray(DimX: Int, DimY: Int, debug_enable: Boolean = false)
    extends Module {

  val io = IO(new ManticoreArrayInterface)

  /** By default the lut vector is initialized as an "address decoder".
    * I.e., Cust(x, y, u, v) = (1 << (v # u # y # x))
    */
  val equations = Seq.fill(1 << ManticoreFullISA.FunctBits) {
    Seq.tabulate(ManticoreFullISA.DataBits) { i => (1 << i) }
  }

  def makeConfigData[T](gen: (Int, Int) => T): Seq[Seq[T]] =
    Seq.tabulate(DimX) { x =>
      Seq.tabulate(DimY) { y =>
        gen(x, y)
      }
    }
  val initial_state = new ComputeGrid.InitialState(
    lut_configs = makeConfigData((x, y) => equations),
    regfile_files = makeConfigData((x, y) => s"rf_${x}_${y}.dat"),
    regarray_files = makeConfigData((x, y) => s"ra_${x}_${y}.dat")
  )

  val compute_grid = Module(
    new ComputeGrid(
      DimX = DimX,
      DimY = DimY,
      power_on_state = initial_state,
      debug_enable = debug_enable
    )
  )

  val orchestrator = Module(
    new ComputeGridOrchestrator(
      DimX = DimX,
      DimY = DimY,
      config = ManticoreFullISA,
      debug_enable = debug_enable
    )
  )

  // connect the periphery cores to the orchestrator
  compute_grid.io.cores.zip(orchestrator.io.periphery_core).foreach {
    case (core, control) =>
      control <> core
  }

  orchestrator.io.registers.from_host := io.host_registers
  io.device_registers                 := orchestrator.io.registers.to_host

  orchestrator.io.start := io.start
  io.idle               := orchestrator.io.idle
  io.done := orchestrator.io.done

  io.cache_backend.zip(orchestrator.io.cache_backend).foreach {
    case (outer, inner) =>
      outer <> inner
  }

  compute_grid.io.external_packet        := orchestrator.io.packet_out
  compute_grid.io.external_packet_enable := orchestrator.io.packet_out.valid
  compute_grid.io.clock_enable_n         := orchestrator.io.clock_enable_n

  
}


object ManticoreArrayGenerator extends App {


    new ChiselStage().emitVerilog(new ManticoreArray(2, 2), Array("-td", "gen-dir"))

}