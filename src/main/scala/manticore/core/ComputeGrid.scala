package manticore.core

import Chisel._
import chisel3.{RawModule, withClockAndReset}
import chisel3.stage.ChiselStage
import manticore.{ISA, ManticoreBaseISA, ManticoreFullISA}

object ComputeGrid {
  type Equations = Seq[Seq[Int]]

  def createEquations(config: ISA)(generator: => Int): Equations =
    Seq.fill(1 << config.FunctBits) {
      Seq.fill(config.DataBits) {
        (generator.abs) % (1 << 16)
      }
    }

  type EquationConfig = Seq[Seq[Equations]]

  class InitialState(
      val lut_configs: EquationConfig,
      val regfile_files: Seq[Seq[String]],
      val regarray_files: Seq[Seq[String]]
  ) {
    val DimX = lut_configs.length
    val DimY = lut_configs.head.length
    require(lut_configs.forall(_.length == DimY))
    require(regfile_files.length == DimX)
    require(regfile_files.forall(_.length == DimY))
    require(regarray_files.length == DimX)
    require(regarray_files.forall(_.length == DimY))

    def lut(x: Int, y: Int) = lut_configs(x)(y)

    def registerFile(x: Int, y: Int) = regfile_files(x)(y)

    def registerArray(x: Int, y: Int) = regarray_files(x)(y)
  }

}

class ComputeGridInterface(DimX: Int, DimY: Int, config: ISA) extends Bundle {
  val cores: Vec[PeripheryProcessorInterface] =
    Vec(4, new PeripheryProcessorInterface(config))
  val external_packet: NoCBundle   = Input(new NoCBundle(DimX, DimY, config))
  val external_packet_enable: Bool = Input(Bool())
  val clock_enable_n: Bool         = Input(Bool())
  
}

class ComputeGrid(
    DimX: Int,
    DimY: Int,
    power_on_state: ComputeGrid.InitialState,
    debug_enable: Boolean = false
) extends Module {

  val io: ComputeGridInterface = IO(
    new ComputeGridInterface(DimX, DimY, ManticoreBaseISA)
  )

  private def generateModules[T <: Module](gen: (Int, Int) => T): Seq[Seq[T]] =
    Range(0, DimX).map { x =>
      Range(0, DimY).map { y =>
        Module(gen(x, y))
      }
    }

  val gated_clock: Clock = Wire(Clock())
  val clock_buffer       = Module(new ClockBuffer())
  clock_buffer.io.I  := clock
  clock_buffer.io.CE := io.clock_enable_n
  gated_clock        := clock_buffer.io.O

  val debug_time = RegInit(UInt(64.W), 0.U)
  if (debug_enable) { // count the number of elapsed cycles
    debug_time := debug_time + 1.U
  }
  withClockAndReset(clock = gated_clock, reset = reset) {
    val cores: Seq[Seq[Processor]] = generateModules { (x: Int, y: Int) =>
      val has_memory =
        (x == 0 && y == 0) ||
          (x == DimX - 1 && y == 0) ||
          (x == 0 && y == DimY - 1) ||
          (x == DimX - 1 && y == DimY - 1)
      val core_config = if (has_memory) ManticoreFullISA else ManticoreBaseISA
      new Processor(
        core_config,
        DimX,
        DimY,
        power_on_state.lut(x, y),
        power_on_state.registerFile(x, y),
        power_on_state.registerArray(x, y),
        s"CoreX${x}Y${y}",
        debug_enable
      )
    }

    val noc = Module(new BareNoC(DimX, DimY, ManticoreBaseISA))
    // connect the processors to the NoC

    noc.io.corePacketInput.flatten
      .zip(noc.io.corePacketOutput.flatten)
      .zip(cores.flatten)
      .foreach { case ((_inp, _out), core) =>
        _inp              := core.io.packet_out
        core.io.packet_in := _out
      }

    val periphery_if: Seq[PeripheryProcessorInterface] = Seq(
      cores.head.head.io.periphery,
      cores.head.last.io.periphery,
      cores.last.head.io.periphery,
      cores.last.last.io.periphery
    )

    periphery_if.zip(io.cores).foreach { case (inner, outer) =>
      outer.exception                 := inner.exception
      outer.active                    := inner.active
      outer.gmem_access_failure_error := inner.gmem_access_failure_error
      inner.cache ==> outer.cache
    }

    noc.io.configEnable := io.external_packet_enable
    noc.io.configPacket := io.external_packet

    if (debug_enable) {
      cores.flatten.foreach { c => c.io.periphery.debug_time := debug_time }
    }
  }

}

object GenerateComputeGrid extends App {

  val dimX, dimY = 4
  val rdgen      = new scala.util.Random(0)

  def makeGrid[T](gen: (Int, Int) => T): Seq[Seq[T]] = Range(0, dimX).map { x =>
    Range(0, dimY).map { y =>
      gen(x, y)
    }
  }

  val init = new ComputeGrid.InitialState(
    makeGrid { (_, _) =>
      ComputeGrid.createEquations(ManticoreBaseISA)(rdgen.nextInt)
    },
    makeGrid { (x, y) =>
      s"rf_${x}_${y}.dat"
    },
    makeGrid { (x, y) =>
      s"ra_${x}_${y}.dat"
    }
  )

  new ChiselStage().emitVerilog(
    new ComputeGrid(dimX, dimY, init),
    Array("--target-dir", "gen-dir")
  )

}
