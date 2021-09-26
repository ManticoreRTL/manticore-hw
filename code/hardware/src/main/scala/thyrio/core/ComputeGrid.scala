package thyrio.core

import Chisel._
import memory.{Cache, CacheBackInterface, CacheConfig}
import thyrio.{ISA, ThyrioISA, ThyrioISAWithGlobalMemory}




class ProgrammableRegisters(config: ISA) extends Bundle {
  val global_mem_inst_base: UInt = UInt(64.W)
  val dynamic_cycle_count: UInt = UInt(64.W)
  val static_cycle_count: UInt = UInt(64.W)

}

class ComputeGridInterface(config: ISA) extends Bundle {
  val cache_interface: Vec[CacheBackInterface] = Flipped(Vec(4, CacheConfig.backInterface()))
  val registers: ProgrammableRegisters = Input(new ProgrammableRegisters(config))
  val start: Bool = Input(Bool()) // start signal from the host
  val done: Bool = Output(Bool()) // done signal, return to host
}

object ComputeGrid {
  type Equations = Seq[Seq[Int]]

  def createEquations(config: ISA)(generator: => Int): Equations = Seq.fill(1 << config.FunctBits) {
    Seq.fill(1 << config.DataBits) {
      generator
    }
  }

  type EquationConfig = Seq[Seq[Equations]]

  class InitialState(val lut_configs: EquationConfig,
                     val regfile_files: Seq[Seq[String]],
                     val regarray_files: Seq[Seq[String]]) {
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

  //  class RandomInitialState(config: ISA, fileDir: Path, DimX: Int, DimY: Int) extends InitialState(
  //    lut_configs = {
  //      val rdgen = new scala.util.Random(0)
  //      Seq.fill(DimX) {
  //        Seq.fill(DimY) {
  //          createEquations(config) {
  //            rdgen.nextInt(1 << config.DATA_BITS)
  //          }
  //        }
  //      }
  //    },
  //    regfile_files = Range(0, DimX).map { x =>
  //      Range(0, DimY).map { y =>
  //        fileDir + s"regfile_${x}_${y}.dat"
  //      }
  //    },
  //    regarray_files = Range(0, DimX).map { x =>
  //      Range(0, DimY).map { y =>
  //        fileDir + s"regarray_${x}_${y}.dat"
  //      }
  //    }
  //  )
}

class ComputeGrid(DimX: Int, DimY: Int,
                  power_on_state: ComputeGrid.InitialState) extends Module {


  val io = IO(new ComputeGridInterface(ThyrioISA))

  private def generateModules[T <: Module](gen: (Int, Int) => T): Seq[Seq[T]] =
    Range(0, DimX).map { x =>
      Range(0, DimY).map { y =>
        Module(gen(x, y))
      }
    }

  val cores: Seq[Seq[Processor]] = generateModules { (x: Int, y: Int) =>
    val has_memory =
      (x == 0 && y == 0) ||
        (x == DimX - 1 && y == 0) ||
        (x == 0 && y == DimY - 1)
    val core_config = if (has_memory) ThyrioISAWithGlobalMemory else ThyrioISA
    new Processor(
      core_config,
      DimX, DimY,
      power_on_state.lut(x, y),
      power_on_state.registerFile(x, y),
      power_on_state.registerArray(x, y))
  }

  val switches: Seq[Seq[Switch]] = generateModules { (_, _) =>
    new Switch(DimX, DimY, ThyrioISA)
  }


  private def connectSwitches(transform: Seq[Seq[Switch]] => Seq[Seq[Switch]],
                              inPort: Switch => NoCBundle,
                              outPort: Switch => NoCBundle): Unit = {
    transform(switches).foreach { col_row =>
      inPort(col_row.head) := outPort(col_row.last)
      col_row.sliding(2, 1).foreach { case Seq(left: Switch, right: Switch) =>
        inPort(right) := outPort(left)
      }
    }
  }

  // connect the row ports in the switches
  connectSwitches(_, _.io.xInput, _.io.xOutput)
  // connect column ports of the switches
  connectSwitches(_.transpose, _.io.yInput, _.io.yOutput)


  switches.flatten.zip(cores.flatten).foreach { case (s, c) =>
    s.io.lInput := c.io.packet_out
    c.io.packet_in := s.io.yOutput
    c.io.packet_in.valid := s.io.terminal
  }


  val caches = Seq.fill(4){
    Module(new Cache)
  }

  /**
   * Master core shares the cache with the programmer module (which essentially
   * boots up all the cores). The core can use the cache (hence the memory bank)
   * for storage but not at full capacity.
   */
  val master_core: Processor = cores.head.head
  val program_loader: Programmer = Module(new Programmer(ThyrioISAWithGlobalMemory,
    DimX, DimY))
  // connect the programmer to the NoC
  when(program_loader.io.packet_out.valid) {
    switches.head.head.io.xInput := program_loader.io.packet_out
  } otherwise { // already assigned in the connectSwitch function, but do it
    // again for clarity of the code
    switches.head.head.io.xInput := switches.head.last.io.xOutput
  }

  val cores_active: Vec[Bool] = Wire(Vec(DimX * DimY, Bool()))
  cores_active := cores.flatten.map(_.io.active)

  program_loader.io.instruction_stream_base := io.registers.global_mem_inst_base
  program_loader.io.start := io.start
  program_loader.io.pause_exec := false.B
  program_loader.io.resume_exec := false.B

  // disable cache access when the core is not active and let the programmer
  // talk to the cache
  when(master_core.io.active === false.B) {
    caches.head.io.front <> program_loader.io.cache_frontend
  } otherwise {
    caches.head.io.front <> master_core.io.cache_interface
  }

  /**
   * Tracker core receives all the waveform tracking information, core the
   * core itself does not have access to the cache.
   */
  val tracker_core: Processor = cores.last.last

  /**
   * Storage cores have access to the full 16GiB of memory each
   */
  val storage_core_0: Processor = cores.head.last
  val storage_core_1: Processor = cores.last.head


}
