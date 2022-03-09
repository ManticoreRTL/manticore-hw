package manticore.machine.xrt

import chisel3._
import manticore.machine.core.{
  ClockDistribution,
  DeviceRegisters,
  HostRegisters,
  ManticoreFlatArray,
  MemoryReadWriteInterface
}
import manticore.machine.ManticoreFullISA
import chisel3.stage.ChiselStage

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.io.File
import java.io.PrintWriter
import scala.collection.immutable.ListMap
import chisel3.util.log2Ceil
import chisel3.experimental.ChiselEnum
import chisel3.util.switch
import chisel3.util.is
import chisel3.util.Cat
import manticore.machine.memory.CacheConfig

class MemoryPointers extends Bundle {
  val pointer_0: UInt = UInt(64.W)
  val pointer_1: UInt = UInt(64.W)
  val pointer_2: UInt = UInt(64.W)
  val pointer_3: UInt = UInt(64.W)
}

object KernelInfo {
  case class KernelMemoryInterface(
      name: String,
      bundle: String,
      width: Int = 256
  ) {
    def portName = name + "_" + bundle
  }

  case class KernelSlaveRegister(
      id: Int,
      name: String,
      offset: Int,
      cpp_type: String,
      is_pointer: Boolean,
      port_interface: String
  ) {
    def withId(new_id: Int) = KernelSlaveRegister(
      new_id,
      this.name,
      this.offset,
      this.cpp_type,
      this.is_pointer,
      this.port_interface
    )

  }
}

class ManticoreFlatKernel(
    DimX: Int,
    DimY: Int,
    debug_enable: Boolean = false
    // m_axi_path: Seq[String] =
    //   Seq() // path to m_axi implementation if exits, uses simulation models otherwise
) extends RawModule {

  val clock = IO(Input(Clock()))
  clock.suggestName("ap_clk")
  val reset_n = IO(Input(Bool()))
  reset_n.suggestName("ap_rst_n")
  val reset = Wire(Bool())
  reset := ~reset_n

  val clock_distribution = Module(new ClockDistribution())

  clock_distribution.io.root_clock := clock

  val m_axi_bank_0  = IO(new AxiMasterIF)
  val s_axi_control = IO(new AxiSlave.AxiSlaveCorenterface())
  val interrupt     = IO(Output(Bool()))

  val slave =
    withClockAndReset(
      clock = clock_distribution.io.control_clock,
      reset = reset
    ) {
      Module(new AxiSlave(ManticoreFullISA))
    }

  slave.io.core.AWADDR  := s_axi_control.AWADDR
  slave.io.core.AWVALID := s_axi_control.AWVALID
  s_axi_control.AWREADY := slave.io.core.AWREADY
  slave.io.core.WDATA   := s_axi_control.WDATA
  slave.io.core.WSTRB   := s_axi_control.WSTRB
  slave.io.core.WVALID  := s_axi_control.WVALID
  s_axi_control.WREADY  := slave.io.core.WREADY
  s_axi_control.BRESP   := slave.io.core.BRESP
  s_axi_control.BVALID  := slave.io.core.BVALID
  slave.io.core.BREADY  := s_axi_control.BREADY
  slave.io.core.ARADDR  := s_axi_control.ARADDR
  slave.io.core.ARVALID := s_axi_control.ARVALID
  s_axi_control.ARREADY := slave.io.core.ARREADY
  s_axi_control.RDATA   := slave.io.core.RDATA
  s_axi_control.RRESP   := slave.io.core.RRESP
  s_axi_control.RVALID  := slave.io.core.RVALID
  slave.io.core.RREADY  := s_axi_control.RREADY
  interrupt             := slave.io.control.interrupt

  val manticore =
    Module(new ManticoreFlatArray(DimX, DimY, debug_enable))

  manticore.io.reset         := reset
  manticore.io.control_clock := clock_distribution.io.control_clock
  manticore.io.compute_clock := clock_distribution.io.compute_clock

  clock_distribution.io.compute_clock_en_n := manticore.io.clock_inactive

  // val gateway: MemoryGateway = withClockAndReset(
  //   clock = clock_distribution.io.control_clock,
  //   reset = reset
  // ) {
  //   Module(new MemoryGatewaySimpleHls(m_axi_path))
  // }

  val axi_mreader = withClockAndReset(
    clock = clock_distribution.io.control_clock,
    reset = reset
  ) {
    Module(new AxiMasterReader)
  }

  axi_mreader.io.bus <> m_axi_bank_0

  axi_mreader.io.user.base_addr := slave.io.pointer_regs.pointer_0

  // gateway.io.wen := manticore.io.memory_backend.wen
  // gateway.io.addr := manticore.io.memory_backend.addr // memory uses short offset (short-addressable)
  // gateway.io.wdata                  := manticore.io.memory_backend.wdata
  // gateway.io.ap_start               := manticore.io.memory_backend.start
  // manticore.io.memory_backend.done  := gateway.io.ap_done
  // manticore.io.memory_backend.rdata := gateway.io.ap_return
  // manticore.io.memory_backend.idle  := gateway.io.ap_idle

  axi_mreader.io.user.read_start := manticore.io.memory_backend.start & (~manticore.io.memory_backend.wen)
  axi_mreader.io.user.raddr := manticore.io.memory_backend.addr // we address the memory using short addresse

  manticore.io.memory_backend.done  := axi_mreader.io.user.read_done
  manticore.io.memory_backend.rdata := axi_mreader.io.user.rdata
  manticore.io.memory_backend.idle  := axi_mreader.io.user.read_idle
  manticore.io.host_registers       := slave.io.host_regs

  slave.io.dev_regs := manticore.io.device_registers

  manticore.io.start := slave.io.control.ap_start

  slave.io.control.ap_done  := manticore.io.done
  slave.io.control.ap_idle  := manticore.io.idle
  slave.io.control.ap_ready := manticore.io.done // is this correct?

}

class ManticoreFlatSimKernel(
    DimX: Int,
    DimY: Int,
    debug_enable: Boolean = false,
    prefix_path: String = "./"
) extends Module {

  clock.suggestName("ap_clk")
  reset.suggestName("ap_rst")

  class KernelRegisters extends Bundle {
    val host   = Input(new HostRegisters(ManticoreFullISA))
    val device = Output(new DeviceRegisters(ManticoreFullISA))
  }
  class KernelControl extends Bundle {
    val start: Bool = Input(Bool())
    val done: Bool  = Output(Bool())
    val idle: Bool  = Output(Bool())
  }

  class DirectMemoryInterface extends Bundle {
    val rdata: UInt  = Output(UInt(16.W))
    val wdata: UInt  = Input(UInt(16.W))
    val locked: Bool = Input(Bool())
    val wen: Bool    = Input(Bool())
    val addr: UInt   = Input(UInt(64.W))
  }

  class KernelInterface extends Bundle {
    val kernel_registers = new KernelRegisters
    val kernel_ctrl      = new KernelControl
    val dmi              = new DirectMemoryInterface
  }

  val io = IO(new KernelInterface)

  val clock_distribution = Module(new ClockDistribution())

  clock_distribution.io.root_clock := clock

  val manticore =
    Module(new ManticoreFlatArray(DimX, DimY, debug_enable, prefix_path))

  manticore.io.reset         := reset
  manticore.io.control_clock := clock_distribution.io.control_clock
  manticore.io.compute_clock := clock_distribution.io.compute_clock

  manticore.io.host_registers := io.kernel_registers.host
  io.kernel_registers.device  := manticore.io.device_registers

  manticore.io.start  := io.kernel_ctrl.start
  io.kernel_ctrl.done := manticore.io.done
  io.kernel_ctrl.idle := manticore.io.idle

  clock_distribution.io.compute_clock_en_n := manticore.io.clock_inactive

  val gateway: MemoryGatewaySim = withClockAndReset(
    clock = clock_distribution.io.control_clock,
    reset = reset
  ) {
    Module(new MemoryGatewaySim(prefix_path + "/exec.dat"))
  }

  gateway.io.memory_pointer := 0xffff.U

  gateway.io.locked := io.dmi.locked

  when(io.dmi.locked) {
    gateway.io.wen   := io.dmi.wen
    gateway.io.wdata := io.dmi.wdata
    gateway.io.addr  := io.dmi.addr
  } otherwise {
    gateway.io.wen := manticore.io.memory_backend.wen
    gateway.io.addr := manticore.io.memory_backend.addr // short-addressable memory
    gateway.io.wdata := manticore.io.memory_backend.wdata
  }

  gateway.io.ap_start               := manticore.io.memory_backend.start
  manticore.io.memory_backend.done  := gateway.io.ap_done
  manticore.io.memory_backend.rdata := gateway.io.ap_return
  manticore.io.memory_backend.idle  := gateway.io.ap_idle

  io.dmi.rdata := gateway.io.ap_return
}

object ManticoreFlatSimKernelGen extends App {

  new ChiselStage().emitVerilog(
    new ManticoreFlatSimKernel(2, 2, true, "haha"),
    Array("--target-dir", "gen-dir/sim2x2")
  )

}

object PackageKernel {

  def apply(
      verilog_path: Path,
      packaged_path: Path,
      temp_kernel_package_path: Path,
      scripts_path: Path,
      kernel_xml_path: Path,
      xo_dir: Path,
      top_name: String = "ManticoreKernel",
      target: String = "hw",
      platform: String = "xilinx_u250_gen3x16_xdma_3_1_202020_1",
      slave_interface: String = "s_axi_control",
      master_interface: Seq[String] = Seq.tabulate(4) { i =>
        "m_axi_bank_" + i
      }
  ) = {

    val package_directory_path = packaged_path
    val temp_project_path      = temp_kernel_package_path
    val verilog_directory      = verilog_path
    val substitutions = Map(
      "@VERILOG_PATH@" ->
        verilog_directory.toAbsolutePath().toString(),
      "@PACKAGED_PATH@" ->
        package_directory_path.toAbsolutePath().toString(),
      "@TEMP_KERNEL_PACKAGE_PATH@" ->
        temp_project_path.toAbsolutePath().toString(),
      "@KERNEL_XML@" ->
        kernel_xml_path.toAbsolutePath().toString(),
      "@SLAVE_INTERFACE@" -> slave_interface
    ) ++ master_interface.zipWithIndex.map { case (m, i) =>
      ("@MASTER_INTERFACE_" + i + "@") -> m
    }.toMap

    def substitute(line: String, subst: Map[String, String]): String = {
      subst
        .foldLeft(line) { case (l, (k, v)) => l.replace(k, v) }
    }

    Files.createDirectories(scripts_path)

    def createTclScript(resource: String, fixer: String => String) = {
      val fp = scripts_path.resolve(resource + ".tcl")
      val tcl_commands = scala.io.Source
        .fromResource(s"hls/${resource}.tcl")
        .getLines()
        .map(fixer)
        .mkString("\n")
      val writer = Files.newBufferedWriter(fp)
      writer.write(tcl_commands)
      writer.close()
      fp
    }

    val packaging_tcl_fp =
      createTclScript("package_kernel", line => substitute(line, substitutions))
    val gen_xo_tcl_fp = createTclScript(
      "gen_xo",
      line =>
        substitute(
          line,
          substitutions ++ Map(
            "@PACKAGE_KERNEL_TCL@" -> packaging_tcl_fp
              .toAbsolutePath()
              .toString()
          )
        )
    )

    import scala.sys.process._

    if (
      "which vivado".!(
        ProcessLogger(l => println("which vivado gives: " + l))
      ) != 0
    ) {
      throw new Exception(
        "vivado not found, make sure vivado related environment variables are set!"
      )
    }

    Files.createDirectories(xo_dir)
    val xo_file =
      xo_dir.resolve(top_name + "." + target + "." + platform + ".xo")
    // remove the outpu path files if they already exist
    if (Files.exists(package_directory_path)) {
      scala.reflect.io
        .Directory(package_directory_path.toFile())
        .deleteRecursively()
    }
    if (Files.exists(temp_project_path)) {
      scala.reflect.io.Directory(temp_project_path.toFile()).deleteRecursively()
    }
    val success =
      sys.process
        .Process(
          command =
            s"vivado -mode batch -source ${gen_xo_tcl_fp.toAbsolutePath().toString()} -tclargs " +
              s"${xo_file.toAbsolutePath().toString} ${top_name} ${target} ${platform}",
          cwd = xo_dir.toFile()
        )
        .!(ProcessLogger(println(_))) == 0
    if (!success) {
      throw new Exception("Failed to package as XO")
    }
    xo_file

  }
}

object BuildXclbin {

  def apply(
      bin_dir: Path,
      xo_path: Path,
      top_name: String = "ManticoreKernel",
      target: String = "hw",
      platform: String = "xilinx_u250_gen3x16_xdma_3_1_202020_1"
  ) = {

    import scala.sys.process._

    if (
      "which vivado".!(
        ProcessLogger(l => println("which vivado gives: " + l))
      ) != 0
    ) {
      throw new Exception(
        "vivado not found, make sure vivado related environment variables are set!"
      )
    }

    Files.createDirectories(bin_dir)

    // connect each axi port to a single DDR bank.
    // val mem_bank_config = mem_if.zipWithIndex
    //   .map { case (m, ix) =>
    //     s"--connectivity.sp ${top_name}_1.${m.portName}:DDR[${ix}]"
    //   }
    //   .mkString(" ")
    val mem_bank_config = " "
    // clock is defaulted to 250MHz with 200MHz tolernace, i.e., it can go
    // as high as 450MHz and as low as 50MHz
    val clock_constraint =
      "--clock.defaultFreqHz 200000000 " +
        "--clock.defaultTolerance 0.1 "
    val xclbin_path =
      bin_dir.resolve(s"${top_name}.${target}.${platform}.xclbin")
    // val command =
    //   s"v++ --link -g -t ${target} --platform ${platform} --save-temps " +
    //     " --to_step vpl.impl  --vivado.synth.jobs 20 --vivado.impl.jobs 16 " +
    //     s"${mem_bank_config} ${clock_constraint} -o ${xclbin_path.toAbsolutePath.toString} " +
    //     s"${xo_path.toAbsolutePath.toString}"
    val command =
      s"v++ --link -g -t ${target} --platform ${platform} --save-temps " +
        "--vivado.synth.jobs 20 --vivado.impl.jobs 16 " +
        s"${mem_bank_config} ${clock_constraint} -o ${xclbin_path.toAbsolutePath.toString} " +
        s"${xo_path.toAbsolutePath.toString}"

    println(s"Executing:\n${command}")
    val success = Process(command = command, cwd = bin_dir.toFile())
      .!(ProcessLogger(println(_))) == 0
    if (!success)
      println("v++ failed!")
  }
}

object ManticoreKernelGenerator {

  object KernelGenerationSteps extends Enumeration {
    val Type                                   = Value
    val Slave, Master, Kernel, Xml, Xo, Xclbin = Value
  }

  val platformDevice = Map(
    "xilinx_u250_gen3x16_xdma_3_1_202020_1" -> "xcu250-figd2104-2l-e",
    "xilinx_u200_gen3x16_xdma_base_1"       -> "xcu200-fsgd2104-2-e"
  )

  def apply(
      target_dir: String,
      platform: String = "xilinx_u250_gen3x16_xdma_3_1_202020_1",
      target: String = "hw_emu",
      dimx: Int = 8,
      dimy: Int = 8
  ) = {

    val out_dir = Paths.get(target_dir)

    // def deleteOrAbort(): Boolean = {

    //   scala.io.StdIn.readLine(
    //     s"Directory already exists. Delete it [Y/n]?"
    //   ) match {
    //     case "Yes" | "YES" | "yes" | "Y" | "y" =>
    //       // scala.reflect.io.Directory(out_dir.toFile()).deleteRecursively()
    //       println("You said yes")
    //       false
    //     case _ =>
    //       println("Aborting")
    //       true
    //   }
    // }

    val hdl_dir = Files.createDirectories(out_dir.resolve("hdl"))

    // generate axi master module
    // val master_fp = AxiMasterGenerator(
    //   name = "memory_gateway",
    //   // out_dir = Some(hdl_dir),
    //   part_num = platformDevice(platform)
    // )
    // println("Created axi master module")

    println("generating top module")
    new ChiselStage().emitVerilog(
      new ManticoreFlatKernel(
        DimX = dimx,
        DimY = dimy,
        debug_enable = true
      ),
      Array("--target-dir", hdl_dir.toAbsolutePath().toString())
    )

    val xml_path   = hdl_dir.resolve("kernel.xml")
    val xml_writer = Files.newBufferedWriter(hdl_dir.resolve("kernel.xml"))
    xml_writer.write(scala.io.Source.fromResource("hls/kernel.xml").mkString)
    xml_writer.close()

    val xo_file = PackageKernel(
      verilog_path = hdl_dir,
      packaged_path = out_dir.resolve("packaged"),
      temp_kernel_package_path = out_dir.resolve("temp_packaged"),
      scripts_path = out_dir.resolve("scripts"),
      kernel_xml_path = xml_path,
      xo_dir = out_dir.resolve("bin")
    )
    println("Created Xilinx Object")

    BuildXclbin(
      bin_dir = out_dir.resolve("bin"),
      xo_path = xo_file,
      target = target
    )

  }

  // private def makeAxiSlaveRegisters() = AxiRegisterBuilder.apply()

}

object KernelTest extends App {
  val dimx = 16
  val dimy = 16
  val out_dir =
    Paths.get(
      s"gen-dir/kernel/${dimx}x${dimy}_100MHz_to_200MHz_8KiB/hw"
    )

  ManticoreKernelGenerator(
    target_dir = out_dir.toAbsolutePath().toString(),
    dimx = dimx,
    dimy = dimy,
    target = "hw"
  )
  // val master_fp = AxiMasterGenerator(
  //   name = "memory_gateway"
  //   // out_dir = Some(hdl_dir),
  // )
  // new ChiselStage().emitVerilog(
  //   new ManticoreKernel(
  //     4,
  //     4,
  //     true,
  //     master_fp.map(_.toAbsolutePath().toString())
  //   ),
  //   Array("-td", "gen-dir/sim/hdl")
  // )

}
