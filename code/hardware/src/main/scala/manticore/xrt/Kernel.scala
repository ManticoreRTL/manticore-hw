package manticore.xrt

import chisel3._
import manticore.core.HostRegisters
import manticore.ManticoreFullISA
import manticore.core.DeviceRegisters
import manticore.core.ManticoreArray
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

class MemoryPointers extends Bundle {
  val pointer_0: UInt  = UInt(64.W)
  val pointer_1: UInt  = UInt(64.W)
  val pointer_2: UInt  = UInt(64.W)
  val pointer_3: UInt  = UInt(64.W)
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

class ManticoreKernel(
    DimX: Int,
    DimY: Int,
    debug_enable: Boolean = false,
    m_axi_path: Seq[String] = Seq() // path to m_axi implementation if exits
) extends MultiIOModule {

  val m_axi_bank    = IO(Vec(4, new AxiMasterInterface))
  val s_axi_control = IO(new AxiSlave.AxiSlaveCorenterface())
  val interrupt     = IO(Output(Bool()))

  val slave = Module(new AxiSlave(ManticoreFullISA))

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

  val master_gateway = Module(new MemoryGateWay(m_axi_path))

  val gateways = master_gateway +: Seq.fill(3) {
    Module(master_gateway.makeCopy())
  }

  

  // connect the outer axi master interface bundles to the gateway interfaces
  gateways.zip(m_axi_bank).foreach { case (g, io) =>
    g.io.m_axi_gmem <> io
  }

  // connect the memory_pointer values to the gateways that comes from the host(base addresses)

  gateways(0).io.memory_pointer := slave.io.pointer_regs.pointer_0
  gateways(1).io.memory_pointer := slave.io.pointer_regs.pointer_1
  gateways(2).io.memory_pointer := slave.io.pointer_regs.pointer_2
  gateways(3).io.memory_pointer := slave.io.pointer_regs.pointer_3
 

  val manticore = Module(new ManticoreArray(DimX, DimY, debug_enable))

  // connect the caches to the memory gateways
  manticore.io.cache_backend.zip(gateways).foreach { case (cache, memory) =>
    memory.io.cmd      := cache.cmd
    memory.io.raddr    := cache.raddr
    memory.io.waddr    := cache.waddr
    memory.io.wline    := cache.wline
    memory.io.ap_start := cache.start
    cache.done         := memory.io.ap_done
    cache.rline        := memory.io.ap_return
  }

  
  manticore.io.host_registers := slave.io.host_regs

  slave.io.dev_regs:= manticore.io.device_registers

  manticore.io.start := slave.io.control.ap_start

  slave.io.control.ap_done  := manticore.io.done
  slave.io.control.ap_idle  := manticore.io.idle
  slave.io.control.ap_ready := manticore.io.done // is this correct?

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
      mem_if: Seq[KernelInfo.KernelMemoryInterface],
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

    val mem_bank_config = mem_if.zipWithIndex
      .map { case (m, ix) =>
        s"--connectivity.sp ${top_name}_1.${m.portName}:DDR[${ix}]"
      }
      .mkString(" ")
    val xclbin_path =
      bin_dir.resolve(s"${top_name}.${target}.${platform}.xclbin")
    val command =
      s"v++ --link -g -t ${target} --platform ${platform} --save-temps " +
        s"${mem_bank_config} -o ${xclbin_path.toAbsolutePath.toString} " +
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

  private val platformDevice = ListMap(
    "xilinx_u250_gen3x16_xdma_3_1_202020_1" -> "xcu250-figd2104-2l-e"
  )

  def apply(target_dir: String, platform: String = platformDevice.head._1) = {

    val out_dir = Paths.get(target_dir)
    def deleteOrAbort(): Boolean = {

      scala.io.StdIn.readLine(
        s"Directory already exists. Delete it [Y/n]?"
      ) match {
        case "Yes" | "YES" | "yes" | "Y" | "y" =>
          // scala.reflect.io.Directory(out_dir.toFile()).deleteRecursively()
          println("You said yes")
          false
        case _ =>
          println("Aborting")
          true
      }
    }

    val hdl_dir = Files.createDirectories(out_dir.resolve("hdl"))

    // generate axi master module
    val master_fp = AxiMasterGenerator(
      name = "memory_gateway",
      // out_dir = Some(hdl_dir),
      part_num = platformDevice(platform)
    )
    println("Created axi master module")

    val mem_if = Seq.tabulate(4) { i =>
      KernelInfo.KernelMemoryInterface("m_axi", "bank_" + i, 256)
    }

    println("generating top module")
    new ChiselStage().emitVerilog(
      new ManticoreKernel(
        DimX = 2,
        DimY = 2,
        debug_enable = true,
        m_axi_path = master_fp.map(_.toAbsolutePath().toString())
      ),
      Array("--target-dir", hdl_dir.toAbsolutePath().toString())
    )

    
    val xml_path = hdl_dir.resolve("kernel.xml")
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
      target = "hw_emu",
      mem_if = Seq.tabulate(4) { i =>
        KernelInfo.KernelMemoryInterface("m_axi", "bank_" + i, 256)
      }
    )

  }

  // private def makeAxiSlaveRegisters() = AxiRegisterBuilder.apply()

}

object KernelTest extends App {
  val out_dir = Paths.get("gen-dir/kernel/01")
  ManticoreKernelGenerator(
    out_dir.toAbsolutePath().toString()
  )
  // val out_dir = Paths.get("gen-dir/kernel/01")

  // val xo_file = PackageKernel(
  //   verilog_path = out_dir.resolve("hdl"),
  //   packaged_path = out_dir.resolve("packaged"),
  //   temp_kernel_package_path = out_dir.resolve("temp_packaged"),
  //   scripts_path = out_dir.resolve("scripts"),
  //   kernel_xml_path = out_dir.resolve("hdl").resolve("kernel.xml"),
  //   xo_dir = out_dir.resolve("bin"),
  //   target = "hw_emu"
  // )

  // BuildXclbin(
  //   bin_dir = out_dir.resolve("bin"),
  //   xo_path = xo_file,
  //   target = "hw_emu",
  //   mem_if = Seq.tabulate(4) { i =>
  //     KernelInfo.KernelMemoryInterface("m_axi", "bank_" + i, 256)
  //   }
  // )

}
