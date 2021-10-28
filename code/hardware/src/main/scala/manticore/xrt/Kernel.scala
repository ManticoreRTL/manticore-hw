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
  val pointer_0_low: UInt  = UInt(32.W)
  val pointer_0_high: UInt = UInt(32.W)
  val pointer_1_low: UInt  = UInt(32.W)
  val pointer_1_high: UInt = UInt(32.W)
  val pointer_2_low: UInt  = UInt(32.W)
  val pointer_2_high: UInt = UInt(32.W)
  val pointer_3_low: UInt  = UInt(32.W)
  val pointer_3_high: UInt = UInt(32.W)
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
    m_axi_path: Seq[String] = Seq(), // path to m_axi implementation if exits
    s_axi_port_map: Option[
      (
          Map[AxiSlaveGenerator.AxiRegister, Seq[
            AxiSlaveGenerator.AxiRegisterMapEntry
          ]],
          String
      )
    ] = None // path to s_axi implementation if exits
) extends MultiIOModule {

  val m_axi_bank    = IO(Vec(4, new AxiMasterInterface))
  val s_axi_control = IO(new AxiSlaveInterface)
  val interrupt     = IO(Output(Bool()))

  val slave = Module(new AxiHlsSlave(s_axi_port_map))

  slave.io.AWADDR       := s_axi_control.AWADDR
  slave.io.AWVALID      := s_axi_control.AWVALID
  s_axi_control.AWREADY := slave.io.AWREADY
  slave.io.WDATA        := s_axi_control.WDATA
  slave.io.WSTRB        := s_axi_control.WSTRB
  slave.io.WVALID       := s_axi_control.WVALID
  s_axi_control.WREADY  := slave.io.WREADY
  s_axi_control.BRESP   := slave.io.BRESP
  s_axi_control.BVALID  := slave.io.BVALID
  slave.io.BREADY       := s_axi_control.BREADY
  slave.io.ARADDR       := s_axi_control.ARADDR
  slave.io.ARVALID      := s_axi_control.ARVALID
  s_axi_control.ARREADY := slave.io.ARREADY
  s_axi_control.RDATA   := slave.io.RDATA
  s_axi_control.RRESP   := slave.io.RRESP
  s_axi_control.RVALID  := slave.io.RVALID
  slave.io.RREADY       := s_axi_control.RREADY
  interrupt             := slave.io.interrupt

  val master_gateway = Module(new MemoryGateWay(m_axi_path))

  val gateways = master_gateway +: Seq.fill(3) {
    Module(master_gateway.makeCopy())
  }

  val manticore_done = Wire(Bool())

  // connect the outer axi master interface bundles to the gateway interfaces
  gateways.zip(m_axi_bank).foreach { case (g, io) =>
    g.io.m_axi_gmem <> io
  }

  // connect the memory_pointer values to the gateways that comes from the host(base addresses)

  gateways(0).io.memory_pointer := Cat(
    slave.io.pregs.pointer_0_high,
    slave.io.pregs.pointer_0_low
  )
  gateways(1).io.memory_pointer := Cat(
    slave.io.pregs.pointer_1_high,
    slave.io.pregs.pointer_1_low
  )
  gateways(2).io.memory_pointer := Cat(
    slave.io.pregs.pointer_2_high,
    slave.io.pregs.pointer_2_low
  )
  gateways(3).io.memory_pointer := Cat(
    slave.io.pregs.pointer_3_high,
    slave.io.pregs.pointer_3_low
  )

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

  // manticore.io.device_registers := slave.io.dregs
  manticore.io.host_registers := slave.io.hregs

  manticore.io.start := slave.io.ap_start

  val device_registers: ListMap[String, UInt] =
    manticore.io.device_registers.elements.flatMap { case (n, t) =>
      // get a mapping from implementation names to the chisel data and implementation offsets
      // i.e., "virtual_cycles" -> manticore.io.device_registers.virtual_cycles
      // or    "exception_id_2" -> manticore.io.device_registers.exception_id(2)
      AxiRegisterBuilder.makeName(n, t) match {
        case Seq(impl_name) => // t is UInt type
          Seq(impl_name -> t.asInstanceOf[UInt])
        case ls @ _ => // t is Vec[UInt] type
          ls.zipWithIndex.map { case (impl_name, ix) =>
            t match {
              case vec: Vec[_] =>
                (impl_name, vec(ix).asInstanceOf[UInt])
              case _ =>
                throw new MatchError(
                  "device registers should only be UInt or Vec[UInt]"
                )
            }
          }
      }
    }

  // create a sorted map from offsets to signals (with names)
  // i.e., 0x4c -> (exception_id_0, device_registers.io.execption_id(0)(31, 0))
  //       0x74 -> (virtual_cycles, device_registers.io.virtual_cycles(31, 0))
  //       0x78 -> (virtual_cycles, device_registers.io.virtual_cycles(63, 32))
  val offsets_to_data =
    device_registers
      .flatMap { case (name, chisel_type) =>
        slave.getOffset(name) match {
          case Some(offset_seq) =>
            offset_seq.sorted match {
              case Seq(x) => //
                assert(chisel_type.getWidth <= 32)
                Seq(x -> (name, chisel_type(chisel_type.getWidth - 1, 0)))
              case Seq(lo, hi) =>
                assert(chisel_type.getWidth <= 64 && chisel_type.getWidth > 32)
                Seq(
                  lo -> (name, chisel_type(31, 0)),
                  hi -> (name, chisel_type(chisel_type.getWidth - 1, 32))
                )
              case _ =>
                throw new UnsupportedOperationException(
                  s"Only up to 64-bit registers are supported! Can not have ${name} with width ${chisel_type.getWidth} bits"
                )
            }
          case None =>
            throw new NoSuchElementException(
              s"Could not find the offset sequence for device registers ${name}"
            )
        }
      }
      .toSeq
      .sortBy { case (offset, _) => offset }

  val device_reg_vec = Wire(Vec(offsets_to_data.size, UInt(32.W)))
  device_reg_vec
    .zip(offsets_to_data.map { case (offset, (name, data)) => data })
    .foreach { case (wire, data) =>
      wire := data
    }

  val offset_vec = Wire(
    Vec(offsets_to_data.size, UInt(log2Ceil(offsets_to_data.size).W))
  )

  offset_vec.zip(offsets_to_data.map { case (offset, _) => offset }).foreach {
    case (wire, value) =>
      wire := value.U
  }
  // when the Manticore is done, start writing device registers to the axi slave
  // val slave_offsets_map = slave.io.dregs.elements

  //   .map { case (pname, _) =>
  //     slave.getOffset(pname)
  //   }
  //   .zip(manticore.io.device_registers.elements)
  //   .map { case (offsets, (name, data)) =>
  //     require(
  //       offsets.nonEmpty,
  //       s"device register ${name} does not have a offset in axi slave implementation"
  //     )
  //     offsets.zipWithIndex.map { case (v, ix) =>
  //       val range_high = data.getWidth.min((ix + 1) * 32)
  //       def extract_off
  //       data match {
  //         case vec: Vec[_] =>
  //         case elem: UInt  =>
  //       }
  //       data(range_high, ix * 32)
  //     }
  //   }

  // val num_offsets = slave_offsets_map.flatMap(_._1.get).size
  // require(
  //   slave_offsets_map.forall { case (offset, _) => offset.nonEmpty },
  //   "All device registers should have defined s_axi slave offsets!"
  // )

  // val device_reg_values =
  //   Vec(slave_offsets_map.flatMap(_._1.get).size, UInt(32.W))

  val slave_id = Reg(UInt(log2Ceil(offsets_to_data.size).W))

  val ap_start_reg = Reg(Bool())
  ap_start_reg := slave.io.ap_start
  val ap_start_pulse = slave.io.ap_start & (!ap_start_reg)

  object WriteState extends ChiselEnum {
    val WaitForStart, WaitForDone, WaitForAwReady, WaitForWReady, WaitForBValid,
        PulseDone =
      Value
  }

  // require(offsets_to_data.size > 0, "Should have at least one device register!")
  val state = RegInit(WriteState.Type(), WriteState.WaitForStart)

  switch(state) {
    is(WriteState.WaitForStart) {
      when(ap_start_pulse) {
        state    := WriteState.WaitForDone
        slave_id := 0.U
      }
    }
    is(WriteState.WaitForDone) {
      when(manticore.io.done) {
        if (offsets_to_data.size > 0) {
          state := WriteState.WaitForDone
        } else {
          state := WriteState.PulseDone
        }
      }
    }
    is(WriteState.WaitForAwReady) {
      when(slave.io.AWREADY) {
        state := WriteState.WaitForWReady
      }
    }
    is(WriteState.WaitForWReady) {
      when(slave.io.WREADY) {
        state := WriteState.WaitForBValid
      }
    }
  }
  slave.io.ap_done  := manticore.io.done
  slave.io.ap_idle  := manticore.io.idle
  slave.io.ap_ready := manticore.io.done // is this correct?

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

    // val mem_bank_config = mem_if.zipWithIndex
    //   .map { case (m, ix) =>
    //     s"--connectivity.sp ${top_name}.${m.portName}:DDR[${ix}]"
    //   }
    //   .mkString(" ")
    val mem_bank_config = ""
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

    val (hregs, dregs, pregs) = makeAxiSlaveRegisters()
    // generate axi slave module
    val (port_map, slave_fp) = AxiSlaveGenerator(
      hregs ++ dregs ++ pregs,
      // hdl_dir.toAbsolutePath().toString(),
      platformDevice(platform)
    )

    println("Created axi slave module")
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

    val slave_regs_args = {
      pregs.zip(mem_if).map { case (r, mem) =>
        KernelInfo.KernelSlaveRegister(
          id = 0,
          name = r.prefix + "_" + r.name,
          offset = port_map(r).sortBy(_.hw_name).head.offset,
          cpp_type = r.cppType,
          is_pointer = true,
          port_interface = mem.portName
        )
      } ++ (hregs ++ dregs).map { r =>
        KernelInfo.KernelSlaveRegister(
          id = 0,
          name = r.prefix + "_" + r.name,
          offset = port_map(r).sortBy(_.hw_name).head.offset,
          cpp_type = r.cppType,
          is_pointer = false,
          port_interface = "s_axi_control"
        )
      }
    }.zipWithIndex.map { case (arg, i) => arg.withId(i) }

    println("generating top module")
    new ChiselStage().emitVerilog(
      new ManticoreKernel(
        DimX = 2,
        DimY = 2,
        debug_enable = true,
        m_axi_path = master_fp.map(_.toAbsolutePath().toString()),
        s_axi_port_map = Some(
          (port_map, slave_fp.toAbsolutePath().toString())
        )
      ),
      Array("--target-dir", hdl_dir.toAbsolutePath().toString())
    )

    val xml_file = hdl_dir.resolve("kernel.xml")
    KernelXmlGenrator(
      "ManticoreKernel",
      mem_if,
      slave_regs_args,
      Some(xml_file)
    )

    val xo_file = PackageKernel(
      verilog_path = hdl_dir,
      packaged_path = out_dir.resolve("packaged"),
      temp_kernel_package_path = out_dir.resolve("temp_packaged"),
      scripts_path = out_dir.resolve("scripts"),
      kernel_xml_path = xml_file,
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

  private def makeAxiSlaveRegisters() = AxiRegisterBuilder.apply()

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
