package manticore.xrt

import java.nio.file.{Files, StandardCopyOption}
import java.io.PrintWriter
import java.nio.file.Path
import java.io.File

import chisel3._
import chisel3.util.HasBlackBoxPath

object AxiMasterGenerator {

  import scala.sys.process._

  private def hasVititsHls = {
    "which vitis_hls".! == 0
  }

  def apply(
      name: String,
      outdir: String = "",
      part_num: String = "xcu250-figd2104-2l-e"
  ) = {

    if (!hasVititsHls) {
      throw new Exception(
        "vitis_hls not found, please ensure all Vivado related environment variables are set"
      )
    }

    def writeCpp() = {
      // write the CPP implementation in file
      val template = scala.io.Source
        .fromResource("hls/memory_gateway.cpp")
        .getLines()
        .map { l =>
          l.replace("@NAME@", name)
        }
        .mkString("\n")

      println(template)

      val hls_source_file =
        Files.createTempFile(
          "manticore_memory_gate_way_",
          s"_${name}.cpp"
        )

      val hls_source_writer = new PrintWriter(hls_source_file.toFile())
      hls_source_writer.write(template)
      hls_source_writer.close()
      hls_source_file.toFile()

    }
    def writeTcl(cpp_source: File) = {

      val hls_synth_script = scala.io.Source
        .fromResource("hls/synth.tcl")
        .getLines()
        .map { line =>
          line
            .replace("@FILE@", cpp_source.toPath().toAbsolutePath().toString())
            .replace("@NAME@", name)
            .replace("@PART@", part_num)
        }
        .mkString("\n")

      val synth_source_file =
        Files.createTempFile("synth___", s"___${name}.tcl")

      val synth_source_writer = new PrintWriter(synth_source_file.toFile())

      synth_source_writer.write(hls_synth_script)
      synth_source_writer.close()
      synth_source_file.toFile()
    }

    val cpp_file = writeCpp()
    println(s"Created cpp source in ${cpp_file.toPath().toAbsolutePath()}")
    val tcl_file = writeTcl(cpp_file)
    println(
      s"Created synthesis script in ${tcl_file.toPath().toAbsolutePath()}"
    )
    val project_dir = Files.createTempDirectory(s"${name}___")
    println(s"Creatig project directory in ${project_dir.toAbsolutePath()}")

    if (
      sys.process
        .Process(
          command = s"vitis_hls -f ${tcl_file.toPath().toAbsolutePath()}",
          cwd = project_dir.toFile()
        )
        .!(ProcessLogger(line => println(line))) != 0
    ) {
      throw new Exception("Synthesis failed!")
    }

    val inner_module = scala.io.Source
      .fromFile(
        project_dir
          .resolve(
            s"${name}/solution/syn/verilog/${name}_gmem_m_axi.v"
          )
          .toFile()
      )

    val outer_module = scala.io.Source.fromFile(
      project_dir.resolve(s"${name}/solution/syn/verilog/${name}.v").toFile()
    )

    val combined_file =
      Files.createTempFile(s"${name}_", "_combined.v")
    val file_writer = new PrintWriter(
      combined_file.toFile()
    )
    file_writer.print((outer_module ++ inner_module).mkString(""))
    file_writer.close()

    if (outdir.nonEmpty) {
      val output_dir = Files.createDirectories(new File(outdir).toPath())
      val f = Files.copy(
        combined_file,
        output_dir.resolve(s"${name}.v"),
        StandardCopyOption.REPLACE_EXISTING
      )

      println(
        s"Verilog files saved to\n\t${f.toAbsolutePath.toString()}\n\t"
      )
      f
    } else {
      combined_file
    }

  }

}

class MemoryGateWayInterface(
    AxiIdWidth: Int = 1,
    AxiAddrWidth: Int = 64,
    AxiGMemDataWidth: Int = 256,
    AxiAwUserWidth: Int = 1,
    AxiArUserWidth: Int = 1,
    AxiWUserWidth: Int = 1,
    AxiRUserWidth: Int = 1,
    AxiBUserWIdth: Int = 1,
    AxiUserValue: Int = 0,
    AxiProtValue: Int = 0,
    AxiCacheValue: Int = 3,
    AxiDataWidth: Int = 32,
    AxiGMemWStrbWidth: Int = 256 / 8,
    AxiWStrbWidth: Int = 32 / 8
) extends Bundle {

  // val ap_clk             = Input(Clock())
  val ap_rst_n           = Input(Bool())
  val ap_start           = Input(Bool())
  val m_axi_gmem_AWREADY = Input(Bool())
  val m_axi_gmem_WREADY  = Input(Bool())
  val m_axi_gmem_ARREADY = Input(Bool())
  val m_axi_gmem_RVALID  = Input(Bool())
  val m_axi_gmem_RDATA   = Input(UInt(AxiGMemDataWidth.W))
  val m_axi_gmem_RLAST   = Input(Bool())
  val m_axi_gmem_RID     = Input(UInt(AxiIdWidth.W))
  val m_axi_gmem_RUSER   = Input(UInt(AxiRUserWidth.W))
  val m_axi_gmem_RRESP   = Input(UInt(2.W))
  val m_axi_gmem_BVALID  = Input(Bool())
  val m_axi_gmem_BRESP   = Input(UInt(2.W))
  val m_axi_gmem_BID     = Input(UInt(AxiIdWidth.W))
  val m_axi_gmem_BUSER   = Input(UInt(AxiBUserWIdth.W))
  val memory_pointer     = Input(UInt(64.W))
  val raddr              = Input(UInt(64.W))
  val waddr              = Input(UInt(64.W))
  val cmd                = Input(UInt(8.W))
  val wline              = Input(UInt(256.W))

  val ap_done             = Output(Bool())
  val ap_idle             = Output(Bool())
  val ap_ready            = Output(Bool())
  val m_axi_gmem_AWVALID  = Output(Bool())
  val m_axi_gmem_AWADDR   = Output(UInt(AxiAddrWidth.W))
  val m_axi_gmem_AWID     = Output(UInt(AxiIdWidth.W))
  val m_axi_gmem_AWLEN    = Output(UInt(8.W))
  val m_axi_gmem_AWSIZE   = Output(UInt(3.W))
  val m_axi_gmem_AWBURST  = Output(UInt(2.W))
  val m_axi_gmem_AWLOCK   = Output(UInt(2.W))
  val m_axi_gmem_AWCACHE  = Output(UInt(4.W))
  val m_axi_gmem_AWPROT   = Output(UInt(3.W))
  val m_axi_gmem_AWQOS    = Output(UInt(4.W))
  val m_axi_gmem_AWREGION = Output(UInt(4.W))
  val m_axi_gmem_AWUSER   = Output(UInt(AxiAwUserWidth.W))
  val m_axi_gmem_WVALID   = Output(Bool())
  val m_axi_gmem_WDATA    = Output(UInt(AxiGMemDataWidth.W))
  val m_axi_gmem_WSTRB    = Output(UInt(AxiGMemWStrbWidth.W))
  val m_axi_gmem_WLAST    = Output(Bool())
  val m_axi_gmem_WID      = Output(UInt(AxiIdWidth.W))
  val m_axi_gmem_WUSER    = Output(UInt(AxiWUserWidth.W))
  val m_axi_gmem_ARVALID  = Output(Bool())
  val m_axi_gmem_ARADDR   = Output(UInt(AxiAddrWidth.W))
  val m_axi_gmem_ARID     = Output(UInt(AxiIdWidth.W))
  val m_axi_gmem_ARLEN    = Output(UInt(8.W))
  val m_axi_gmem_ARSIZE   = Output(UInt(3.W))
  val m_axi_gmem_ARBURST  = Output(UInt(2.W))
  val m_axi_gmem_ARLOCK   = Output(UInt(2.W))
  val m_axi_gmem_ARCACHE  = Output(UInt(4.W))
  val m_axi_gmem_ARPROT   = Output(UInt(3.W))
  val m_axi_gmem_ARQOS    = Output(UInt(4.W))
  val m_axi_gmem_ARREGION = Output(UInt(4.W))
  val m_axi_gmem_ARUSER   = Output(UInt(AxiArUserWidth.W))
  val m_axi_gmem_RREADY   = Output(Bool())
  val m_axi_gmem_BREADY   = Output(Bool())
  val ap_return           = Output(UInt(256.W))

}

class MemoryGateWay extends Module {

  val io = IO(new MemoryGateWayInterface)
  class memory_gateway extends BlackBox with HasBlackBoxPath {
    val io = IO(new MemoryGateWayInterface {
      val ap_clk = Input(Clock())
    })
    // generate the black box implementation
    val path = AxiMasterGenerator(this.getClass().getName())
    addPath(path.toAbsolutePath().toString())

  }

  val impl = Module(new memory_gateway)

  impl.io.ap_rst_n           := io.ap_rst_n
  impl.io.ap_start           := io.ap_start
  impl.io.m_axi_gmem_AWREADY := io.m_axi_gmem_AWREADY
  impl.io.m_axi_gmem_WREADY  := io.m_axi_gmem_WREADY
  impl.io.m_axi_gmem_ARREADY := io.m_axi_gmem_ARREADY
  impl.io.m_axi_gmem_RVALID  := io.m_axi_gmem_RVALID
  impl.io.m_axi_gmem_RDATA   := io.m_axi_gmem_RDATA
  impl.io.m_axi_gmem_RLAST   := io.m_axi_gmem_RLAST
  impl.io.m_axi_gmem_RID     := io.m_axi_gmem_RID
  impl.io.m_axi_gmem_RUSER   := io.m_axi_gmem_RUSER
  impl.io.m_axi_gmem_RRESP   := io.m_axi_gmem_RRESP
  impl.io.m_axi_gmem_BVALID  := io.m_axi_gmem_BVALID
  impl.io.m_axi_gmem_BRESP   := io.m_axi_gmem_BRESP
  impl.io.m_axi_gmem_BID     := io.m_axi_gmem_BID
  impl.io.m_axi_gmem_BUSER   := io.m_axi_gmem_BUSER
  impl.io.memory_pointer     := io.memory_pointer
  impl.io.raddr              := io.raddr
  impl.io.waddr              := io.waddr
  impl.io.cmd                := io.cmd
  impl.io.wline              := io.wline

  io.ap_done             := impl.io.ap_done
  io.ap_idle             := impl.io.ap_idle
  io.ap_ready            := impl.io.ap_ready
  io.m_axi_gmem_AWVALID  := impl.io.m_axi_gmem_AWVALID
  io.m_axi_gmem_AWADDR   := impl.io.m_axi_gmem_AWADDR
  io.m_axi_gmem_AWID     := impl.io.m_axi_gmem_AWID
  io.m_axi_gmem_AWLEN    := impl.io.m_axi_gmem_AWLEN
  io.m_axi_gmem_AWSIZE   := impl.io.m_axi_gmem_AWSIZE
  io.m_axi_gmem_AWBURST  := impl.io.m_axi_gmem_AWBURST
  io.m_axi_gmem_AWLOCK   := impl.io.m_axi_gmem_AWLOCK
  io.m_axi_gmem_AWCACHE  := impl.io.m_axi_gmem_AWCACHE
  io.m_axi_gmem_AWPROT   := impl.io.m_axi_gmem_AWPROT
  io.m_axi_gmem_AWQOS    := impl.io.m_axi_gmem_AWQOS
  io.m_axi_gmem_AWREGION := impl.io.m_axi_gmem_AWREGION
  io.m_axi_gmem_AWUSER   := impl.io.m_axi_gmem_AWUSER
  io.m_axi_gmem_WVALID   := impl.io.m_axi_gmem_WVALID
  io.m_axi_gmem_WDATA    := impl.io.m_axi_gmem_WDATA
  io.m_axi_gmem_WSTRB    := impl.io.m_axi_gmem_WSTRB
  io.m_axi_gmem_WLAST    := impl.io.m_axi_gmem_WLAST
  io.m_axi_gmem_WID      := impl.io.m_axi_gmem_WID
  io.m_axi_gmem_WUSER    := impl.io.m_axi_gmem_WUSER
  io.m_axi_gmem_ARVALID  := impl.io.m_axi_gmem_ARVALID
  io.m_axi_gmem_ARADDR   := impl.io.m_axi_gmem_ARADDR
  io.m_axi_gmem_ARID     := impl.io.m_axi_gmem_ARID
  io.m_axi_gmem_ARLEN    := impl.io.m_axi_gmem_ARLEN
  io.m_axi_gmem_ARSIZE   := impl.io.m_axi_gmem_ARSIZE
  io.m_axi_gmem_ARBURST  := impl.io.m_axi_gmem_ARBURST
  io.m_axi_gmem_ARLOCK   := impl.io.m_axi_gmem_ARLOCK
  io.m_axi_gmem_ARCACHE  := impl.io.m_axi_gmem_ARCACHE
  io.m_axi_gmem_ARPROT   := impl.io.m_axi_gmem_ARPROT
  io.m_axi_gmem_ARQOS    := impl.io.m_axi_gmem_ARQOS
  io.m_axi_gmem_ARREGION := impl.io.m_axi_gmem_ARREGION
  io.m_axi_gmem_ARUSER   := impl.io.m_axi_gmem_ARUSER
  io.m_axi_gmem_RREADY   := impl.io.m_axi_gmem_RREADY
  io.m_axi_gmem_BREADY   := impl.io.m_axi_gmem_BREADY
  io.ap_return           := impl.io.ap_return

}
object AxiMasterGeneratorApplication extends App {

  args.foreach { println(_) }
  if (args.length != 3) {
    println("Usage:")
    println("\tPROGRAM MEMROY_GATEWAY_NAME BUNDLE_NAME OUTPUT_PATH")
  } else {
    AxiMasterGenerator(args(0), args(2))
  }

}
