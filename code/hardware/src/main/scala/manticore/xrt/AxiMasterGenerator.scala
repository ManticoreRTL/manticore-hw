package manticore.xrt

import java.nio.file.{Files, StandardCopyOption}
import java.io.PrintWriter
import java.nio.file.Path
import java.io.File

import chisel3._
import chisel3.util.HasBlackBoxPath
import chisel3.stage.ChiselStage

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

    val inner_path = project_dir
      .resolve(
        s"${name}/solution/syn/verilog/${name}_gmem_m_axi.v"
      )
    val inner_module = scala.io.Source.fromFile(inner_path.toFile())

    val outer_path =
      project_dir.resolve(s"${name}/solution/syn/verilog/${name}.v")
    val outer_module = scala.io.Source.fromFile(outer_path.toFile())

    if (outdir.nonEmpty) {
      val output_dir = Files.createDirectories(new File(outdir).toPath())
      val f0 = Files.copy(
        outer_path,
        output_dir.resolve(s"${name}.v"),
        StandardCopyOption.REPLACE_EXISTING
      )
      val f1 = Files.copy(
        inner_path,
        output_dir.resolve(s"${name}_gmem_m_axi.v"),
        StandardCopyOption.REPLACE_EXISTING
      )
      Seq(f0, f1)
    } else {
      Seq(outer_path, inner_path)
    }

  }

}

class AxiMasterInterface(
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
  val AWREADY  = Input(Bool())
  val WREADY   = Input(Bool())
  val ARREADY  = Input(Bool())
  val RVALID   = Input(Bool())
  val RDATA    = Input(UInt(AxiGMemDataWidth.W))
  val RLAST    = Input(Bool())
  val RID      = Input(UInt(AxiIdWidth.W))
  val RUSER    = Input(UInt(AxiRUserWidth.W))
  val RRESP    = Input(UInt(2.W))
  val BVALID   = Input(Bool())
  val BRESP    = Input(UInt(2.W))
  val BID      = Input(UInt(AxiIdWidth.W))
  val BUSER    = Input(UInt(AxiBUserWIdth.W))
  val AWVALID  = Output(Bool())
  val AWADDR   = Output(UInt(AxiAddrWidth.W))
  val AWID     = Output(UInt(AxiIdWidth.W))
  val AWLEN    = Output(UInt(8.W))
  val AWSIZE   = Output(UInt(3.W))
  val AWBURST  = Output(UInt(2.W))
  val AWLOCK   = Output(UInt(2.W))
  val AWCACHE  = Output(UInt(4.W))
  val AWPROT   = Output(UInt(3.W))
  val AWQOS    = Output(UInt(4.W))
  val AWREGION = Output(UInt(4.W))
  val AWUSER   = Output(UInt(AxiAwUserWidth.W))
  val WVALID   = Output(Bool())
  val WDATA    = Output(UInt(AxiGMemDataWidth.W))
  val WSTRB    = Output(UInt(AxiGMemWStrbWidth.W))
  val WLAST    = Output(Bool())
  val WID      = Output(UInt(AxiIdWidth.W))
  val WUSER    = Output(UInt(AxiWUserWidth.W))
  val ARVALID  = Output(Bool())
  val ARADDR   = Output(UInt(AxiAddrWidth.W))
  val ARID     = Output(UInt(AxiIdWidth.W))
  val ARLEN    = Output(UInt(8.W))
  val ARSIZE   = Output(UInt(3.W))
  val ARBURST  = Output(UInt(2.W))
  val ARLOCK   = Output(UInt(2.W))
  val ARCACHE  = Output(UInt(4.W))
  val ARPROT   = Output(UInt(3.W))
  val ARQOS    = Output(UInt(4.W))
  val ARREGION = Output(UInt(4.W))
  val ARUSER   = Output(UInt(AxiArUserWidth.W))
  val RREADY   = Output(Bool())
  val BREADY   = Output(Bool())
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
  val m_axi_gmem = new AxiMasterInterface(
    AxiIdWidth,
    AxiAddrWidth,
    AxiGMemDataWidth,
    AxiAwUserWidth,
    AxiArUserWidth,
    AxiWUserWidth,
    AxiRUserWidth,
    AxiBUserWIdth,
    AxiUserValue,
    AxiProtValue,
    AxiCacheValue,
    AxiDataWidth,
    AxiGMemWStrbWidth,
    AxiWStrbWidth
  )

  val ap_start = Input(Bool())

  val ap_done  = Output(Bool())
  val ap_idle  = Output(Bool())
  val ap_ready = Output(Bool())

  val ap_return = Output(UInt(256.W))

  val memory_pointer = Input(UInt(64.W))
  val raddr          = Input(UInt(64.W))
  val waddr          = Input(UInt(64.W))
  val cmd            = Input(UInt(8.W))
  val wline          = Input(UInt(256.W))

}

class MemoryGateWay(cached_path: Seq[String] = Seq()) extends Module {

  val io = IO(new MemoryGateWayInterface)
  // generate the black box implementation
  val path =
    if (cached_path.isEmpty) {
      println("Generating axi master because there is no cached path")
      AxiMasterGenerator("memory_gateway").map(_.toAbsolutePath().toString())
    } else {
      require(cached_path.size == 2)
      println("Using cached path " + cached_path)
      cached_path
    }
  class memory_gateway() extends BlackBox with HasBlackBoxPath {
    val io = IO(new MemoryGateWayInterface {
      val ap_clk = Input(Clock())
      val ap_rst = Input(Bool())
    })
    path.foreach(addPath(_))
  }

  val impl = Module(new memory_gateway)

  impl.io.ap_clk := clock

  impl.io.ap_rst   := !(reset.asBool())
  impl.io.ap_start := io.ap_start

  impl.io.memory_pointer := io.memory_pointer
  impl.io.raddr          := io.raddr
  impl.io.waddr          := io.waddr
  impl.io.cmd            := io.cmd
  impl.io.wline          := io.wline

  io.ap_done  := impl.io.ap_done
  io.ap_idle  := impl.io.ap_idle
  io.ap_ready := impl.io.ap_ready

  io.ap_return := impl.io.ap_return

  io.m_axi_gmem <> impl.io.m_axi_gmem

  def makeCopy() = new MemoryGateWay(path)
}
object AxiMasterGeneratorApplication extends App {

  
  new ChiselStage()
    .emitVerilog(new MemoryGateWay(), Array("-td", "gen-dir/mem_gate"))

}
