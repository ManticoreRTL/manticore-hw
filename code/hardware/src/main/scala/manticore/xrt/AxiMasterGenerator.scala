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
      out_dir: Option[Path] = None,
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

    val outer_path =
      project_dir.resolve(s"${name}/solution/syn/verilog/${name}.v")

    if (out_dir.nonEmpty) {

      Files.createDirectories(out_dir.get)
      val f0 = out_dir.get.resolve(s"${name}.v")
      Files.copy(
        outer_path,
        f0,
        StandardCopyOption.REPLACE_EXISTING
      )
      val f1 = out_dir.get.resolve(s"${name}_gmem_m_axi.v").toAbsolutePath()
      Files.copy(
        inner_path,
        f1,
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


object AxiMasterGeneratorApplication extends App {

  new ChiselStage()
    .emitVerilog(new MemoryGateway(), Array("-td", "gen-dir/mem_gate"))

}
