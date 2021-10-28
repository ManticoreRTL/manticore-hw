package manticore.xrt

import manticore.core.HostRegisters
import manticore.ManticoreFullISA

import manticore.core.DeviceRegisters
import chisel3.Aggregate
import java.io.PrintWriter
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import java.io.File
import scala.annotation.tailrec
import chisel3.util.HasBlackBoxPath
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.internal.noPrefix

object AxiSlaveGenerator {

  import scala.sys.process._

  def translateType(elem: UInt): String = {
    if (elem.getWidth <= 8) {
      "unsigned char"
    } else if (elem.getWidth <= 16) {
      "unsigned short"
    } else if (elem.getWidth <= 32) {
      "unsigned int"
    } else if (elem.getWidth <= 64) {
      "unsigned long"
    } else {
      throw new Exception(
        "Can not have axi registers with more than 64 bits!"
      )
    }

  }

  private def hasVitisHls = {
    "which vitis_hls".! == 0
  }

  case class AxiRegister(name: String, cppType: String, prefix: String)
  case class AxiRegisterMapEntry(
      name: String,
      hw_name: String,
      offset: Int,
      range: Int
  )
  def apply(
      registers: Seq[AxiRegister],
      outdir: String = "",
      part_num: String = "xcu250-figd2104-2l-e"
  ): (Map[AxiRegister, Seq[AxiRegisterMapEntry]], Path) = {

    def writeCppSource() = {
      val cpp_arg =
        registers
          .map { case AxiRegister(name, cppType, prefix) =>
            s"${cppType} ${prefix}_${name}"
          }
          .mkString(",\n\t")

      val cpp_pragmas = registers.map { case AxiRegister(name, _, prefix) =>
        s"#pragma HLS interface s_axilite port = ${prefix}_${name}\n"
      }.mkString

      val no_dce = registers
        .map { case AxiRegister(name, _, prefix) =>
          s"\tNO_DCE(${prefix}_${name})"
        }
        .mkString("\n")

      val code = s"""
#include <cstdint>
#define NO_DCE(var) { auto val = reinterpret_cast<volatile uint8_t &>(var); }
void slave_registers(${cpp_arg}) {
${cpp_pragmas}
${no_dce}
}    
    """

      println(code)
      val source_path   = Files.createTempFile("slave_registers__", ".cpp")
      val source_writer = new PrintWriter(source_path.toFile())
      source_writer.write(code)
      source_writer.close()
      source_path
    }

    def writeTcl(cpp_source: Path) = {

      val synth_script = scala.io.Source
        .fromResource("hls/synth_slave.tcl")
        .getLines()
        .map { line =>
          line
            .replace("@PART@", part_num)
            .replace("@FILE@", cpp_source.toAbsolutePath().toString())
        }
        .mkString("\n")
      val synth_path = Files.createTempFile("slave_synth__", ".tcl")

      val synth_writer = new PrintWriter(synth_path.toFile())
      synth_writer.write(synth_script)
      synth_writer.close()
      synth_path
    }

    val source_path = writeCppSource()
    val synth_path  = writeTcl(source_path)

    val project_dir = Files.createTempDirectory("slave_registers")
    println(s"Creatig project directory in ${project_dir.toAbsolutePath()}")

    if (
      Process(
        command = s"vitis_hls -f ${synth_path.toAbsolutePath().toString()}",
        cwd = project_dir.toFile()
      ).!(ProcessLogger(line => println(line))) != 0
    ) {

      throw new Exception("Synthesis failed!")
    }

    if (outdir.nonEmpty) {
      val output_dir = Files.createDirectories(new File(outdir).toPath())
      val cp = Files.copy(
        project_dir.resolve(
          "slave_registers/solution/syn/verilog/slave_registers_control_s_axi.v"
        ),
        output_dir.resolve("slave_registers_control_s_axi.v"),
        StandardCopyOption.REPLACE_EXISTING
      )
      println(
        s"Verilog files saved to\n\t${cp.toAbsolutePath().toString()}"
      )
      val cp0 = Files.copy(
        project_dir.resolve(
          "slave_registers/solution/syn/report/csynth.rpt"
        ),
        output_dir.resolve("slave_registers_csynth.rpt"),
        StandardCopyOption.REPLACE_EXISTING
      )

    }

    def extractAddressMap(report: Seq[String]) = {

      @tailrec
      def findTableStart(lines: Seq[String]): Seq[String] = {
        if (lines.nonEmpty) {
          if (lines.head != "* SW-to-HW Mapping")
            findTableStart(lines.tail)
          else
            lines.tail
        } else {
          throw new Exception("Could not find the SW-to-HW mappings")
        }

      }

      val lines_to_search = findTableStart(report)

      val entries = lines_to_search
        .map { line =>
          val expr =
            raw"\|\s*(\w*)\s*\|\s*s_axi_control\s*(\w*)\s*\|\s*register\s*\|\soffset\s*=\s*0[xX]([0-9a-fA-F]+)\s*,\s*range\s*=\s*(\d*)\s*\|".r
          line match {
            case expr(arg, hw_name, offset, range) =>
              Some(
                AxiRegisterMapEntry(
                  arg,
                  hw_name,
                  Integer.parseInt(offset, 16),
                  range.toInt
                )
              )
            case _ =>
              None
          }
        }
        .filter(_.isDefined)
        .map(_.get)

      registers.map { case r @ AxiRegister(name, _, prefix) =>
        r -> entries.filter(e => e.name == prefix + "_" + name)
      }.toMap
    }

    val mapping = extractAddressMap(
      scala.io.Source
        .fromFile(
          project_dir
            .resolve(
              "slave_registers/solution/syn/report/csynth.rpt"
            )
            .toFile()
        )
        .getLines()
        .toSeq
    )

    mapping.toSeq.sortBy(_._2.map(_.offset).min).foreach { case (r, xs) =>
      println(s"${r.name} ->")
      xs.foreach { x =>
        println(f"\t0x${x.offset}%x (as ${x.hw_name}%s)")
      }
    }
    (
      mapping,
      project_dir.resolve(
        "slave_registers/solution/syn/verilog/slave_registers_control_s_axi.v"
      )
    )
  }

}

object AxiRegisterBuilder {

  
  // create a name for the Cpp implementation
  def makeName(chisel_name: String, chisel_type: Data): Seq[String] = {
    val cpp_type = lowerType(chisel_type)
    cpp_type match {
      case x +: Nil  => Seq(chisel_name)
      case x +: tail => cpp_type.indices.map(i => chisel_name + s"_${i}")
    }
  }
  private def lowerType(data: Data): Seq[String] = {

    data match {
      case vec: Vec[_] =>
        vec.map(x => AxiSlaveGenerator.translateType(x.asInstanceOf[UInt]))
      case elem: UInt =>
        Seq(AxiSlaveGenerator.translateType(elem))
      case _ =>
        throw new Exception(
          "Can not lower anything other than UInt and Vec[UInt]"
        )
    }

  }

  private def makeRegs(
      name: String,
      t: Data,
      prefix: String
  ): Seq[AxiSlaveGenerator.AxiRegister] = {
    val cpp_type = lowerType(t)
    val cpp_name = cpp_type match {
      case x +: Nil  => Seq(name)
      case x +: tail => cpp_type.indices.map(i => name + s"_${i}")
    }

    cpp_name.zip(cpp_type).map { case (_n, _t) =>
      AxiSlaveGenerator.AxiRegister(_n, _t, prefix)
    }
  }

  def apply(): (
      Seq[AxiSlaveGenerator.AxiRegister],
      Seq[AxiSlaveGenerator.AxiRegister],
      Seq[AxiSlaveGenerator.AxiRegister]
  ) = {
    val io_inst = new AxiHlsSlaveInterface
    val host_regs =
      new HostRegisters(ManticoreFullISA).elements.flatMap { case (name, t) =>
        makeRegs(
          name,
          t,
          io_inst.elements
            .filter { case (_, ht) => ht.isInstanceOf[HostRegisters] }
            .map { case (n, _) => n }
            .head
        )
      }.toSeq
    val dev_regs = new DeviceRegisters(ManticoreFullISA).elements.flatMap {
      case (name, t) =>
        makeRegs(
          name,
          t,
          io_inst.elements
            .filter { case (_, ht) => ht.isInstanceOf[DeviceRegisters] }
            .map { case (n, _) => n }
            .head
        )
    }.toSeq

    val pointer_regs = (new MemoryPointers).elements.flatMap { case (name, t) =>
      makeRegs(
        name,
        t,
        io_inst.elements
          .filter { case (_, ht) => ht.isInstanceOf[MemoryPointers] }
          .map { case (n, _) => n }
          .head
      )
    }.toSeq

    (host_regs, dev_regs, pointer_regs)

  }
}

class AxiSlaveInterface(AxiSlaveAddrWidth: Int = 8, AxiSlaveDataWidth: Int = 32)
    extends Bundle {

  // val ACCLK    = Input(Clock())
  // val ARESET   = Input(Bool())
  // val ACLK_EN = Input(Bool())
  val AWADDR  = Input(UInt(AxiSlaveAddrWidth.W))
  val AWVALID = Input(Bool())
  val AWREADY = Output(Bool())
  val WDATA   = Input(UInt(AxiSlaveDataWidth.W))
  val WSTRB   = Input(UInt((AxiSlaveDataWidth / 8).W))
  val WVALID  = Input(Bool())
  val WREADY  = Output(Bool())
  val BRESP   = Output(UInt(2.W))
  val BVALID  = Output(Bool())
  val BREADY  = Input(Bool())
  val ARADDR  = Input(UInt(AxiSlaveAddrWidth.W))
  val ARVALID = Input(Bool())
  val ARREADY = Output(Bool())
  val RDATA   = Output(UInt(AxiSlaveDataWidth.W))
  val RRESP   = Output(UInt(2.W))
  val RVALID  = Output(Bool())
  val RREADY  = Input(Bool())

}

class AxiHlsSlaveInterface(
    AxiSlaveAddrWidth: Int = 8,
    AxiSlaveDataWidth: Int = 32
) extends AxiSlaveInterface(AxiSlaveAddrWidth, AxiSlaveDataWidth) {
  val hregs       = Output(new HostRegisters(ManticoreFullISA))
  val dregs       = Output(new DeviceRegisters(ManticoreFullISA))
  val pregs       = Output(new MemoryPointers)
  val ap_start    = Output(Bool())
  val ap_done     = Input(Bool())
  val ap_ready    = Input(Bool())
  val ap_continue = Output(Bool())
  val ap_idle     = Input(Bool())
  val interrupt   = Output(Bool())
}
class AxiHlsSlave(
    cached_map: Option[
      (
          Map[AxiSlaveGenerator.AxiRegister, Seq[
            AxiSlaveGenerator.AxiRegisterMapEntry
          ]],
          String
      )
    ] = None
) extends Module {

  val io = IO(new AxiHlsSlaveInterface)

  class slave_registers_control_s_axi extends BlackBox with HasBlackBoxPath {

    val io = IO(new AxiHlsSlaveInterface {
      val ACLK    = Input(Clock())
      val ARESET  = Input(Reset())
      val ACLK_EN = Input(Bool())
    })

    // generate the slave implemenation by calling Vitis HLS and keep the
    // register address map
    val (
      port_map: Map[AxiSlaveGenerator.AxiRegister, Seq[
        AxiSlaveGenerator.AxiRegisterMapEntry
      ]],
      path: String
    ) = if (cached_map.isEmpty) {
      val (hregs, dregs, pregs) = AxiRegisterBuilder()
      val (m, p) = AxiSlaveGenerator(
        hregs ++ dregs ++ pregs
      )
      (m, p.toAbsolutePath().toString())
    } else {
      cached_map.get
    }

    addPath(
      path
    )

  }

  val impl = Module(new slave_registers_control_s_axi)

  /** Get the port offset in the axi slave implementation, depending on the port
    * size (i.e., 32-bit ot 64-bit) one or two port offsets are returned. The
    * first offset corresponds to the lower 32 bits (i.e., little-endian)
    * @param port_name
    * @return
    */
  def getOffset(port_name: String): Option[Seq[Int]] = {

    impl.port_map.find { case (k, v) => k.name == port_name }.map {
      case (k, v) => v.map(_.offset)
    }

  }
  impl.io.ACLK   := clock
  impl.io.ARESET := reset

  io.hregs := impl.io.hregs
  io.dregs := impl.io.dregs

  io.pregs := impl.io.pregs

  impl.io.ACLK_EN := true.B
  impl.io.AWADDR  := io.AWADDR
  impl.io.AWVALID := io.AWVALID
  io.AWREADY      := impl.io.AWREADY
  impl.io.WDATA   := io.WDATA
  impl.io.WSTRB   := io.WSTRB
  impl.io.WVALID  := io.WVALID
  io.WREADY       := impl.io.WREADY
  io.BRESP        := impl.io.BRESP
  io.BVALID       := impl.io.BVALID
  impl.io.BREADY  := io.BREADY
  impl.io.ARADDR  := io.ARADDR
  impl.io.ARVALID := io.ARVALID
  io.ARREADY      := impl.io.ARREADY
  io.RDATA        := impl.io.RDATA
  io.RRESP        := impl.io.RRESP
  io.RVALID       := impl.io.RVALID
  impl.io.RREADY  := io.RREADY
  io.interrupt    := impl.io.interrupt

  io.ap_start      := impl.io.ap_start
  impl.io.ap_done  := io.ap_done
  impl.io.ap_ready := io.ap_ready
  io.ap_continue   := impl.io.ap_continue
  impl.io.ap_idle  := io.ap_idle

}

object Tmp extends App {

  new ChiselStage().emitVerilog(new AxiHlsSlave(), Array("-td", "gen-dir/02"))
}
