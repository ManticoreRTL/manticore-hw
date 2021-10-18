package manticore.xrt

import manticore.core.HostRegisters
import manticore.ManticoreFullISA
import chisel3.{Data, UInt, Vec}
import manticore.core.DeviceRegisters
import chisel3.Aggregate
import java.io.PrintWriter
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import java.io.File
import scala.annotation.tailrec

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
      outdir: String,
      part_num: String = "xcu250-figd2104-2l-e"
  ): Map[AxiRegister, Seq[AxiRegisterMapEntry]] = {

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
              println(line)
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
      scala.io.Source.fromFile(cp0.toFile()).getLines().toSeq
    )

    mapping.foreach { case (r, xs) =>
      println(s"${r.name} ->")
      xs.foreach { x =>
        println(f"\t0x${x.offset}%x (as ${x.hw_name}%s)")
      }
    }
    mapping
  }

}

