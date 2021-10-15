package memory

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

object AxiSlaveGenerator {

  import scala.sys.process._

  def translateType(elem: UInt): String = {
    if (elem.getWidth <= 8) {
      "uint8_t"
    } else if (elem.getWidth <= 16) {
      "uint16_t"
    } else if (elem.getWidth <= 32) {
      "uint32_t"
    } else if (elem.getWidth <= 64) {
      "uint64_t"
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
  def apply(
      registers: Seq[AxiRegister],
      outdir: String,
      part_num: String = "xcu250-figd2104-2l-e"
  ) = {

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
  }

}

object AxiSlaveGeneratorApplication extends App {

  def lowerType(data: Data): Seq[String] = {

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

  def makeRegs(
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

  val hregs =
    new HostRegisters(ManticoreFullISA).elements.flatMap { case (name, t) =>
      makeRegs(name, t, "h")
    }.toSeq
  val dregs = new DeviceRegisters(ManticoreFullISA).elements.flatMap {
    case (name, t) =>
      makeRegs(name, t, "d")
  }.toSeq

  AxiSlaveGenerator(
    hregs ++ dregs,
    "gen-dir/slave"
  )

}
