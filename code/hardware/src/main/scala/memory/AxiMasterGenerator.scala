package memory

import java.nio.file.{Files, StandardCopyOption}
import java.io.PrintWriter
import java.nio.file.Path
import java.io.File

object AxiMasterGenerator {

  import scala.sys.process._

  private def hasVititsHls = {
    "which vitis_hls".! == 0
  }

  def apply(
      name: String,
      bundle: String,
      outdir: String,
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
        .map { line =>
          line.replace("@NAME@", name).replace("@BUNDLE@", bundle) + "\n"
        }
        .reduce(_ + _)
      println(template)

      val hls_source_file =
        Files.createTempFile(
          "manticore_memory_gate_way_",
          s"_${name}_${bundle}.cpp"
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
            .replace("@BUNDLE@", bundle)
            .replace("@PART@", part_num) + "\n"
        }
        .reduce(_ + _)

      val synth_source_file =
        Files.createTempFile("synth___", s"___${name}_${bundle}.tcl")

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
    val project_dir = Files.createTempDirectory(s"${name}_${bundle}___")
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

    val output_dir = Files.createDirectories(new File(outdir).toPath())
    val p0 = Files.copy(
      project_dir.resolve(
        s"${name}/solution/syn/verilog/${name}_${bundle}_m_axi.v"
      ),
      output_dir.resolve(s"${name}_${bundle}_m_axi.v"),
      StandardCopyOption.REPLACE_EXISTING
    )
    val p1 = Files.copy(
      project_dir.resolve(s"${name}/solution/syn/verilog/${name}.v"),
      output_dir.resolve(s"${name}.v"),
      StandardCopyOption.REPLACE_EXISTING
    )
    val p2 = Files.copy(
      project_dir.resolve(s"${name}/solution/syn/report/${name}_csynth.rpt"),
      output_dir.resolve(s"${name}_csynth.rpt")
    )
    println(
      s"Verilog files saved to\n\t${p0.toAbsolutePath().toString()}\n\t${p1.toAbsolutePath.toString()}"
    )
    // Files.createDirectories(outdir, Files.Crea)
  }

}

object AxiMasterGeneratorApplication extends App {

  args.foreach { println(_) }
  if (args.length != 3) {
    println("Usage:")
    println("\tPROGRAM MEMROY_GATEWAY_NAME BUNDLE_NAME OUTPUT_PATH")
  } else {
    AxiMasterGenerator(args(0), args(1), args(2))
  }

}
