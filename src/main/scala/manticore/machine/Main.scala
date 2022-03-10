package manticore.machine

import chisel3.stage.ChiselStage

import java.io.File
import scopt.OParser
import manticore.machine.xrt.{ManticoreFlatSimKernel, ManticoreKernelGenerator}

object Main {

  case class CliConfig(
      dimx: Int = 2,
      dimy: Int = 2,
      target: String = "<INV>",
      output: File = new File("."),
      platform: String = "",
      list_platforms: Boolean = false,
      freq: Double = 200.0
  )
  def parseArgs(args: Seq[String]): CliConfig = {
    val project_version = getClass.getPackage.getImplementationVersion
    val parser = {
      val builder = OParser.builder[CliConfig]
      import builder._
      OParser.sequence(
        programName("mgen"),
        head("Manticore hardware generator", project_version),
        opt[File]('o', "output")
          .action { case (f, c) => c.copy(output = f) }
          .required()
          .text("Output directory"),
        opt[Int]('x', "dimx")
          .action { case (x, c) => c.copy(dimx = x) }
          .required()
          .text("X dimension size"),
        opt[Int]('y', "dimy")
          .action { case (y, c) => c.copy(dimy = y) }
          .required()
          .text("Y dimension size"),
        opt[String]('t', "target")
          .action { case (t, c) => c.copy(target = t) }
          .required()
          .text("build target (hw, hw_emu, sim)"),
        opt[String]('p', "platform")
          .action { case (p, c) => c.copy(platform = p) }
          .text(
            "build platform (use --list to see a list of available ones), only affects hw and hw_emu"
          ),
        opt[Double]('f', "freq")
          .action { case (p, c) => c.copy(freq = p) }
          .text(
            "desired operating frequency, default = 200"
          ),
        opt[Unit]("list")
          .action { case (b, c) => c.copy(list_platforms = true) }
          .text("print a list of available platforms"),
        checkConfig { c =>
          c.target match {
            case _ @("hw" | "hw_emu" | "sim") => success
            case t @ _                        => failure(s"invalid target ${t}")
          }
        },
        help('h', "help").text("print usage text and exit")
      )

    }

    OParser.parse(parser, args, CliConfig()) match {
      case Some(config: CliConfig) => config
      case _ =>
        sys.error("Failed parsing arguments")
    }
  }

  def listPlatforms(): Unit = {

    Console.println("Available platforms: \n")
    ManticoreKernelGenerator.platformDevice.foreach { case (p, _) =>
      println(s"\t${p}")
    }
  }

  def main(args: Array[String]): Unit = {

    val cfg = parseArgs(args.toSeq)

    if (cfg.list_platforms) {
      listPlatforms()
      sys.exit(0)
    }
    cfg.target match {

      case t @ ("hw" | "hw_emu") =>
        if (cfg.platform.isEmpty) {
          sys.error("the selected target requires a platform.")
        }

        ManticoreKernelGenerator.platformDevice.get(cfg.platform) match {
          case None => sys.error(s"Unknown platform ${cfg.platform}")
          case _    => // ok
        }
        Console.println(
          s"Starting code generation for ${cfg.platform}.${cfg.target}"
        )
        ManticoreKernelGenerator(
          target_dir = cfg.output.toPath.toAbsolutePath.toString,
          platform = cfg.platform,
          dimx = cfg.dimx,
          dimy = cfg.dimy,
          target = t,
          freqMhz = cfg.freq
        )
level0_i/level1/level1_i/ulp/ManticoreKernel_1/inst/manticore/controller/clock_inactive_reg
level0_i/level1/level1_i/ulp/ManticoreKernel_1/inst/clock_distribution/compute_clock_impl/CE
level0_i/level1/level1_i/ulp/ManticoreKernel_1/inst/clock_distrib
      case "sim" =>
        new ChiselStage().emitVerilog(
          new ManticoreFlatSimKernel(
            DimX = cfg.dimx,
            DimY = cfg.dimy,
            debug_enable = true
          ),
          Array("--target-dir", cfg.output.toPath.toString)
        )
      case _ =>
        sys.error("Unknown platform!")

    }

  }

}
