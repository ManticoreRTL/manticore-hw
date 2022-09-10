package manticore.machine

import chisel3.stage.ChiselStage
import manticore.machine.xrt.ManticoreFlatSimKernel
import manticore.machine.xrt.ManticoreKernelGenerator
import scopt.OParser

import java.io.File
import manticore.machine.xrt.PhysicalPlacement
import java.io.PrintWriter
import manticore.machine.xrt.IterativePlacement

object Main {

  case class CliConfig(
      dimx: Int = 2,
      dimy: Int = 2,
      enable_custom_alu: Boolean = true,
      target: String = "<INV>",
      output: File = new File("."),
      platform: String = "",
      list_platforms: Boolean = false,
      freq: Double = 200.0,
      n_hop: Int = 1,
      do_placement: Boolean = false,
      pblock: Option[File] = None,
      anchor: (Int, Int) = (2, 7),
      max_cores_per_pblock: Int = 5
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
        opt[Boolean]("enable_custom_alu")
          .action { case (b, c) => c.copy(enable_custom_alu = b) }
          .text("Enable custom ALUs in cores"),
        opt[String]('t', "target")
          .action { case (t, c) => c.copy(target = t) }
          // .required()
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
        opt[Int]("n_hop")
          .action { case (p, c) => c.copy(n_hop = p) }
          .text("number of hops between cores"),
        opt[Unit]("list")
          .action { case (b, c) => c.copy(list_platforms = true) }
          .text("print a list of available platforms"),
        opt[File]('P', "pblock")
          .action { case (f, c) => c.copy(pblock = Some(f)) }
          .text("pblock constraint file (advanced)"),
        cmd("placement")
          .action { case (_, c) => c.copy(do_placement = true) }
          .text("generate a pblock constraint file")
          .children(
            opt[String]('a', "anchor")
              .action { case (a, c) =>
                val an = raw"X(\d+)Y(\d+)".r
                val pan = a match {
                  case an(x, y) => (x.toInt, y.toInt)
                }
                c.copy(anchor = pan)
              }
              .valueName("XnYm")
              .required()
              .text("pblock anchor point which contains the privileged core."),
            opt[Int]('m', "max-cores")
              .action { case (n, c) => c.copy(max_cores_per_pblock = n) }
              .text("maximum number of cores allowed in a pblock")
          ),
        checkConfig { c =>
          if (!c.do_placement) {
            c.target match {
              case _ @("hw" | "hw_emu" | "sim") => success
              case t @ _                        => failure(s"invalid target ${t}")
            }
          } else {
            success
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
    if (cfg.do_placement) {
      // val placer = new PhysicalPlacement(cfg.dimx, cfg.dimy, cfg.anchor, cfg.max_cores_per_pblock)
      val placer = new IterativePlacement(cfg.dimx, cfg.dimy, IterativePlacement.Pblock(7, IterativePlacement.Right))
      val writer = new PrintWriter(cfg.output)
      writer.write(placer.pblockConstraint)
      writer.close()
    } else {

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
            enable_custom_alu = cfg.enable_custom_alu,
            freqMhz = cfg.freq,
            n_hop = cfg.n_hop,
            pblock = cfg.pblock
          )

        case "sim" =>
          new ChiselStage().emitVerilog(
            new ManticoreFlatSimKernel(
              DimX = cfg.dimx,
              DimY = cfg.dimy,
              debug_enable = true,
              enable_custom_alu = cfg.enable_custom_alu
            ),
            Array("--target-dir", cfg.output.toPath.toString)
          )
        case _ =>
          sys.error("Unknown platform!")

      }
    }

  }

}
