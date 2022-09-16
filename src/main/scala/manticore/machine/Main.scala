package manticore.machine

import chisel3.stage.ChiselStage
import manticore.machine.xrt.ManticoreFlatSimKernel
import manticore.machine.xrt.ManticoreKernelGenerator
import scopt.OParser

import java.io.File
import java.io.PrintWriter
import manticore.machine.xrt.Floorplanning
import manticore.machine.xrt.SwitchLeftHighway
import manticore.machine.xrt.SwitchCentralIsland
import manticore.machine.xrt.U200

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
      placement_alg: String = "island",
      pblock: Option[File] = None,
      max_cores_per_pblock: Int = 5,
      strategy: Seq[String] = Nil
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
        opt[Seq[String]]('s', "strategy")
          .action { case (s, c) => c.copy(strategy = s) }
          .text("Implementation strategy (see ug904)")
          .validate { s =>
            val validChoices = Set(
              "Performance_Explore",
              "Congestion_SpreadLogic_Explore",
              "Performance_NetDelay_high",
              "Performance_NetDelay_low",
              "Performance_ExtraTimingOpt",
              "Congestion_SpreadLogic_high",
              "Congestion_SpreadLogic_medium",
              "Congestion_SpreadLogic_low"
            )
            if (s.forall(validChoices.contains)) {
              success
            } else {
              failure(s"Invalid strategy! Possible strategies are ${validChoices.mkString(", ")} ")
            }
          },
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
            opt[String]("algorithm")
              .action { case (alg, c) => c.copy(placement_alg = alg) }
              .text("placement algorithm to use")
              .required()
              .validate { s =>
                val validChoices = Set(
                  "island",
                  "highway"
                )
                if (validChoices.contains(s)) {
                  success
                } else {
                  failure(s"Invalid placement algorithm! Possible algorithms are ${validChoices.mkString(", ")} ")
                }
              }
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
      val constraints = if (cfg.placement_alg == "highway") {
        // We want to anchor in clock region X2Y7. Setting the anchor to c2y12 in the grid results in x0y0 being assigned
        // to pblock_cores_Y7_Left (experimentally verified, no algorithm to derive automatically).
        new SwitchLeftHighway(cfg.dimx, cfg.dimy, Floorplanning.GridLoc(2, 12), U200).getConstraints()
      } else if (cfg.placement_alg == "island") {
        // We want to anchor core x0y0 in clock region X2Y10. We are lucky and it so happens to be that setting the
        // anchor to c2y10 in the grid results in x0y0 being assigned to pblock_cores_Y10_Left (experimentally derived,
        // no algorithm to derive automatically).
        new SwitchCentralIsland(cfg.dimx, cfg.dimy, Floorplanning.GridLoc(2, 10), U200).getConstraints()
      } else {
        ""
      }

      val writer = new PrintWriter(cfg.output)
      writer.write(constraints)
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
            pblock = cfg.pblock,
            strategy = cfg.strategy
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
