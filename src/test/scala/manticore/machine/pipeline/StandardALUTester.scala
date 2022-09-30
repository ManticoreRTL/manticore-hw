package manticore.machine.pipeline

import chisel3._
import chiseltest._
import manticore.machine.core.alu.CustomAlu
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sys.process._
import java.nio.file.Path
import chisel3.stage.ChiselStage
import manticore.machine.core.alu.StandardALUComb
import java.io.PrintWriter
import java.nio.file.Paths
import java.nio.file.Files

class StandardALUTester extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "ALU"

  def runTestbench(testName: String, extraDefs: Seq[String], seed: Int = 780761): Unit = {

    val runDir = Paths.get("test_run_dir").resolve(testName)

    Files.createDirectories(runDir)

    // generate the ALU code
    new ChiselStage().emitSystemVerilog(new StandardALUComb(16), Array("-td", runDir.toAbsolutePath.toString))
    def dumpFile(name: String, content: String): Path = {
      val fp     = runDir.resolve(name)
      val writer = new PrintWriter(fp.toFile)
      writer.write(content)
      writer.close()
      fp
    }
    dumpFile("Testbench.sv", scala.io.Source.fromResource("Dsp48/Testbench.sv").getLines().mkString("\n"))
    dumpFile(
      "sim.tcl",
      s"""|run -all
          |exit [get_value /Testbench/failed]
          |""".stripMargin
    )

    val xVLogCmd =
      List("xvlog", "--sv", "AluDsp48.v", "StandardALUComb.sv", "Testbench.sv", "-L", "unisim") ++ extraDefs.flatMap {
        List("-d", _)
      }
    val xElabCmd = List("xelab", "-debug", "typical", "Testbench", "-s", "Testbench_snapshot", "-L", "unisim")
    val xSimCmd  = List("xsim", "Testbench_snapshot", "-t", "sim.tcl", "--sv_seed", seed.toString)

    if (
      Process(
        command = xVLogCmd,
        cwd = runDir.toFile
      ).! != 0
    ) {
      fail("Could not compile verilog files")
    }
    if (
      Process(
        command = xElabCmd,
        cwd = runDir.toFile
      ).! != 0
    ) {
      fail("Could not elaborate verilog files")
    }

    if (
      Process(
        command = xSimCmd,
        cwd = runDir.toFile
      ).! != 0
    ) {
      fail("Simulation failed")
    }

  }
  "ALU functional model" should "behave as expected" in {

    // "DUMP_ON"
    // runTestbench(getTestName, Seq("VERILATOR", "TEST_SIZE=5000", "DUMP_ON"))
    runTestbench(getTestName, Seq("VERILATOR", "TEST_SIZE=40000"))

  }
  "ALU DSP model" should "behave as expected" in {
    runTestbench(getTestName, Seq("VERILATOR", "TEST_SIZE=40000"))
  }

}
