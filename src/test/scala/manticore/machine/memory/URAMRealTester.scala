package manticore.machine.memory

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sys.process._
import java.nio.file.Path
import chisel3.stage.ChiselStage
import java.io.PrintWriter
import java.nio.file.Paths
import java.nio.file.Files

class URAMRealTester extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "ALU"

  def runTestbench(testName: String, extraDefs: Seq[String], seed: Int = 780761): Unit = {

    val runDir = Paths.get("test_run_dir").resolve(testName)

    Files.createDirectories(runDir)

    // generate the Verilog code
    new ChiselStage().emitSystemVerilog(
      new SimpleDualPortMemory(
        ADDRESS_WIDTH = 14,
        DATA_WIDTH = 16,
        STYLE = MemStyle.URAMReal
      ),
      Array("-td", runDir.toAbsolutePath.toString)
    )
    def dumpFile(name: String, content: String): Path = {
      val fp     = runDir.resolve(name)
      val writer = new PrintWriter(fp.toFile)
      writer.write(content)
      writer.close()
      fp
    }
    dumpFile("Testbench.sv", scala.io.Source.fromResource("URAMReal/Testbench.sv").getLines().mkString("\n"))
    dumpFile(
      "sim.tcl",
      s"""|run -all
          |exit [get_value /Testbench/failed]
          |""".stripMargin
    )

    val xVLogCmd =
      List("xvlog", "--sv", "URAMReal.v", "SimpleDualPortMemory.sv", "Testbench.sv", "-L", "unisim") ++ extraDefs.flatMap {
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
//   "ALU functional model" should "behave as expected" in {

//     // "DUMP_ON"
//     // runTestbench(getTestName, Seq("VERILATOR", "TEST_SIZE=5000", "DUMP_ON"))
//     runTestbench(getTestName, Seq("VERILATOR", "TEST_SIZE=40000"))

//   }
  "URAM implementation" should "behave as expected" in {
    runTestbench(getTestName, Seq("TEST_SIZE=8192"))
  }

  "URAM functional model" should "behave as expected" in {
    runTestbench(getTestName, Seq("VERILATOR", "TEST_SIZE=8192"))
  }

}
