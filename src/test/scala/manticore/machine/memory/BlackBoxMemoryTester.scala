package manticore.machine.memory

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.io.PrintWriter
import java.nio.file.Files
import java.nio.file.Paths
import scala.util.Random

class BlackBoxMemoryTester extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  val rgen = new Random(0)
//  val initial_values = Seq.fill(1 << 11)(rgen.nextInt(1 << 16))
  val initial_values = Range(0, 1<< 11)
  def singlePortTest[T](dut: T): Unit = {


  }

  behavior of "BlackBoxMemory"
  it should "be able to read or write a value in a single clock edge" in {

    val filenameb = Paths.get("test_data_dir" + File.separator +
      getTestName + File.separator + "rfb.data").toAbsolutePath
    Files.createDirectories(filenameb.getParent)
    val regwriterb = new PrintWriter(filenameb.toFile)
    initial_values.foreach { v => regwriterb.println(s"%016d".format(v.toBinaryString.toLong)) }
    regwriterb.close()

    test(new GenericMemory(11, 16, filenameb.toString))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>

      // check initial values
      initial_values.zipWithIndex.foreach{ case (v, ix) =>

        dut.io.wea.poke(false.B)
        dut.io.web.poke(false.B)
        dut.io.addra.poke(ix.U)
        dut.clock.step()
        dut.io.douta.expect(v.U)
      }

      // check write operation
      val test_size = 256
      val test_values = for (i <- Range(0, test_size)) yield rgen.nextInt(255)


      dut.io.wea.poke(false.B)
      dut.io.web.poke(false.B)
      dut.io.addra.poke(0.U)
      dut.io.addrb.poke(0.U)
      dut.io.dina.poke(0.U)
      dut.io.dinb.poke(0.U)

      dut.clock.step()
      for(address <- Range(0, test_size)) {
        dut.io.dina.poke(test_values(address).U)
        dut.io.addra.poke(address.U)
        dut.io.wea.poke(true.B)
        dut.clock.step()
      }

      for(address <- Range(0, test_size)) {
        dut.io.addra.poke(address.U)
        dut.io.wea.poke(false.B)
        dut.clock.step()
        dut.io.douta.expect(test_values(address).U)
      }
      dut.clock.step()
    }




    test(new SimpleDualPortMemory(11, 16,16, 2, MemStyle.BRAM, filenameb.toString))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>

        // check initial values
        initial_values.zipWithIndex.foreach{ case (v, ix) =>
          dut.io.wen.poke(false.B)
          dut.io.raddr.poke(ix.U)
          dut.clock.step()
          dut.io.dout.expect(v.U)
        }

        // check write operation
        val test_size = 256
        val test_values = for (i <- Range(0, test_size)) yield rgen.nextInt(1 << 16)


        dut.io.wen.poke(false.B)
        dut.io.raddr.poke(0.U)
        dut.io.waddr.poke(0.U)
        dut.io.din.poke(0.U)

        dut.clock.step()
        for(address <- Range(0, test_size)) {
          dut.io.din.poke(test_values(address).U)
          dut.io.waddr.poke(address.U)
          dut.io.wen.poke(true.B)
          dut.clock.step()
        }

        for(address <- Range(0, test_size)) {
          dut.io.raddr.poke(address.U)
          dut.io.wen.poke(false.B)
          dut.clock.step()
          dut.io.dout.expect(test_values(address).U)
        }

        for(address <- Seq.fill(20){rgen.nextInt(1 << 11)}) {
//        for (address <- Range(10, 30)) {
          dut.io.din.poke(address.U)
          dut.io.waddr.poke(address.U)
          dut.io.wen.poke(true.B)
          dut.clock.step()
          dut.io.raddr.poke(address.U)
          dut.clock.step()
          dut.io.dout.expect(address.U)

        }
        dut.clock.step()
      }
  }
}

