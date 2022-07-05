package manticore.machine.processor

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import manticore.machine.ISA
import manticore.machine.ManticoreBaseISA
import manticore.machine.ManticoreFullISA
import manticore.machine.assembly
import manticore.machine.assembly.Assembler
import manticore.machine.assembly.Instruction.Add2
import manticore.machine.assembly.Instruction.GlobalLoad
import manticore.machine.assembly.Instruction.GlobalStore
import manticore.machine.assembly.Instruction.Instruction
import manticore.machine.assembly.Instruction.Nop
import manticore.machine.assembly.Instruction.Predicate
import manticore.machine.assembly.Instruction.R
import manticore.machine.core.Processor
import manticore.machine.core.ProcessorInterface
import manticore.machine.memory.CacheCommand
import manticore.machine.processor.UniProcessorTestUtils.ClockedProcessor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import manticore.machine.core.BareNoCBundle
import java.io.File
import java.nio.file.Paths
import scala.annotation.tailrec
import manticore.machine.xrt.CacheKernelWithSlave
import manticore.machine.core.topmoduleVCD
import scala.util.Random




class topmoduleVCDTester extends AnyFlatSpec with Matchers with ChiselScalatestTester {
    val config = ManticoreFullISA
  val addrArray = new Array[Int](50000)
  val dataArray = new Array[Int](50000)
  behavior of "topmoduleVCD"
  // it should "perform dumps for multiple virtual cycles" in {
  //     test(new topmoduleVCD(config=ManticoreFullISA)).withAnnotations(Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)){dut =>
  //       val randGen  = new Random(3123)
  //       val randGen2 = new Random(5173)
  //       val randGen3 = new Random(6581)
  //       dut.clock.setTimeout(1000)
  //       val virtual_cycles = randGen3.nextInt(500)
  //         dut.io.valid_in.poke(1.B)
  //       dut.io.data_in.poke(virtual_cycles)
  //       dut.clock.step()


  //       for (j <-0 until 20){

  //       for (i <- 0 until virtual_cycles) {
  //         val randAddr = randGen.nextInt(1000)
  //         addrArray(j*virtual_cycles+i) = randAddr
  //         val randData = (j*virtual_cycles+i+1)
  //         dataArray(j*virtual_cycles+i) = randData
  //         dut.io.valid_in.poke(1.B)


  //         dut.io.id_in.poke(randAddr)
  //         dut.io.data_in.poke(randData)

  //         dut.clock.step()
  //       }
  //     }
  //       dut.io.valid_in.poke(0.B)

  //       for (i <- 0 until (500)) {
  //         dut.clock.step()
  //       }





  //       for (i <- 0 until 20*virtual_cycles){

  //         val randAddr = addrArray(i)
  //         val randData = dataArray(i)
  //         dut.io.mem_raddr.poke(i*2)
  //         dut.clock.step()
  //         val out_data = dut.io.mem_data_out.peek().litValue
  //         val randAddr_hex = randAddr.toHexString
  //         dut.io.mem_raddr.poke(i*2+1)
  //         dut.clock.step()
  //         val out_id = dut.io.mem_data_out.peek().litValue
  //         val out_id_hex = out_id.toString(16)
  //         if(out_data!==randData)
  //         {
  //         println(i)
  //         println(out_data)
  //         println(randData)
  //         println(randAddr)
  //         println(out_id_hex)


  //         // dut.io.mem_raddr.poke(2*(i))
  //         // dut.clock.step()
  //         // println(dut.io.mem_data_out.peek().litValue)
  //         // dut.io.mem_raddr.poke(2*(i)+1)
  //         // dut.clock.step()
  //         // println(dut.io.mem_data_out.peek().litValue)
  //         // dut.io.mem_raddr.poke(2*(i+1))
  //         // dut.clock.step()
  //         // println(dut.io.mem_data_out.peek().litValue)
  //         // dut.io.mem_raddr.poke(2*(i+1)+1)
  //         // dut.clock.step()
  //         // println(dut.io.mem_data_out.peek().litValue)
  //         // dut.io.mem_raddr.poke(2*(i+2))
  //         // dut.clock.step()
  //         // println(dut.io.mem_data_out.peek().litValue)
  //         // dut.io.mem_raddr.poke(2*(i+2)+1)
  //         // dut.clock.step()
  //         // println(dut.io.mem_data_out.peek().litValue)
  //         }

  //         val temp = s"${out_id_hex(1)}${out_id_hex(2)}${out_id_hex(3)}"

  //         val temp_int = Integer.parseInt(temp,16)
  //         out_data should be(randData)

  //         temp_int should be(randAddr)
  //         // if((out_data!==randData)){
  //         // println(i)
  //         // dut.io.mem_raddr.poke(510)
  //         // dut.clock.step()
  //         // println(dut.io.mem_data_out.peek().litValue)
  //         // dut.io.mem_raddr.poke(511)
  //         // dut.clock.step()
  //         // println(dut.io.mem_data_out.peek().litValue)
  //         // dut.io.mem_raddr.poke(512)
  //         // dut.clock.step()
  //         // println(dut.io.mem_data_out.peek().litValue)
  //         // dut.io.mem_raddr.poke(513)
  //         // dut.clock.step()
  //         // println(dut.io.mem_data_out.peek().litValue)
  //         // dut.io.mem_raddr.poke(514)
  //         // dut.clock.step()
  //         // println(dut.io.mem_data_out.peek().litValue)
  //         // dut.io.mem_raddr.poke(515)
  //         // dut.clock.step()
  //         // println(dut.io.mem_data_out.peek().litValue)

  //         //}
  //       }

  //     }
  // }
  it should "kill the clock" in {
      test(new topmoduleVCD(config=ManticoreFullISA)).withAnnotations(Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)){dut =>
        val randGen  = new Random(3123)
        val randGen2 = new Random(5173)
        val randGen3 = new Random(6581)
        dut.clock.setTimeout(1000)
        val virtual_cycles = randGen3.nextInt(500)
        //println(virtual_cycles)
          dut.io.valid_in.poke(1.B)
        dut.io.data_in.poke(virtual_cycles)
        dut.clock.step()


        for (j <-0 until 20){

        for (i <- 0 until virtual_cycles) {
          val randAddr = randGen.nextInt(1000)
          addrArray(j*virtual_cycles+i) = randAddr
          val randData = (j*virtual_cycles+i+1)
          dataArray(j*virtual_cycles+i) = randData
          dut.io.valid_in.poke(1.B)


          dut.io.id_in.poke(randAddr)
          dut.io.data_in.poke(randData)

          dut.clock.step()
          while (dut.io.kill_clock.peek().litValue==1)
          {
            dut.clock.step()
          }
        }
      }
        dut.io.valid_in.poke(0.B)

        for (i <- 0 until (500)) {
          dut.clock.step()
        }





        for (i <- 0 until 20*virtual_cycles){

          val randAddr = addrArray(i)
          val randData = dataArray(i)
          dut.io.mem_raddr.poke(i*2)
          dut.clock.step()
          val out_data = dut.io.mem_data_out.peek().litValue
          val randAddr_hex = randAddr.toHexString
          dut.io.mem_raddr.poke(i*2+1)
          dut.clock.step()
          val out_id = dut.io.mem_data_out.peek().litValue
          val out_id_hex = out_id.toString(16)
          if(out_data!==randData)
          {
          println(i)
          println(out_data)
          println(randData)
          println(randAddr)
          println(out_id_hex)
          }

          val temp = s"${out_id_hex(1)}${out_id_hex(2)}${out_id_hex(3)}"

          val temp_int = Integer.parseInt(temp,16)
          out_data should be(randData)

          temp_int should be(randAddr)
      }
  }
}
}