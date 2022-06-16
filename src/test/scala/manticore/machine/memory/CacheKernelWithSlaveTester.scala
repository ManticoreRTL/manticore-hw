package manticore.machine.xrt

import chisel3._
import chiseltest._
import manticore.machine.xrt.CacheKernelWithSlave
import chisel3.tester.testableClock
import chisel3.tester.testableData

import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import chiseltest.WriteVcdAnnotation
import org.scalatest.matchers.should.Matchers

class CacheKernelWithSlaveTester
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {

  val addrArray = new Array[Int](10000)
  val dataArray = new Array[Int](10000)
  behavior of "CacheKernelWithSlave"

  it should "perform direct slave memory read and write" in {

    test(new CacheKernelWithSlave())
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        val randGen  = new Random(3123)
        val randGen2 = new Random(5173)

        ////////writing directly to memory 
        for (i <- 0 until 1000) {
          val randAddr = (randGen.nextInt(10000))
          addrArray(i) = randAddr
          val randData = (randGen2.nextInt(10000))
          dataArray(randAddr) = randData

          dut.io.wen.poke(1.B)

          dut.io.waddr.poke(randAddr)

          dut.io.data_in.poke(randData)

          dut.clock.step()
        }




        ////////reading directly from memory 

        for (i <- 0 until 1000) {
          val randAddr = addrArray(i)
          val randData = dataArray(randAddr)

          dut.io.raddr.poke(randAddr)
          dut.clock.step()

          val out = dut.io.data_out.peek()
          out.litValue should be(randData)

        }
      }
  }

  it should "perform AXI writes" in {

    test(new CacheKernelWithSlave())
      .withAnnotations(Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)) { dut =>
        val randGen  = new Random(7641)
        val randGen2 = new Random(9760)
        dut.clock.setTimeout(0)
        

        for (i <-0 until 1000) {
          val randAddr = (randGen.nextInt(10000))
          addrArray(i) = randAddr
          val randData = (randGen2.nextInt(10000))
          dataArray(randAddr) = randData

          //////////writing to cache
          dut.io.cache_data_in.poke(randData) //wdata
          dut.io.lock.poke(0.B) //wdata
          dut.io.cache_addr.poke(randAddr) //addr
          dut.io.cache_cmd.poke(0.U) //cache_command
          dut.io.cache_start.poke(1.U) //start
          dut.clock.step()

          dut.io.cache_start.poke(0.U) //start

         

          while (dut.io.cache_done.peek().litValue == 0) {
            dut.clock.step()
          }
          dut.clock.step()
        }

        //////flushing the cache
        dut.io.lock.poke(0.B) //lock
        dut.io.cache_cmd.poke(3.U) //cache command
        dut.io.cache_start.poke(1.U) //start
        dut.clock.step()
        dut.io.cache_start.poke(0.U) //start

       
        while (dut.io.cache_done.peek().litValue == 0) {
          dut.clock.step()
        }


        /////////reading from memory
        dut.clock.step()
        for (i <- 0 until 1000){
          dut.clock.step()
          dut.io.raddr.poke(addrArray(i))
          dut.clock.step()
          val out = dut.io.data_out.peek()
          out.litValue should be (dataArray(addrArray(i)))
        }
      }

  }

  it should "perform AXI reads" in {

    test(new CacheKernelWithSlave())
      .withAnnotations(Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)) { dut =>
        val randGen  = new Random(7641)
        val randGen2 = new Random(9760)
        dut.clock.setTimeout(10000)


         /////////directly writing to memory
        dut.io.wen.poke(1.B)
        for (i <- 0 until 1000){
          val randAddr = (randGen.nextInt(10000))
          addrArray(i) = randAddr
          val randData = (randGen2.nextInt(10000))
      

          dataArray(randAddr) = randData
          dut.clock.step()
          dut.io.waddr.poke(randAddr)
          dut.io.data_in.poke(randData)
          dut.clock.step()
        }


        //////resetting the cache
        dut.io.lock.poke(0.B) //lock
        dut.io.cache_cmd.poke(2.U) //cache command
        dut.io.cache_start.poke(1.U) //start
        dut.clock.step()
        dut.io.cache_start.poke(0.U) //start

       
        while (dut.io.cache_done.peek().litValue == 0) {
          dut.clock.step()
        }

        dut.clock.step()

        //////////reading from memory through cache
        for (i <-0 until 1000) {
          val randAddr = addrArray(i)
          val randData = dataArray(randAddr) 

          dut.io.lock.poke(0.B) 
          dut.io.cache_addr.poke(randAddr) //addr
          dut.io.cache_cmd.poke(1.U) //cache_command
          dut.io.cache_start.poke(1.U) //start
          dut.clock.step()

          dut.io.cache_start.poke(0.U) //start

         

          while (dut.io.cache_done.peek().litValue == 0) {
            dut.clock.step()
          }
          val Out = dut.io.cache_data_out.peek().litValue //rdata
          Out should be (randData)
          dut.clock.step()
        }
       
      }

  }

}
