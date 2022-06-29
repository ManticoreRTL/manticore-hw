package manticore.machine.memory

import chisel3._
import chiseltest.ChiselScalatestTester

import chisel3.tester.testableClock
import chisel3.tester.testableData

import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import chiseltest.WriteVcdAnnotation
import org.scalatest.matchers.should.Matchers

class SlicerTester extends AnyFlatSpec with ChiselScalatestTester with Matchers {


    behavior of "Slicer"


    "Slicer" should "take the 8 most-significant bits of the input" in {

        test(new Slicer()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            val randGen = new Random(3123)        
            
            for(i <- 0 until 20) {
                val rand = (randGen.nextLong() & 0xfffffff.toLong)
                dut.io.wordIn.poke(rand.U)
                val out = dut.io.wordOut.peek()
                dut.io.wordOut.expect(((rand >> 16) & 0xff).U)
                
                // assert(out.litValue != ((rand >> 16) & 0xff))
                // println(s"out = ${out}")
                // println(s"out = ${out.litValue}")
                
                dut.clock.step()
            }
            
        }

    }

    // "Slcier" should "be able to generate verilig"

}