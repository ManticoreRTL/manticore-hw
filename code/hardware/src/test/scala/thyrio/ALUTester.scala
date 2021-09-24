package thyrio

import Chisel._
import chisel3.tester.{testableClock, testableData}
import chisel3.util.HasBlackBoxInline
import chiseltest.ChiselScalatestTester
import org.scalatest.{FlatSpec, Matchers}
import thyrio.core.alu.{CustomALU, CustomFunction, StandardALU, _}
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation => USE_VERILATOR, WriteVcdAnnotation => DUMP_VCD}
import thyrio.core.alu.StandardALU.Functs

import scala.language.implicitConversions
import scala.util.{Failure, Random}

object ALUSpec {
  import StandardALU.Functs._
  def clipped(x: Int): Int = {
    x & ((1 << ThyrioISA.DataBits) - 1)
  }
  def compute(x: Int, y: Int, funct: Functs): Int =
    funct match {
      case ADD2 =>
        clipped(x + y)
      case SUB2 =>
        clipped(x - y)
      case MUL2 =>
        clipped(x * y)
      case AND2 =>
        clipped(x & y)
      case OR2 =>
        clipped(x | y)
      case XOR2 =>
        clipped(x ^ y)
      case SLL =>
        clipped(clipped(x) << clipped(y))
      case SRL =>
        clipped(clipped(x) >>> clipped(y))
      case SEQ =>
        if (x == y) 1 else 0
      case SLTS =>
        if (x.toShort < y.toShort) 1 else 0
      case SLTU =>
        if (clipped(x) < clipped(y)) 1 else 0
      case SGTS =>
        if (x.toShort > y.toShort) 1 else 0
      case SGTU =>
        if (clipped(x) > clipped(y)) 1 else 0
      case _  => throw new Exception("Stateful ALU functions can be computed")

    }
}
class ALUTester extends FlatSpec with ChiselScalatestTester with Matchers{

  val rdgen = new scala.util.Random(0)

  val NUM_TESTS = 20000

  def randomValue = rdgen.nextInt(1 << 16)

  def randomStatelessFunct = rdgen.nextInt(StandardALU.Functs.maxId - 1)

  behavior of "ALU with 2 pipeline stages"
  it should "implement all ALU stateless functionalities" in  {

    test(new StandardALU(DATA_BITS = 16)).withAnnotations(Seq(USE_VERILATOR)) { dut =>

      dut.io.funct.poke(0.U)
      dut.io.in.x.poke(0.U)
      dut.io.in.y.poke(0.U)
      dut.clock.step()

      def startPipe = {
        val funct = randomStatelessFunct
        val x = randomValue
        val y = if (Functs(funct) == Functs.SLL || Functs(funct) == Functs.SRL) rdgen.nextInt(16) else randomValue
        val ref = ALUSpec.compute(x, y, StandardALU.Functs(funct))
        dut.io.in.x.poke(x.U)
        dut.io.in.y.poke(y.U)
        dut.io.funct.poke(funct.U)
        dut.clock.step()
        (x, y, funct, ref)
      }

      def keepAlive(expected: Seq[(Int, Int, Int, Int)], num_left: Int): Unit = {
        if (num_left > 0) {
          dut.io.out.expect(expected.head._4.U,
            s"ALU(%d, %d, %s) = %d but got %d".format(
            expected.head._1, expected.head._2,
            Functs(expected.head._3), expected.head._4, dut.io.out.peek.litValue()))
          val ref = startPipe
          keepAlive(expected.tail :+ ref, num_left - 1)
        } else {
          dut.io.out.expect(expected.head._4.U,
            s"ALU(%d, %d, %s) = %d but got %d".format(
              expected.head._1, expected.head._2,
              Functs(expected.head._3), expected.head._4, dut.io.out.peek.litValue()))
          dut.clock.step()
          dut.io.out.expect(expected.tail.head._4.U,
            s"ALU(%d, %d, %s) = %d but got %d".format(
              expected.tail.head._1, expected.tail.head._2,
              Functs(expected.tail.head._3), expected.tail.head._4, dut.io.out.peek.litValue()))
        }
      }
      val ref1 = startPipe
      val ref2 = startPipe
      keepAlive(Seq(ref1, ref2), NUM_TESTS)
      dut.clock.step(2)
    }

  }

  it should "implement MUX functionality correctly" in  {


    test(new StandardALU(DATA_BITS = 16)).withAnnotations(Seq(USE_VERILATOR)) { dut =>

      dut.io.funct.poke(0.U)
      dut.io.in.x.poke(0.U)
      dut.io.in.y.poke(0.U)
      dut.clock.step()

      def startPipe = {
        val funct = randomStatelessFunct
        val x = randomValue
        val y = if (Functs(funct) == Functs.SLL || Functs(funct) == Functs.SRL) rdgen.nextInt(16) else randomValue
        val ref = ALUSpec.compute(x, y, StandardALU.Functs(funct))
        dut.io.in.x.poke(x.U)
        dut.io.in.y.poke(y.U)
        dut.io.funct.poke(funct.U)
        dut.clock.step()
        (x, y, StandardALU.Functs(funct), ref)
      }

      def keepAlive(expected: Seq[(Int, Int, StandardALU.Functs.Functs, Int)], num_left: Int)(create_mux: (Boolean, Int)): Unit = {
        if (num_left > 0) {
          dut.io.out.expect(expected.head._4.U,
            s"ALU(%d, %d, %s) = %d but got %d".format(
              expected.head._1, expected.head._2,
              expected.head._3, expected.head._4, dut.io.out.peek.litValue()))

          val ref = if (create_mux._1) {
            val x, y = randomValue
            val funct = StandardALU.Functs.MUX
            dut.io.in.x.poke(x.U)
            dut.io.in.y.poke(y.U)
            dut.io.funct.poke(funct.id.U)
            dut.clock.step()
            val v = if (create_mux._2 == 1) y else x
//            println(s"Mux(%d, %d, %d) = %d".format(x, y, create_mux._2, v))
            (x, y, funct, v)
          } else {
            startPipe
          }
          keepAlive(expected.tail :+ ref, num_left - 1) {
            val m = (expected.head._3.id >= Functs.SEQ.id &&
              expected.head._3.id < Functs.MUX.id &&
              expected.tail.forall(_._3.id < Functs.SEQ.id) && // ensure no other SET instruction is in between
              rdgen.nextInt(2) == 1 && num_left > 1,
              expected.head._4)
//            if (m._1) {
//              println(expected.head)
//            }
            m
          }

        } else {
          dut.io.out.expect(expected.head._4.U,
            s"ALU(%d, %d, %s) = %d but got %d".format(
              expected.head._1, expected.head._2,
              expected.head._3, expected.head._4, dut.io.out.peek.litValue()))
          dut.clock.step()
          dut.io.out.expect(expected.tail.head._4.U,
            s"ALU(%d, %d, %s) = %d but got %d".format(
              expected.tail.head._1, expected.tail.head._2,
              expected.tail.head._3, expected.tail.head._4, dut.io.out.peek.litValue()))
        }
      }
      val ref1 = startPipe
      val ref2 = startPipe
      keepAlive(Seq(ref1, ref2), NUM_TESTS){ (false, 0) }
      dut.clock.step(2)
    }
  }

}
