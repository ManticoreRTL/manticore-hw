package manticore.pipeline

import Chisel._
import chisel3.tester.{testableClock, testableData}
import chiseltest.ChiselScalatestTester
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation => USE_VERILATOR}
import manticore.ManticoreBaseISA
import manticore.core.alu.StandardALU
import manticore.core.alu.StandardALU.Functs
import org.scalatest.{FlatSpec, Matchers}

import scala.language.implicitConversions

object ALUSpec {
  import StandardALU.Functs._
  def clipped(x: Int): Int = {
    x & ((1 << ManticoreBaseISA.DataBits) - 1)
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
      case _  => ???

    }
}
class ALUTester extends FlatSpec with ChiselScalatestTester with Matchers{

  val rdgen = new scala.util.Random(0)

  val NUM_TESTS = 20000

  def randomValue = rdgen.nextInt(1 << 16)

  def randomStatelessFunct = rdgen.nextInt(StandardALU.Functs.maxId - 1)



}
