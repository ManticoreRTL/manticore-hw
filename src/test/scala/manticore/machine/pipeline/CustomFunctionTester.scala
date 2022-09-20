package manticore.machine.pipeline

import chisel3._
import chiseltest._
import manticore.machine.core.alu.CustomAlu
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class CustomAluTester extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  def RegNext4[T <: Data](src: T): T = {
    RegNext(RegNext(RegNext(RegNext(src))))
  }

  class MuxInterface extends Bundle {
    val a = Input(UInt(16.W))
    val b = Input(UInt(16.W))
    val s = Input(Bool())

    val o = Output(UInt(16.W))
  }
  class Mux2to1 extends Module {
    val io = IO(new MuxInterface)
    when(io.s) {
      io.o := io.b
    } otherwise {
      io.o := io.a
    }
  }

  class Miter extends Module {
    val io = IO(new Bundle {
      val a     = Input(UInt(16.W))
      val b     = Input(UInt(16.W))
      val s     = Input(Bool())
      val equal = Output(Bool())
    })

    val ref_module = Module(new Mux2to1)
    val custom_module = Module(
      new CustomAlu(
        dataWidth = 16,
        functBits = 5,
        lutArity = 4,
        equations = Seq.fill(32) { Seq.fill(16) { BigInt(0xcaca) } }
      )
    )

    custom_module.io.rsx(3)   := 0.U(16.W)
    custom_module.io.rsx(2)   := util.Cat(Seq.fill(16)(io.s)).asUInt
    custom_module.io.rsx(1)   := io.b
    custom_module.io.rsx(0)   := io.a
    custom_module.io.selector := 0.U(5.W)
    for (i <- Range(0, 16)) {
      custom_module.io.config(i).loadData    := 0.U
      custom_module.io.config(i).writeEnable := 0.B
    }

    ref_module.io.a := io.a
    ref_module.io.b := io.b
    ref_module.io.s := io.s

    io.equal := RegNext4(ref_module.io.o) === custom_module.io.out
    printf(
      s"Expected = %d, received = %d, a = %d, b = %d, equal = %b\n",
      RegNext4(ref_module.io.o),
      custom_module.io.out,
      io.a,
      io.b,
      io.equal
    )
  }

  behavior of "CustomAlu as Mux2to1"
  it should "Implement a 16-bit 2-to-1 multiplexer" in {

    test(new Miter).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
      val rgen = Random

      for (i <- Range(0, 1000)) {
        val a = rgen.nextInt(1 << 16)
        val b = rgen.nextInt(1 << 16)
        val s = rgen.nextInt(2)
        dut.io.a.poke(a.U)
        dut.io.b.poke(b.U)
        dut.io.s.poke(s.B)
        // Custom ALU's latency is 4 cycles
        dut.clock.step(4)
        dut.io.equal.expect(true.B, "Custom function does not implement a Mux")
      }
    }

    test(
      new CustomAlu(
        dataWidth = 16,
        functBits = 5,
        lutArity = 4,
        equations = Seq.fill(32) { Seq.fill(16) { BigInt(0xcaca) } }
      )
    ).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      val rgen = Random

      for (i <- Range(0, 1000)) {
        val rs1      = rgen.nextInt(1 << 16).U                      // a
        val rs2      = rgen.nextInt(1 << 16).U                      // b
        val rs3      = if (rgen.nextInt(2) == 1) 0xffff else 0x0000 // s
        val rs4      = 0.U
        val expected = if (rs3 == 0xffff) rs2 else rs1

        dut.io.rsx(0).poke(rs1)
        dut.io.rsx(1).poke(rs2)
        dut.io.rsx(2).poke(rs3)
        dut.io.rsx(3).poke(rs4)
        // Custom ALU's latency is 4 cycles
        dut.clock.step(4)
        dut.io.out
          .expect(expected, s"Expected Mux(a = %s, b = %s, s = %s) = %s".format(rs1, rs2, rs3, expected))
      }
    }
  }

  behavior of "CustomALU"
  it should "implement a Mux(x, y) and Mux(y, x) based on the value of funct" in {
    test(
      new CustomAlu(
        dataWidth = 16,
        functBits = 5,
        lutArity = 4,
        equations = Seq.fill(16) { Seq.fill(16) { BigInt(0xcaca) } } ++ Seq.fill(16) { Seq.fill(16) { BigInt(0xacac) } }
      )
    ).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      val rgen = Random
      for (i <- Range(0, 1000)) {
        val rs1   = rgen.nextInt(1 << 16).U                        // a
        val rs2   = rgen.nextInt(1 << 16).U                        // b
        val rs3   = if (rgen.nextInt(32) >= 16) 0xffff else 0x0000 // s
        val rs4   = 0.U
        val funct = if (rgen.nextInt(32) >= 16) 31 else 0
        dut.io.rsx(0).poke(rs1)
        dut.io.rsx(1).poke(rs2)
        dut.io.rsx(2).poke(rs3)
        dut.io.rsx(3).poke(rs4)
        dut.io.selector.poke(funct.U)
        dut.clock.step(4)
        if (funct == 0) {
          val expected = if (rs3 == 0x0000) rs1 else rs2
          dut.io.out.expect(
            expected,
            s"Expected Mux(a = %s, b = %s, s = %s)".format(rs1, rs2, rs3, expected)
          )
        } else {
          val expected = if (rs3 == 0xffff) rs1 else rs2
          dut.io.out.expect(
            expected,
            s"Expected Mux(a = %s, b = %s, s = %s)".format(rs1, rs2, rs3, expected)
          )
        }
      }
    }
  }

}
