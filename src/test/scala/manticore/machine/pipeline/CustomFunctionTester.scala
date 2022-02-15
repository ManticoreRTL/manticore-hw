package manticore.machine.pipeline


import chisel3._

import chiseltest._

import manticore.machine.core.alu.{CustomALU, CustomFunction}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random


class CustomFunctionTester
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
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
      new CustomFunction(
        DATA_BITS = 16,
        EQUATIONS = Seq.fill(16) { 0xca }
      )
    )

    custom_module.io.in.x := 0.U(16.W)
    custom_module.io.in.y := util.Cat(Seq.fill(16)(io.s)).asUInt
    custom_module.io.in.u := io.b
    custom_module.io.in.v := io.a

    ref_module.io.a := io.a
    ref_module.io.b := io.b
    ref_module.io.s := io.s

    io.equal := custom_module.io.out === ref_module.io.o

  }
  behavior of "CustomFunction as Mux2to1"
  it should "Implement a 16-bit 2-to-1 multiplexer" in {

    test(new Miter).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      val rgen = Random

      for (i <- Range(0, 1000)) {
        val a = rgen.nextInt(1 << 16)
        val b = rgen.nextInt(1 << 16)
        val s = rgen.nextInt(2)
        dut.io.a.poke(a.U)
        dut.io.b.poke(b.U)
        dut.io.s.poke(s.B)
        dut.clock.step()
        dut.io.equal.expect(true.B, "Custom function does not implement a Mux")

      }
    }

    test(
      new CustomFunction(
        DATA_BITS = 16,
        EQUATIONS = Seq.fill(16) {
          0xca
        }
      )
    ).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      val rgen = Random

      for (i <- Range(0, 1000)) {
        val u        = rgen.nextInt(1 << 16).U
        val v        = rgen.nextInt(1 << 16).U
        val y        = if (rgen.nextInt(2) == 1) 0xffff else 0x0000
        val expected = if (y == 0xffff) u else v
        val x        = 0.U

        dut.io.in.x.poke(x)
        dut.io.in.y.poke(y.U)
        dut.io.in.u.poke(u)
        dut.io.in.v.poke(v)
        dut.clock.step()
        dut.io.out
          .expect(expected, s"Expected Mux(%s, %s) = %s".format(u, v, expected))

      }
    }

  }
  behavior of "CustomALU"
  it should "implement a Mux(x, y) and Mux(y, x) based on the value of funct" in {
    test(
      new CustomALU(
        DATA_BITS = 16,
        EQUATIONS = Seq(
          Seq.fill(16) { 0xca },
          Seq.fill(16) { 0xac }
        )
      )
    ).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      val rgen = Random
      for (i <- Range(0, 1000)) {
        val u        = rgen.nextInt(1 << 16).U
        val v        = rgen.nextInt(1 << 16).U
        val y        = if (rgen.nextInt(2) == 1) 0xffff else 0x0000
        val expected = if (y == 0xffff) u else v
        val x        = 0.U
        val funct    = (i % 2)
        dut.io.in.x.poke(x)
        dut.io.in.y.poke(y.U)
        dut.io.in.u.poke(u)
        dut.io.in.v.poke(v)
        dut.io.funct.poke(funct.B)
        dut.clock.step(1)
        dut.io.funct.poke((1 - funct).B)
        dut.clock.step(1)
        if (funct == 1) {
          val expected = if (y == 0x0000) u else v
          dut.io.out.expect(
            expected,
            s"Expected Mux(%s, %s) = %s".format(u, v, expected)
          )
        } else {
          val expected = if (y == 0xffff) u else v
          dut.io.out.expect(
            expected,
            s"Expected Mux(%s, %s) = %s".format(u, v, expected)
          )
        }

      }
    }
  }

}
