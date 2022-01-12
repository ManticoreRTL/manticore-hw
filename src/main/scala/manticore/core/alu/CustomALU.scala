/** Copyright 2021 Mahyar Emami
  *
  * Permission is hereby granted, free of charge, to any person obtaining a copy
  * of this software and associated documentation files (the "Software"), to
  * deal in the Software without restriction, including without limitation the
  * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  * sell copies of the Software, and to permit persons to whom the Software is
  * furnished to do so, subject to the following conditions:
  *
  * The above copyright notice and this permission notice shall be included in
  * all copies or substantial portions of the Software.
  *
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  * IN THE SOFTWARE.
  */
package manticore.core.alu

import Chisel._
import chisel3.stage.ChiselStage

import scala.util.Random
import chisel3.util.HasBlackBoxResource

class ALUInput(DATA_BITS: Int) extends Bundle {
  val x = UInt(DATA_BITS.W)
  val y = UInt(DATA_BITS.W)
  val u = UInt(DATA_BITS.W)
  val v = UInt(DATA_BITS.W)
}

class CustomFunctionInterface(DATA_BITS: Int) extends Bundle {

  val in  = Input(new ALUInput(DATA_BITS))
  val out = Output(UInt(DATA_BITS.W))
}

/** @param DATA_BITS
  *   number of bits in the data path
  * @param EQUATIONS
  *   a sequence of lut equation, its length should be {{{DATA_BITS}}}
  */

class CustomFunction(DATA_BITS: Int, EQUATIONS: Seq[Int]) extends Module {

  val io         = IO(new CustomFunctionInterface(DATA_BITS))
  val comb_impl  = Module(new CustomFunctionComb(DATA_BITS, EQUATIONS))
  val result_reg = Reg(UInt(DATA_BITS.W))
  result_reg      := comb_impl.io.out
  comb_impl.io.in := io.in
  io.out          := result_reg

}

class CustomFunctionComb(DATA_BITS: Int, EQUATIONS: Seq[Int]) extends Module {

  val io = IO(new CustomFunctionInterface(DATA_BITS))
  require(
    EQUATIONS.size == DATA_BITS,
    s"invalid number of lut EQUATIONS! Expected %d got %d.".format(
      DATA_BITS,
      EQUATIONS.size
    )
  )
  val bad_equation = EQUATIONS.find(e => e >= (1 << 16))
  require(
    bad_equation.isEmpty,
    s"Bad lut equation %x".format(bad_equation.getOrElse(0))
  )
  val result = Wire(Vec(DATA_BITS, Bool()))

  class WrappedLut4(INIT: Int)
      extends BlackBox(Map("INIT" -> INIT))
      with HasBlackBoxResource {
    val io = IO(new Bundle {
      val x   = Input(Bool())
      val y   = Input(Bool())
      val u   = Input(Bool())
      val v   = Input(Bool())
      val out = Output(Bool())
    })
    addResource("/verilog/WrappedLut4.v")
  }

  for (ix <- Range(0, DATA_BITS)) {
    val lut_impl = Module(new WrappedLut4(EQUATIONS(ix)))
    lut_impl.suggestName(s"lut_impl_${ix}")
    lut_impl.io.x := io.in.x(ix)
    lut_impl.io.y := io.in.y(ix)
    lut_impl.io.u := io.in.u(ix)
    lut_impl.io.v := io.in.v(ix)
    result(ix)    := lut_impl.io.out
  }
  // for (ix <- Range(0, DATA_BITS)) {
  //   result(ix) := EQUATIONS(ix).U(width = 16.W) >> Cat(
  //     io.in.x(ix),
  //     io.in.y(ix),
  //     io.in.u(ix),
  //     io.in.v(ix)
  //   )
  // }
  io.out := result.asUInt
}

class CustomALUInterface(DATA_BITS: Int, FUNCT_BITS: Int)
    extends CustomFunctionInterface(DATA_BITS) {
  val funct = Input(UInt(FUNCT_BITS.W))
}

/** A pipelined 2-cycle custom function ALU
  * @param DATA_BITS
  * @param EQUATIONS
  */
class CustomALU(DATA_BITS: Int, EQUATIONS: Seq[Seq[Int]]) extends Module {

  val NUM_FUNCTS: Int = EQUATIONS.size
  val FUNCT_BITS      = log2Ceil(EQUATIONS.size)

  val io        = IO(new CustomALUInterface(DATA_BITS, FUNCT_BITS))
  val funct_reg = Reg(UInt(FUNCT_BITS.W))
  funct_reg := io.funct

  val funcs   = EQUATIONS.map(EQ => Module(new CustomFunction(DATA_BITS, EQ)))
  val results = Vec(funcs.map(_.io.out))

  val out_reg = Reg(UInt(DATA_BITS.W))

  out_reg := results(funct_reg)

  for (i <- Range(0, funcs.size)) {
    results(i)     := funcs(i).io.out
    funcs(i).io.in := io.in
  }
  io.out := out_reg
}

class CustomALUComb(DATA_BITS: Int, EQUATIONS: Seq[Seq[Int]]) extends Module {

  val NUM_FUNCTS: Int = EQUATIONS.size
  val FUNCT_BITS      = log2Ceil(EQUATIONS.size)
  val io              = IO(new CustomALUInterface(DATA_BITS, FUNCT_BITS))
  val funcs = EQUATIONS.map(EQ => Module(new CustomFunctionComb(DATA_BITS, EQ)))
  val results = Vec(funcs.map(_.io.out))
  for (i <- Range(0, funcs.size)) {
    results(i)     := funcs(i).io.out
    funcs(i).io.in := io.in
  }
  io.out := results(io.funct)

}

object CustomALUGen extends App {

  val rgen = Random

  val DATA_BITS = 16
  val EQUATIONS =
    for (i <- Range(0, 32))
      yield for (i <- Range(0, DATA_BITS)) yield rgen.nextInt(1 << 16)

  new ChiselStage().emitVerilog(new CustomALU(DATA_BITS, EQUATIONS))
  new ChiselStage().emitVerilog(new CustomFunction(DATA_BITS, EQUATIONS(0)))

}
