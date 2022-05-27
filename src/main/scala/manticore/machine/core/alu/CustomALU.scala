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
package manticore.machine.core.alu

import Chisel._
import chisel3.stage.ChiselStage
import chisel3.util.HasBlackBoxResource

import scala.util.Random

class CustomAluInterface(
  dataWidth: Int,
  functBits: Int,
  lutArity: Int
) extends Bundle {

  val in = Input(new Bundle {
    val customFuncs = Vec(1 << functBits, new CustomFunctionInterface2(dataWidth, lutArity))
    val selector = UInt(functBits.W)
  })
  val out = Output(UInt(dataWidth.W))

}

class CustomAlu(
  dataWidth: Int,
  functBits: Int,
  lutArity: Int
) extends Module {
  val io = IO(new CustomAluInterface(dataWidth, functBits, lutArity))

  val numFuncts = 1 << functBits
  val results = Wire(Vec(numFuncts, UInt(dataWidth.W)))

  for (i <- Range(0, numFuncts)) {
    val customFunct = Module(new CustomFunction(dataWidth, lutArity))
    customFunct.io.config := io.in.customFuncs(i).config
    customFunct.io.data := io.in.customFuncs(i).data
    results(i) := customFunct.io.out
  }

  // Mux out the 32 custom functions' outputs to select a single one.
  io.out := results(io.in.selector)
}

class CustomFunctionInterface2(
  dataWidth: Int,
  lutArity: Int
) extends Bundle {

  class DataInterface extends Bundle {
    val rsx = Vec(lutArity, UInt(dataWidth.W))
  }

  class ConfigInterface extends Bundle {
    val writeEnable = Bool()
    val loadData = UInt(dataWidth.W)
  }

  val config = Input(new ConfigInterface)
  val data = Input(new DataInterface)
  val out = Output(UInt(dataWidth.W))

}

class CustomFunction(
  dataWidth: Int,
  lutArity: Int
) extends Module {
  val io = IO(new CustomFunctionInterface2(dataWidth, lutArity))

  class WrappedLut6(init: Int) extends BlackBox(Map("INIT" -> init)) with HasBlackBoxResource {
    val io = IO(new Bundle {
      val clock = Input(Clock())
      val we = Input(Bool())
      val data = Input(Bool())
      val a0 = Input(Bool())
      val a1 = Input(Bool())
      val a2 = Input(Bool())
      val a3 = Input(Bool())
      val a4 = Input(Bool())
      val a5 = Input(Bool())
      val out = Output(Bool())
    })
    addResource("/verilog/WrappedLut6.v")
  }

  val result = Wire(Vec(dataWidth, Bool()))

  for (i <- Range(0, dataWidth)) {
    val lut = Module(new WrappedLut6(0))

    // If the arity of the function is less than 6, we force the upper ax bits to 0.
    val lutArgs = Seq(
      lut.io.a0,
      lut.io.a1,
      lut.io.a2,
      lut.io.a3,
      lut.io.a4,
      lut.io.a5
    ).zipWithIndex
    .map { case (lutInput, idx) =>
      if (idx < lutArity) {
        io.data.rsx(idx)
      } else {
        0.B
      }
    }

    lut.suggestName(s"lut_${i}")
    lut.io.clock := clock
    lut.io.we := io.config.writeEnable
    lut.io.data := io.config.loadData(i)
    lut.io.a0 := lutArgs(0)(i)
    lut.io.a1 := lutArgs(1)(i)
    lut.io.a2 := lutArgs(2)(i)
    lut.io.a3 := lutArgs(3)(i)
    lut.io.a4 := lutArgs(4)(i)
    lut.io.a5 := lutArgs(5)(i)
    result(i) := lut.io.out
  }

  io.out := result.asUInt
}

class CustomFunctionInterface(DATA_BITS: Int) extends Bundle {
  val in  = Input(new ALUInput(DATA_BITS))
  val out = Output(UInt(DATA_BITS.W))
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
  io.out := result.asUInt
}

class CustomALUInterface(DATA_BITS: Int, FUNCT_BITS: Int)
    extends CustomFunctionInterface(DATA_BITS) {
  val funct = Input(UInt(FUNCT_BITS.W))
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

  new ChiselStage().emitVerilog(
    new CustomAlu(16, 1, 2)
    // new CustomFunction(16, 4)
  )

  // val rgen = Random

  // val DATA_BITS = 16
  // val EQUATIONS =
  //   for (i <- Range(0, 32))
  //     yield for (i <- Range(0, DATA_BITS)) yield rgen.nextInt(1 << 16)

  // new ChiselStage().emitVerilog(new CustomALUComb(DATA_BITS, EQUATIONS))
  // new ChiselStage().emitVerilog(new CustomFunctionComb(DATA_BITS, EQUATIONS(0)))

}
