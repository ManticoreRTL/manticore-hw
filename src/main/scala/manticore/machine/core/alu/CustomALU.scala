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

class CustomAlu(
  dataWidth: Int,
  functBits: Int,
  lutArity: Int,
  equations: Seq[Seq[BigInt]],
  enable: Boolean = true
) extends Module {

  val numFuncts = 1 << functBits

  val io = IO(new Bundle {
    val config = Input(Vec(numFuncts, new CustomFunctionConfigInterface(dataWidth)))
    val rsx = Input(Vec(lutArity, UInt(dataWidth.W)))
    val selector = Input(UInt(functBits.W))
    val out = Output(UInt(dataWidth.W))
  })

  if (enable) {

    val results = Wire(Vec(numFuncts, UInt(dataWidth.W)))

    // Create numFunct custom functions.
    for (i <- Range(0, numFuncts)) {
      val customFunct = Module(new CustomFunction(dataWidth, lutArity, equations(i)))
      customFunct.io.config := io.config(i)
      customFunct.io.rsx := io.rsx
      results(i) := customFunct.io.out
    }

    // Select the output from one of the custom functions.
    io.out := results(io.selector)

  } else {

    io.out := 0.U

  }

}

class CustomFunctionInterface(
  dataWidth: Int,
  lutArity: Int
) extends Bundle {
  val config = Input(new CustomFunctionConfigInterface(dataWidth))
  val rsx = Input(Vec(lutArity, UInt(dataWidth.W)))
  val out = Output(UInt(dataWidth.W))
}

class CustomFunctionConfigInterface(
  dataWidth: Int
) extends Bundle {
  val writeEnable = Bool()
  val loadData = UInt(dataWidth.W)
}

class CustomFunction(
  dataWidth: Int,
  lutArity: Int,
  equations: Seq[BigInt]
) extends Module {
  require(
    (2 <= lutArity) && (lutArity <= 6),
    s"Provided LUT arity of ${lutArity} is invalid. LUT arities must be in range 2-6."
  )

  require(
    equations.size == dataWidth,
    s"Invalid number of lut equations! Expected %d got %d.".format(
      dataWidth,
      equations.size
    )
  )

  val bad_equation = equations.find(e => e >= (1 << 16))
  require(
    bad_equation.isEmpty,
    s"Bad lut equation %x".format(bad_equation.getOrElse(0))
  )

  val io = IO(new CustomFunctionInterface(dataWidth, lutArity))

  class WrappedLut6(init: BigInt) extends BlackBox(Map("INIT" -> init)) with HasBlackBoxResource {
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
    val lut = Module(new WrappedLut6(equations(i)))

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
        io.rsx(idx)
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


// ///////// OLD /////////

// class CustomFunctionInterface(DATA_BITS: Int) extends Bundle {
//   val in  = Input(new ALUInput(DATA_BITS))
//   val out = Output(UInt(DATA_BITS.W))
// }

// class CustomFunctionComb(DATA_BITS: Int, EQUATIONS: Seq[Int]) extends Module {

//   val io = IO(new CustomFunctionInterface(DATA_BITS))
//   require(
//     EQUATIONS.size == DATA_BITS,
//     s"invalid number of lut EQUATIONS! Expected %d got %d.".format(
//       DATA_BITS,
//       EQUATIONS.size
//     )
//   )
//   val bad_equation = EQUATIONS.find(e => e >= (1 << 16))
//   require(
//     bad_equation.isEmpty,
//     s"Bad lut equation %x".format(bad_equation.getOrElse(0))
//   )
//   val result = Wire(Vec(DATA_BITS, Bool()))

//   class WrappedLut4(INIT: Int)
//       extends BlackBox(Map("INIT" -> INIT))
//       with HasBlackBoxResource {
//     val io = IO(new Bundle {
//       val x   = Input(Bool())
//       val y   = Input(Bool())
//       val u   = Input(Bool())
//       val v   = Input(Bool())
//       val out = Output(Bool())
//     })
//     addResource("/verilog/WrappedLut4.v")
//   }

//   for (ix <- Range(0, DATA_BITS)) {
//     val lut_impl = Module(new WrappedLut4(EQUATIONS(ix)))
//     lut_impl.suggestName(s"lut_impl_${ix}")
//     lut_impl.io.x := io.in.x(ix)
//     lut_impl.io.y := io.in.y(ix)
//     lut_impl.io.u := io.in.u(ix)
//     lut_impl.io.v := io.in.v(ix)
//     result(ix)    := lut_impl.io.out
//   }
//   io.out := result.asUInt
// }

// class CustomALUInterface(DATA_BITS: Int, FUNCT_BITS: Int)
//     extends CustomFunctionInterface(DATA_BITS) {
//   val funct = Input(UInt(FUNCT_BITS.W))
// }
//
// class CustomALUComb(DATA_BITS: Int, EQUATIONS: Seq[Seq[Int]]) extends Module {

//   val NUM_FUNCTS: Int = EQUATIONS.size
//   val FUNCT_BITS      = log2Ceil(EQUATIONS.size)
//   val io              = IO(new CustomALUInterface(DATA_BITS, FUNCT_BITS))
//   val funcs = EQUATIONS.map(EQ => Module(new CustomFunctionComb(DATA_BITS, EQ)))
//   val results = Vec(funcs.map(_.io.out))
//   for (i <- Range(0, funcs.size)) {
//     results(i)     := funcs(i).io.out
//     funcs(i).io.in := io.in
//   }
//   io.out := results(io.funct)

// }

// ///////// OLD /////////


object CustomALUGen extends App {

  val rgen = Random

  val dataWidth = 16
  val functBits = 5
  val lutArity = 2
  val equations = Seq.fill(1 << functBits) {
    Seq.fill(dataWidth) { BigInt(rgen.nextInt(1 << 16)) }
  }

  new ChiselStage().emitVerilog(
    new CustomAlu(dataWidth, functBits, lutArity, equations)
  )

}
