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
    val config   = Input(Vec(dataWidth, new CustomBitConfigInterface(dataWidth)))
    val rsx      = Input(Vec(lutArity, UInt(dataWidth.W)))
    val selector = Input(UInt(functBits.W))
    val out      = Output(UInt(dataWidth.W))
  })

  val equations_t = equations.transpose // 16x32
  val rsx_t       = Wire(Vec(dataWidth, UInt(lutArity.W)))
  for (i <- Range(0, dataWidth)) {
    // transpose
    rsx_t(i) := Vec(io.rsx(0)(i), io.rsx(1)(i), io.rsx(2)(i), io.rsx(3)(i)).asUInt
  }

  if (enable) {

    val result = Wire(Vec(dataWidth, UInt(1.W)))

    // Create dataWidth custom bits.
    for (i <- Range(0, dataWidth)) {
      val customBit = Module(new CustomBit(dataWidth, functBits, lutArity, equations_t(i)))
      customBit.io.config := RegNext(io.config(i))
      customBit.io.rsx    := RegNext(rsx_t(i))
      customBit.io.addr   := RegNext(io.selector)
      result(i)           := RegNext(customBit.io.out)
    }

    io.out := result.asUInt

  } else {

    io.out := 0.U

  }

}

class CustomBitInterface(
    dataWidth: Int,
    functBits: Int,
    lutArity: Int
) extends Bundle {
  val config = Input(new CustomBitConfigInterface(dataWidth))
  val rsx    = Input(UInt(lutArity.W))
  val addr   = Input(UInt(functBits.W))
  val out    = Output(UInt(1.W))
}

class CustomBitConfigInterface(
    dataWidth: Int
) extends Bundle {
  val writeEnable = Bool()
  val loadData    = UInt(dataWidth.W)
}

class CustomBit(
    dataWidth: Int,
    functBits: Int,
    lutArity: Int,
    equations: Seq[BigInt]
) extends Module {

  val numFuncts = 1 << functBits

  require(
    lutArity == 4,
    s"Provided LUT arity of ${lutArity} is invalid. LUT arities must be in range 4."
  )

  require(
    equations.size == numFuncts,
    s"Invalid number of lut equations! Expected %d got %d.".format(
      numFuncts,
      equations.size
    )
  )

  val bad_equation = equations.find(e => e >= (1 << 16))
  require(
    bad_equation.isEmpty,
    s"Bad lut equation %x".format(bad_equation.getOrElse(0))
  )

  // As the RAM32M16 has eight ports and accepts the initial values in an interleaved format,
  // the `equations` need to be transformed as follows:
  //
  //   |         Port A         |         Port B         |     |         Port H         |
  //   |     15          14     |     13          12     | ... |      1           0     |
  // --|------------------------|------------------------|-----|------------------------|
  //  0|init[0][ 1]  init[0][ 0]|init[1][ 1]  init[1][ 0]|     |init[7][ 1]  init[7][ 0]|
  //  1|init[0][ 3]  init[0][ 2]|init[1][ 3]  init[1][ 2]|     |init[7][ 3]  init[7][ 2]|
  //  2|init[0][ 5]  init[0][ 4]|init[1][ 5]  init[1][ 4]|     |init[7][ 5]  init[7][ 4]|
  //   |          ...           |          ...           |     |          ...           |
  // 30|init[0][61]  init[0][60]|init[1][61]  init[1][60]|     |init[7][61]  init[7][60]|
  // 31|init[0][63]  init[0][62]|init[1][63]  init[1][62]|     |init[7][63]  init[7][62]|
  val init = Seq.tabulate(8) { i =>
    equations.map(x => ((x >> (2 * (7 - i))) & 3)).foldLeft(BigInt(0)) { (a, b) =>
      a + (b << 2).toInt
    }
  } // 8x64

  val io = IO(new CustomBitInterface(dataWidth, functBits, lutArity))

  class Wrapped32x16RAM(init: Seq[BigInt])
      extends BlackBox(
        Map(
          "INIT_A" -> init(0),
          "INIT_B" -> init(1),
          "INIT_C" -> init(2),
          "INIT_D" -> init(3),
          "INIT_E" -> init(4),
          "INIT_F" -> init(5),
          "INIT_G" -> init(6),
          "INIT_H" -> init(7)
        )
      )
      with HasBlackBoxResource {
    val io = IO(new Bundle {
      val clock = Input(Clock())
      val we    = Input(Bool())
      val addr  = Input(UInt(functBits.W))
      val din   = Input(UInt(dataWidth.W))
      val dout  = Output(UInt(dataWidth.W))
    })
    addResource("/verilog/Wrapped32x16RAM/Wrapped32x16RAM.v")
  }

  val ram   = Module(new Wrapped32x16RAM(init))
  val dout  = Wire(UInt(16.W))
  val mid0  = Reg(UInt(1.W))
  val mid1  = Reg(UInt(1.W))
  val mid2  = Reg(UInt(1.W))
  val mid3  = Reg(UInt(1.W))
  val rsx_1 = RegNext(io.rsx)
  val rsx_2 = RegNext(rsx_1)

  ram.io.clock := clock
  ram.io.we    := io.config.writeEnable
  ram.io.addr  := io.addr
  ram.io.din   := io.config.loadData
  dout         := RegNext(ram.io.dout)

  mid0 := Mux(rsx_1(1).asBool, Mux(rsx_1(0).asBool, dout(15), dout(14)), Mux(rsx_1(0).asBool, dout(13), dout(12)))
  mid1 := Mux(rsx_1(1).asBool, Mux(rsx_1(0).asBool, dout(11), dout(10)), Mux(rsx_1(0).asBool, dout(9), dout(8)))
  mid2 := Mux(rsx_1(1).asBool, Mux(rsx_1(0).asBool, dout(7), dout(6)), Mux(rsx_1(0).asBool, dout(5), dout(4)))
  mid3 := Mux(rsx_1(1).asBool, Mux(rsx_1(0).asBool, dout(3), dout(2)), Mux(rsx_1(0).asBool, dout(1), dout(0)))

  io.out := Mux(rsx_2(3).asBool, Mux(rsx_2(2).asBool, mid0, mid1), Mux(rsx_2(2).asBool, mid2, mid3))
}

object CustomALUGen extends App {

  val rgen = Random

  val dataWidth = 16
  val functBits = 5
  val lutArity  = 4
  val equations = Seq.fill(1 << functBits) {
    Seq.fill(dataWidth) { BigInt(rgen.nextInt(1 << 16)) }
  }

  new ChiselStage().emitVerilog(
    new CustomAlu(dataWidth, functBits, lutArity, equations)
  )

}
