/**
 * Copyright 2021 Mahyar Emami
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */
package thyrio.core


import Chisel._
import chisel3.stage.ChiselStage
import memory.{GenericMemory, GenericMemoryInterface, SimpleDualPortMemory, SimpleDualPortMemoryInterface}
import thyrio.{ISA, ThyrioISA}

class RegisterFileInterface(config: ISA) extends Bundle {
  def makeAddr = Input(UInt(config.ID_BITS.W))
  def makeDout = Output(UInt(config.DATA_BITS.W))
  class ReadIf extends Bundle {
    val addr = makeAddr
    val dout = makeDout

    def <->(mem_if : GenericMemoryInterface): Unit = {
      mem_if.addra := addr
      dout := mem_if.douta
      mem_if.wea := false.B
    }
    def <->(mem_if: SimpleDualPortMemoryInterface): Unit = {
      mem_if.raddr := addr
      dout := mem_if.dout
    }

  }
  class WriteIf extends Bundle {
    val addr = makeAddr
    val din = Input(UInt(config.DATA_BITS.W))
    val en = Input(Bool())

    def <->(mem_if: GenericMemoryInterface): Unit = {
      mem_if.addrb := addr
      mem_if.dinb := din
      mem_if.web := en
    }
    def <->(mem_if: SimpleDualPortMemoryInterface): Unit = {
      mem_if.waddr := addr
      mem_if.din := din
      mem_if.wen := en
    }
  }
  val rx, ry, ru, rv = new ReadIf
  val w = new WriteIf

}

class RegisterFile(config: ISA, INIT: String = "") extends Module {

  val io = IO(new RegisterFileInterface(config))

  def makeBank = new SimpleDualPortMemory(ADDRESS_WIDTH = config.ID_BITS,
    DATA_WIDTH = config.DATA_BITS, INIT = INIT)
  val xbank, ybank, ubank, vbank = Module(makeBank)

  io.w <-> xbank.io
  io.w <-> ybank.io
  io.w <-> ubank.io
  io.w <-> vbank.io

  io.rx <-> xbank.io
  io.ry <-> ybank.io
  io.ru <-> ubank.io
  io.rv <-> vbank.io

}

object RegisterFileGen extends App {


  new ChiselStage().emitVerilog(new RegisterFile(ThyrioISA), Array("--help"))
}
