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
package manticore.machine.core

import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util.log2Ceil
import manticore.machine.ISA
import manticore.machine.ManticoreBaseISA
import manticore.machine.memory.DummyDualPortMemory
import manticore.machine.memory.GenericMemoryInterface
import manticore.machine.memory.SimpleDualPortMemory
import manticore.machine.memory.SimpleDualPortMemoryInterface

class RegisterFileInterface(config: ISA) extends Bundle {
  def makeAddr = Input(UInt(config.IdBits.W))
  class ReadIf(data_width: Int) extends Bundle {
    val addr = makeAddr
    val dout = Output(UInt(data_width.W))

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
    val din = Input(UInt((config.DataBits + 1).W))
    val en = Input(Bool())

    def <->(mem_if: GenericMemoryInterface): Unit = {
      if (mem_if.DATA_WIDTH > config.DataBits) {
        // both write interface and actual register have DataBits + 1 width
        mem_if.addrb := addr
        mem_if.dinb := din
        mem_if.web := en
      } else {
        // write interface has DataBits + 1 width, but actual register is DataBits width
        mem_if.addrb := addr 
        mem_if.dinb := din(config.DataBits - 1, 0)
        mem_if.web := en
      }
    }
    def <->(mem_if: SimpleDualPortMemoryInterface): Unit = {
      if (mem_if.DATA_WIDTH > config.DataBits) {
        // both write interface and actual register have DataBits + 1 width
        mem_if.waddr := addr
        mem_if.din := din
        mem_if.wen := en
      } else {
        // write interface has DataBits + 1 width, but actual register is DataBits width
        mem_if.waddr := addr 
        mem_if.din := din(config.DataBits - 1, 0)
        mem_if.wen := en
      }
    }
  }
  val rs1, rs2, rs4 = new ReadIf(config.DataBits)
  val rs3 = new ReadIf(config.DataBits + 1) // only rs3 has one additional bit
  val w = new WriteIf

}

class RegisterFile(
  config: ISA,
  INIT: String = "",
  enable_custom_alu: Boolean = true
) extends Module {

  val io = IO(new RegisterFileInterface(config))

  def makeBank(
    enable: Boolean = true,
    data_width: Int = config.DataBits
  ) = {
    if (enable) {
      new SimpleDualPortMemory(
        ADDRESS_WIDTH = config.IdBits, 
        DATA_WIDTH = data_width,
        READ_LATENCY = 2, 
        INIT = INIT
      )
    } else {
      new DummyDualPortMemory(ADDRESS_WIDTH = config.IdBits, DATA_WIDTH = data_width)
    }
  }

  // Banks 1, 2, and 3 are always enabled (bank 3 is needed for mux instructions' select bit for now).
  // Bank 4 is disabled if the custom ALU is disabled.
  val rs1bank = Module(makeBank(true, config.DataBits))
  val rs2bank = Module(makeBank(true, config.DataBits))
  val rs3bank = Module(makeBank(true, config.DataBits + 1)) // one additional bit for carry 
  val rs4bank = Module(makeBank(enable_custom_alu, config.DataBits))

  io.w <-> rs1bank.io
  io.w <-> rs2bank.io
  io.w <-> rs3bank.io
  io.w <-> rs4bank.io

  io.rs1 <-> rs1bank.io
  io.rs2 <-> rs2bank.io
  io.rs3 <-> rs3bank.io
  io.rs4 <-> rs4bank.io

}


// class CarryRegisterFileInterface(config: ISA) extends Bundle {
//   val AddressBits = log2Ceil(config.CarryCount)
//   val raddr = Input(UInt(AddressBits.W))
//   val waddr = Input(UInt(AddressBits.W))
//   val din = Input(UInt(1.W))
//   val dout = Output(UInt(1.W))
//   val wen = Input(Bool())
// }


// class CarryRegisterFile(config: ISA) extends Module {
//   val io = IO(new CarryRegisterFileInterface(config))
//   val storage = SyncReadMem(config.CarryCount, UInt(1.W))
//   when(io.wen) {
//     storage(io.waddr) := io.din
//   }
//   io.dout := storage(io.raddr)
// }

// This is not really a "register file" as it doesn't have a raddr port. All
// outputs are available in parallel (i.e., it is an array of registers). Only
// one register can be updated at a time though.
class LutLoadDataRegisterFileInterface(config: ISA) extends Bundle {
  val din = Input(UInt(config.DataBits.W))
  val dout = Output(UInt(config.DataBits.W))
  val wen = Input(Bool())
}

class LutLoadDataRegisterFile(config: ISA, enable_custom_alu: Boolean = true) extends Module {
  // TODO: as this module is essentially just a 16 bit register, it may be better for us to
  // incorporate this functionality in the execute stage.
  val io = IO(new LutLoadDataRegisterFileInterface(config))
  if (enable_custom_alu) {
    val storage = Reg(UInt(config.DataBits.W))
    when(io.wen) {
      storage := io.din
    }
    io.dout := storage
  } else {
    io.dout := 0.U
  }
}

object RegisterFileGen extends App {
  new ChiselStage().emitVerilog(new RegisterFile(ManticoreBaseISA), Array("--help"))
}
