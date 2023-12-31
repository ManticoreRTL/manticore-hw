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
package manticore.machine.core

import Chisel._
import chisel3.stage.ChiselStage
import firrtl.transforms.DontTouchAllTargets
import manticore.machine.ISA
import manticore.machine.ManticoreBaseISA
import manticore.machine.memory.MemStyle
import manticore.machine.memory.SimpleDualPortMemory
import manticore.machine.memory.SimpleDualPortMemoryInterface

/** Fetch module interface
  *   - `program_counter`: output signal to the controller, might be unsed.
  *
  *   - `execution_enable`: input bit that enables execution, if low then the PC
  *     is reset to zero and nops are implicitly fetched and pushed down the
  *     pipeline (i.e., pipeline never stalls)
  * 
  *   - `is_final_instruction`: input bit that indicates whether current 
  *     instruction is the final one or not
  *
  *   - `instruction`: output wires to the decode unit, the fetched instruction
  *
  *   - `packet`: input packet from the NoC switch, used for memory
  *     initialization and receiving data
  *
  *   - `init_data`: input bit enabling memory initialization, asserted by the
  *     controller
  *
  * @param config
  *   ISA configuration
  */
class FetchInterface(config: ISA) extends Bundle {
  class ProgrammingInterface extends Bundle {
    val enable: Bool      = Bool()
    val instruction: UInt = UInt(config.NumBits.W)
    val address: UInt     = UInt(config.NumPcBits.W)
  }
  val program_counter: UInt      = Output(UInt(config.NumPcBits.W))
  val execution_enable: Bool     = Input(Bool())
  val is_final_instruction: Bool = Input(Bool())
  val instruction: UInt          = Output(UInt(config.NumBits.W))
//  val packet: BareNoCBundle = Input(new BareNoCBundle(config))
//  val init_enable: Bool = Input(Bool())
  val programmer: ProgrammingInterface = Input(new ProgrammingInterface)
}

class Fetch(config: ISA) extends Module {

  val io = IO(new FetchInterface(config))

  require(config.NumBits == 64)

  val inst_memory = Module(
    new SimpleDualPortMemory(
      READ_LATENCY = 2,
      ADDRESS_WIDTH = 12,
      DATA_WIDTH = 64,
      STYLE = MemStyle.URAM
    )
  )

  val fetch_core = Module(new FetchCore(config))

  io <> fetch_core.io.core_interface
  fetch_core.io.memory_interface <> inst_memory.io

}

class FetchCoreInterface(config: ISA) extends Bundle {
  val core_interface = new FetchInterface(config)
  val memory_interface: SimpleDualPortMemoryInterface = Flipped(
    new SimpleDualPortMemoryInterface(
      ADDRESS_WIDTH = 12,
      DATA_WIDTH = 64
    )
  )
}

class FetchCore(config: ISA) extends Module {

  val io = IO(new FetchCoreInterface(config))

  val pc = Reg(UInt(config.NumPcBits.W))

//  // a pointer to the next place and instruction can be written
//  val inst_write_pointer = RegInit(UInt(config.NUM_PC_BITS.W), 0.U)
//  val message_write_pointer = RegInit(UInt(config.NUM_PC_BITS.W), 0.U)

//  init_enable_reg := io.core_interface.init_enable

  io.memory_interface.raddr     := pc
  io.core_interface.instruction := io.memory_interface.dout

  val stopped = Wire(Bool())

  require(
    config.NumPcBits / 8 <= 8,
    "Can only support up to 64-bit instructions"
  )
  require(
    config.DataBits == 16,
    "Not sure if can do other than 16-bit data path"
  )
  // 8 bits of byte enable assuming 64-bit instruction
//  val write_byte_en = RegInit(UInt((config.NUM_BITS / 8).W), 0x00003.U)

  /** Handle the actual "instruction fetch". The processor starts computing 
    * when it enters the `StaticExecutionPhase`, i.e. when `execution_enable`
    * is asserted. If `execution_enable` is de-asserted, then the PC is reset
    * to 0, and the processor will keep sending NOP instructions until the
    * execution is enabled again.
    * The `is_final_instruction` signal indicates if the current instruction  
    * is the final one or not. The PC is incremented when the execution is 
    * enabled AND current instruction is not the final one, to avoid the PC
    * value becoming bigger than the maximum value scheduled by the compiler.
    */
  when(io.core_interface.execution_enable && !io.core_interface.is_final_instruction) {
    pc := pc + 1.U
  } otherwise {
    pc := 0.U
  }

  stopped := !io.core_interface.execution_enable

  when(RegNext(RegNext(stopped))) {
    io.core_interface.instruction := 0.U // nop
  } otherwise {
    io.core_interface.instruction := io.memory_interface.dout
  }

  io.core_interface.program_counter := RegNext(RegNext(pc))

  io.memory_interface.waddr := io.core_interface.programmer.address
  io.memory_interface.din   := io.core_interface.programmer.instruction
  io.memory_interface.wen   := io.core_interface.programmer.enable

}

//object Fetch{
//
//  def apply(config: ISA) = new FetchCore(config)
//  def emitVerilog() = new ChiselStage().emitSystemVerilog(apply(ScalpISA0),
//    Array("--target-dir", "gen-dir"))
//}

object FetchGenerator extends App {
  println(ManticoreBaseISA.IdBits)
  println(ManticoreBaseISA.FunctBits)
  println(ManticoreBaseISA.DataBits)
  println(ManticoreBaseISA.NumPcBits)

  def emitVerilog() = new ChiselStage().emitSystemVerilog(
    new FetchCore(ManticoreBaseISA),
    Array("--target-dir", "gen-dir")
  )
}
