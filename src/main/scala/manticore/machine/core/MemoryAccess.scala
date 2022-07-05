package manticore.machine.core

import Chisel._
import chisel3.experimental.IO
import manticore.machine.ISA
import manticore.machine.memory.CacheConfig
import manticore.machine.memory.SimpleDualPortMemoryInterface

object MemoryAccess {

  type PipeIn = ExecuteInterface.PipeOut

  class PipeOut(config: ISA, DimX: Int, DimY: Int) extends Bundle {
    val result: UInt      = UInt(config.DataBits.W)
    val packet: NoCBundle = new NoCBundle(DimX, DimY, config)
    val write_back: Bool  = Bool()
    val rd: UInt          = UInt(config.IdBits.W)
    val send: Bool        = Bool()
    val nop: Bool         = Bool()
    val mul: Bool         = Bool()
    val mulh: Bool        = Bool()
  }

}

class MemoryInterface(config: ISA, DimX: Int, DimY: Int) extends Bundle {
  val pipe_in  = Input(new MemoryAccess.PipeIn(config))
  val pipe_out = Output(new MemoryAccess.PipeOut(config, DimX, DimY))
  val local_memory_interface = Flipped(
    new SimpleDualPortMemoryInterface(
      ADDRESS_WIDTH = 12,
      DATA_WIDTH = config.DataBits
    )
  )
  val global_memory_interface = Flipped(CacheConfig.frontInterface())
}

class MemoryAccess(config: ISA, DimX: Int, DimY: Int) extends Module {

  val io = IO(new MemoryInterface(config, DimX, DimY))

  // connect to memory for read and write
  io.local_memory_interface.raddr := io.pipe_in.result
  io.local_memory_interface.waddr := io.pipe_in.result
  io.local_memory_interface.din   := io.pipe_in.data
  io.local_memory_interface.wen   := io.pipe_in.opcode.lstore && io.pipe_in.pred

  // connect to the global memory (i.e., cache)
  if (config.WithGlobalMemory) {
    io.global_memory_interface.cmd   := io.pipe_in.gmem.command
    io.global_memory_interface.addr  := io.pipe_in.gmem.address
    io.global_memory_interface.start := io.pipe_in.gmem.start
    io.global_memory_interface.wdata := io.pipe_in.gmem.wdata
  }

  val packet_reg = RegInit(
    NoCBundle(DimX, DimY, config),
    NoCBundle.empty(DimX, DimY, config)
  )

  require(log2Ceil(DimX) + log2Ceil(DimY) <= io.pipe_in.immediate.getWidth)
  require(log2Ceil(DimX) <= io.pipe_in.immediate.getWidth / 2)
  require(log2Ceil(DimY) <= io.pipe_in.immediate.getWidth / 2)

  require(io.pipe_in.immediate.getWidth % 2 == 0)

  val hop_bits: Int = io.pipe_in.immediate.getWidth / 2

  //  packet_reg.xHops := io.pipe_in.immediate(log2Ceil(DimX) - 1, 0)
  packet_reg.xHops := io.pipe_in.immediate.tail(hop_bits)
  packet_reg.yHops := io.pipe_in.immediate.head(hop_bits)
  //  packet_reg.yHops := io.pipe_in.immediate(log2Ceil(DimY) + log2Ceil(DimX) - 1, log2Ceil(DimX))
  packet_reg.data    := io.pipe_in.data
  packet_reg.address := io.pipe_in.rd
  packet_reg.valid   := (io.pipe_in.opcode.send)

  io.pipe_out.packet := packet_reg

  def pipeIt[T <: Data](dest: T)(source: T): Unit = {
    val pipereg = Reg(chisel3.chiselTypeOf(source))
    pipereg := source
    dest    := pipereg
  }

  pipeIt(io.pipe_out.write_back) {
    io.pipe_in.opcode.lload ||
    io.pipe_in.opcode.cust ||
    io.pipe_in.opcode.arith || {
      if (config.WithGlobalMemory) io.pipe_in.opcode.gload else false.B
    } ||
    io.pipe_in.opcode.set ||
    io.pipe_in.opcode.slice
  }
  pipeIt(io.pipe_out.rd) {
    io.pipe_in.rd
  }
  
  pipeIt(io.pipe_out.nop) {
    io.pipe_in.opcode.nop
  }
  pipeIt(io.pipe_out.mul) {
    io.pipe_in.opcode.mul
  }
  pipeIt(io.pipe_out.mulh) {
    io.pipe_in.opcode.mulh
  }
  val lload_r = Reg(Bool())
  val gload_r = Reg(Bool())

  lload_r := io.pipe_in.opcode.lload
  gload_r := io.pipe_in.opcode.gload
  if (config.WithGlobalMemory) {
    when(lload_r) {
      io.pipe_out.result := io.local_memory_interface.dout
    }.elsewhen(gload_r) {
      io.pipe_out.result := io.global_memory_interface.rdata
    } otherwise {
      pipeIt(io.pipe_out.result) { io.pipe_in.result }
    }
  } else {
    when(lload_r) {
      io.pipe_out.result := io.local_memory_interface.dout
    } otherwise {
      pipeIt(io.pipe_out.result) { io.pipe_in.result }
    }
  }


}
