package thyrio.core


import Chisel._
import chisel3.experimental.IO
import thyrio.ISA
import memory.{CacheConfig, GenericMemoryInterface, SimpleDualPortMemoryInterface}


object MemoryAccess {

  type PipeIn = ExecuteInterface.PipeOut

  class PipeOut(config: ISA, DimX: Int, DimY: Int) extends Bundle {
    val mem_data: UInt = UInt(config.DataBits.W)
    val result: UInt = UInt(config.DataBits.W)
    val packet: NoCBundle = new NoCBundle(DimX, DimY, config)
    val write_back: Bool = Bool()
    val rd: UInt = UInt(config.DataBits.W)
    val lload: Bool = Bool()
    val send: Bool = Bool()
    val nop: Bool = Bool()
  }


}


class MemoryInterface(config: ISA, DimX: Int, DimY: Int) extends Bundle {
  val pipe_in = Input(new MemoryAccess.PipeIn(config))
  val pipe_out = Output(new MemoryAccess.PipeOut(config, DimX, DimY))
  val local_memory_interface = Flipped(
    new SimpleDualPortMemoryInterface(ADDRESS_WIDTH = 11, DATA_WIDTH = config.DataBits))
  val global_memory_interface = Flipped(CacheConfig.frontInterface())
}

class MemoryAccess(config: ISA, DimX: Int, DimY: Int) extends Module {

  val io = IO(new MemoryInterface(config, DimX, DimY))

  // connect to memory for read and write
  io.local_memory_interface.raddr := io.pipe_in.result
  io.local_memory_interface.waddr := io.pipe_in.result
  io.local_memory_interface.din := io.pipe_in.data
  io.local_memory_interface.wen := io.pipe_in.opcode.lstore


  // connect to the global memory (i.e., cache)
  if (config.WithGlobalMemory) {
    io.global_memory_interface.cmd := io.pipe_in.gmem.command
    io.global_memory_interface.addr := io.pipe_in.gmem.address
    io.global_memory_interface.start := io.pipe_in.gmem.start
    io.global_memory_interface.wdata := io.pipe_in.gmem.wdata
  }


  val packet_reg = Reg(new NoCBundle(16, 16, config))

  require(log2Ceil(DimX) + log2Ceil(DimY) <= io.pipe_in.immediate.getWidth)
  require(log2Ceil(DimX) <= io.pipe_in.immediate.getWidth / 2)
  require(log2Ceil(DimY) <= io.pipe_in.immediate.getWidth / 2)

  require(io.pipe_in.immediate.getWidth % 2 == 0)

  val hop_bits: Int = io.pipe_in.immediate.getWidth / 2

  //  packet_reg.xHops := io.pipe_in.immediate(log2Ceil(DimX) - 1, 0)
  packet_reg.xHops := io.pipe_in.immediate.tail(hop_bits)
  packet_reg.yHops := io.pipe_in.immediate.head(hop_bits)
  //  packet_reg.yHops := io.pipe_in.immediate(log2Ceil(DimY) + log2Ceil(DimX) - 1, log2Ceil(DimX))
  packet_reg.data := io.pipe_in.data
  packet_reg.address := io.pipe_in.rd
  packet_reg.valid := (io.pipe_in.opcode.send || io.pipe_in.opcode.expect)

  io.pipe_out.packet := packet_reg

  if (config.WithGlobalMemory) {
    when(io.pipe_in.opcode.gload) {
      io.pipe_out.mem_data := io.global_memory_interface.rdata
    } otherwise {
      io.pipe_out.mem_data := io.local_memory_interface.dout
    }
  } else {
    io.pipe_out.mem_data := io.local_memory_interface.dout
  }



  def pipeIt[T <: Data](dest: T)(source: T): Unit = {
    val pipereg = Reg(chisel3.chiselTypeOf(source))
    pipereg := source
    dest := pipereg
  }


  pipeIt(io.pipe_out.write_back) {
    io.pipe_in.opcode.lload ||
      io.pipe_in.opcode.cust0 ||
      io.pipe_in.opcode.arith ||
      { if (config.WithGlobalMemory) io.pipe_in.opcode.gload else false.B } ||
      io.pipe_in.opcode.set
  }
  pipeIt(io.pipe_out.rd){
    io.pipe_in.rd
  }
  pipeIt(io.pipe_out.lload){
    io.pipe_in.opcode.lload
  }

  pipeIt(io.pipe_out.nop) {
    io.pipe_in.opcode.nop
  }

  pipeIt(io.pipe_out.result) {
    io.pipe_in.result
  }




}
