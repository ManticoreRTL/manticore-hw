package thyrio.core


import Chisel._
import chisel3.experimental.IO
import thyrio.ISA
import memory.{GenericMemoryInterface, SimpleDualPortMemoryInterface}
import thyrio.core.MemoryAccess.{DIMX, DIMY}

object MemoryAccess {

  val DIMX = 16
  val DIMY = 16
  type PipeIn = ExecuteInterface.PipeOut

  class PipeOut(config : ISA) extends Bundle {
    val mem_data: UInt = UInt(config.DATA_BITS.W)
    val result: UInt = UInt(config.DATA_BITS.W)
    val packet: NoCBundle = new NoCBundle(DIMX, DIMY, config)
    val write_back: Bool = Bool()
    val rd: UInt = UInt(config.DATA_BITS.W)
    val load: Bool = Bool()
    val send: Bool = Bool()
    val nop: Bool = Bool()
  }


}


class MemoryInterface(config: ISA) extends Bundle {
  val pipe_in = Input(new MemoryAccess.PipeIn(config))
  val pipe_out = Output(new MemoryAccess.PipeOut(config))
  val memory_interface = Flipped(
    new SimpleDualPortMemoryInterface(ADDRESS_WIDTH=11, DATA_WIDTH=config.DATA_BITS))

}
class MemoryAccess(config: ISA) extends Module {

  val io = IO(new MemoryInterface(config))

  // connect to memory for read and write
  io.memory_interface.raddr := io.pipe_in.result
  io.memory_interface.waddr := io.pipe_in.result
  io.memory_interface.din := io.pipe_in.data
  io.memory_interface.wen := io.pipe_in.opcode.lstore



  val packet_reg = Reg(new NoCBundle(16, 16, config))

  packet_reg.xHops := io.pipe_in.immediate(log2Ceil(DIMX) - 1, 0)
  packet_reg.yHops := io.pipe_in.immediate(log2Ceil(DIMY) + log2Ceil(DIMX) - 1, log2Ceil(DIMX))
  packet_reg.data := io.pipe_in.data
  packet_reg.address := io.pipe_in.rd
  packet_reg.valid := (io.pipe_in.opcode.send || io.pipe_in.opcode.expect)

  io.pipe_out.packet := packet_reg
  io.pipe_out.mem_data := io.memory_interface.dout

  val write_back_reg: Bool = Reg(Bool())
  val rd_reg: UInt = Reg(UInt(config.DATA_BITS.W))
  val result_reg: UInt = Reg(UInt(config.DATA_BITS.W))
  val load: Bool = Reg(Bool())
  val nop: Bool = Reg(Bool())

  write_back_reg :=
    io.pipe_in.opcode.lload ||
    io.pipe_in.opcode.cust0 ||
    io.pipe_in.opcode.arith ||
    io.pipe_in.opcode.gload ||
    io.pipe_in.opcode.set
  rd_reg := io.pipe_in.rd

  io.pipe_out.write_back := write_back_reg
  io.pipe_out.rd := rd_reg

  result_reg := io.pipe_in.result
  io.pipe_out.result := result_reg


  load := io.pipe_in.opcode.lload
  io.pipe_out.load := load
  nop := io.pipe_in.opcode.nop
  io.pipe_out.nop := nop


}
