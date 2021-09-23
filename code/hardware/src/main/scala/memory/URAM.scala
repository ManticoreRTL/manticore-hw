package memory

import Chisel._
import chisel3.SyncReadMem

object URAMBuilder {

  class URAM4kInterface(DATA_BITS: Int) extends Bundle {
    val addra: UInt = Input(UInt(12.W))
    val douta: UInt = Output(UInt(DATA_BITS.W))

    val addrb: UInt = Input(UInt(12.W))
    val dinb: UInt = Input(UInt(DATA_BITS.W))
    val web: Bool = Input(Bool())
    val bweb: UInt = Input(UInt((DATA_BITS / 8).W))
  }

  class URAMImpl(DATA_BITS: Int) extends Module {
    class Interface extends URAM4kInterface(DATA_BITS)
    val io: URAM4kInterface = IO(new Interface)
    val num_bytes = DATA_BITS / 8
    val memory: Seq[SyncReadMem[UInt]] = Seq.fill(num_bytes)(SyncReadMem(4096, UInt(8.W)))
    val douta: UInt = Wire(UInt(DATA_BITS.W))
    douta := Cat(memory.map(_(io.addra)).reverse)
    io.douta := douta
    when(io.web) {
      for (idx <- Range(0, num_bytes)) {
        when(io.bweb(idx)) {
          memory(idx)(io.addrb) := io.dinb((idx + 1) * 8 - 1, idx * 8)
        }
      }
    }
  }

}
class URAM4kx64Interface extends Bundle {

  val addra: UInt = Input(UInt(12.W))
  val douta: UInt = Output(UInt(64.W))

  val addrb: UInt = Input(UInt(12.W))
  val dinb: UInt = Input(UInt(64.W))
  val web: Bool = Input(Bool())
  val bweb: UInt = Input(UInt(8.W))

}
//class URAM4kx64 extends Module {
//  val io: URAM4kx64Interface = IO(new URAM4kx64Interface)
//  val memory: Seq[Mem[UInt]] = Seq.fill(8)(Mem(4096, UInt(8.W)))
//  val reg_douta: UInt = Reg(UInt(64.W))
//  reg_douta :=
//    Cat(for(idx <- Range(0, 8).reverse) yield memory(idx)(io.addra))
//  io.douta := reg_douta
//  when(io.web) {
//    for (idx <- Range(0, 8)) {
//      when(io.bweb(idx)) {
//        memory(idx)(io.addrb) := io.dinb((idx + 1) * 8 - 1, idx * 8)
//      }
//    }
//  }
//}
//
//class URAM4kx72Interface extends Bundle {
//
//  val addra: UInt = Input(UInt(12.W))
//  val douta: UInt = Output(UInt(72.W))
//
//  val addrb: UInt = Input(UInt(12.W))
//  val dinb: UInt = Input(UInt(72.W))
//  val web: Bool = Input(Bool())
//  val bweb: UInt = Input(UInt(9.W))
//
//}

class URAM4kx72 extends URAMBuilder.URAMImpl(72)
class URAM4kx64 extends URAMBuilder.URAMImpl(64)

object URAM4Kx64 {
  def apply() = new URAM4kx64()
  def emitVerilog() = new chisel3.stage.ChiselStage().emitVerilog(apply())
}