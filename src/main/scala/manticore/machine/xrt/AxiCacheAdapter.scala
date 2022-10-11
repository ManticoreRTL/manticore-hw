package manticore.machine.xrt

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum
import manticore.machine.memory.Cache
import manticore.machine.memory.CacheConfig
import manticore.machine.memory.CacheBackendCommand
import chisel3.stage.ChiselStage
import manticore.machine.core.SkidBuffer

object AxiCacheAdapter {
  object CacheAxiParameters extends AxiParameters {
    override val DataWidth: Int = CacheConfig.CacheLineBits
  }

}
class AxiCacheAdapterInterface extends Bundle {

  val cache = Flipped(CacheConfig.backInterface())
  val bus   = new AxiMasterIF(AxiCacheAdapter.CacheAxiParameters)
  val base  = Input(UInt(64.W)) // base of memory

}

class AxiCacheBusSkidBuffer extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(new AxiCacheAdapterInterface().bus)
    val deq = new AxiCacheAdapterInterface().bus
  })

  class AddressChannel(
      addrWidth: Int
  ) extends Bundle {
    val ADDR  = UInt(addrWidth.W)
    val LEN   = UInt(8.W)
    val SIZE  = UInt(3.W)
    val BURST = UInt(3.W)
  }

  class WriteDataChannel(
      dataWidth: Int
  ) extends Bundle {
    val DATA = UInt(dataWidth.W)
    val STRB = UInt((dataWidth / 8).W)
    val LAST = Bool()
  }

  class WriteResponseChannel extends Bundle {
    val BRESP = UInt(2.W)
  }

  class ReadDataChannel(
      dataWidth: Int
  ) extends Bundle {
    val RDATA = UInt(dataWidth.W)
    val RLAST = Bool()
    val RRESP = UInt(2.W)
  }

  val ar_skid = Module(new SkidBuffer(new AddressChannel(io.enq.ARADDR.getWidth)))
  val r_skid  = Module(new SkidBuffer(new ReadDataChannel(io.enq.RDATA.getWidth)))
  val aw_skid = Module(new SkidBuffer(new AddressChannel(io.enq.AWADDR.getWidth)))
  val w_skid  = Module(new SkidBuffer(new WriteDataChannel(io.enq.WDATA.getWidth)))
  val b_skid  = Module(new SkidBuffer(new WriteResponseChannel))

  // AR channel
  // enq-side
  ar_skid.io.enq.bits.ADDR  := io.enq.ARADDR
  ar_skid.io.enq.bits.BURST := io.enq.ARBURST
  ar_skid.io.enq.bits.LEN   := io.enq.ARLEN
  ar_skid.io.enq.bits.SIZE  := io.enq.ARSIZE
  ar_skid.io.enq.valid      := io.enq.ARVALID
  io.enq.ARREADY            := ar_skid.io.enq.ready
  // deq-side
  io.deq.ARADDR        := ar_skid.io.deq.bits.ADDR
  io.deq.ARBURST       := ar_skid.io.deq.bits.BURST
  io.deq.ARLEN         := ar_skid.io.deq.bits.LEN
  io.deq.ARSIZE        := ar_skid.io.deq.bits.SIZE
  io.deq.ARVALID       := ar_skid.io.deq.valid
  ar_skid.io.deq.ready := io.deq.ARREADY

  // R channel
  // enq-side
  r_skid.io.enq.bits.RDATA := io.deq.RDATA
  r_skid.io.enq.bits.RLAST := io.deq.RLAST
  r_skid.io.enq.bits.RRESP := io.deq.RRESP
  r_skid.io.enq.valid      := io.deq.RVALID
  io.deq.RREADY            := r_skid.io.enq.ready
  // deq-side
  io.enq.RDATA        := r_skid.io.deq.bits.RDATA
  io.enq.RLAST        := r_skid.io.deq.bits.RLAST
  io.enq.RRESP        := r_skid.io.deq.bits.RRESP
  io.enq.RVALID       := r_skid.io.deq.valid
  r_skid.io.deq.ready := io.enq.RREADY

  // AW channel
  // enq-side
  aw_skid.io.enq.bits.ADDR  := io.enq.AWADDR
  aw_skid.io.enq.bits.BURST := io.enq.AWBURST
  aw_skid.io.enq.bits.LEN   := io.enq.AWLEN
  aw_skid.io.enq.bits.SIZE  := io.enq.AWSIZE
  aw_skid.io.enq.valid      := io.enq.AWVALID
  io.enq.AWREADY            := aw_skid.io.enq.ready
  // deq-side
  io.deq.AWADDR        := aw_skid.io.deq.bits.ADDR
  io.deq.AWBURST       := aw_skid.io.deq.bits.BURST
  io.deq.AWLEN         := aw_skid.io.deq.bits.LEN
  io.deq.AWSIZE        := aw_skid.io.deq.bits.SIZE
  io.deq.AWVALID       := aw_skid.io.deq.valid
  aw_skid.io.deq.ready := io.deq.AWREADY

  // W channel
  // enq-side
  w_skid.io.enq.bits.DATA := io.enq.WDATA
  w_skid.io.enq.bits.STRB := io.enq.WSTRB
  w_skid.io.enq.bits.LAST := io.enq.WLAST
  w_skid.io.enq.valid     := io.enq.WVALID
  io.enq.WREADY           := w_skid.io.enq.ready
  // deq-side
  io.deq.WDATA        := w_skid.io.deq.bits.DATA
  io.deq.WSTRB        := w_skid.io.deq.bits.STRB
  io.deq.WLAST        := w_skid.io.deq.bits.LAST
  io.deq.WVALID       := w_skid.io.deq.valid
  w_skid.io.deq.ready := io.deq.WREADY

  // B channel
  // enq-side
  b_skid.io.enq.bits.BRESP := io.deq.BRESP
  b_skid.io.enq.valid      := io.deq.BVALID
  io.deq.BREADY            := b_skid.io.enq.ready
  // deq-side
  io.enq.BRESP        := b_skid.io.deq.bits.BRESP
  io.enq.BVALID       := b_skid.io.deq.valid
  b_skid.io.deq.ready := io.enq.BREADY
}

// set the base address base on the memory bank you wish to use
class AxiCacheAdapter extends Module {

  val io = IO(new AxiCacheAdapterInterface)

  val waddr = Reg(chiselTypeOf(io.cache.waddr))
  val raddr = Reg(chiselTypeOf(io.cache.raddr))
  val cmd   = Reg(chiselTypeOf(io.cache.cmd))
  val wline = Reg(chiselTypeOf(io.cache.wline))
  val rline = Reg(chiselTypeOf(io.cache.rline))
  val base  = RegNext(io.base)
  io.cache.rline := rline
  io.cache.done  := false.B

  object TxState extends ChiselEnum {
    val sIdle, sAxiReadAddr, sAxiReadData, sAxiWriteAddr, sAxiWriteData, sDone = Value
  }

  def decode(data: UInt, cmd: CacheBackendCommand.Type): Bool = {
    data === cmd.id.U
  }

  import TxState._
  val state = RegInit(TxState.Type(), sIdle)

  when(io.cache.start) {
    waddr := io.cache.waddr
    raddr := io.cache.raddr
    cmd   := io.cache.cmd
    wline := io.cache.wline
  }

  // read address channel default values
  io.bus.ARADDR  := ((raddr + (base >> 1)) << 1.U)            // bytes-align the address
  io.bus.ARVALID := false.B
  io.bus.ARLEN   := 0.U                                       // burst length is 1
  io.bus.ARSIZE  := log2Ceil(CacheConfig.CacheLineBits / 8).U // each transfer contains (256 / 8) = 32 bytes
  io.bus.ARBURST := 1.U                                       // incr burst mode, not really used

  // read data channel defaults
  io.bus.RREADY := false.B

  // write address channel defaults
  io.bus.AWADDR  := ((waddr + (base >> 1)) << 1.U)            // bytes-align the address
  io.bus.AWLEN   := 0.U                                       // burst length is 1
  io.bus.AWSIZE  := log2Ceil(CacheConfig.CacheLineBits / 8).U // 32 bytes in each transfer
  io.bus.AWVALID := false.B

  // write data channel defaults
  io.bus.WDATA   := wline
  io.bus.WSTRB   := Fill(CacheConfig.CacheLineBits / 8, 1.U)
  io.bus.WLAST   := false.B
  io.bus.WVALID  := false.B
  io.bus.BREADY  := true.B // ignores the response
  io.bus.AWBURST := 1.U    // incr burst, not used

  when(state === sAxiReadData && io.bus.RVALID) {
    rline := io.bus.RDATA
  }
  switch(state) {
    is(sIdle) {
      when(io.cache.start) {
        when(decode(io.cache.cmd, CacheBackendCommand.WriteBack) || decode(io.cache.cmd, CacheBackendCommand.Write)) {
          state := sAxiWriteAddr
        } otherwise {
          state := sAxiReadAddr
        }
      }
    }
    is(sAxiWriteAddr) {
      io.bus.AWVALID := true.B
      when(io.bus.AWREADY) {
        state := sAxiWriteData
      }
    }
    is(sAxiWriteData) {
      io.bus.WVALID := true.B
      io.bus.WLAST  := true.B
      when(io.bus.WREADY) {
        // go to idle if the command was a write, otherwise perform a read
        state := Mux(decode(cmd, CacheBackendCommand.WriteBack), sAxiReadAddr, sDone)
      }
    }
    is(sAxiReadAddr) {
      io.bus.ARVALID := true.B
      when(io.bus.ARREADY) {
        state := sAxiReadData
      }
    }
    is(sAxiReadData) {
      io.bus.RREADY := true.B
      when(io.bus.RVALID) {
        state := sDone
      }
    }
    is(sDone) {
      state         := sIdle
      io.cache.done := true.B
    }
  }

}

class CacheSubsystemInterface extends Bundle {
  val core     = CacheConfig.frontInterface()
  val bus      = new AxiMasterIF(AxiCacheAdapter.CacheAxiParameters)
  val base     = Input(UInt(64.W))
  val counters = CacheConfig.counterInterface()
}

class CacheSubsystem extends Module {
  val io = IO(new CacheSubsystemInterface)

  val front_pipe = Module(CacheConfig.frontPipe())
  val cache      = Module(new Cache)
  val back_pipe  = Module(CacheConfig.backPipe())
  val axi        = Module(new AxiCacheAdapter)
  // val bus_skid   = Module(new AxiCacheBusSkidBuffer)

  axi.io.base := io.base
  front_pipe.io.in <> io.core
  cache.io.front <> front_pipe.io.out
  back_pipe.io.in <> cache.io.back
  axi.io.cache <> back_pipe.io.out
  io.bus <> axi.io.bus
  // bus_skid.io.enq <> axi.io.bus
  // io.bus <> bus_skid.io.deq

  io.counters := cache.io.perf

}

object Generator2132 extends App {
  new ChiselStage().emitVerilog(new AxiCacheAdapter, Array("-td", "gen-dir"))
}
