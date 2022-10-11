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

  val ar_skid = Module(new SkidBuffer(new AddressChannel(io.enq.AR.ARADDR.getWidth)))
  val r_skid  = Module(new SkidBuffer(new ReadDataChannel(io.enq.R.RDATA.getWidth)))
  val aw_skid = Module(new SkidBuffer(new AddressChannel(io.enq.AW.AWADDR.getWidth)))
  val w_skid  = Module(new SkidBuffer(new WriteDataChannel(io.enq.W.WDATA.getWidth)))
  val b_skid  = Module(new SkidBuffer(new WriteResponseChannel))

  // AR channel
  // enq-side
  ar_skid.io.enq.bits.ADDR  := io.enq.AR.ARADDR
  ar_skid.io.enq.bits.BURST := io.enq.AR.ARBURST
  ar_skid.io.enq.bits.LEN   := io.enq.AR.ARLEN
  ar_skid.io.enq.bits.SIZE  := io.enq.AR.ARSIZE
  ar_skid.io.enq.valid      := io.enq.AR.ARVALID
  io.enq.AR.ARREADY         := ar_skid.io.enq.ready
  // deq-side
  io.deq.AR.ARADDR     := ar_skid.io.deq.bits.ADDR
  io.deq.AR.ARBURST    := ar_skid.io.deq.bits.BURST
  io.deq.AR.ARLEN      := ar_skid.io.deq.bits.LEN
  io.deq.AR.ARSIZE     := ar_skid.io.deq.bits.SIZE
  io.deq.AR.ARVALID    := ar_skid.io.deq.valid
  ar_skid.io.deq.ready := io.deq.AR.ARREADY

  // R channel
  // enq-side
  r_skid.io.enq.bits.RDATA := io.deq.R.RDATA
  r_skid.io.enq.bits.RLAST := io.deq.R.RLAST
  r_skid.io.enq.bits.RRESP := io.deq.R.RRESP
  r_skid.io.enq.valid      := io.deq.R.RVALID
  io.deq.R.RREADY          := r_skid.io.enq.ready
  // deq-side
  io.enq.R.RDATA      := r_skid.io.deq.bits.RDATA
  io.enq.R.RLAST      := r_skid.io.deq.bits.RLAST
  io.enq.R.RRESP      := r_skid.io.deq.bits.RRESP
  io.enq.R.RVALID     := r_skid.io.deq.valid
  r_skid.io.deq.ready := io.enq.R.RREADY

  // AW channel
  // enq-side
  aw_skid.io.enq.bits.ADDR  := io.enq.AW.AWADDR
  aw_skid.io.enq.bits.BURST := io.enq.AW.AWBURST
  aw_skid.io.enq.bits.LEN   := io.enq.AW.AWLEN
  aw_skid.io.enq.bits.SIZE  := io.enq.AW.AWSIZE
  aw_skid.io.enq.valid      := io.enq.AW.AWVALID
  io.enq.AW.AWREADY         := aw_skid.io.enq.ready
  // deq-side
  io.deq.AW.AWADDR     := aw_skid.io.deq.bits.ADDR
  io.deq.AW.AWBURST    := aw_skid.io.deq.bits.BURST
  io.deq.AW.AWLEN      := aw_skid.io.deq.bits.LEN
  io.deq.AW.AWSIZE     := aw_skid.io.deq.bits.SIZE
  io.deq.AW.AWVALID    := aw_skid.io.deq.valid
  aw_skid.io.deq.ready := io.deq.AW.AWREADY

  // W channel
  // enq-side
  w_skid.io.enq.bits.DATA := io.enq.W.WDATA
  w_skid.io.enq.bits.STRB := io.enq.W.WSTRB
  w_skid.io.enq.bits.LAST := io.enq.W.WLAST
  w_skid.io.enq.valid     := io.enq.W.WVALID
  io.enq.W.WREADY         := w_skid.io.enq.ready
  // deq-side
  io.deq.W.WDATA      := w_skid.io.deq.bits.DATA
  io.deq.W.WSTRB      := w_skid.io.deq.bits.STRB
  io.deq.W.WLAST      := w_skid.io.deq.bits.LAST
  io.deq.W.WVALID     := w_skid.io.deq.valid
  w_skid.io.deq.ready := io.deq.W.WREADY

  // B channel
  // enq-side
  b_skid.io.enq.bits.BRESP := io.deq.B.BRESP
  b_skid.io.enq.valid      := io.deq.B.BVALID
  io.deq.B.BREADY          := b_skid.io.enq.ready
  // deq-side
  io.enq.B.BRESP      := b_skid.io.deq.bits.BRESP
  io.enq.B.BVALID     := b_skid.io.deq.valid
  b_skid.io.deq.ready := io.enq.B.BREADY
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
  io.bus.AR.ARADDR  := ((raddr + (base >> 1)) << 1.U)            // bytes-align the address
  io.bus.AR.ARVALID := false.B
  io.bus.AR.ARLEN   := 0.U                                       // burst length is 1
  io.bus.AR.ARSIZE  := log2Ceil(CacheConfig.CacheLineBits / 8).U // each transfer contains (256 / 8) = 32 bytes
  io.bus.AR.ARBURST := 1.U                                       // incr burst mode, not really used

  // read data channel defaults
  io.bus.R.RREADY := false.B

  // write address channel defaults
  io.bus.AW.AWADDR  := ((waddr + (base >> 1)) << 1.U)            // bytes-align the address
  io.bus.AW.AWLEN   := 0.U                                       // burst length is 1
  io.bus.AW.AWSIZE  := log2Ceil(CacheConfig.CacheLineBits / 8).U // 32 bytes in each transfer
  io.bus.AW.AWVALID := false.B

  // write data channel defaults
  io.bus.W.WDATA    := wline
  io.bus.W.WSTRB    := Fill(CacheConfig.CacheLineBits / 8, 1.U)
  io.bus.W.WLAST    := false.B
  io.bus.W.WVALID   := false.B
  io.bus.B.BREADY   := true.B // ignores the response
  io.bus.AW.AWBURST := 1.U    // incr burst, not used

  when(state === sAxiReadData && io.bus.R.RVALID) {
    rline := io.bus.R.RDATA
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
      io.bus.AW.AWVALID := true.B
      when(io.bus.AW.AWREADY) {
        state := sAxiWriteData
      }
    }
    is(sAxiWriteData) {
      io.bus.W.WVALID := true.B
      io.bus.W.WLAST  := true.B
      when(io.bus.W.WREADY) {
        // go to idle if the command was a write, otherwise perform a read
        state := Mux(decode(cmd, CacheBackendCommand.WriteBack), sAxiReadAddr, sDone)
      }
    }
    is(sAxiReadAddr) {
      io.bus.AR.ARVALID := true.B
      when(io.bus.AR.ARREADY) {
        state := sAxiReadData
      }
    }
    is(sAxiReadData) {
      io.bus.R.RREADY := true.B
      when(io.bus.R.RVALID) {
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
