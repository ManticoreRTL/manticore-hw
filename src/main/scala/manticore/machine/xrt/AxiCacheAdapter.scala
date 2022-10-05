package manticore.machine.xrt

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum
import manticore.machine.memory.Cache
import manticore.machine.memory.CacheConfig
import manticore.machine.memory.CacheBackendCommand
import chisel3.stage.ChiselStage

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
  val core = CacheConfig.frontInterface()
  val bus  = new AxiMasterIF(AxiCacheAdapter.CacheAxiParameters)
  val base = Input(UInt(64.W))
}

class CacheSubsystem extends Module {
  val io = IO(new CacheSubsystemInterface)

  val front_pipe = Module(CacheConfig.frontPipe())
  val cache      = Module(new Cache)
  val back_pipe  = Module(CacheConfig.backPipe())
  val axi        = Module(new AxiCacheAdapter)

  axi.io.base := io.base
  front_pipe.io.in <> io.core
  cache.io.front <> front_pipe.io.out
  back_pipe.io.in <> cache.io.back
  axi.io.cache <> back_pipe.io.out
  io.bus <> axi.io.bus

}

object Generator2132 extends App {
  new ChiselStage().emitVerilog(new AxiCacheAdapter, Array("-td", "gen-dir"))
}
