package manticore.machine.core

import Chisel._
import chisel3.experimental.ChiselEnum
import manticore.machine.memory.CacheConfig
import manticore.machine.memory.CacheFrontInterface




class CacheRequestInterceptInterface extends Bundle {

  val front_side = new Bundle {
    val core : CacheFrontInterface = CacheConfig.frontInterface()
    val cache: CacheFrontInterface = Flipped(CacheConfig.frontInterface())
  }

  val clock_manager = new Bundle {
    val gate_request_start: Bool = Output(Bool())
    val gate_request_end: Bool = Output(Bool())
    val clock_enable: Bool = Input(Bool())
  }

}
class CacheRequestIntercept extends Module {

  val io: CacheRequestInterceptInterface = IO (new CacheRequestInterceptInterface)

  object State extends ChiselEnum {
    val WaitForStart,
      WaitCacheResponse,
      WaitClockResume,
      RelayCacheResponse = Value
  }


  val rdata_reg: UInt = Reg(UInt(CacheConfig.DataBits.W))

  val state = RegInit(State.Type(), State.WaitForStart)

  io.clock_manager.gate_request_start := false.B
  io.clock_manager.gate_request_end := false.B


  io.front_side.core <> io.front_side.cache

  io.front_side.cache.start := false.B
  io.front_side.core.done := false.B

  io.front_side.core.rdata := rdata_reg

  switch(state) {
    is (State.WaitForStart) {
      when(io.front_side.core.start) {
        state := State.WaitCacheResponse
        io.clock_manager.gate_request_start := true.B
        io.front_side.cache.start := true.B
      }
    }
    is (State.WaitCacheResponse) {
      when (io.front_side.cache.done) {
        io.clock_manager.gate_request_end := true.B
        rdata_reg := io.front_side.cache.rdata
        state := State.WaitClockResume
      }
    }
    is (State.WaitClockResume) {
      when (io.clock_manager.clock_enable) {
        io.front_side.core.done := true.B
        state := State.WaitForStart
      }
    }
  }
}

