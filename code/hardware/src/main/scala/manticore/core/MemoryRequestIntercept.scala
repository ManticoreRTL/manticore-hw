package manticore.core

import chisel3._

import chisel3.experimental.ChiselEnum
import memory.CacheInterface
import memory.CacheConfig
import memory.CacheBackInterface
import memory.CacheBackendCommand
import memory.CacheFrontInterface
import manticore.ISA
import memory.CacheCommand
import chisel3.util.switch
import chisel3.util.is

class MemoryReadWriteInterface(config: ISA) extends Bundle {
  val addr  = Output(UInt(64.W))
  val wdata = Output(UInt(config.DataBits.W))
  val rdata = Input(UInt(config.DataBits.W))
  val wen   = Output(Bool())
  val start = Output(Bool())
  val done  = Input(Bool())
  val idle  = Input(Bool())
}
class MemoryRequestInterceptInterface(config: ISA) extends Bundle {

  val core: CacheFrontInterface = CacheConfig.frontInterface()
  val memory                    = new MemoryReadWriteInterface(config)

  val clock_manager = new Bundle {
    val gate_request_start: Bool = Output(Bool())
    val gate_request_end: Bool   = Output(Bool())
    val clock_enable: Bool       = Input(Bool())
  }
}

class MemoryRequestIntercept(config: ISA) extends Module {

  val io = IO(new MemoryRequestInterceptInterface(config))

  object State extends ChiselEnum {
    val WaitForStart, StartMemoryRequest, WaitForMemoryResponse,
        WaitClockResume, RelayResponse =
      Value
  }

  val rdata_reg: UInt = Reg(UInt(config.DataBits.W))
  val raddr_reg: UInt = Reg(UInt(64.W))
  val addr_reg: UInt  = Reg(UInt(64.W))
  val wdata_reg: UInt = Reg(UInt(config.DataBits.W))
  val mem_done: Bool  = Reg(Bool())

  mem_done := io.memory.done
  when(io.memory.done) { rdata_reg := io.memory.rdata }

  when(io.core.start) {
    addr_reg  := io.core.addr
    wdata_reg := io.core.wdata
  }

  io.memory.wdata := wdata_reg
  io.memory.addr  := addr_reg

  io.core.rdata := rdata_reg

  val state = RegInit(State.Type(), State.WaitForStart)

  val start_int = Reg(Bool())
  start_int := io.core.start
  val wen_int = Reg(Bool())

  when(io.core.start) {

    when(io.core.cmd === CacheCommand.Read) {
      wen_int := false.B
    }.elsewhen(io.core.cmd === CacheCommand.Write) {
      wen_int := true.B
    }.elsewhen(io.core.cmd === CacheCommand.Flush) {
      printf(
        "Invalid memory operation! Only the the controller can issue a cache flush!"
      )
    }.elsewhen(io.core.cmd === CacheCommand.Reset) {
      printf(
        "Invalid memory operation! Only the controller can issue a cache reset!"
      )
    }
  }

  io.memory.wen := wen_int

  io.clock_manager.gate_request_start := false.B
  io.clock_manager.gate_request_end   := false.B
  io.memory.start                     := false.B
  io.core.done                        := false.B
  io.core.idle                        := false.B
  switch(state) {
    is(State.WaitForStart) {
      when(start_int) {
        state                               := State.StartMemoryRequest
        io.clock_manager.gate_request_start := true.B
        io.core.idle                        := true.B
      }
    }
    is(State.StartMemoryRequest) {
      state           := State.WaitForMemoryResponse
      io.memory.start := true.B
    }
    is(State.WaitForMemoryResponse) {
      when(mem_done) {
        io.clock_manager.gate_request_end := true.B
        state                             := State.WaitClockResume
      }
    }
    is(State.WaitClockResume) {
      when(io.clock_manager.clock_enable) {
        io.core.done := true.B
        state        := State.RelayResponse
      }
    }
  }
}
