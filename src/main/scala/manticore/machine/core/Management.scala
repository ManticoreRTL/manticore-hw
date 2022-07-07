package manticore.machine.core

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum
import manticore.machine.ManticoreFullISA
import manticore.machine.memory.CacheConfig

object ManagementConstants {

  val CMD_START         = 0.asUInt(7.W)
  val CMD_RESUME        = 1.asUInt(7.W)
  val CMD_CACHE_FLUSH   = 2.asUInt(7.W)
  val EXCEPTION_TIMEOUT = (1 << 16).asUInt(32.W)

}

class Management extends Module {

  import ManagementConstants._
  val io = IO(new Bundle {

    val start = Input(Bool())
    val done  = Output(Bool())
    val idle  = Output(Bool())

    val device_registers = Output(new DeviceRegisters)

    val boot_start    = Output(Bool())
    val boot_finished = Input(Bool())

    val core_kill_clock         = Input(Bool())
    val core_revive_clock       = Input(Bool())
    val core_exception_occurred = Input(Bool())
    val clock_active            = Output(Bool())
    val clock_locked            = Input(Bool())

    val schedule_config = Input(UInt(64.W))
    val exception_id    = Input(UInt(32.W))

    val config_enable = Output(Bool())

    val cache_reset_start = Output(Bool())
    val cache_flush_start = Output(Bool())
    val cache_done        = Input(Bool())
    val execution_active  = Input(Bool())

    val soft_reset = Output(Bool())
  })

  val clock_active = RegInit(true.B)
  io.clock_active := clock_active

  val start_reg   = RegNext(io.start)
  val start_pulse = WireDefault((!start_reg) & io.start)

  val dev_regs        = Reg(new DeviceRegisters)
  val command         = WireDefault(io.schedule_config.head(7))                // the first 7 bit are the run command
  val timeout_enabled = WireDefault(io.schedule_config.head(8).tail(1).asBool) // 8th bit, used to enable timeout
  val timeout         = WireDefault(io.schedule_config.tail(8))                // timeout value

  object State extends ChiselEnum {
    val sIdle, sCoreReset, sCoreResetWait, sCacheReset, sCacheResetWait, sBoot, sVirtualCycle, sCacheFlush,
        sCacheFlushWait, sDone = Value
  }
  import State._

  val state = RegInit(sIdle)

  val done_out = RegNext(state === sDone)
  io.done := done_out

  val idle_out = RegNext(state === sIdle)
  io.idle := idle_out

  val config_enable = RegNext(state =/= sVirtualCycle)
  io.config_enable := config_enable

  val execution_active         = RegNext(io.execution_active)
  val execution_active_negedge = execution_active && !io.execution_active

  val enable_core_reset = WireDefault(false.B)
  val core_reset = {
    // the reset is passed through many registers to enable re-timing
    val pipes = Seq.fill(16) { Reg(Bool()) }
    pipes.foldLeft(state === sCoreReset) { case (prev, curr) =>
      curr := prev
      curr
    }
  }

  io.cache_reset_start := (state === sCacheReset)
  io.cache_flush_start := (state === sCacheFlush)
  switch(state) {

    is(sIdle) {
      when(start_pulse && io.clock_locked) {
        state := Mux(command === CMD_START, sCoreReset, Mux(command === CMD_RESUME, sVirtualCycle, sCacheFlush))
        clock_active := Mux(
          command === CMD_RESUME,
          true.B,
          false.B
        ) // only enable the clock if we are resuming execution
        dev_regs.bootloader_cycles := 0.U
        dev_regs.virtual_cycles    := 0.U
        dev_regs.execution_cycles  := 0.U
      }
    }
    is(sCoreReset) {
      clock_active := true.B
      state        := sCoreResetWait
    }
    is(sCoreResetWait) {
      clock_active := true.B // enable the clock so that cores can be reset
      when(core_reset) {
        state := sCacheReset
      }
    }
    is(sCacheReset) {
      state := sCacheResetWait
    }
    is(sCacheResetWait) {
      when(io.cache_done) {
        state := sBoot
      }
      dev_regs.bootloader_cycles := dev_regs.bootloader_cycles + 1.U
    }
    is(sCacheFlush) {
      state := sCacheFlushWait
    }
    is(sCacheFlushWait) {
      state := sDone
    }
    is(sBoot) {
      dev_regs.bootloader_cycles := dev_regs.bootloader_cycles + 1.U
      clock_active               := true.B // enable the clock so that NoC can be used
      when(io.boot_finished) {
        state := sVirtualCycle
      }
    }

    is(sVirtualCycle) {
      dev_regs.execution_cycles := dev_regs.execution_cycles + 1.U
      when(clock_active) {
        clock_active := !(io.core_kill_clock || io.core_exception_occurred)
      } otherwise {
        clock_active := io.core_revive_clock // revive the clock
      }
      when(clock_active) {
        // check exceptions for stopping execution
        when(io.core_exception_occurred) {
          state                 := sDone
          dev_regs.exception_id := io.exception_id

        }.elsewhen(timeout_enabled && dev_regs.virtual_cycles === timeout) {
          // or when we timeout
          dev_regs.exception_id := EXCEPTION_TIMEOUT
          // give some id that users can not
          // make (they are restricted to 16 bits, i.e., up to 0xFFFF)
          // NOTE: max time out  is 1 << 56 (more than enough)
          state := sDone
        }

      }
      when(execution_active_negedge) {
        dev_regs.virtual_cycles := dev_regs.virtual_cycles + 1.U
      }
    }
    is(sDone) {
      clock_active := false.B
      state        := sIdle
    }
  }

}

class MemoryIntercept extends Module {

  object State extends ChiselEnum {
    val sIdle, sReq, sWait, sRevive, sKill = Value
  }
  import State._
  val io = IO(new Bundle {

    val core          = CacheConfig.frontInterface()
    val boot          = CacheConfig.frontInterface()
    val config_enable = Input(Bool())

    val cache             = Flipped(CacheConfig.frontInterface())
    val core_kill_clock   = Output(Bool())
    val core_revive_clock = Output(Bool())
    val core_clock        = Input(Clock())

  })

  val state = RegInit(sIdle)

  io.boot.done  := false.B
  io.boot.idle  := false.B
  io.boot.rdata := DontCare

  io.core.done         := false.B
  io.core_kill_clock   := false.B
  io.core_revive_clock := false.B

  io.core.idle := false.B

  // Necessary evil: we need to register the result of the cache read in the
  // core (compute) clock domain, otherwise back to back reads will discard the
  // first read and double-counts the last read. This is done to fake the
  // behavior of a 2-cycle read latency on the core side, so the core can be
  // designed completely oblivious to the fact that DRAM access can take
  // arbitrary many cycles, instead it sees it as a SRAM memory with 2-cycle
  // read latency!
  io.core.rdata := withClock(clock = io.core_clock) {
    RegNext(io.cache.rdata)
  }
  io.cache.start := false.B

  when(!io.config_enable) {
    io.cache.addr  := RegEnable(io.core.addr, io.core.start)
    io.cache.wdata := RegEnable(io.core.wdata, io.core.start)
    io.cache.cmd   := RegEnable(io.core.cmd, io.core.start)

    switch(state) {
      is(sIdle) {
        when(io.core.start) {
          state              := sReq
          io.core_kill_clock := true.B
        }
      }
      is(sReq) {
        io.cache.start := true.B
        state          := sWait
      }
      is(sWait) {
        when(io.cache.done) {
          state := sRevive
        }
      }
      is(sRevive) {
        io.core_revive_clock := true.B
        when(io.core.start) {
          state := sKill // there is another request
        } otherwise {
          state := sIdle
        }
      }
      is(sKill) {
        io.core_kill_clock := true.B
        state              := sReq
      }
    }
  } otherwise {
    state := sIdle
    io.cache <> io.boot
    io.core_kill_clock   := false.B
    io.core_revive_clock := false.B

  }

}
