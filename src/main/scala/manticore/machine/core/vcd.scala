package manticore.machine.core

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util.is
import chisel3.util.switch
import chisel3.stage.ChiselStage
import manticore.machine.ISA
import manticore.machine.ManticoreBaseISA
import manticore.machine.ManticoreFullISA
import manticore.machine.memory.VcdDualPortMemory
import manticore.machine.memory.SimpleDualPortMemory
import manticore.machine.memory.MemStyle
import manticore.machine.xrt.AxiParameters
import manticore.machine.xrt.AxiMasterIF
import manticore.machine.memory.CacheConfig
import manticore.machine.xrt.AxiMaster
import scala.math
import chisel3.util.Cat
import java.io.Flushable

object VcdConstants {

  val MemoryBaseAddress  = 0x00000000
  val MemoryAddressWidth = 32

  //for BRAM
  val DataOutWidth = 64
  val DataInWidth  = 32

  def MaxBramCapacity    = 1024.U(32.W)
  def KillClockThreshold = 768.U(32.W) //0.75*MaxBramCapacity
  def BurstLength        = 128.U(8.W)

}

class VcdEngine(config: ISA) extends Module {

  val axiParams = new AxiParameters {
    override val IdWidth: Int   = 1
    override val AddrWidth: Int = VcdConstants.MemoryAddressWidth
    override val DataWidth: Int = VcdConstants.DataOutWidth
  }

  val io = IO(new Bundle {
    val id_in      = Input(UInt(config.IdBits.W))
    val valid      = Input(Bool())
    val data_in    = Input(UInt(config.DataBits.W))
    val m_axi      = new AxiMasterIF(axiParams)
    val kill_clock = Output(Bool())
    val root_clock = Input(Clock())
  })

  val vcd_clock = clock

  val flush_bram = withClockAndReset(vcd_clock, reset) {
    Module(
      new VcdDualPortMemory(
        ADDRESS_WIDTH = config.IdBits,
        READ_LATENCY = 1,
        DATA_IN_WIDTH = VcdConstants.DataInWidth,
        DATA_OUT_WIDTH = VcdConstants.DataOutWidth,
        STYLE = MemStyle.BRAM
      )
    )
  }
  flush_bram.io.clock := clock
  val AxiMaster = withClockAndReset(vcd_clock, reset) {
    Module(
      new AxiMaster(
        axiParams,
        VcdConstants.MemoryBaseAddress,
        VcdConstants.MemoryAddressWidth,
        VcdConstants.DataOutWidth
      )
    )
  }
  val virtual_cycles = withClockAndReset(vcd_clock, reset) { RegInit(UInt(32.W), 0.U) }
  AxiMaster.io.axi <> io.m_axi

  //initialising AxiMaster inputs
  AxiMaster.io.read_txn_start  := 0.B
  AxiMaster.io.write_txn_start := 0.B
  AxiMaster.io.write_addr      := 0.B
  AxiMaster.io.read_addr       := 0.B
  AxiMaster.io.data_in         := withClock(vcd_clock) { flush_bram.io.dout }
  AxiMaster.io.burst_size      := VcdConstants.BurstLength

  object FillValue extends ChiselEnum {
    val Idle, Start, FillCheck, FillWait, FillActive, Done = Value
  }
  object FlushValue extends ChiselEnum {
    val Idle, FlushActive, WaitForSlave, FlushBurst, Done = Value
  }
  val current_value_uram = withClockAndReset(vcd_clock, reset) {
    Module(
      new SimpleDualPortMemory(
        ADDRESS_WIDTH = config.IdBits,
        READ_LATENCY = 2,
        DATA_WIDTH = VcdConstants.DataInWidth,
        STYLE = MemStyle.URAM
      )
    )
  }

  val virtual_cycle_complete = WireInit(Bool(), 0.B)

  val data_out_width_wire = WireInit(UInt(32.W), VcdConstants.DataOutWidth.U)

  val actual_burst_length = withClockAndReset(vcd_clock, reset) { RegInit(UInt(32.W), 256.U) }
  val flush_state         = withClockAndReset(vcd_clock, reset) { RegInit(FlushValue.Type(), FlushValue.Idle) }
  val fill_state          = withClockAndReset(vcd_clock, reset) { RegInit(FillValue.Type(), FillValue.Idle) }
  val pip_reg1            = withClockAndReset(vcd_clock, reset) { RegInit(UInt(32.W), 0.U) }
  val pip_reg2            = withClockAndReset(vcd_clock, reset) { RegInit(UInt(32.W), 0.U) }

  val temp_reg1 = withClockAndReset(vcd_clock, reset) { RegInit(UInt(32.W), 0.U) }
  val temp_reg2 = withClockAndReset(vcd_clock, reset) { RegInit(UInt(32.W), 0.U) }

  val timeout_counter = withClockAndReset(vcd_clock, reset) { RegInit(UInt(32.W), 0.U) }
  val flush_counter   = withClockAndReset(vcd_clock, reset) { RegInit(UInt(32.W), 0.U) }
  val burst_counter   = withClockAndReset(vcd_clock, reset) { RegInit(UInt(32.W), 0.U) }
  val fill_counter    = withClockAndReset(vcd_clock, reset) { RegInit(UInt(32.W), 0.U) }
  val flush_pointer   = withClockAndReset(vcd_clock, reset) { RegInit(UInt(32.W), 0.U) }
  val start_pointer   = withClockAndReset(vcd_clock, reset) { RegInit(UInt(32.W), 0.U) }
  val mem_pointer     = withClockAndReset(vcd_clock, reset) { RegInit(UInt(32.W), 0.U) }
  val dump_done       = withClockAndReset(vcd_clock, reset) { RegInit(Bool(), 0.B) }

  val active_bram_size = withClockAndReset(vcd_clock, reset) { RegInit(UInt(32.W), 0.U) }

  when(active_bram_size >= VcdConstants.KillClockThreshold) {
    io.kill_clock := withClock(clock) { 1.B }
  }.otherwise {
    io.kill_clock := withClock(clock) { 0.B }
  }

  when(start_pointer >= flush_pointer) {
    active_bram_size := withClock(vcd_clock) { start_pointer - flush_pointer }
  }.otherwise {
    active_bram_size := withClock(vcd_clock) { (start_pointer - 0.U) + (VcdConstants.MaxBramCapacity - flush_pointer) }

  }

  val dump_start = withClockAndReset(vcd_clock, reset) { RegInit(Bool(), 0.B) }

  flush_bram.io.waddr         := DontCare
  flush_bram.io.raddr         := 0.B
  flush_bram.io.din           := DontCare
  flush_bram.io.wen           := 0.B
  current_value_uram.io.wen   := 0.B
  current_value_uram.io.waddr := 0.B
  current_value_uram.io.raddr := 0.B
  current_value_uram.io.din   := 0.B

  switch(flush_state) {
    is(FlushValue.Idle) {
      when(dump_start === 1.B) {
        timeout_counter := withClock(vcd_clock) { 0.U }
        flush_state     := withClock(vcd_clock) { FlushValue.FlushActive }
      }
    }
    is(FlushValue.FlushActive) {
      when(active_bram_size >= 2.U * VcdConstants.BurstLength) {
        AxiMaster.io.write_addr      := withClock(vcd_clock) { mem_pointer }
        AxiMaster.io.burst_size      := withClock(vcd_clock) { VcdConstants.BurstLength }
        actual_burst_length          := withClock(vcd_clock) { VcdConstants.BurstLength }
        AxiMaster.io.write_txn_start := withClock(vcd_clock) { 1.B }
        mem_pointer := withClock(vcd_clock) { mem_pointer + VcdConstants.BurstLength * (data_out_width_wire / 8.U) }
        flush_bram.io.raddr := withClock(vcd_clock) { flush_pointer }
        when(flush_pointer === VcdConstants.MaxBramCapacity - 2.U) {
          flush_pointer := withClock(vcd_clock) { 0.U }
        }.elsewhen(flush_pointer === VcdConstants.MaxBramCapacity - 1.U) {
          flush_pointer := withClock(vcd_clock) { 1.U }
        }.otherwise {
          flush_pointer := withClock(vcd_clock) { flush_pointer + 2.U }
        }

        flush_state := withClock(vcd_clock) { FlushValue.WaitForSlave }

      }.elsewhen((active_bram_size > 0.U) && ((timeout_counter >= 2.U * virtual_cycles))) {
        AxiMaster.io.write_addr      := withClock(vcd_clock) { mem_pointer }
        AxiMaster.io.burst_size      := withClock(vcd_clock) { (active_bram_size + 1.U) / 2.U }
        actual_burst_length          := withClock(vcd_clock) { active_bram_size }
        AxiMaster.io.write_txn_start := withClock(vcd_clock) { 1.B }
        mem_pointer         := withClock(vcd_clock) { mem_pointer + active_bram_size * (data_out_width_wire / 4.U) }
        flush_bram.io.raddr := withClock(vcd_clock) { flush_pointer }

        when(flush_pointer === VcdConstants.MaxBramCapacity - 2.U) {
          flush_pointer := withClock(vcd_clock) { 0.U }
        }.elsewhen(flush_pointer === VcdConstants.MaxBramCapacity - 1.U) {
          flush_pointer := withClock(vcd_clock) { 1.U }
        }.otherwise {
          flush_pointer := withClock(vcd_clock) { flush_pointer + 2.U }
        }

        flush_state := withClock(vcd_clock) { FlushValue.WaitForSlave }

      }.otherwise {

        AxiMaster.io.burst_size      := withClock(vcd_clock) { 0.U }
        AxiMaster.io.write_txn_start := withClock(vcd_clock) { 0.U }
      }

    }

    is(FlushValue.WaitForSlave) {

      when(AxiMaster.io.axi.AWREADY === 1.B && AxiMaster.io.axi.AWVALID === 1.B) {
        flush_state := withClock(vcd_clock) { FlushValue.FlushBurst }
        when(flush_pointer === VcdConstants.MaxBramCapacity - 2.U) {
          flush_pointer := withClock(vcd_clock) { 0.U }
        }.elsewhen(flush_pointer === VcdConstants.MaxBramCapacity - 1.U) {
          flush_pointer := withClock(vcd_clock) { 1.U }
        }.otherwise {
          flush_pointer := withClock(vcd_clock) { flush_pointer + 2.U }
        }
        flush_bram.io.raddr := withClock(vcd_clock) { flush_pointer }
      }
    }

    is(FlushValue.FlushBurst) {

      when(flush_pointer === start_pointer) {
        flush_state := withClock(vcd_clock) { FlushValue.Done }
      }.elsewhen(flush_pointer === start_pointer + 1.U) {
        flush_state   := withClock(vcd_clock) { FlushValue.Done }
        flush_pointer := withClock(vcd_clock) { flush_pointer - 1.U }
      }.elsewhen(burst_counter === VcdConstants.BurstLength - 2.U) {
        flush_state := withClock(vcd_clock) { FlushValue.Done }
      }.otherwise {
        AxiMaster.io.data_in := withClock(vcd_clock) { flush_bram.io.dout }
        burst_counter        := burst_counter + 1.U

        flush_bram.io.raddr := withClock(vcd_clock) { flush_pointer }
        when(flush_pointer === VcdConstants.MaxBramCapacity - 2.U) {
          flush_pointer := withClock(vcd_clock) { 0.U }
        }.elsewhen(flush_pointer === VcdConstants.MaxBramCapacity - 1.U) {
          flush_pointer := withClock(vcd_clock) { 1.U }
        }.otherwise {
          flush_pointer := withClock(vcd_clock) { flush_pointer + 2.U }
        }
      }

    }

    is(FlushValue.Done) {
      burst_counter                := withClock(vcd_clock) { 0.U }
      AxiMaster.io.write_txn_start := withClock(vcd_clock) { 0.B }
      when((active_bram_size > 0.U) || (timeout_counter >= 2.U * virtual_cycles)) {
        flush_state := withClock(vcd_clock) { FlushValue.FlushActive }
      }
        .elsewhen(active_bram_size === 0.U && timeout_counter >= 2.U * virtual_cycles) {
          dump_done   := withClock(vcd_clock) { 1.B }
          flush_state := withClock(vcd_clock) { FlushValue.Idle }
        }

    }

  }

  switch(fill_state) {

    is(FillValue.Idle) {
      when(io.valid === 1.B) {
        when(io.data_in > 0.U) {
          virtual_cycles := withClock(vcd_clock) { io.data_in }
          fill_state     := withClock(vcd_clock) { FillValue.Start }
        }
      }
      fill_counter    := withClock(vcd_clock) { 0.U }
      timeout_counter := withClock(vcd_clock) { 0.U }
    }

    is(FillValue.Start) {
      when(io.valid === 1.B) {
        dump_start                  := withClock(vcd_clock) { 1.B }
        current_value_uram.io.raddr := withClock(vcd_clock) { io.id_in }
        temp_reg1                   := withClock(vcd_clock) { Cat(1.U(1.W), 0.U(3.W), io.id_in, io.data_in) }
        fill_state                  := withClock(vcd_clock) { FillValue.FillWait }
      }
    }

    is(FillValue.FillWait) {
      when(io.valid === 1.B) {

        current_value_uram.io.raddr := withClock(vcd_clock) { io.id_in }
        temp_reg1                   := withClock(vcd_clock) { Cat(1.U(1.W), 0.U(3.W), io.id_in, io.data_in) }
      }
      temp_reg2  := withClock(vcd_clock) { temp_reg1 }
      fill_state := withClock(vcd_clock) { FillValue.FillActive }

    }

    is(FillValue.FillActive) {
      when(current_value_uram.io.dout =/= temp_reg2(15, 0) && temp_reg2(30) === 1.B) {

        pip_reg1 := withClock(vcd_clock) { Cat(1.U(1.W), temp_reg2(29, 0)) }

        pip_reg2     := withClock(vcd_clock) { Cat(virtual_cycle_complete, pip_reg1(30, 0)) }
        fill_counter := withClock(vcd_clock) { fill_counter + 1.U }
      }.otherwise {
        pip_reg1 := withClock(vcd_clock) { Cat(0.U(1.W), temp_reg2(29, 0)) }

        pip_reg2 := withClock(vcd_clock) { pip_reg1 }
      }

      when(fill_counter % virtual_cycles === 0.U && fill_counter =/= 0.U) {
        virtual_cycle_complete := { 1.B }
      }

      when(io.kill_clock === 0.U) {
        temp_reg1 := withClock(vcd_clock) { Cat(io.valid, 0.U(3.W), io.id_in, io.data_in) }
        temp_reg2 := withClock(vcd_clock) { temp_reg1 }
      }
        .otherwise {
          temp_reg1 := withClock(vcd_clock) { Cat(0.U(1.W), 0.U(3.W), io.id_in, io.data_in) }
          temp_reg2 := withClock(vcd_clock) { temp_reg1 }
        }
      when(io.valid === 1.B) {
        current_value_uram.io.raddr := withClock(vcd_clock) { io.id_in }
        current_value_uram.io.din   := withClock(vcd_clock) { temp_reg2(15, 0) }
        current_value_uram.io.waddr := withClock(vcd_clock) { temp_reg2(27, 16) }
        current_value_uram.io.wen   := withClock(vcd_clock) { 1.B }
      }.elsewhen(io.valid === 0.B) {
        timeout_counter := withClock(vcd_clock) { timeout_counter + 1.B }
      }

      when(pip_reg2(30) === 1.B) {
        flush_bram.io.wen   := withClock(vcd_clock) { 1.B }
        flush_bram.io.waddr := withClock(vcd_clock) { start_pointer }
        flush_bram.io.din   := withClock(vcd_clock) { pip_reg2 }
        when(start_pointer === VcdConstants.MaxBramCapacity - 1.U) {
          start_pointer := withClock(vcd_clock) { 0.U }

        }
          .otherwise {
            start_pointer := withClock(vcd_clock) { start_pointer + 1.U }

          }
      }.otherwise {
        flush_bram.io.wen := withClock(vcd_clock) { 0.B }
      }

      when(timeout_counter >= 2.U * virtual_cycles) {
        fill_state := withClock(vcd_clock) { FillValue.Idle }
      }

    }

  }

}
