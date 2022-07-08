package manticore.machine.xrt

import chisel3._

import manticore.machine.memory.Cache
import manticore.machine.memory.CacheCommand
import manticore.machine.memory.CacheInterface
import manticore.machine.memory.CacheBackendCommand
import manticore.machine.xrt.AxiSlave
import manticore.machine.xrt.AxiMasterIF
import manticore.machine.xrt.AxiMaster
import manticore.machine.ManticoreFullISA

import chisel3.experimental.ChiselEnum
import chisel3.util._
import manticore.machine.memory.CacheConfig
import chisel3.stage.ChiselStage
import os.temp


class CacheKernel extends RawModule {

  val clock = IO(Input(Clock()))
  clock.suggestName("ap_clk")
  val reset_n = IO(Input(Bool()))
  reset_n.suggestName("ap_rst_n")

  val reset = Wire(Bool())
  reset := ~reset_n

  val axiParams = new AxiParameters {
    override val IdWidth: Int   = 1
    override val AddrWidth: Int = CacheConfig.AddressBits
    override val DataWidth: Int =
      CacheConfig.CacheLineBits // can not be less than 32 bits
    override val UserDataWidth: Int = CacheConfig.DataBits
    override val UserAddrWidth: Int = 64
  }
  val m_axi_bank_0 = IO((new AxiMasterIF(axiParams)))

  val s_axi_control = IO(new AxiSlave.AxiSlaveCoreInterface())
  val interrupt     = IO(Output(Bool()))
  interrupt := false.B
  // val io = IO(
  //   new Bundle {
  //     val KernelStart = Input(Bool())
  //     val KernelScheduleConfig = Input (UInt(64.W))
  //     val KernelDataIn = Input(UInt(16.W))
  //     val KernelDone = Output(Bool())
  //     val KernelDataOut = Output(UInt(32.W))
  //   }
  // )
  val MemoryBaseAddress: Long = 0x005000000000L

  val cache = withClockAndReset(clock, reset) { Module(new Cache) }

  val axiSlave = withClockAndReset(clock, reset) {
    Module(new AxiSlave(ManticoreFullISA))
  }

 val AxiMaster = withClockAndReset(clock, reset) {
    Module(
      new AxiMaster(axiParams, MemoryBaseAddress, CacheConfig.AddressBits, CacheConfig.DataBits)
    )
  }

  axiSlave.io.dev_regs.virtual_cycles := 0.U
  // axiSlave.io.host_regs.schedule_config := io.KernelScheduleConfig
  object KernelState extends ChiselEnum {
    val Idle, Wait, Done, Read = Value
  }

  object WriteBackState extends ChiselEnum {
    val Idle, Write, Read = Value
  }
  //val test = axiSlave.io.host_regs.value_change_symbol_table_base
  // val state

  val commandReg = withClock(clock) { Reg(CacheCommand.Type()) }
  val state = withClockAndReset(clock, reset) {
    RegInit(KernelState.Type(), KernelState.Idle)
  }

  val write_back_state = withClockAndReset(clock, reset) {
    RegInit(WriteBackState.Type(), WriteBackState.Idle)
  }

  val commandWire = WireInit(CacheCommand.Type(), CacheCommand.Read)

  switch(axiSlave.io.host_regs.schedule_config(1, 0)) {
    is(0.U) {
      commandWire := CacheCommand.Write
    }
    is(1.U) {
      commandWire := CacheCommand.Read
    }
    is(2.U) {
      commandWire := CacheCommand.Reset
    }
    is(3.U) {
      commandWire := CacheCommand.Flush
    }
  }
  val bootloader_cycles_temp = withClockAndReset(clock, reset) {
    RegInit(UInt(64.W), 0.U)
  }
  val trigger            = withClockAndReset(clock, reset) { RegInit(Bool(), 0.B) }
  val write_back_trigger = withClockAndReset(clock, reset) { RegInit(Bool(), 0.B) }
  val temp_addr          = withClockAndReset(clock, reset) { RegInit(UInt(CacheConfig.AddressBits.W), 0.U) }
  val trigger2_temp      = withClockAndReset(clock, reset) { RegInit(Bool(), 0.B) }
  val trigger3           = withClockAndReset(clock, reset) { RegInit(Bool(), 0.B) }
  val trigger3_temp      = withClockAndReset(clock, reset) { RegInit(Bool(), 0.B) }
  val txn_mode_temp      = withClockAndReset(clock, reset) { RegInit(Bool(), 0.B) }
  val txn_start_temp     = withClockAndReset(clock, reset) { RegInit(Bool(), 0.B) }
  val temp_counter = withClockAndReset(clock, reset) {
    RegInit(UInt(32.W), 0.U)
  }

  val counter = withClockAndReset(clock, reset) { RegInit(UInt(32.W), 0.U) }

  val start_flag      = withClockAndReset(clock, reset) { RegInit(Bool(), 0.B) }
  val start_flag_next = withClockAndReset(clock, reset) { RegInit(Bool(), 0.B) }

  val cacheReadData = withClock(clock) { Reg(UInt(CacheConfig.DataBits.W)) }
  axiSlave.io.dev_regs.virtual_cycles := cacheReadData
  cache.io.front.start                := false.B
  axiSlave.io.control.ap_done         := (state === KernelState.Done)

  // //doubt
  axiSlave.io.control.ap_ready := (state === KernelState.Done)
  axiSlave.io.control.ap_idle  := (state === KernelState.Idle)

  AxiMaster.io.burst_size               := 0.U
  AxiMaster.io.read_txn_start           := 0.B
  AxiMaster.io.write_txn_start          := 0.B
  axiSlave.io.core.AWVALID              := s_axi_control.AWVALID
  axiSlave.io.core.ARVALID              := s_axi_control.ARVALID
  axiSlave.io.core.WVALID               := s_axi_control.WVALID
  axiSlave.io.core.ARADDR               := s_axi_control.ARADDR
  axiSlave.io.core.AWADDR               := s_axi_control.AWADDR
  axiSlave.io.core.WDATA                := s_axi_control.WDATA
  axiSlave.io.core.RREADY               := s_axi_control.RREADY
  axiSlave.io.core.BREADY               := s_axi_control.BREADY
  axiSlave.io.dev_regs.exception_id     := 0.U
  axiSlave.io.dev_regs.execution_cycles := 0.U

  s_axi_control.BRESP   := axiSlave.io.core.BRESP
  s_axi_control.RVALID  := axiSlave.io.core.RVALID
  s_axi_control.WREADY  := axiSlave.io.core.WREADY
  s_axi_control.RDATA   := axiSlave.io.core.RDATA
  s_axi_control.RRESP   := axiSlave.io.core.RRESP
  s_axi_control.AWREADY := axiSlave.io.core.AWREADY
  s_axi_control.ARREADY := axiSlave.io.core.ARREADY
  s_axi_control.BVALID  := axiSlave.io.core.BVALID
  s_axi_control.AWREADY := axiSlave.io.core.AWREADY

  // m_axi_bank_0.ARID := 0.U

  //m_axi_bank_0 := 0.U

  //s_axi_control.WDATA   := 0.U
  //s_axi_control.RREADY  := 0.B
  //s_axi_control.BREADY  := 0.B

  axiSlave.io.core.WSTRB := s_axi_control.WSTRB
  cache.io.front.addr    := DontCare
  cache.io.front.wdata   := DontCare
  cache.io.front.cmd     := DontCare
  cache.io.back.done     := DontCare

  axiSlave.io.dev_regs.exception_id := 0.U

  // io.KernelDone                          := DontCare
  cache.io.back.rline          := DontCare
  axiSlave.io.control.ap_done  := 0.U
  axiSlave.io.control.ap_ready := 0.U
  axiSlave.io.control.ap_idle  := 0.U
  commandReg                   := commandWire

  // when(cache.io.back.done === 1.B){
  // axiSlave.io.dev_regs.exception_id := withClock(clock) { 1.U }
  //           axiSlave.io.control.ap_ready := withClock(clock) { 1.B }
  //           cacheReadData := withClock(clock) { cache.io.front.rdata }
  //           trigger       := withClock(clock) { 0.B }
  //           axiSlave.io.control.ap_done := withClock(clock) { 1.B }

  // }
  // .elsewhen(start_flag===1.B)
  // {
  //   axiSlave.io.dev_regs.exception_id := withClock(clock){0.U}

  // }.otherwise{
  //       axiSlave.io.dev_regs.exception_id := withClock(clock){1.U}

  // }

  // when(axiSlave.io.host_regs.value_change_symbol_table_base(31, 0) === 1.U && cache.io.back.done===0.B && counter === 0.U)
  // {
  //   start_flag := withClock(clock){1.B}
  // }.elsewhen(cache.io.back.done===1.B){
  //   start_flag := withClock(clock){0.B}
  // }

  when(cache.io.front.idle === 1.B) {
    temp_counter := withClock(clock) { 1.U }
  }

  when(cache.io.front.done === 1.B) {
    counter := withClock(clock) { 1.U }
  }
  //.elsewhen(cache.io.back.done===1.B)
  // {
  //   counter := withClock(clock){temp_counter+1.U}
  //   temp_counter := 1.U
  // } otherwise{
  //   temp_counter := withClock(clock){1.U}
  // }

  when(AxiMaster.io.axi.BVALID === 1.B) {
    axiSlave.io.dev_regs.bootloader_cycles := 8.U + AxiMaster.io.axi.BRESP
    bootloader_cycles_temp                 := 8.U + AxiMaster.io.axi.BRESP
  }
    .elsewhen((AxiMaster.io.axi.RVALID === 1.B)) {
      axiSlave.io.dev_regs.bootloader_cycles := 16.U + AxiMaster.io.axi.RRESP
      bootloader_cycles_temp                 := 16.U + AxiMaster.io.axi.RRESP
    }
    .otherwise {
      axiSlave.io.dev_regs.bootloader_cycles := bootloader_cycles_temp
    }

  switch(state) {
    is(KernelState.Idle) {
      txn_start_temp := 0.B
      txn_mode_temp  := 0.B

      axiSlave.io.dev_regs.execution_cycles := withClock(clock) { 0.U }
  
      axiSlave.io.control.ap_idle := withClock(clock) { true.B }
      when(
        axiSlave.io.host_regs.value_change_symbol_table_base(31, 0) === 1.U
      ) {
        state := withClock(clock) { KernelState.Wait }
      }.elsewhen(cache.io.front.idle === 1.U) {

        //cache.io.front.cmd   := DontCare
        cache.io.back.rline  := DontCare
        cache.io.front.wdata := DontCare
      }
      axiSlave.io.dev_regs.exception_id := withClock(clock) { 1.U }
      trigger                           := withClock(clock) { 0.B }
      trigger3                          := withClock(clock) { 0.B }
      trigger2_temp                     := withClock(clock) { 0.B }
      trigger3_temp                     := withClock(clock) { 0.B }

    }
    is(KernelState.Wait) {
      axiSlave.io.dev_regs.execution_cycles := withClock(clock) { 1.U }

      cache.io.front.cmd := withClock(clock) { commandReg }
      cache.io.front.addr := withClock(clock) {
        axiSlave.io.host_regs
          .global_memory_instruction_base(CacheConfig.AddressBits, 0)
      }
      cache.io.front.wdata := withClock(clock) {
        axiSlave.io.host_regs.value_change_log_base(31, 0)
      }
      when(cache.io.front.idle === 1.B) {
        cache.io.front.start := withClock(clock) { true.B }
      }

      //}
      // .elsewhen(trigger3 === 1.B) {
      //   AxiMaster.io.txn_start := withClock(clock) { 1.B }

      //   //cache.io.back.done := withClock(clock) { AxiMaster.io.TXN_DONE }
      //   //trigger := withClock(clock) { AxiMaster.io.TXN_DONE }

      // }
      cache.io.back.done := withClock(clock) {
        (AxiMaster.io.read_txn_done || AxiMaster.io.write_txn_done) && write_back_state === WriteBackState.Idle
      }
      when(cache.io.back.start === 1.B) {

        when(cache.io.back.cmd === CacheBackendCommand.Read.id.U) {
          AxiMaster.io.read_txn_start := withClock(clock) { 1.B }
          AxiMaster.io.read_addr      := withClock(clock) { cache.io.back.raddr * 2.U }
          cache.io.back.rline         := withClock(clock) { AxiMaster.io.data_out }
          AxiMaster.io.burst_size     := withClock(clock) { 1.U }
        }
          .elsewhen(cache.io.back.cmd === CacheBackendCommand.Write.id.U) {
            AxiMaster.io.write_txn_start := withClock(clock) { 1.B }
            AxiMaster.io.write_addr      := withClock(clock) { cache.io.back.waddr * 2.U }
            AxiMaster.io.data_in         := withClock(clock) { cache.io.back.wline }
            AxiMaster.io.burst_size      := withClock(clock) { 1.U }
          }

        // .elsewhen(cache.io.back.cmd === CacheBackendCommand.WriteBack.id.U) {
        //   trigger2 := withClock(clock){1.B}
        //   trigger2_temp := withClock(clock){1.B}
        //   AxiMaster.io.axi.txn_mode := withClock(clock) { 0.B }
        //   txn_mode_temp         := withClock(clock) { 0.B }

        //   AxiMaster.io.addr    := withClock(clock) { cache.io.back.waddr }
        //   AxiMaster.io.burst_size := withClock(clock) { 1.U }

        //   AxiMaster.io.data_in := withClock(clock) { cache.io.back.wline }
        // }

      }

      switch(write_back_state) {
        is(WriteBackState.Idle) {
          when(
            cache.io.back.start === 1.B && cache.io.back.cmd === CacheBackendCommand.WriteBack.id.U
          ) {
            write_back_state             := WriteBackState.Write
            AxiMaster.io.write_txn_start := withClock(clock) { 1.B }
            AxiMaster.io.write_addr      := withClock(clock) { cache.io.back.waddr * 2.U }
            AxiMaster.io.data_in         := withClock(clock) { cache.io.back.wline }
            AxiMaster.io.burst_size      := withClock(clock) { 1.U }
          }
          temp_addr := withClock(clock) { cache.io.back.raddr * 2.U }

        }
        is(WriteBackState.Write) {
          when(AxiMaster.io.write_txn_done === 1.B) {
            write_back_state            := WriteBackState.Read
            AxiMaster.io.read_txn_start := withClock(clock) { 1.B }
            AxiMaster.io.read_addr      := withClock(clock) { temp_addr }
            cache.io.back.rline         := withClock(clock) { AxiMaster.io.data_out }
            AxiMaster.io.burst_size     := withClock(clock) { 1.U }

          }
        }
        is(WriteBackState.Read) {
          when(AxiMaster.io.read_txn_done === 1.B) {
            write_back_state := WriteBackState.Idle
            cache.io.back.done := withClock(clock) {
              AxiMaster.io.read_txn_done
            }
          }
        }
      }
      // when(
      //   trigger2 === 1.B && AxiMaster.io.TXN_DONE === 1.B
      // ) {
      //   trigger3 := withClock(clock) {1.B}
      //   trigger3_temp := withClock(clock){1.B}

      //   AxiMaster.io.txn_mode := withClock(clock) { 1.B }
      //   txn_mode_temp         := withClock(clock) { 1.B }

      //   AxiMaster.io.addr := withClock(clock) { cache.io.back.raddr }
      //   cache.io.back.rline := withClock(clock) {
      //     AxiMaster.io.data_out
      //   }
      //   AxiMaster.io.burst_size := withClock(clock) { 1.U }
      //   //trigger                 := withClock(clock) { 1.B }

      // } otherwise {
      //   trigger3 := withClock(clock){trigger3_temp}

      // }

      // when(AxiMaster.io.TXN_DONE === 1.B && trigger === 1.B) {
      //   axiSlave.io.control.ap_ready := withClock(clock) { 1.B }
      //   state                        := withClock(clock) { KernelState.Wait }
      // }

      when(cache.io.front.done) {
        cacheReadData                := withClock(clock) { cache.io.front.rdata }
        axiSlave.io.control.ap_ready := withClock(clock) { 1.B }

        state := withClock(clock) { KernelState.Done }
      }
      axiSlave.io.dev_regs.exception_id := withClock(clock) { 0.U }

    }
    is(KernelState.Done) {
      axiSlave.io.dev_regs.execution_cycles := withClock(clock) { 2.U }
      axiSlave.io.control.ap_done           := withClock(clock) { 1.B }
      axiSlave.io.dev_regs.exception_id     := withClock(clock) { 1.U }
      state                                 := withClock(clock) { KernelState.Idle }

    }

  }

}

object CacheKernelGen extends App {

  new ChiselStage()
    .emitVerilog(new CacheKernel(), Array("-td", "gen-dir/cache-kernel"))
}
