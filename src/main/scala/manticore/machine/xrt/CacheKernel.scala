package manticore.machine.xrt

import chisel3._

import manticore.machine.memory.Cache
import manticore.machine.memory.CacheCommand
import manticore.machine.memory.CacheInterface
import manticore.machine.memory.CacheBackendCommand
import manticore.machine.xrt.AxiSlave
import manticore.machine.xrt.AxiMasterIF

import manticore.machine.ManticoreFullISA

import chisel3.experimental.ChiselEnum
import chisel3.util._
import manticore.machine.memory.CacheConfig
import chisel3.stage.ChiselStage
import os.temp

class MasterAXIFromFile(
    val C_M_TARGET_SLAVE_BASE_ADDR: Long,
    val C_M_AXI_ADDR_WIDTH: Int,
    val C_M_AXI_DATA_WIDTH: Int
) extends BlackBox(
      Map(
        "C_M_TARGET_SLAVE_BASE_ADDR" -> C_M_TARGET_SLAVE_BASE_ADDR,
        "C_M_AXI_ADDR_WIDTH"         -> C_M_AXI_ADDR_WIDTH,
        "C_M_AXI_DATA_WIDTH"         -> C_M_AXI_DATA_WIDTH
      )
    )
    with HasBlackBoxResource {

  val io = IO(
    new Bundle {
      val read_txn_start  = Input(Bool())
      val write_txn_start = Input(Bool())
      val data_in         = Input(UInt(C_M_AXI_DATA_WIDTH.W))
      val data_out        = Output(UInt(C_M_AXI_DATA_WIDTH.W))
      val burst_size      = Input(UInt(8.W))
      val read_addr            = Input(UInt(C_M_AXI_ADDR_WIDTH.W))
      val write_addr            = Input(UInt(C_M_AXI_ADDR_WIDTH.W))
      val cache_ready     = Input(Bool())
      val read_txn_done   = Output(Bool())
      val write_txn_done  = Output(Bool())
      val M_AXI_ARESETN   = Input(Bool())
      val M_AXI_ACLK      = Input(Clock())
      val M_AXI_AWADDR    = Output(UInt(C_M_AXI_ADDR_WIDTH.W))
      val M_AXI_AWLEN     = Output(UInt(8.W))
      val M_AXI_AWSIZE    = Output(UInt(3.W))
      val M_AXI_AWBURST   = Output(UInt(2.W))
      val M_AXI_AWVALID   = Output(Bool())
      val M_AXI_AWREADY   = Input(Bool())
      val M_AXI_WDATA     = Output(UInt(C_M_AXI_DATA_WIDTH.W))
      val M_AXI_WSTRB     = Output(UInt(((C_M_AXI_DATA_WIDTH) / 8).W))
      val M_AXI_WLAST     = Output(Bool())
      val M_AXI_WVALID    = Output(Bool())
      val M_AXI_WREADY    = Input(Bool())
      val M_AXI_BRESP     = Input(UInt(2.W))
      val M_AXI_BVALID    = Input(Bool())
      val M_AXI_BREADY    = Output(Bool())
      val M_AXI_ARADDR    = Output(UInt(C_M_AXI_ADDR_WIDTH.W))
      val M_AXI_ARLEN     = Output(UInt(8.W))
      val M_AXI_ARSIZE    = Output(UInt(3.W))
      val M_AXI_ARVALID   = Output(Bool())
      val M_AXI_ARREADY   = Input(Bool())
      val M_AXI_RDATA     = Input(UInt(C_M_AXI_DATA_WIDTH.W))
      val M_AXI_RLAST     = Input(Bool())
      val M_AXI_RVALID    = Input(Bool())
      val M_AXI_RREADY    = Output(Bool())
      val M_AXI_RRESP     = Input(UInt(2.W))

    }
  )

  addResource("/verilog/MasterAXIFromFile.v")
}

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

  val cache             = withClockAndReset(clock, reset) { Module(new Cache) }

  val axiSlave = withClockAndReset(clock, reset) {
    Module(new AxiSlave(ManticoreFullISA))
  }

  val axiMaster =
    Module(
      new MasterAXIFromFile(
        MemoryBaseAddress,
        CacheConfig.AddressBits,
        CacheConfig.CacheLineBits
      )
    )

  // axiMaster.io.data_in       := withClock(clock){cache.io.back.wline}
  // axiMaster.io.addr          := withClock(clock){cache.io.back.waddr}
  axiMaster.io.M_AXI_AWREADY := m_axi_bank_0.AWREADY
  axiMaster.io.M_AXI_BRESP   := m_axi_bank_0.BRESP
  axiMaster.io.M_AXI_RRESP   := m_axi_bank_0.RRESP
  axiMaster.io.M_AXI_BVALID  := m_axi_bank_0.BVALID
  axiMaster.io.M_AXI_WREADY  := m_axi_bank_0.WREADY
  axiMaster.io.M_AXI_RLAST   := m_axi_bank_0.RLAST
  axiMaster.io.M_AXI_ARREADY := m_axi_bank_0.ARREADY
  // axiMaster.io.M_AXI_RRESP   := 0.U
  axiMaster.io.M_AXI_RDATA := m_axi_bank_0.RDATA

  m_axi_bank_0.AWADDR  := axiMaster.io.M_AXI_AWADDR
  m_axi_bank_0.AWLEN   := axiMaster.io.M_AXI_AWLEN
  m_axi_bank_0.AWSIZE  := axiMaster.io.M_AXI_AWSIZE
  m_axi_bank_0.BURST   := axiMaster.io.M_AXI_AWBURST
  m_axi_bank_0.AWVALID := axiMaster.io.M_AXI_AWVALID
  m_axi_bank_0.WDATA   := axiMaster.io.M_AXI_WDATA
  m_axi_bank_0.WSTRB   := axiMaster.io.M_AXI_WSTRB
  m_axi_bank_0.WLAST   := axiMaster.io.M_AXI_WLAST
  m_axi_bank_0.WVALID  := axiMaster.io.M_AXI_WVALID
  m_axi_bank_0.BREADY  := axiMaster.io.M_AXI_BREADY
  m_axi_bank_0.ARADDR  := axiMaster.io.M_AXI_ARADDR
  m_axi_bank_0.ARLEN   := axiMaster.io.M_AXI_ARLEN
  m_axi_bank_0.ARSIZE  := axiMaster.io.M_AXI_ARSIZE
  m_axi_bank_0.ARVALID := axiMaster.io.M_AXI_ARVALID

  //m_axi_bank_0.RLAST    := axiMaster.io.M_AXI_RLAST
  axiMaster.io.M_AXI_RVALID := m_axi_bank_0.RVALID
  m_axi_bank_0.RREADY       := axiMaster.io.M_AXI_RREADY

  axiMaster.io.M_AXI_ACLK             := clock
  axiMaster.io.M_AXI_ARESETN          := reset_n
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
  val trigger        = withClockAndReset(clock, reset) { RegInit(Bool(), 0.B) }
  val write_back_trigger       = withClockAndReset(clock, reset) { RegInit(Bool(), 0.B) }
  val temp_addr       = withClockAndReset(clock, reset) { RegInit(UInt(CacheConfig.AddressBits.W), 0.U) }
  val trigger2_temp  = withClockAndReset(clock, reset) { RegInit(Bool(), 0.B) }
  val trigger3       = withClockAndReset(clock, reset) { RegInit(Bool(), 0.B) }
  val trigger3_temp  = withClockAndReset(clock, reset) { RegInit(Bool(), 0.B) }
  val txn_mode_temp  = withClockAndReset(clock, reset) { RegInit(Bool(), 0.B) }
  val txn_start_temp = withClockAndReset(clock, reset) { RegInit(Bool(), 0.B) }
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

  axiMaster.io.burst_size               := 0.U
  axiMaster.io.read_txn_start           := 0.B
  axiMaster.io.write_txn_start          := 0.B
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

  m_axi_bank_0.ARID := 0.U

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

  when(axiMaster.io.M_AXI_BVALID === 1.B) {
    axiSlave.io.dev_regs.bootloader_cycles := 8.U + axiMaster.io.M_AXI_BRESP
    bootloader_cycles_temp                 := 8.U + axiMaster.io.M_AXI_BRESP
  }
    .elsewhen((axiMaster.io.M_AXI_RVALID === 1.B)) {
      axiSlave.io.dev_regs.bootloader_cycles := 16.U + axiMaster.io.M_AXI_RRESP
      bootloader_cycles_temp                 := 16.U + axiMaster.io.M_AXI_RRESP
    }
    .otherwise {
      axiSlave.io.dev_regs.bootloader_cycles := bootloader_cycles_temp
    }

  switch(state) {
    is(KernelState.Idle) {
      txn_start_temp := 0.B
      txn_mode_temp  := 0.B

      axiSlave.io.dev_regs.execution_cycles := withClock(clock) { 0.U }
      when(
        axiSlave.io.host_regs
          .value_change_symbol_table_base(31, 0) === withClock(clock) { 0.U }
      ) {
        axiMaster.io.cache_ready := withClock(clock) { 1.B }
      }
        .otherwise {
          axiMaster.io.cache_ready := withClock(clock) { 0.B }
        }
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
      //   axiMaster.io.txn_start := withClock(clock) { 1.B }

      //   //cache.io.back.done := withClock(clock) { axiMaster.io.TXN_DONE }
      //   //trigger := withClock(clock) { axiMaster.io.TXN_DONE }

      // }
      cache.io.back.done := withClock(clock) {
        (axiMaster.io.read_txn_done || axiMaster.io.write_txn_done) && write_back_state === WriteBackState.Idle
      }
      when(cache.io.back.start === 1.B) {

        when(cache.io.back.cmd === CacheBackendCommand.Read.id.U) {
          axiMaster.io.read_txn_start := withClock(clock) { 1.B }
          axiMaster.io.read_addr       := withClock(clock) { cache.io.back.raddr*2.U}
          cache.io.back.rline     := withClock(clock) { axiMaster.io.data_out }
          axiMaster.io.burst_size := withClock(clock) { 1.U }
        }
          .elsewhen(cache.io.back.cmd === CacheBackendCommand.Write.id.U) {
            axiMaster.io.write_txn_start := withClock(clock) { 1.B }
            axiMaster.io.write_addr       := withClock(clock) { cache.io.back.waddr*2.U}
            axiMaster.io.data_in    := withClock(clock) { cache.io.back.wline }
            axiMaster.io.burst_size := withClock(clock) { 1.U }
      }


        // .elsewhen(cache.io.back.cmd === CacheBackendCommand.WriteBack.id.U) {
        //   trigger2 := withClock(clock){1.B}
        //   trigger2_temp := withClock(clock){1.B}
        //   axiMaster.io.txn_mode := withClock(clock) { 0.B }
        //   txn_mode_temp         := withClock(clock) { 0.B }

        //   axiMaster.io.addr    := withClock(clock) { cache.io.back.waddr }
        //   axiMaster.io.burst_size := withClock(clock) { 1.U }

        //   axiMaster.io.data_in := withClock(clock) { cache.io.back.wline }
        // }

      }

      switch(write_back_state) {
        is(WriteBackState.Idle) {
          when(
            cache.io.back.start === 1.B && cache.io.back.cmd === CacheBackendCommand.WriteBack.id.U
          ) {
            write_back_state             := WriteBackState.Write
            axiMaster.io.write_txn_start := withClock(clock) { 1.B }
            axiMaster.io.write_addr       := withClock(clock) { cache.io.back.waddr*2.U}
            axiMaster.io.data_in    := withClock(clock) { cache.io.back.wline }
            axiMaster.io.burst_size := withClock(clock) { 1.U }
          }
          temp_addr       := withClock(clock) { cache.io.back.raddr*2.U}

        }
        is(WriteBackState.Write) {
          when(axiMaster.io.write_txn_done === 1.B) {
            write_back_state            := WriteBackState.Read
            axiMaster.io.read_txn_start := withClock(clock) { 1.B }
            axiMaster.io.read_addr   := withClock(clock) { temp_addr }
            cache.io.back.rline := withClock(clock) { axiMaster.io.data_out }
            axiMaster.io.burst_size := withClock(clock) { 1.U }

          }
        }
        is(WriteBackState.Read) {
          when(axiMaster.io.read_txn_done === 1.B) {
            write_back_state := WriteBackState.Idle
            cache.io.back.done := withClock(clock) {
        axiMaster.io.read_txn_done
      }
          }
        }
      }
      // when(
      //   trigger2 === 1.B && axiMaster.io.TXN_DONE === 1.B
      // ) {
      //   trigger3 := withClock(clock) {1.B}
      //   trigger3_temp := withClock(clock){1.B}

      //   axiMaster.io.txn_mode := withClock(clock) { 1.B }
      //   txn_mode_temp         := withClock(clock) { 1.B }

      //   axiMaster.io.addr := withClock(clock) { cache.io.back.raddr }
      //   cache.io.back.rline := withClock(clock) {
      //     axiMaster.io.data_out
      //   }
      //   axiMaster.io.burst_size := withClock(clock) { 1.U }
      //   //trigger                 := withClock(clock) { 1.B }

      // } otherwise {
      //   trigger3 := withClock(clock){trigger3_temp}

      // }

      // when(axiMaster.io.TXN_DONE === 1.B && trigger === 1.B) {
      //   axiSlave.io.control.ap_ready := withClock(clock) { 1.B }
      //   state                        := withClock(clock) { KernelState.Wait }
      // }

      when(cache.io.front.done) {
        cacheReadData := withClock(clock) { cache.io.front.rdata }
        axiSlave.io.control.ap_ready := withClock(clock) { 1.B }

        state := withClock(clock) { KernelState.Done }
      }
      axiSlave.io.dev_regs.exception_id := withClock(clock) { 0.U }

    }
    is(KernelState.Done) {
      axiSlave.io.dev_regs.execution_cycles := withClock(clock) { 2.U }
      axiSlave.io.control.ap_done           := withClock(clock) { 1.B }
      axiSlave.io.dev_regs.exception_id     := withClock(clock) { 1.U }
      state := withClock(clock) { KernelState.Idle }

    }

  }

}

object CacheKernelGen extends App {

  new ChiselStage()
    .emitVerilog(new CacheKernel(), Array("-td", "gen-dir/cache-kernel"))
}