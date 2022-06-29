package manticore.machine.xrt

import chisel3._
import manticore.machine.ISA
import manticore.machine.core.BareNoCBundle
import java.io.File
import java.nio.file.Paths
import manticore.machine.ManticoreBaseISA
import manticore.machine.ManticoreFullISA
import manticore.machine.memory.Cache
import manticore.machine.memory.CacheCommand
import manticore.machine.memory.CacheInterface
import manticore.machine.memory.CacheBackendCommand
import manticore.machine.xrt.AxiSlave
import manticore.machine.xrt.AxiMasterIF
import manticore.machine.xrt.MasterAXIFromFile
import manticore.machine.ManticoreFullISA
import manticore.machine.core.ClockDistribution
import chisel3.experimental.ChiselEnum
import chisel3.util._
import manticore.machine.memory.CacheConfig
import chisel3.stage.ChiselStage
import os.temp
import manticore.machine.core.Processor
import os.proc

class axislave_vip(
    val C_S_AXI_ADDR_WIDTH: Int,
    val C_S_AXI_DATA_WIDTH: Int,
    val CACHE_DATA_WIDTH: Int,
    val MEM_SIZE: Int
) extends BlackBox(
      Map(
        "C_S_AXI_ADDR_WIDTH" -> C_S_AXI_ADDR_WIDTH,
        "C_S_AXI_DATA_WIDTH" -> C_S_AXI_DATA_WIDTH,
        "CACHE_DATA_WIDTH"   -> CACHE_DATA_WIDTH,
        "MEM_SIZE"           -> MEM_SIZE
      )
    )
    with HasBlackBoxResource {

  val io = IO(
    new Bundle {
      val mem_wen      = Input(Bool())
      val mem_data_in  = Input(UInt(CACHE_DATA_WIDTH.W))
      val mem_data_out = Output(UInt(CACHE_DATA_WIDTH.W))
      val mem_raddr    = Input(UInt((log2Ceil(MEM_SIZE)).W))
      val mem_waddr    = Input(UInt((log2Ceil(MEM_SIZE)).W))
      val lock         = Input(Bool())

      val S_AXI_ARESETN = Input(Bool())
      val S_AXI_ACLK    = Input(Clock())
      val S_AXI_AWADDR  = Input(UInt((log2Ceil(MEM_SIZE)).W))
      val S_AXI_AWLEN   = Input(UInt(8.W))
      val S_AXI_AWSIZE  = Input(UInt(3.W))
      val S_AXI_AWBURST = Input(UInt(2.W))
      val S_AXI_AWVALID = Input(Bool())
      val S_AXI_AWREADY = Output(Bool())
      val S_AXI_WDATA   = Input(UInt(C_S_AXI_DATA_WIDTH.W))
      val S_AXI_WSTRB   = Input(UInt(((C_S_AXI_DATA_WIDTH) / 8).W))
      val S_AXI_WLAST   = Input(Bool())
      val S_AXI_WVALID  = Input(Bool())
      val S_AXI_WREADY  = Output(Bool())
      val S_AXI_BRESP   = Output(UInt(2.W))
      val S_AXI_BVALID  = Output(Bool())
      val S_AXI_BREADY  = Input(Bool())
      val S_AXI_ARADDR  = Input(UInt((log2Ceil(MEM_SIZE)).W))
      val S_AXI_ARLEN   = Input(UInt(8.W))
      val S_AXI_ARSIZE  = Input(UInt(3.W))
      val S_AXI_ARVALID = Input(Bool())
      val S_AXI_ARREADY = Output(Bool())
      val S_AXI_RDATA   = Output(UInt(C_S_AXI_DATA_WIDTH.W))
      val S_AXI_RLAST   = Output(Bool())
      val S_AXI_RVALID  = Output(Bool())
      val S_AXI_RREADY  = Input(Bool())
      val S_AXI_RRESP   = Output(UInt(2.W))

    }
  )

  addResource("/verilog/axislave_vip.v")
}

class CacheKernelWithSlave(config: ISA) extends Module {
  val MEM_SIZE = 0x1000000
  val io = IO(
    new Bundle {

      //   val clock = Input(Clock())
      //   clock.suggestName("ap_clk")
      //   val reset_n = Input(Bool())
      //   reset_n.suggestName("ap_rst_n")
      val packet_in      = Input(new BareNoCBundle(config))
      val data_in        = Input(UInt(CacheConfig.DataBits.W))
      val data_out       = Output(UInt(CacheConfig.DataBits.W))
      val wen            = Input(Bool())
      val lock           = Input(Bool())
      val raddr          = Input(UInt((log2Ceil(MEM_SIZE)).W))
      val waddr          = Input(UInt((log2Ceil(MEM_SIZE)).W))
      val interrupt      = Input(Bool())
      val cache_cmd      = Input(UInt(2.W))
      val cache_addr     = Input(UInt((log2Ceil(MEM_SIZE)).W))
      val cache_data_in  = Input(UInt(CacheConfig.DataBits.W))
      val cache_data_out = Output(UInt(CacheConfig.DataBits.W))
      val cache_done     = Output(Bool())
      val cache_start    = Input(Bool())
    }
  )

//   val reset = Wire(Bool())
//   reset := ~io.reset_n

  val s_axi_bank_0 = Module(
    (new axislave_vip(
      log2Ceil(MEM_SIZE),
      CacheConfig.CacheLineBits,
      CacheConfig.DataBits,
      MEM_SIZE
    ))
  )
  val initial_registers = (Paths
    .get(
      "test_data_dir" + File.separator +
        "CacheKernelWithSlave" + File.separator + "rf.data"
    )
    .toAbsolutePath)
    .toString

  val MemoryBaseAddress: Long = 0x00000000

  val cache = withClockAndReset(clock, reset) { Module(new Cache) }
  val rdgen = new scala.util.Random(0)

  val equations = Seq.fill(1 << ManticoreFullISA.FunctBits) {
    Seq.fill(ManticoreFullISA.DataBits) {
      BigInt(rdgen.nextInt(1 << 16))
    }
  }
  val clock_distribution = Module(new ClockDistribution(freqMhz = 200.0))
  val processor = withClock(clock_distribution.io.compute_clock) {
    Module(new Processor(config, 16, 16, equations, initial_registers, "", "core", false, 0, true))
  }

  processor.io.packet_in            := io.packet_in
  processor.io.periphery.debug_time := 0.U
  val axiMaster =
    Module(
      new MasterAXIFromFile(
        MemoryBaseAddress,
        CacheConfig.AddressBits,
        CacheConfig.CacheLineBits
      )
    )
  clock_distribution.io.compute_clock_en_n := 1.B
  val control_clock = clock_distribution.io.control_clock

  val rdata_delay_register = withClock(control_clock){RegInit(UInt(CacheConfig.DataBits.W),0.U)}
  clock_distribution.io.root_clock   := clock
  cache.io.front.start               := withClock(clock_distribution.io.compute_clock) { processor.io.periphery.cache.start }
  cache.io.front.cmd                 := withClock(clock_distribution.io.compute_clock) { processor.io.periphery.cache.cmd }
  cache.io.front.addr                := withClock(clock_distribution.io.compute_clock) { processor.io.periphery.cache.addr }
  cache.io.front.wdata               := withClock(clock_distribution.io.compute_clock) { processor.io.periphery.cache.wdata }
  rdata_delay_register := withClock(control_clock) { cache.io.front.rdata }
  processor.io.periphery.cache.rdata := withClock(clock_distribution.io.compute_clock) { rdata_delay_register }
  processor.io.periphery.cache.idle  := withClock(clock_distribution.io.compute_clock) { cache.io.front.idle }
  processor.io.periphery.cache.done  := withClock(clock_distribution.io.compute_clock) { cache.io.front.done }

  object ProcessorState extends ChiselEnum {
    val Active, Dead = Value
  }

  val processor_state = withClock(clock) { RegInit(ProcessorState.Type(), ProcessorState.Active) }

  s_axi_bank_0.io.mem_data_in := io.data_in
  s_axi_bank_0.io.mem_raddr   := io.raddr
  s_axi_bank_0.io.mem_waddr   := io.waddr
  s_axi_bank_0.io.lock        := io.lock
  s_axi_bank_0.io.mem_wen     := io.wen
  io.data_out                 := s_axi_bank_0.io.mem_data_out
  // axiMaster.io.data_in       := withClock(clock){cache.io.back.wline}
  // axiMaster.io.addr          := withClock(clock){cache.io.back.waddr}
  axiMaster.io.M_AXI_AWREADY := s_axi_bank_0.io.S_AXI_AWREADY
  axiMaster.io.M_AXI_BRESP   := s_axi_bank_0.io.S_AXI_BRESP
  axiMaster.io.M_AXI_RRESP   := s_axi_bank_0.io.S_AXI_RRESP
  axiMaster.io.M_AXI_BVALID  := s_axi_bank_0.io.S_AXI_BVALID
  axiMaster.io.M_AXI_WREADY  := s_axi_bank_0.io.S_AXI_WREADY
  axiMaster.io.M_AXI_RLAST   := s_axi_bank_0.io.S_AXI_RLAST
  axiMaster.io.M_AXI_ARREADY := s_axi_bank_0.io.S_AXI_ARREADY
  // axiMaster.io.M_AXI_RRESP   := 0.U
  axiMaster.io.M_AXI_RDATA := s_axi_bank_0.io.S_AXI_RDATA

  s_axi_bank_0.io.S_AXI_AWADDR  := axiMaster.io.M_AXI_AWADDR
  s_axi_bank_0.io.S_AXI_AWLEN   := axiMaster.io.M_AXI_AWLEN
  s_axi_bank_0.io.S_AXI_AWSIZE  := axiMaster.io.M_AXI_AWSIZE
  s_axi_bank_0.io.S_AXI_AWVALID := axiMaster.io.M_AXI_AWVALID
  s_axi_bank_0.io.S_AXI_WDATA   := axiMaster.io.M_AXI_WDATA
  s_axi_bank_0.io.S_AXI_WSTRB   := axiMaster.io.M_AXI_WSTRB
  s_axi_bank_0.io.S_AXI_WLAST   := axiMaster.io.M_AXI_WLAST
  s_axi_bank_0.io.S_AXI_WVALID  := axiMaster.io.M_AXI_WVALID
  s_axi_bank_0.io.S_AXI_BREADY  := axiMaster.io.M_AXI_BREADY
  s_axi_bank_0.io.S_AXI_ARADDR  := axiMaster.io.M_AXI_ARADDR
  s_axi_bank_0.io.S_AXI_ARLEN   := axiMaster.io.M_AXI_ARLEN
  s_axi_bank_0.io.S_AXI_ARSIZE  := axiMaster.io.M_AXI_ARSIZE
  s_axi_bank_0.io.S_AXI_ARVALID := axiMaster.io.M_AXI_ARVALID

  //m_axi_bank_0.RLAST    := axiMaster.io.M_AXI_RLAST
  axiMaster.io.M_AXI_RVALID    := s_axi_bank_0.io.S_AXI_RVALID
  s_axi_bank_0.io.S_AXI_RREADY := axiMaster.io.M_AXI_RREADY

  axiMaster.io.M_AXI_ACLK       := clock
  s_axi_bank_0.io.S_AXI_ACLK    := clock
  axiMaster.io.M_AXI_ARESETN    := 1.B
  s_axi_bank_0.io.S_AXI_ARESETN := 1.B
  //axiSlave.io.dev_regs.virtual_cycles := 0.U
  // axiSlave.io.host_regs.schedule_config := io.KernelScheduleConfig
  object KernelState extends ChiselEnum {
    val Idle, Wait, Done, Read = Value
  }

  object WriteBackState extends ChiselEnum {
    val Idle, Write, Read = Value
  }
  //val test = axiSlave.io.host_regs.value_change_symbol_table_base
  // val state

  val commandReg = withClock(control_clock) { Reg(CacheCommand.Type()) }
  val state = withClockAndReset(control_clock, reset) {
    RegInit(KernelState.Type(), KernelState.Idle)
  }

  val write_back_state = withClockAndReset(control_clock, reset) {
    RegInit(WriteBackState.Type(), WriteBackState.Idle)
  }

  val commandWire = WireInit(CacheCommand.Type(), CacheCommand.Read)

  switch(io.cache_cmd) {
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
  val bootloader_cycles_temp = withClockAndReset(control_clock, reset) {
    RegInit(UInt(64.W), 0.U)
  }
  val trigger = withClockAndReset(control_clock, reset) { RegInit(Bool(), 0.B) }
  val write_back_trigger = withClockAndReset(control_clock, reset) {
    RegInit(Bool(), 0.B)
  }
  val temp_addr = withClockAndReset(control_clock, reset) {
    RegInit(UInt(CacheConfig.AddressBits.W), 0.U)
  }
  val trigger2_temp = withClockAndReset(control_clock, reset) {
    RegInit(Bool(), 0.B)
  }
  val trigger3 = withClockAndReset(control_clock, reset) { RegInit(Bool(), 0.B) }
  val trigger3_temp = withClockAndReset(control_clock, reset) {
    RegInit(Bool(), 0.B)
  }
  val txn_mode_temp = withClockAndReset(control_clock, reset) {
    RegInit(Bool(), 0.B)
  }
  val txn_start_temp = withClockAndReset(control_clock, reset) {
    RegInit(Bool(), 0.B)
  }
  val temp_counter = withClockAndReset(control_clock, reset) {
    RegInit(UInt(32.W), 0.U)
  }

  val counter = withClockAndReset(control_clock, reset) { RegInit(UInt(32.W), 0.U) }

  val start_flag = withClockAndReset(control_clock, reset) { RegInit(Bool(), 0.B) }
  val start_flag_next = withClockAndReset(control_clock, reset) {
    RegInit(Bool(), 0.B)
  }

  val cacheReadData = withClock(control_clock) { Reg(UInt(CacheConfig.DataBits.W)) }
  io.cache_data_out := cacheReadData
  io.cache_done     := (state === KernelState.Done)

  // //doubt
  //axiSlave.io.control.ap_ready := (state === KernelState.Done)
  //axiSlave.io.control.ap_idle  := (state === KernelState.Idle)

  axiMaster.io.burst_size      := 0.U
  axiMaster.io.read_txn_start  := 0.B
  axiMaster.io.write_txn_start := 0.B

  //axiSlave.io.dev_regs.exception_id     := 0.U
  //axiSlave.io.dev_regs.execution_cycles := 0.U

  //m_axi_bank_0 := 0.U
  //s_axi_control.WDATA   := 0.U
  //s_axi_control.RREADY  := 0.B
  //s_axi_control.BREADY  := 0.B

  cache.io.back.done := DontCare

  // io.KernelDone                          := DontCare
  cache.io.back.rline := DontCare
//   axiSlave.io.control.ap_done  := 0.U
//   axiSlave.io.control.ap_ready := 0.U
//   axiSlave.io.control.ap_idle  := 0.U
  commandReg := commandWire

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
    temp_counter := withClock(control_clock) { 1.U }
  }

  when(cache.io.front.done === 1.B) {
    counter := withClock(control_clock) { 1.U }
  }
  //.elsewhen(cache.io.back.done===1.B)
  // {
  //   counter := withClock(clock){temp_counter+1.U}
  //   temp_counter := 1.U
  // } otherwise{
  //   temp_counter := withClock(clock){1.U}
  // }

//   when(axiMaster.io.M_AXI_BVALID === 1.B) {
//     axiSlave.io.dev_regs.bootloader_cycles := 8.U + axiMaster.io.M_AXI_BRESP
//     bootloader_cycles_temp                 := 8.U + axiMaster.io.M_AXI_BRESP
//   }
//     .elsewhen((axiMaster.io.M_AXI_RVALID === 1.B)) {
//       axiSlave.io.dev_regs.bootloader_cycles := 16.U + axiMaster.io.M_AXI_RRESP
//       bootloader_cycles_temp                 := 16.U + axiMaster.io.M_AXI_RRESP
//     }
//     .otherwise {
//       axiSlave.io.dev_regs.bootloader_cycles := bootloader_cycles_temp
//     }

  switch(state) {
    is(KernelState.Idle) {
      txn_start_temp := 0.B
      txn_mode_temp  := 0.B

      //axiSlave.io.dev_regs.execution_cycles := withClock(clock) { 0.U }
      //   when(
      //     axiSlave.io.host_regs
      //       .value_change_symbol_table_base(31, 0) === withClock(clock) { 0.U }
      //   ) {
      //     axiMaster.io.cache_ready := withClock(clock) { 1.B }
      //   }
      //     .otherwise {
      //       axiMaster.io.cache_ready := withClock(clock) { 0.B }
      //     }
      //axiSlave.io.control.ap_idle := withClock(clock) { true.B }
      when(
        cache.io.front.start === 1.B
      ) {
        state := withClock(control_clock) { KernelState.Wait }
      }.elsewhen(cache.io.front.idle === 1.U) {

        //cache.io.front.cmd   := DontCare
        cache.io.back.rline  := DontCare
        cache.io.front.wdata := DontCare
      }
      io.cache_done := withClock(control_clock) { 0.U }
      trigger       := withClock(control_clock) { 0.B }
      trigger3      := withClock(control_clock) { 0.B }
      trigger2_temp := withClock(control_clock) { 0.B }
      trigger3_temp := withClock(control_clock) { 0.B }

    }
    is(KernelState.Wait) {

      // cache.io.front.cmd := withClock(control_clock) { commandReg }
      // cache.io.front.addr := withClock(control_clock) {
      //   io.cache_addr
      // }
      // cache.io.front.wdata := withClock(control_clock) {
      //   io.cache_data_in
      // }
      // when(cache.io.front.idle === 1.B) {
      //   cache.io.front.start := withClock(control_clock) { true.B }
      // }

      cache.io.back.done := withClock(control_clock) {
        (axiMaster.io.read_txn_done || axiMaster.io.write_txn_done) && write_back_state === WriteBackState.Idle
      }
      when(cache.io.back.start === 1.B) {

        when(cache.io.back.cmd === CacheBackendCommand.Read.id.U) {
          axiMaster.io.read_txn_start := withClock(control_clock) { 1.B }
          axiMaster.io.read_addr := withClock(control_clock) {
            cache.io.back.raddr
          }
          cache.io.back.rline     := withClock(control_clock) { axiMaster.io.data_out }
          axiMaster.io.burst_size := withClock(control_clock) { 1.U }
        }
          .elsewhen(cache.io.back.cmd === CacheBackendCommand.Write.id.U) {
            axiMaster.io.write_txn_start := withClock(control_clock) { 1.B }
            axiMaster.io.write_addr := withClock(control_clock) {
              cache.io.back.waddr
            }
            axiMaster.io.data_in    := withClock(control_clock) { cache.io.back.wline }
            axiMaster.io.burst_size := withClock(control_clock) { 1.U }
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
            axiMaster.io.write_txn_start := withClock(control_clock) { 1.B }
            axiMaster.io.write_addr := withClock(control_clock) {
              cache.io.back.waddr
            }
            axiMaster.io.data_in    := withClock(control_clock) { cache.io.back.wline }
            axiMaster.io.burst_size := withClock(control_clock) { 1.U }
          }
          temp_addr := withClock(control_clock) { cache.io.back.raddr * 2.U }

        }
        is(WriteBackState.Write) {
          when(axiMaster.io.write_txn_done === 1.B) {
            write_back_state            := WriteBackState.Read
            axiMaster.io.read_txn_start := withClock(control_clock) { 1.B }
            axiMaster.io.read_addr      := withClock(control_clock) { temp_addr }
            cache.io.back.rline         := withClock(control_clock) { axiMaster.io.data_out }
            axiMaster.io.burst_size     := withClock(control_clock) { 1.U }

          }
        }
        is(WriteBackState.Read) {
          when(axiMaster.io.read_txn_done === 1.B) {
            write_back_state := WriteBackState.Idle
            cache.io.back.done := withClock(control_clock) {
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
        cacheReadData := withClock(control_clock) { cache.io.front.rdata }
        //axiSlave.io.control.ap_ready := withClock(control_clock) { 1.B }

        state := withClock(control_clock) { KernelState.Done }
      }
      //axiSlave.io.d := withClock(control_clock) { 0.U }

    }
    is(KernelState.Done) {
      //axiSlave.io.dev_regs.execution_cycles := withClock(control_clock) { 2.U }
      io.cache_done := withClock(control_clock) { 1.B }
      //axiSlave.io.dev_regs.exception_id     := withClock(control_clock) { 1.U }
      state := withClock(control_clock) { KernelState.Idle }

    }

  }

  switch(processor_state) {
    is(ProcessorState.Active) {
      when(processor.io.periphery.dynamic_cycle === 1.B) {
        clock_distribution.io.compute_clock_en_n := withClock(control_clock) { 1.B }
        processor_state                          := withClock(control_clock) { ProcessorState.Dead }
      }
        .otherwise {
          clock_distribution.io.compute_clock_en_n := withClock(control_clock) { 0.B }
        }
    }
    is(ProcessorState.Dead) {
      when(state === KernelState.Done) {
        clock_distribution.io.compute_clock_en_n := withClock(control_clock) { 0.B }
        when(processor.io.periphery.dynamic_cycle === 0.B) {
          processor_state := withClock(control_clock) { ProcessorState.Active }
        }
      }
    }
  }

}

object CacheKernelWithSlaveGen extends App {

  new ChiselStage()
    .emitVerilog(
      new CacheKernelWithSlave(config = ManticoreBaseISA),
      Array("-td", "gen-dir/cache-kernel-withslave")
    )
}
