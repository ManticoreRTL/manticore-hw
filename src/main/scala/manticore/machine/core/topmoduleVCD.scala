package manticore.machine.core

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util.is
import chisel3.util._
import chisel3.util.switch
import chisel3.stage.ChiselStage
import manticore.machine.ISA
import manticore.machine.ManticoreBaseISA
import manticore.machine.ManticoreFullISA
import manticore.machine.memory.SimpleDualPortMemory
import manticore.machine.memory.MemStyle
import manticore.machine.xrt.MasterAXIFromFile
import manticore.machine.xrt.AxiParameters
import manticore.machine.xrt.AxiMasterIF
import manticore.machine.memory.CacheConfig
import manticore.machine.xrt.axislave_vip
import manticore.machine
import scala.math
import os.stat

class topmoduleVCD (config:ISA) extends Module{
        val MEM_SIZE = 0x100000

    val io = IO(new Bundle {
        val mem_data_in = Input(UInt(config.DataBits.W))
      val mem_raddr    = Input(UInt((log2Ceil(MEM_SIZE)).W))
      val mem_waddr    = Input(UInt((log2Ceil(MEM_SIZE)).W))
        val mem_lock = Input(Bool())
        val mem_wen = Input(Bool())
        val mem_data_out = Output(UInt(32.W))
        val id_in = Input(UInt(16.W))
        val data_in = Input(UInt(16.W))
        val valid_in = Input(Bool())
        val kill_clock = Output(Bool())
    })

    val vcd = withClockAndReset(clock,reset){Module(new vcd(config=ManticoreFullISA))}
      val s_axi_bank_0 = Module(
    (new axislave_vip(
      log2Ceil(MEM_SIZE),
      64,
      16,
      MEM_SIZE
    ))
  )
    io.kill_clock := vcd.io.kill_clock
    //val bundle = Wire(new NoCBundle(DimX=2, DimY=2, config=ManticoreFullISA))
    // vcd.io.id_in := bundle.address
    // vcd.io.data_in := bundle.data
    // vcd.io.valid := bundle.valid
    
    
    vcd.io.id_in := io.id_in
    vcd.io.data_in := io.data_in
    vcd.io.valid := io.valid_in

      s_axi_bank_0.io.mem_data_in := io.data_in
  s_axi_bank_0.io.mem_raddr   := io.mem_raddr
  s_axi_bank_0.io.mem_waddr   := io.mem_waddr
  s_axi_bank_0.io.lock        := io.mem_lock
  s_axi_bank_0.io.mem_wen     := io.mem_wen
  io.mem_data_out                 := s_axi_bank_0.io.mem_data_out
  // axiMaster.io.data_in       := withClock(clock){cache.io.back.wline}
  // axiMaster.io.addr          := withClock(clock){cache.io.back.waddr}
  vcd.io.m_axi_bank_0.AWREADY := s_axi_bank_0.io.S_AXI_AWREADY
  vcd.io.m_axi_bank_0.BRESP  := s_axi_bank_0.io.S_AXI_BRESP
  vcd.io.m_axi_bank_0.RRESP    := s_axi_bank_0.io.S_AXI_RRESP
  vcd.io.m_axi_bank_0.BVALID  := s_axi_bank_0.io.S_AXI_BVALID
  vcd.io.m_axi_bank_0.WREADY  := s_axi_bank_0.io.S_AXI_WREADY
  vcd.io.m_axi_bank_0.RLAST   := s_axi_bank_0.io.S_AXI_RLAST
  vcd.io.m_axi_bank_0.ARREADY := s_axi_bank_0.io.S_AXI_ARREADY
  // axiMaster.io.M_AXI_RRESP   := 0.U
  vcd.io.m_axi_bank_0.RDATA := s_axi_bank_0.io.S_AXI_RDATA
  vcd.io.m_axi_bank_0.RID := DontCare
  s_axi_bank_0.io.S_AXI_AWADDR  := vcd.io.m_axi_bank_0.AWADDR
  s_axi_bank_0.io.S_AXI_AWLEN   := vcd.io.m_axi_bank_0.AWLEN
  s_axi_bank_0.io.S_AXI_AWSIZE  := vcd.io.m_axi_bank_0.AWSIZE
  s_axi_bank_0.io.S_AXI_AWVALID := vcd.io.m_axi_bank_0.AWVALID
  s_axi_bank_0.io.S_AXI_WDATA   := vcd.io.m_axi_bank_0.WDATA
  s_axi_bank_0.io.S_AXI_WSTRB   := vcd.io.m_axi_bank_0.WSTRB
  s_axi_bank_0.io.S_AXI_WLAST   := vcd.io.m_axi_bank_0.WLAST
  s_axi_bank_0.io.S_AXI_WVALID  := vcd.io.m_axi_bank_0.WVALID
  s_axi_bank_0.io.S_AXI_BREADY  := vcd.io.m_axi_bank_0.BREADY
  s_axi_bank_0.io.S_AXI_ARADDR  := vcd.io.m_axi_bank_0.ARADDR
  s_axi_bank_0.io.S_AXI_ARLEN   := vcd.io.m_axi_bank_0.ARLEN
  s_axi_bank_0.io.S_AXI_ARSIZE  := vcd.io.m_axi_bank_0.ARSIZE
  s_axi_bank_0.io.S_AXI_ARVALID := vcd.io.m_axi_bank_0.ARVALID

  //m_axi_bank_0.RLAST    := axiMaster.io.M_AXI_RLAST
  vcd.io.m_axi_bank_0.RVALID    := s_axi_bank_0.io.S_AXI_RVALID
  s_axi_bank_0.io.S_AXI_RREADY := vcd.io.m_axi_bank_0.RREADY

  s_axi_bank_0.io.S_AXI_ACLK    := clock
  s_axi_bank_0.io.S_AXI_ARESETN := 1.B



}

