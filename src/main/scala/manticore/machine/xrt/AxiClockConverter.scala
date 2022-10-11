package manticore.machine.xrt

import chisel3._
import chisel3.util._
import manticore.machine.ISA
import manticore.machine.xrt.AxiSlave.AxiSlaveCoreInterface
import chisel3.stage.ChiselStage

object AxiLiteClockConverter {

// vivado creates and IP with lower case names, so we can not use manticore.machine.xrt.AxiSlave.AxiSlaveInterface :(
  class AxiLiteConverterInterface(AxiSlaveAddrWidth: Int = 8, AxiSlaveDataWidth: Int = 32) extends Bundle {

    val awaddr = Input(UInt(AxiSlaveAddrWidth.W))
    val awprot = Input(UInt(3.W)) // should be hardcoded to 0

    val awvalid = Input(Bool())
    val awready = Output(Bool())
    val wdata   = Input(UInt(AxiSlaveDataWidth.W))
    val wstrb   = Input(UInt((AxiSlaveDataWidth / 8).W))
    val wvalid  = Input(Bool())
    val wready  = Output(Bool())
    val bresp   = Output(UInt(2.W))
    val bvalid  = Output(Bool())
    val bready  = Input(Bool())
    val araddr  = Input(UInt(AxiSlaveAddrWidth.W))
    val arprot  = Input(UInt(3.W)) // should be hardcoded to 0

    val arvalid = Input(Bool())
    val arready = Output(Bool())
    val rdata   = Output(UInt(AxiSlaveDataWidth.W))
    val rresp   = Output(UInt(2.W))
    val rvalid  = Output(Bool())
    val rready  = Input(Bool())

  }

  class axi4lite_clock_converter(AxiSlaveAddrWidth: Int = 8, AxiSlaveDataWidth: Int = 32) extends BlackBox {
    val io = IO(new Bundle {
      val s_axi         = new AxiLiteConverterInterface(AxiSlaveAddrWidth, AxiSlaveDataWidth)
      val m_axi_aclk    = Input(Clock()) // input wire m_axi_aclk
      val m_axi_aresetn = Input(Bool())  // input wire m_axi_aresetn
      val m_axi         = Flipped(new AxiLiteConverterInterface(AxiSlaveAddrWidth, AxiSlaveDataWidth))
      val s_axi_aclk    = Input(Clock())
      val s_axi_aresetn = Input(Bool())

    })
  }

}
class AxiLiteClockConverter(AxiSlaveAddrWidth: Int = 8, AxiSlaveDataWidth: Int = 32) extends RawModule {

  val s_axi        = IO(new AxiSlaveCoreInterface(AxiSlaveAddrWidth, AxiSlaveDataWidth))
  val s_axi_aclk   = IO(Input(Clock()))
  val s_axi_resetn = IO(Input(Bool()))

  val m_axi        = IO(Flipped(new AxiSlaveCoreInterface(AxiSlaveAddrWidth, AxiSlaveDataWidth)))
  val m_axi_aclk   = IO(Input(Clock()))
  val m_axi_resetn = IO(Input(Bool()))

  val impl = Module(new AxiLiteClockConverter.axi4lite_clock_converter(AxiSlaveAddrWidth, AxiSlaveDataWidth))

  impl.io.s_axi_aclk    := s_axi_aclk
  impl.io.s_axi_aresetn := s_axi_resetn
  impl.io.s_axi.awaddr <> s_axi.AWADDR
  impl.io.s_axi.awprot := 0.U(3.W)
  impl.io.s_axi.awvalid <> s_axi.AWVALID
  impl.io.s_axi.awready <> s_axi.AWREADY
  impl.io.s_axi.wdata <> s_axi.WDATA
  impl.io.s_axi.wstrb <> s_axi.WSTRB
  impl.io.s_axi.wvalid <> s_axi.WVALID
  impl.io.s_axi.wready <> s_axi.WREADY
  impl.io.s_axi.bresp <> s_axi.BRESP
  impl.io.s_axi.bvalid <> s_axi.BVALID
  impl.io.s_axi.bready <> s_axi.BREADY
  impl.io.s_axi.araddr <> s_axi.ARADDR
  impl.io.s_axi.arprot := 0.U(3.W)
  impl.io.s_axi.arvalid <> s_axi.ARVALID
  impl.io.s_axi.arready <> s_axi.ARREADY
  impl.io.s_axi.rdata <> s_axi.RDATA
  impl.io.s_axi.rresp <> s_axi.RRESP
  impl.io.s_axi.rvalid <> s_axi.RVALID
  impl.io.s_axi.rready <> s_axi.RREADY

  impl.io.m_axi_aclk    := m_axi_aclk
  impl.io.m_axi_aresetn := m_axi_resetn
  impl.io.m_axi.awaddr <> m_axi.AWADDR

  impl.io.m_axi.awvalid <> m_axi.AWVALID
  impl.io.m_axi.awready <> m_axi.AWREADY
  impl.io.m_axi.wdata <> m_axi.WDATA
  impl.io.m_axi.wstrb <> m_axi.WSTRB
  impl.io.m_axi.wvalid <> m_axi.WVALID
  impl.io.m_axi.wready <> m_axi.WREADY
  impl.io.m_axi.bresp <> m_axi.BRESP
  impl.io.m_axi.bvalid <> m_axi.BVALID
  impl.io.m_axi.bready <> m_axi.BREADY
  impl.io.m_axi.araddr <> m_axi.ARADDR

  impl.io.m_axi.arvalid <> m_axi.ARVALID
  impl.io.m_axi.arready <> m_axi.ARREADY
  impl.io.m_axi.rdata <> m_axi.RDATA
  impl.io.m_axi.rresp <> m_axi.RRESP
  impl.io.m_axi.rvalid <> m_axi.RVALID
  impl.io.m_axi.rready <> m_axi.RREADY

}

object Axi4ClockConverter {

  class Axi4ConverterInterface(params: AxiParameters) extends Bundle {

    val awaddr   = Output(UInt(params.AddrWidth.W))       // output wire [63 : 0] m_axi_awaddr
    val awlen    = Output(UInt(8.W))                      // output wire [7 : 0] m_axi_awlen
    val awsize   = Output(UInt(3.W))                      // output wire [2 : 0] m_axi_awsize
    val awburst  = Output(UInt(2.W))                      // output wire [1 : 0] m_axi_awburst
    val awlock   = Output(UInt(1.W))                      // output wire [0 : 0] m_axi_awlock
    val awcache  = Output(UInt(4.W))                      // output wire [3 : 0] m_axi_awcache
    val awprot   = Output(UInt(3.W))                      // output wire [2 : 0] m_axi_awprot
    val awregion = Output(UInt(4.W))                      // output wire [3 : 0] m_axi_awregion
    val awqos    = Output(UInt(4.W))                      // output wire [3 : 0] m_axi_awqos
    val awvalid  = Output(Bool())                         // output wire m_axi_awvalid
    val awready  = Input(Bool())                          // input wire m_axi_awready
    val wdata    = Output(UInt(params.DataWidth.W))       // output wire [31 : 0] m_axi_wdata
    val wstrb    = Output(UInt((params.DataWidth / 8).W)) // output wire [3 : 0] m_axi_wstrb
    val wlast    = Output(Bool())                         // output wire m_axi_wlast
    val wvalid   = Output(Bool())                         // output wire m_axi_wvalid
    val wready   = Input(Bool())                          // input wire m_axi_wready
    val bresp    = Input(UInt(2.W))                       // input wire [1 : 0] m_axi_bresp
    val bvalid   = Input(Bool())                          // input wire m_axi_bvalid
    val bready   = Output(Bool())                         // output wire m_axi_bready
    val araddr   = Output(UInt(params.AddrWidth.W))       // output wire [63 : 0] m_axi_araddr
    val arlen    = Output(UInt(8.W))                      // output wire [7 : 0] m_axi_arlen
    val arsize   = Output(UInt(3.W))                      // output wire [2 : 0] m_axi_arsize
    val arburst  = Output(UInt(2.W))                      // output wire [1 : 0] m_axi_arburst
    val arlock   = Output(UInt(1.W))                      // output wire [0 : 0] m_axi_arlock
    val arcache  = Output(UInt(4.W))                      // output wire [3 : 0] m_axi_arcache
    val arprot   = Output(UInt(3.W))                      // output wire [2 : 0] m_axi_arprot
    val arregion = Output(UInt(4.W))                      // output wire [3 : 0] m_axi_arregion
    val arqos    = Output(UInt(4.W))                      // output wire [3 : 0] m_axi_arqos
    val arvalid  = Output(Bool())                         // output wire m_axi_arvalid
    val arready  = Input(Bool())                          // input wire m_axi_arready
    val rdata    = Input(UInt(params.DataWidth.W))        // input wire [31 : 0] m_axi_rdata
    val rresp    = Input(UInt(2.W))                       // input wire [1 : 0] m_axi_rresp
    val rlast    = Input(Bool())                          // input wire m_axi_rlast
    val rvalid   = Input(Bool())                          // input wire m_axi_rvalid
    val rready   = Output(Bool())                         // output wire m_axi_rready

  }

  class axi4_clock_converter(params: AxiParameters) extends BlackBox {
    val io = IO(new Bundle {
      val m_axi         = new Axi4ConverterInterface(params)
      val m_axi_aclk    = Input(Clock()) // input wire m_axi_aclk
      val m_axi_aresetn = Input(Bool())  // input wire m_axi_aresetn
      val s_axi         = Flipped(new Axi4ConverterInterface(params))
      val s_axi_aclk    = Input(Clock()) // input wire m_axi_aclk
      val s_axi_aresetn = Input(Bool())  // input wire m_axi_aresetn
    })
  }

}

class Axi4ClockConverter(params: AxiParameters) extends RawModule {

  val m_axi         = IO(new AxiMasterIF(params))
  val m_axi_aclk    = IO(Input(Clock()))
  val m_axi_aresetn = IO(Input(Bool()))

  val s_axi         = IO(Flipped(new AxiMasterIF(params)))
  val s_axi_aclk    = IO(Input(Clock()))
  val s_axi_aresetn = IO(Input(Bool()))

  val impl = Module(new Axi4ClockConverter.axi4_clock_converter(params))

  impl.io.m_axi_aclk    := m_axi_aclk
  impl.io.m_axi_aresetn := m_axi_aresetn
  impl.io.m_axi.awaddr <> m_axi.AWADDR
  impl.io.m_axi.awlen <> m_axi.AWLEN
  impl.io.m_axi.awsize <> m_axi.AWSIZE
  impl.io.m_axi.awburst <> m_axi.AWBURST

  impl.io.m_axi.awvalid <> m_axi.AWVALID
  impl.io.m_axi.awready <> m_axi.AWREADY
  impl.io.m_axi.wdata <> m_axi.WDATA
  impl.io.m_axi.wstrb <> m_axi.WSTRB
  impl.io.m_axi.wlast <> m_axi.WLAST
  impl.io.m_axi.wvalid <> m_axi.WVALID
  impl.io.m_axi.wready <> m_axi.WREADY
  impl.io.m_axi.bresp <> m_axi.BRESP
  impl.io.m_axi.bvalid <> m_axi.BVALID
  impl.io.m_axi.bready <> m_axi.BREADY
  impl.io.m_axi.araddr <> m_axi.ARADDR
  impl.io.m_axi.arlen <> m_axi.ARLEN
  impl.io.m_axi.arsize <> m_axi.ARSIZE
  impl.io.m_axi.arburst <> m_axi.ARBURST

  impl.io.m_axi.arvalid <> m_axi.ARVALID
  impl.io.m_axi.arready <> m_axi.ARREADY
  impl.io.m_axi.rdata <> m_axi.RDATA
  impl.io.m_axi.rresp <> m_axi.RRESP
  impl.io.m_axi.rlast <> m_axi.RLAST
  impl.io.m_axi.rvalid <> m_axi.RVALID
  impl.io.m_axi.rready <> m_axi.RREADY

  impl.io.s_axi_aclk    := s_axi_aclk
  impl.io.s_axi_aresetn := s_axi_aresetn
  impl.io.s_axi.awaddr <> s_axi.AWADDR
  impl.io.s_axi.awlen <> s_axi.AWLEN
  impl.io.s_axi.awsize <> s_axi.AWSIZE
  impl.io.s_axi.awburst <> s_axi.AWBURST
  // impl.io.s_axi.awlock unused
  // impl.io.s_axi.awcache unused
  // impl.io.awprot unused
  // impl.io.s_axi.awregion unused
  // impl.io.s_axi.awqos unused
  impl.io.s_axi.awvalid <> s_axi.AWVALID
  impl.io.s_axi.awready <> s_axi.AWREADY
  impl.io.s_axi.wdata <> s_axi.WDATA
  impl.io.s_axi.wstrb <> s_axi.WSTRB
  impl.io.s_axi.wlast <> s_axi.WLAST
  impl.io.s_axi.wvalid <> s_axi.WVALID
  impl.io.s_axi.wready <> s_axi.WREADY
  impl.io.s_axi.bresp <> s_axi.BRESP
  impl.io.s_axi.bvalid <> s_axi.BVALID
  impl.io.s_axi.bready <> s_axi.BREADY
  impl.io.s_axi.araddr <> s_axi.ARADDR
  impl.io.s_axi.arlen <> s_axi.ARLEN
  impl.io.s_axi.arsize <> s_axi.ARSIZE
  impl.io.s_axi.arburst <> s_axi.ARBURST
  // impl.io.s_axi.arlock unused
  // impl.io.s_axi.arcache unused
  // impl.io.s_axi.arprot unused
  // impl.io.s_axi.arregion unused
  // impl.io.s_axi.arqos unused
  impl.io.s_axi.arvalid <> s_axi.ARVALID
  impl.io.s_axi.arready <> s_axi.ARREADY
  impl.io.s_axi.rdata <> s_axi.RDATA
  impl.io.s_axi.rresp <> s_axi.RRESP
  impl.io.s_axi.rlast <> s_axi.RLAST
  impl.io.s_axi.rvalid <> s_axi.RVALID
  impl.io.s_axi.rready <> s_axi.RREADY

}
