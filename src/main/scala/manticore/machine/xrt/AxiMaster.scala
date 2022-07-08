package manticore.machine.xrt

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._

trait AxiParameters {
  val IdWidth: Int       = 1
  val AddrWidth: Int     = 64
  val DataWidth: Int     = 32 // can not be less than 32 bits
  val UserDataWidth: Int = 16
  val UserAddrWidth: Int = 64
}

object DefaultAxiParameters extends AxiParameters

class AxiMasterIF(params: AxiParameters = DefaultAxiParameters) extends Bundle {

  // val ARID    = Output(UInt(width = params.IdWidth.W))
  val ARADDR  = Output(UInt(width = params.AddrWidth.W))
  val ARVALID = Output(Bool())
  val ARREADY = Input(Bool())
  val ARLEN   = Output(UInt(8.W))
  val ARSIZE  = Output(UInt(3.W))
  val ARBURST = Output(UInt(8.W))

  val RDATA  = Input(UInt(width = params.DataWidth.W))
  val RVALID = Input(Bool())
  val RREADY = Output(Bool())
  // val RID    = Input(UInt(width = params.IdWidth.W))
  val RLAST  = Input(Bool())
  val RRESP  = Input(UInt(2.W))

  val AWADDR  = Output(UInt(params.AddrWidth.W))
  val AWLEN   = Output(UInt(8.W))
  val AWBURST = Output(UInt(8.W))
  val AWSIZE  = Output(UInt(3.W))

  val AWVALID = Output(Bool())
  val AWREADY = Input(Bool())
  val WDATA   = Output(UInt(params.DataWidth.W))
  val WSTRB   = Output(UInt((params.DataWidth / 8).W))
  val WLAST   = Output(Bool())
  val WVALID  = Output(Bool())
  val WREADY  = Input(Bool())
  val BRESP   = Input(UInt(2.W))
  val BVALID  = Input(Bool())
  val BREADY  = Output(Bool())
//   // unused features
//   val ARBURST = Output(UInt(2.W))
//   val ARCACHE = Output(UInt(4.W))
//   val
}

class AxiMasterUserIF(params: AxiParameters = DefaultAxiParameters) extends Bundle {
  // user interfaces
  val base_addr = Input(
    UInt(width = params.AddrWidth.W)
  ) // base byte-aligned address
  val read_start = Input(Bool())
  val read_done  = Output(Bool())
  val read_idle  = Output(Bool())
  val rdata      = Output(UInt(width = params.UserDataWidth.W))
  val raddr      = Input(UInt(width = params.UserAddrWidth.W)) // base

}

class AxiMasterReader(params: AxiParameters = DefaultAxiParameters) extends Module {

  val io = IO(new Bundle {
    val user = new AxiMasterUserIF(params)
    val bus  = new AxiMasterIF(params)
  })

  io.bus.ARLEN  := 0.U // burst length is set to 1
  io.bus.ARSIZE := log2Ceil(params.DataWidth / 8).U

  val rdata_r = Reg(io.user.rdata.cloneType)

  object ReadState extends ChiselEnum {
    val Idle, AssertAddress, WaitResponse = Value
  }

  val rstate      = RegInit(ReadState.Type(), ReadState.Idle)
  val user_raddr  = Reg(io.user.raddr.cloneType)
  val lane_select = Reg(Bool())

  io.bus.ARVALID := false.B
  // io.bus.ARID    := 0.U
  io.bus.ARADDR := DontCare
  io.bus.RREADY := true.B
  // disable writing
  io.bus.WDATA   := DontCare
  io.bus.AWBURST := 1.U
  io.bus.ARBURST := 1.U
  io.bus.WSTRB   := DontCare
  io.bus.AWADDR  := DontCare
  io.bus.AWLEN   := DontCare
  io.bus.AWVALID := false.B
  io.bus.WLAST   := false.B
  io.bus.BREADY  := true.B
  io.bus.WVALID  := false.B
  io.bus.AWSIZE  := DontCare

  io.user.read_done := false.B
  io.user.rdata := Mux(
    lane_select,
    io.bus.RDATA >> params.UserDataWidth,
    io.bus.RDATA
  )
  io.user.read_idle := false.B

  switch(rstate) {
    is(ReadState.Idle) {
      io.user.read_idle := true.B
      when(io.user.read_start) {
        rstate := ReadState.AssertAddress
        // save the byte-aligned address
        user_raddr := ((io.user.raddr >> 1) + (io.user.base_addr >> 2)) << 2
        lane_select := io.user.raddr(
          0
        ) // used later to select 16-bit word out of the 32-bit word
      }
    }
    is(ReadState.AssertAddress) {
      io.bus.ARVALID := true.B
      io.bus.ARADDR  := user_raddr
      when(io.bus.ARREADY) {
        rstate := ReadState.WaitResponse
      }
    }
    is(ReadState.WaitResponse) {
      io.user.read_done := io.bus.RVALID
      when(io.bus.RVALID) {
        rstate := ReadState.Idle
      }
    }
  }

}

class AxiMaster(axiParams: AxiParameters, baseAddr: Long, addrWidth: Int, dataWidth: Int) extends Module {


  val io = IO(new Bundle {
    val axi = new AxiMasterIF(axiParams)
    val write_txn_start = Input(Bool())
    val read_txn_start = Input(Bool())
    val write_addr = Input(UInt(addrWidth.W))
    val read_addr = Input(UInt(addrWidth.W))
    val data_in = Input(UInt(dataWidth.W))
    val data_out = Output(UInt(dataWidth.W))
    val burst_size = Input(UInt(8.W))
    val read_txn_done = Output(Bool())
    val write_txn_done = Output(Bool())
  })


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
      val read_addr       = Input(UInt(C_M_AXI_ADDR_WIDTH.W))
      val write_addr      = Input(UInt(C_M_AXI_ADDR_WIDTH.W))
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


  val impl = Module(
    new MasterAXIFromFile(
      C_M_TARGET_SLAVE_BASE_ADDR = baseAddr,
      C_M_AXI_ADDR_WIDTH = 64,
      C_M_AXI_DATA_WIDTH = axiParams.DataWidth
    )
  )



  impl.io.M_AXI_ARESETN := !reset.asBool
  impl.io.M_AXI_ACLK    := clock
  
  impl.io.write_txn_start := io.write_txn_start
  impl.io.read_txn_start := io.read_txn_start
  impl.io.write_addr := io.write_addr
  impl.io.read_addr := io.read_addr
  impl.io.burst_size := io.burst_size
  impl.io.data_in := io.data_in

  impl.io.M_AXI_AWREADY := io.axi.AWREADY
  impl.io.M_AXI_BRESP   := io.axi.BRESP
  impl.io.M_AXI_RRESP   := io.axi.RRESP
  impl.io.M_AXI_BVALID  := io.axi.BVALID
  impl.io.M_AXI_WREADY  := io.axi.WREADY
  impl.io.M_AXI_RLAST   := io.axi.RLAST
  impl.io.M_AXI_ARREADY := io.axi.ARREADY
  impl.io.M_AXI_RRESP   := 0.U
  impl.io.M_AXI_RDATA   := io.axi.RDATA

  io.axi.AWADDR  := impl.io.M_AXI_AWADDR
  io.axi.AWLEN   := impl.io.M_AXI_AWLEN
  io.axi.AWSIZE  := impl.io.M_AXI_AWSIZE
  io.axi.AWBURST := impl.io.M_AXI_AWBURST
  io.axi.AWVALID := impl.io.M_AXI_AWVALID
  io.axi.WDATA   := impl.io.M_AXI_WDATA
  io.axi.WSTRB   := impl.io.M_AXI_WSTRB
  io.axi.WLAST   := impl.io.M_AXI_WLAST
  io.axi.WVALID  := impl.io.M_AXI_WVALID
  io.axi.BREADY  := impl.io.M_AXI_BREADY
  io.axi.ARADDR  := impl.io.M_AXI_ARADDR
  io.axi.ARLEN   := impl.io.M_AXI_ARLEN
  io.axi.ARSIZE  := impl.io.M_AXI_ARSIZE
  io.axi.ARVALID := impl.io.M_AXI_ARVALID
  io.axi.ARBURST := 1.U

  impl.io.M_AXI_RLAST  := io.axi.RLAST
  impl.io.M_AXI_RVALID := io.axi.RVALID
  io.axi.RREADY    := impl.io.M_AXI_RREADY  
  io.read_txn_done := impl.io.read_txn_done
  io.write_txn_done := impl.io.write_txn_done
  io.data_out := impl.io.data_out

}
