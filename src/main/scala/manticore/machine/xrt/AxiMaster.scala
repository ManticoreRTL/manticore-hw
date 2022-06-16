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

  val ARID    = Output(UInt(width = params.IdWidth.W))
  val ARADDR  = Output(UInt(width = params.AddrWidth.W))
  val ARVALID = Output(Bool())
  val ARREADY = Input(Bool())
  val ARLEN   = Output(UInt(8.W))
  val ARSIZE  = Output(UInt(3.W))

  val RDATA  = Input(UInt(width = params.DataWidth.W))
  val RVALID = Input(Bool())
  val RREADY = Output(Bool())
  val RID    = Input(UInt(width = params.IdWidth.W))
  val RLAST   = Input(Bool())
  val RRESP = Input(UInt(2.W))


  val AWADDR  = Output(UInt(params.AddrWidth.W))
  val AWLEN   = Output(UInt(8.W))
  val AWSIZE  = Output(UInt(3.W))
  val BURST   = Output(UInt(2.W))
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

class AxiMasterUserIF(params: AxiParameters = DefaultAxiParameters)
    extends Bundle {
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

class AxiMasterReader(params: AxiParameters = DefaultAxiParameters)
    extends Module {

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

  io.bus.ARVALID    := false.B
  io.bus.ARID       := 0.U
  io.bus.ARADDR     := DontCare
  io.bus.RREADY     := true.B
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
