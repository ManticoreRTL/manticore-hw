package manticore.machine.xrt
import chisel3._
import chisel3.util._

class AxiMemoryModelSimInterface(userDataWidth: Int) extends Bundle {
  val wen   = Input(Bool())
  val wdata = Input(UInt(userDataWidth.W))
  val rdata = Output(UInt(userDataWidth.W))
  val raddr = Input(UInt(64.W))
  val waddr = Input(UInt(64.W))
  val lock  = Input(Bool())

}
class AxiMemoryModel(axiParams: AxiParameters, memorySize: Int, dataWidth: Int) extends Module {


  val io = IO(new Bundle {
    val axi = Flipped(new AxiMasterIF(axiParams))
    val sim = new AxiMemoryModelSimInterface(dataWidth)
  })

  val impl = Module(
    new axislave_vip(
      C_S_AXI_ADDR_WIDTH = 64,
      C_S_AXI_DATA_WIDTH = axiParams.DataWidth,
      CACHE_DATA_WIDTH = dataWidth,
      MEM_SIZE = memorySize
    )
  )

  impl.io.mem_wen     := io.sim.wen
  impl.io.mem_data_in := io.sim.wdata
  io.sim.rdata        := impl.io.mem_data_out
  impl.io.mem_raddr   := io.sim.raddr
  impl.io.mem_waddr   := io.sim.waddr
  impl.io.lock        := io.sim.lock

  impl.io.S_AXI_ARESETN := !reset.asBool
  impl.io.S_AXI_ACLK    := clock
  impl.io.S_AXI_AWADDR  := io.axi.AWADDR
  impl.io.S_AXI_AWLEN   := io.axi.AWLEN
  impl.io.S_AXI_AWSIZE  := io.axi.AWSIZE
  impl.io.S_AXI_AWBURST := io.axi.AWBURST
  impl.io.S_AXI_AWVALID := io.axi.AWVALID
  io.axi.AWREADY        := impl.io.S_AXI_AWREADY

  impl.io.S_AXI_WDATA  := io.axi.WDATA
  impl.io.S_AXI_WSTRB  := io.axi.WSTRB
  impl.io.S_AXI_WLAST  := io.axi.WLAST
  impl.io.S_AXI_WVALID := io.axi.WVALID
  io.axi.WREADY        := impl.io.S_AXI_WREADY
  io.axi.BRESP         := impl.io.S_AXI_BRESP

  io.axi.BVALID         := impl.io.S_AXI_BVALID
  impl.io.S_AXI_BREADY  := io.axi.BREADY
  impl.io.S_AXI_ARADDR  := io.axi.ARADDR
  impl.io.S_AXI_ARLEN   := io.axi.ARLEN
  impl.io.S_AXI_ARSIZE  := io.axi.ARSIZE
  impl.io.S_AXI_ARVALID := io.axi.ARVALID
  io.axi.ARREADY        := impl.io.S_AXI_ARREADY
  io.axi.RDATA          := impl.io.S_AXI_RDATA
  io.axi.RLAST          := impl.io.S_AXI_RLAST
  io.axi.RVALID         := impl.io.S_AXI_RVALID
  impl.io.S_AXI_RREADY  := io.axi.RREADY
  io.axi.RRESP          := impl.io.S_AXI_RRESP

}
