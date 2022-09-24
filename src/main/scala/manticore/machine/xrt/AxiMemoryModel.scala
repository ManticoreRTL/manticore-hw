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
      val S_AXI_AWADDR  = Input(UInt(64.W))
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
      val S_AXI_ARADDR  = Input(UInt(64.W))
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
