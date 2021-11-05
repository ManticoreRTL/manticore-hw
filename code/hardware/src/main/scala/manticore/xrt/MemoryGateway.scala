package manticore.xrt

import chisel3._
import chisel3.util.HasBlackBoxResource
import chisel3.util.HasBlackBoxPath

class MemoryGatewayInterface(
    AxiIdWidth: Int = 1,
    AxiAddrWidth: Int = 64,
    AxiGMemDataWidth: Int = 256,
    AxiAwUserWidth: Int = 1,
    AxiArUserWidth: Int = 1,
    AxiWUserWidth: Int = 1,
    AxiRUserWidth: Int = 1,
    AxiBUserWIdth: Int = 1,
    AxiUserValue: Int = 0,
    AxiProtValue: Int = 0,
    AxiCacheValue: Int = 3,
    AxiDataWidth: Int = 32,
    AxiGMemWStrbWidth: Int = 256 / 8,
    AxiWStrbWidth: Int = 32 / 8
) extends Bundle {

  // val ap_clk             = Input(Clock())
  val m_axi_gmem = new AxiMasterInterface(
    AxiIdWidth,
    AxiAddrWidth,
    AxiGMemDataWidth,
    AxiAwUserWidth,
    AxiArUserWidth,
    AxiWUserWidth,
    AxiRUserWidth,
    AxiBUserWIdth,
    AxiUserValue,
    AxiProtValue,
    AxiCacheValue,
    AxiDataWidth,
    AxiGMemWStrbWidth,
    AxiWStrbWidth
  )

  val ap_start = Input(Bool())

  val ap_done  = Output(Bool())
  val ap_idle  = Output(Bool())
  val ap_ready = Output(Bool())

  val ap_return = Output(UInt(256.W))

  val memory_pointer = Input(UInt(64.W))
  val addr           = Input(UInt(64.W))

  val wen   = Input(UInt(8.W))
  val wdata = Input(UInt(16.W))

}

class MemoryGateway(GMemDataWidth: Int = 256) extends Module {
  val io = IO(
    new MemoryGatewayInterface(
      AxiGMemDataWidth = GMemDataWidth,
      AxiGMemWStrbWidth = GMemDataWidth / 8
    )
  )
}

/** Implementation models for DDR memory access gateway relying on generated
  * m_axi modules from Vitis HLS (see AxiMasterGenerator)
  */

// class MemoryGatewayHls(cached_path: Seq[String] = Seq()) extends MemoryGateway {

//   // generate the black box implementation
//   val path =
//     if (cached_path.isEmpty) {
//       println("Generating axi master because there is no cached path")
//       AxiMasterGenerator("memory_gateway").map(_.toAbsolutePath().toString())
//     } else {
//       require(cached_path.size == 2)
//       println("Using cached path " + cached_path)
//       cached_path
//     }
//   class memory_gateway() extends BlackBox with HasBlackBoxPath {
//     val io = IO(new MemoryGatewayInterface {
//       val ap_clk   = Input(Clock())
//       val ap_rst_n = Input(Bool())
//     })
//     path.foreach(addPath(_))
//   }

//   val impl = Module(new memory_gateway)

//   impl.io.ap_clk := clock

//   impl.io.ap_rst_n := !(reset.asBool())
//   impl.io.ap_start := io.ap_start

//   impl.io.memory_pointer := io.memory_pointer
//   impl.io.raddr          := io.raddr
//   impl.io.waddr          := io.waddr
//   impl.io.cmd            := io.cmd
//   impl.io.wline          := io.wline

//   io.ap_done  := impl.io.ap_done
//   io.ap_idle  := impl.io.ap_idle
//   io.ap_ready := impl.io.ap_ready

//   io.ap_return := impl.io.ap_return

//   io.m_axi_gmem <> impl.io.m_axi_gmem

//   def makeCopy() = new MemoryGatewayHls(path)
// }

// /** Simulation model for DDR memories
//   */

// class MemoryGatewarySim extends MemoryGateway {

//   class memory_gateway() extends BlackBox with HasBlackBoxResource {

//     val io = IO(new Bundle {
//       val clock = Input(Clock())
//       val start = Input(Bool())

//       val done  = Output(Bool())
//       val idle  = Output(Bool())
//       val ready = Output(Bool())

//       val memory_pointer = Input(UInt(64.W))
//       val raddr          = Input(UInt(64.W))
//       val waddr          = Input(UInt(64.W))
//       val cmd            = Input(UInt(8.W))
//       val wline          = Input(UInt(256.W))
//       val rline          = Output(UInt(256.W))
//     })
//     addResource("/verilog/memory_gateway.sv")
//   }

//   val impl = Module(new memory_gateway)

//   impl.io.clock          := clock
//   impl.io.start          := io.ap_start
//   impl.io.memory_pointer := io.memory_pointer
//   impl.io.raddr          := io.raddr
//   impl.io.waddr          := io.waddr
//   impl.io.cmd            := io.cmd
//   impl.io.wline          := io.wline
//   io.ap_return           := impl.io.rline
//   io.ap_done             := impl.io.done
//   io.ap_idle             := impl.io.idle
//   io.ap_ready            := impl.io.ready
//   io.m_axi_gmem.AWVALID  := 0.U
//   io.m_axi_gmem.AWADDR   := 0.U
//   io.m_axi_gmem.AWID     := 0.U
//   io.m_axi_gmem.AWLEN    := 0.U
//   io.m_axi_gmem.AWSIZE   := 0.U
//   io.m_axi_gmem.AWBURST  := 0.U
//   io.m_axi_gmem.AWLOCK   := 0.U
//   io.m_axi_gmem.AWCACHE  := 0.U
//   io.m_axi_gmem.AWPROT   := 0.U
//   io.m_axi_gmem.AWQOS    := 0.U
//   io.m_axi_gmem.AWREGION := 0.U
//   io.m_axi_gmem.AWUSER   := 0.U
//   io.m_axi_gmem.WVALID   := 0.U
//   io.m_axi_gmem.WDATA    := 0.U
//   io.m_axi_gmem.WSTRB    := 0.U
//   io.m_axi_gmem.WLAST    := 0.U
//   io.m_axi_gmem.WID      := 0.U
//   io.m_axi_gmem.WUSER    := 0.U
//   io.m_axi_gmem.ARVALID  := 0.U
//   io.m_axi_gmem.ARADDR   := 0.U
//   io.m_axi_gmem.ARID     := 0.U
//   io.m_axi_gmem.ARLEN    := 0.U
//   io.m_axi_gmem.ARSIZE   := 0.U
//   io.m_axi_gmem.ARBURST  := 0.U
//   io.m_axi_gmem.ARLOCK   := 0.U
//   io.m_axi_gmem.ARCACHE  := 0.U
//   io.m_axi_gmem.ARPROT   := 0.U
//   io.m_axi_gmem.ARQOS    := 0.U
//   io.m_axi_gmem.ARREGION := 0.U
//   io.m_axi_gmem.ARUSER   := 0.U
//   io.m_axi_gmem.RREADY   := 0.U
//   io.m_axi_gmem.BREADY   := 0.U

// }

class MemoryGatewaySimpleHls(cached_path: Seq[String] = Seq())
    extends MemoryGateway(GMemDataWidth = 32) {

  require(cached_path.length == 2, "Need an implementation path")
  class memory_gateway() extends BlackBox with HasBlackBoxPath {
    val io = IO(
      new MemoryGatewayInterface(
        AxiGMemDataWidth = 32,
        AxiGMemWStrbWidth = 32 / 8
      ) {

        val ap_clk   = Input(Clock())
        val ap_rst_n = Input(Bool())

      }
    )
    cached_path.foreach(addPath(_))
  }

  val impl = Module(new memory_gateway)

  impl.io.ap_clk := clock

  impl.io.ap_rst_n := !(reset.asBool())

  def pipeConnect[T <: Data](target: T, source: T) = {
    val preg = Reg(source.cloneType)
    preg   := source
    target := preg
  }

  pipeConnect(impl.io.ap_start, io.ap_start)
  pipeConnect(impl.io.memory_pointer, io.memory_pointer)
  pipeConnect(impl.io.addr, io.addr)
  pipeConnect(impl.io.wen, io.wen)
  pipeConnect(impl.io.wdata, io.wdata)
  pipeConnect(io.ap_done, impl.io.ap_done)
  pipeConnect(io.ap_idle, impl.io.ap_idle)
  pipeConnect(io.ap_ready, impl.io.ap_ready)
  pipeConnect(io.ap_return, impl.io.ap_return)

  io.m_axi_gmem <> impl.io.m_axi_gmem

  def makeCopy() = new MemoryGatewaySimpleHls(cached_path)
}
