package manticore.xrt

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum
import chisel3.stage.ChiselStage
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

  val ap_return = Output(UInt(16.W))

  val memory_pointer = Input(UInt(64.W))
  val addr           = Input(UInt(64.W))

  val wen   = Input(Bool())
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

class MemoryGatewaySim extends Module {
  class MemoryGateWaySimInterface extends Bundle {
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
  class MemoryGatewaySimImpl extends BlackBox() with HasBlackBoxResource {
    val io = IO(new MemoryGateWaySimInterface {
      val clock = Input(Clock())
      val reset = Input(Reset())
    })
    addResource("/verilog/MemoryGatewaySimImpl.sv")
  }
  val io = IO(new MemoryGateWaySimInterface())
  val impl = Module(new MemoryGatewaySimImpl())


  impl.io.ap_start := io.ap_start
  io.ap_done := impl.io.ap_done
  io.ap_idle := impl.io.ap_idle
  io.ap_ready := impl.io.ap_ready
  io.ap_return := impl.io.ap_return
  impl.io.memory_pointer := io.memory_pointer
  impl.io.addr           := io.addr
  impl.io.wen   := io.wen
  impl.io.wdata := io.wdata

  impl.io.clock := clock
  impl.io.reset := reset


}

object MemoryGatewaySimpleHlsGen extends App {

//  new ChiselStage().emitVerilog(new MemoryGatewaySimpleHls(), Array("--target-dir", "gen-dir"))
  new ChiselStage().emitVerilog(new MemoryGatewaySim(), Array("--target-dir", "gen-dir"))
}
