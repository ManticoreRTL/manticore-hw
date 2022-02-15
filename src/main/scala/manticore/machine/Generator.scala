package manticore.machine

import chisel3.stage.ChiselStage
import manticore.machine.xrt.ManticoreFlatSimKernel

object Generator {

  def generateSimulationKernel(
      output_dir: String,
      dimx: Int,
      dimy: Int,
      debug_enable: Boolean
  ): Unit = {

    new ChiselStage().emitVerilog(
      new ManticoreFlatSimKernel(
        DimX = dimx,
        DimY = dimy,
        debug_enable = debug_enable,
        reset_latency = 16
      ),
      Array("--target-dir", output_dir)
    )
  }

}
