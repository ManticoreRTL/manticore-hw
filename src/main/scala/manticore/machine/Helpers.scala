package manticore.machine

import chisel3._

object Helpers {
  def PipeNoSRL[T <: Data](data: T, latency: Int): T = {
    require(latency >= 0, "Pipe latency must be greater than or equal to zero!")

    if (latency == 0) {
      val notDelayed = Wire(chiselTypeOf(data))
      notDelayed := data
      notDelayed

    } else {
      // (skashani): The name "regManticorePipeNoSrl" is matched in a placement
      // script to disable the generation of SRLs. If you modify this name here,
      // do not forget to modify it in the placement script!
      val regManticorePipeNoSrl = Wire(Vec(latency + 1, chiselTypeOf(data)))

      regManticorePipeNoSrl(0) := data
      Range.inclusive(1, latency).foreach { idx =>
        val prev = regManticorePipeNoSrl(idx - 1)
        regManticorePipeNoSrl(idx) := RegNext(prev)
      }

      regManticorePipeNoSrl.last
    }
  }
}
