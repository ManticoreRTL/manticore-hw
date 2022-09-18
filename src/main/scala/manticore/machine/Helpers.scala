package manticore.machine

import chisel3._

object Helpers {
  def regPipe[T <: Data](data: T, latency: Int): T = {
    require(latency >= 0, "Pipe latency must be greater than or equal to zero!")

    if (latency == 0) {
      val notDelayed = Wire(chiselTypeOf(data))
      notDelayed := data
      notDelayed

    } else if (latency == 1) {
      val delayed = RegNext(data)
      delayed

    } else {
      val delayed = Wire(Vec(latency + 1, chiselTypeOf(data)))

      delayed(0) := data
      Range.inclusive(1, latency).foreach { idx =>
        val prev = delayed(idx - 1)
        delayed(idx) := RegNext(prev)
      }

      delayed.last
    }
  }
}
