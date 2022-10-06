package manticore.machine

import chisel3._
import chisel3.util._


class PerfCounter(bits: Int = 64, levels: Int = 1) {

  require(bits > 1)

  def maxValue: BigInt = (BigInt(1) << (bits * levels)) - 1
  // val io = IO(new PerfCounterIO(bits, levels))

  private val parts = Seq.fill(levels) { RegInit(0.U(bits.W)) }

  private val stable_count = parts.tail
    .foldLeft(RegNext(parts.head)) { case (prev, v) =>
      RegNext(Cat(v, prev))
    }
    .suggestName("stable_count")

  // io.value := stable_count

  def value = stable_count

  private val cin = WireDefault(false.B).suggestName("count_en")
  // cin := io.en
  private val wrap = parts.foldLeft(RegNext(cin.asUInt)) { case (ci, v) =>

    val pPadded = WireDefault(UInt((bits + 1).W), v)
    val sum = pPadded + ci
    v   := sum.tail(1)
    sum.head(1)
  }

  def inc(): Unit = {
    cin := true.B
    wrap.asBool
  }

  def clear(): Unit = {
    parts.foreach { _ := 0.U }
  }

}

object PerfCounter {

  /**
      * Create a pipelined performance counter consists of multiple levels with
      * each level only performing "bits" wide counting. The total bit length of
      * the counter it bits * levels. i.e., PerfCounter(32, 2) defines a 2 level deep
      * 64-bit counter
      *
      * @param bits the number of bits at each level
      * @param levels the number of pipeline levels
      * @return
      */
  def apply(bits: Int = 64, levels: Int = 1): PerfCounter = {
    new PerfCounter(bits, levels)
  }

}
