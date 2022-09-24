package manticore.machine

import chisel3._

object Helpers {

  // (skashani): The name "regManticoreNoSrl" is matched in code that
  // edits the generated verilog to disable SRL generation.
  // If you modify this name here, do not forget to modify it in the other
  // parts of the code!
  val pipeNoSlrPrefix = "regManticoreNoSrl"

  // (skashani): The name "manticoreSlrCrossing" is matched code that
  // generates USER_SLL_REG constraints.
  // If you modify the name here, do not forget to modify it in the other
  // parts of the code!
  val slrCrossingSuffix = "manticoreSlrCrossing"

  def PipeNoSRL[T <: Data](
      data: T,
      latency: Int,
      regIdxSuffix: Map[Int, String] = Map.empty
  ): T = {
    require(latency >= 0, "Pipe latency must be >= 0!")

    val prefix = pipeNoSlrPrefix

    // I use a foldLeft here as I want to name individual wires. If I use a Vec,
    // then calling `suggestName` on individual elements of the Vec does not
    // assign the name I want to the individual registers.
    val pipe = Range.inclusive(1, latency).foldLeft(WireInit(data)) { case (prevWire, idx) =>
      val nextWireName = regIdxSuffix.get(idx) match {
        case None         => s"${prefix}_${idx}"
        case Some(suffix) => s"${prefix}_${idx}_${suffix}"
      }

      val nextWire = Wire(chiselTypeOf(data))
      nextWire.suggestName(nextWireName)
      nextWire := RegNext(prevWire)
      nextWire
    }

    pipe
  }

  // This function creates a pipeline register with the given latency and a
  // specific suffix for certain of the registers in the chain.
  // This code exists as we want a unique name for SLR-crossing wires so that
  // we can emit USER_SLL_REG properties when we create pblock constraints.
  def SlrCrossing[T <: Data](
      data: T,
      latency: Int,
      slrCrossingIndices: Set[Int]
  ): T = {
    require(latency >= 2, "SLR crossing needs >= 2 registers!")

    val suffix    = slrCrossingSuffix
    val suffixMap = slrCrossingIndices.map(idx => idx -> suffix).toMap

    val pipe = PipeNoSRL(data, latency, suffixMap)
    pipe
  }
}
