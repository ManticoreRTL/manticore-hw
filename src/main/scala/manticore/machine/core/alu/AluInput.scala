package manticore.machine.core.alu

import Chisel._

class ALUInput(DATA_BITS: Int) extends Bundle {
  val x = UInt(DATA_BITS.W)
  val y = UInt(DATA_BITS.W)
  val u = UInt(DATA_BITS.W)
  val v = UInt(DATA_BITS.W)
}