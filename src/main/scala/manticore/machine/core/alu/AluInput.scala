package manticore.machine.core.alu

import Chisel._

class ALUInput(DATA_BITS: Int) extends Bundle {
  val rs1 = UInt(DATA_BITS.W)
  val rs2 = UInt(DATA_BITS.W)
  val rs3 = UInt((DATA_BITS + 1).W)
  val rs4 = UInt(DATA_BITS.W)
}