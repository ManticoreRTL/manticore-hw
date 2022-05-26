package manticore.machine.core

import Chisel._
import manticore.machine.ISA


class BareNoCInterface(DimX: Int, DimY: Int, config: ISA) extends Bundle {
  def makePacketArray(): Vec[Vec[NoCBundle]] = Vec(DimX, Vec(DimY, new NoCBundle(DimX, DimY, config)))

  val corePacketInput: Vec[Vec[NoCBundle]] = Input(makePacketArray())
  val corePacketOutput: Vec[Vec[NoCBundle]] = Output(makePacketArray())
  val configPacket: NoCBundle = Input(new NoCBundle(DimX, DimY, config))
  val configEnable: Bool = Input(Bool())
}

/**
 * Bare-bone NoC that only contains an 2D torus of switches
 * @param DimX
 * @param DimY
 * @param config
 */
class BareNoC(DimX: Int, DimY: Int, config: ISA) extends Module {

  val io = IO(new BareNoCInterface(DimX, DimY, config))

  val switch_array: Seq[Seq[Switch]] = Seq.fill(DimX) {
    Seq.fill(DimY) {
      Module(new Switch(DimX, DimY, config))
    }
  }

  // connect the row ports in the switches


  switch_array.transpose.foreach { row =>
    row.head.io.xInput := row.last.io.xOutput
    row.sliding(2, 1).foreach { case Seq(left: Switch, right: Switch) =>
      right.io.xInput := left.io.xOutput
    }
  }

  when(io.configEnable) {
    switch_array.head.head.io.xInput := io.configPacket
  }

  // connect column ports of the switches
  switch_array.foreach { col =>
    col.head.io.yInput := col.last.io.yOutput
    col.sliding(2, 1).foreach { case Seq(top: Switch, bot: Switch) =>
      bot.io.yInput := top.io.yOutput
    }
  }


  // connect ios
  switch_array.flatten.
    zip(io.corePacketInput.flatten.zip(io.corePacketOutput.flatten))
    .foreach { case (_switch, (_in, _out)) =>
      _switch.io.lInput := _in
      _out := _switch.io.yOutput
      _out.valid := _switch.io.terminal
    }

}
