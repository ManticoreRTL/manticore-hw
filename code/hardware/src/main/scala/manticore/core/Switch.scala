/**
 * Copyright 2021 Mahyar Emami
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */
package manticore.core

import Chisel._
import firrtl.ir.Width
import manticore.ISA


class BareNoCBundle(val config: ISA) extends Bundle {
  val data: UInt = UInt(config.DataBits.W)
  val address: UInt = UInt(config.IdBits.W)
  val valid: Bool = Bool()

}

/**
 * A data and control bundle that traverses NoC hops
 *
 * @param DimX   number of switches in the X direction
 * @param DimY   number of switches in the Y direction
 * @param config the configuration of the processors
 */
class NoCBundle(val DimX: Int, val DimY: Int, override val config: ISA) extends
  BareNoCBundle(config) {
  val xHops: UInt = UInt(log2Ceil(DimX).W)
  val yHops: UInt = UInt(log2Ceil(DimY).W)
}

object NoCBundle {

  def apply(DIMX: Int, DIMY: Int, config: ISA) = new NoCBundle(DIMX, DIMY, config)

  /**
   * Create and empty packet with valid bit set to false
   *
   * @param DIMX
   * @param DIMY
   * @param config
   * @return
   */
  def empty(DIMX: Int, DIMY: Int, config: ISA): NoCBundle = {
    val bundle = Wire(new NoCBundle(DIMX, DIMY, config))
    bundle.data := 0.U
    bundle.address := 0.U
    bundle.valid := false.B
    bundle.xHops := 0.U
    bundle.yHops := 0.U
    bundle
  }

  /**
   * Create a new packet from the original with the xHops decremented
   *
   * @param orig original packet
   * @return
   */
  def passX(orig: NoCBundle): NoCBundle = {
    val passed = Wire(new NoCBundle(orig.DimX, orig.DimY, orig.config))
    passed := orig
    passed.xHops := orig.xHops - 1.U
    passed
  }

  /**
   * Create a new packet from the original with yHops decremented
   *
   * @param orig original packet
   * @return
   */
  def passY(orig: NoCBundle): NoCBundle = {
    val passed = Wire(new NoCBundle(orig.DimX, orig.DimY, orig.config))
    passed := orig
    passed.yHops := orig.yHops - 1.U
    passed
  }

  /**
   * Create a terminal packet, i.e., an invalid packet (valid = false) that retains the `data` and
   * `address` fields
   *
   * @param orig the original packet
   * @return
   */
  def terminal(orig: NoCBundle): NoCBundle = {
    val reached = Wire(empty(orig.DimX, orig.DimY, orig.config))
    reached.address := orig.address
    reached.data := orig.data
    reached
  }

}


/**
 * NoC Switch input and output interface with three input interfaces X, Y, L and two output interfaces X and Y. The
 * is time-multiplexed between routing packets through Y or delivering them to the local PE.
 *
 * @param DimX
 * @param DimY
 * @param config
 */
class SwitchInterface(DimX: Int, DimY: Int, config: ISA) extends Bundle {
  // input from x direction
  val xInput: NoCBundle = Input(NoCBundle(DimX, DimY, config))
  // input from y direction
  val yInput: NoCBundle = Input(NoCBundle(DimX, DimY, config))
  // input from the local PE
  val lInput: NoCBundle = Input(NoCBundle(DimX, DimY, config))

  // output to the PE, defining whether the PE is the destination,
  // note that it should always be the case:
  // 1. that terminal == true -> yOutput.valid == false
  // 2. yOutput.valid == true -> terminal == false
  val terminal: Bool = Output(Bool())

  // output in the x direction
  val xOutput: NoCBundle = Output(NoCBundle(DimX, DimY, config))
  // output in the y direction
  val yOutput: NoCBundle = Output(NoCBundle(DimX, DimY, config))

}

/**
 * A one-dimensional NoC switch that can be used to create a torus network on chip.
 * The switch does not provide any "flow control" mechanisms and is supposed. The routing is a variant of dimension
 * ordered routing in which packets first flow in the X direction and the Y. Packets carry a tuple (x, y) that is
 * decremented in the respective dimensions when a hop is traversed. When `x == 0 && y == 0`, the packet has reached its
 * destination, such a packet is again routed through the Y dimension but is invalidated. The `terminal` signal in the
 * `SwitchInterface` determines that the packet has reached its destination and the PE should consume this packet
 * immediately.
 *
 * There 7 possible routes each packet can take depending with the given priorities:
 * X -> X, X -> Y, X -> L (terminal) > Y -> Y, Y -> L (terminal) > L -> X, L -> Y
 *
 * The router can route at most two packets in a single cycle given the source and destination as long as the
 * destinations are distinct. Excess packets are dropped respecting the priority of paths. For instance,
 * in a single cycle we can have both X -> X, Y -> Y. In this case any packet originating from L is dropped (e.g.,
 * L -> Y). Another example is X->Y, L->X, in this case any packet on Y is dropped.
 *
 * @param DimX
 * @param DimY
 * @param config
 */
class Switch(DimX: Int, DimY: Int, config: ISA) extends Module {
  val io = IO(new SwitchInterface(DimX, DimY, config))

  val empty = Wire(NoCBundle(DimX, DimY, config))

  val x_reg: NoCBundle = Reg(NoCBundle(DimX, DimY, config))
  val y_reg: NoCBundle = Reg(NoCBundle(DimX, DimY, config))
  val terminal_reg: Bool = Reg(Bool())


  // default values of the outputs
  x_reg := empty
  y_reg := empty
  terminal_reg := false.B


  /**
   * Dimension-ordered routing, first route X, then Y, and finally L (local), this translate to writing the code
   * in the "opposite way", i.e., first we try to route L either to the xOutput or yOutput, then we route yInput
   * to the yOutput and finally we route xInput to the xOutput or the yOutput
   */

  when(io.lInput.valid) {
    when(io.lInput.xHops === 0.U) {
      // route the local message to the yOutput, drop if self message
      when(io.lInput.yHops =/= 0.U) {
        y_reg := NoCBundle.passY(io.lInput)
        // this second check can be avoided if we a self message is never sent
      } // otherwise is implicit, because default value is of io.yOutput is set to empty
    } otherwise {
      // route the local message to the xOutput
      x_reg := NoCBundle.passX(io.lInput)
    }
  }

  when(io.yInput.valid) {
    when(io.yInput.yHops === 0.U) {
      // reached the destination, route to the yOutput as a terminal message
      y_reg := NoCBundle.terminal(io.yInput)
      terminal_reg := true.B
    } otherwise {
      // route to the yOutput
      y_reg := NoCBundle.passY(io.yInput)
      terminal_reg := false.B
    }
  }
  when(io.xInput.valid) {
    when(io.xInput.xHops === 0.U) {
      when(io.xInput.yHops === 0.U) {
        // the message from the X port has reached its destination and should be routed to the local PE which receives
        // the message from the yOutput port
        y_reg := NoCBundle.terminal(io.xInput)
        terminal_reg := true.B
        // invalidate the message on xOutput port
      } otherwise {
        // route the message to the yOutput
        y_reg := NoCBundle.passY(io.xInput)
        terminal_reg := false.B
        // invalidate the message on the xOutput
      }
    } otherwise {
      // route the message to the xOutput
      x_reg := NoCBundle.passX(io.xInput)

    }
  }

  io.xOutput := x_reg
  io.yOutput := y_reg
  io.terminal := terminal_reg

}

//
//class NoCPayload(width: Int) extends Bundle {
//  val data: UInt = UInt(width)
//  val valid: UInt = Bool()
//}
//
//class NoCPacket(width: Int, dimx: Int, dimy: Int) extends Bundle {
//  val payload: NoCPayload = new NoCPayload(width)
//  val xHops: UInt = UInt(log2Ceil(dimx).W)
//  val yHops: UInt = UInt(log2Ceil(dimy).W)
//}
//
//
//object NoCPacket {
//
//  def apply(width: Int, dimx: Int, dimy: Int) = new NoCPacket(width, dimx, dimy)
//
//  /**
//   * Create and empty packet with valid bit set to false
//   * @param DIMX
//   * @param DIMY
//   * @param config
//   * @return
//   */
//  def empty(width: Int, dimx: Int, dimy: Int): NoCPacket = {
//    val bundle = Wire(new NoCPacket(width, dimx, dimy))
//    bundle.payload.data  := 0.U
//    bundle.payload.valid := false.B
//    bundle.xHops := 0.U
//    bundle.yHops := 0.U
//    bundle
//  }
//
//  def
//}