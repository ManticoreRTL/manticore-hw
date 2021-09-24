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

package thyrio

import Chisel._
import chisel3.tester.{testableClock, testableData}
import chiseltest.ChiselScalatestTester
import org.scalatest.{FlatSpec, Matchers}
import chiseltest.internal.{VerilatorBackendAnnotation => USE_VERILATOR, WriteVcdAnnotation => DUMP_VCD}
import thyrio.core.{NoCBundle, Switch}
import chisel3.experimental.BundleLiterals._
import chisel3.tester.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder

import scala.util.Random




class SwitchTester extends FlatSpec  with ChiselScalatestTester with Matchers {
  object SwitchPort extends Enumeration {
    type Type = Value
    val X, Y, L = Value
  }
  val DIMX, DIMY = 16
  val config = ThyrioISA
  val NUM_TESTS = 4096
  val rdgen = Random
  def genAddress = Random.nextInt(1 << config.IdBits).U
  def genData = Random.nextInt(1 << config.DataBits).U
  def genValid = Random.nextInt(2).B

  def randomPacketXY: NoCBundle = {
    NoCBundle(DIMX, DIMY, config).Lit(
      _.data -> genData,
      _.address -> genAddress,
      _.valid -> true.B,
      _.xHops -> Random.nextInt(1 << log2Ceil(DIMX)).U,
      _.yHops -> Random.nextInt(1 << log2Ceil(DIMY)).U
    )
  }
  def randomPacketY: NoCBundle = {
    NoCBundle(DIMX, DIMY, config).Lit(
      _.data -> genData,
      _.address -> genAddress,
      _.valid -> true.B,
      _.xHops -> 0.U,
      _.yHops -> (Random.nextInt((1 << log2Ceil(DIMY)) - 1)).U
    )
  }
  def randomPacketX: NoCBundle = {
    NoCBundle(DIMX, DIMY, config).Lit(
      _.data -> genData,
      _.address -> genAddress,
      _.valid -> true.B,
      _.xHops -> (Random.nextInt((1 << log2Ceil(DIMX)) - 1)).U,
      _.yHops -> 0.U
    )
  }
  def emptyPacket = {
    NoCBundle(DIMX, DIMY, config).Lit(
      _.data -> 0.U,
      _.address -> 0.U,
      _.valid -> false.B,
      _.xHops -> 0.U,
      _.yHops -> 0.U
    )
  }

  def passX(orig: NoCBundle): NoCBundle = {
    NoCBundle(DIMX, DIMY, config).Lit(
      _.data -> orig.data,
      _.address -> orig.address,
      _.valid -> true.B,
      _.xHops -> (orig.xHops.litValue() - 1).U,
      _.yHops -> orig.yHops
    )
  }
  def passY(orig: NoCBundle): NoCBundle = {
    NoCBundle(DIMX, DIMY, config).Lit(
      _.data -> orig.data,
      _.address -> orig.address,
      _.valid -> true.B,
      _.xHops -> orig.xHops,
      _.yHops -> (orig.yHops.litValue() - 1).U
    )
  }

  def invalidated(orig: NoCBundle): NoCBundle = {
    require(orig.yHops.litValue() == 0 & orig.xHops.litValue() == 0,
      s"Can not invalidate a non-terminal packet %s".format(orig))
    NoCBundle(DIMX, DIMY, config).Lit(
      _.data -> orig.data,
      _.address -> orig.address,
      _.valid -> false.B,
      _.xHops -> orig.xHops,
      _.yHops -> orig.yHops
    )
  }
  def routeFromLocal(packet: NoCBundle) = {
    if (packet.xHops.litValue() != 0 ) {
      SwitchPort.X
    } else if (packet.yHops.litValue() != 0) {
      SwitchPort.Y
    } else {
      SwitchPort.L
    }
  }
  def routeFromX(packet: NoCBundle) = {
    routeFromLocal(packet)
  }

  behavior of "Switch"
  it should "route L to X and Y based on the xHops and yHops in the packet " +
    "and drop \"self\" packets" in  {
    test(new Switch(config = config, DimX = DIMX, DimY = DIMY)).withAnnotations(Seq(USE_VERILATOR)) { dut =>
      for (i <- Range(0, NUM_TESTS)) {
        // simple test, route through X
        val packet: NoCBundle = randomPacketXY
        // send the packet from the local port
        dut.io.lInput.poke(packet)
        dut.io.yInput.poke(emptyPacket)
        dut.io.xInput.poke(emptyPacket)
        dut.clock.step()

        routeFromLocal(packet) match {
          case SwitchPort.Y =>
            dut.io.yOutput.expect(passY(packet), s"[%d] Expected local packet %s on Y".format(i, passY(packet)))
            dut.io.xOutput.expect(emptyPacket, s"[%d] expected empty packet on port X, got %s"
              .format(i, dut.io.peek))
          case SwitchPort.X =>
            dut.io.xOutput.expect(passX(packet), s"[%d] Expected local packet %s on X".format(i, passX(packet)))
            dut.io.yOutput.expect(emptyPacket, s"[%d] expected empty packet on port y, got %s"
              .format(i, dut.io.yOutput.peek))
          case SwitchPort.L =>
            dut.io.xOutput.expect(emptyPacket, s"[%d] expected empty packet on port X, got %s"
              .format(i, dut.io.xOutput.peek))
            dut.io.yOutput.expect(emptyPacket, s"[%d] expected empty packet on port y, got %s"
              .format(i, dut.io.yOutput.peek))
            dut.io.terminal.expect(false.B)
        }
      }
    }
  }
  it should "drop L if Y is present" in  {
    test(new Switch(DIMX, DIMY, config)).withAnnotations(Seq()) {dut =>
      for (i <- Range(0, NUM_TESTS)) {

        val packetL = randomPacketXY
        val packetY = randomPacketY
        dut.io.lInput.poke(packetL)
        dut.io.xInput.poke(emptyPacket)
        dut.io.yInput.poke(packetY)
        dut.clock.step()

        def checkY(): Unit =
          if (packetY.yHops.litValue() == 0) {
            dut.io.yOutput.expect(invalidated(packetY),
              s"expected packet %s to become %s (terminal)".format(packetY, invalidated(packetY)))
            dut.io.terminal.expect(true.B)
          } else {
            dut.io.yOutput.expect(passY(packetY),
              s"expected %s to pass as %s".format(packetY, passY(packetY)))
            dut.io.terminal.expect(false.B)
          }

        routeFromLocal(packetL) match {
          case SwitchPort.X =>
            // L can send to X simultaneously with Y passing down
            checkY()
            dut.io.xOutput.expect(passX(packetL), s"[%d] Expected local packet %s on X".format(i, passX(packetL)))
          case SwitchPort.L  | SwitchPort.Y =>
            // L is dropped because it tried to go through Y
            dut.io.xOutput.expect(emptyPacket, s"[%d] expected empty packet on port X, got %s"
              .format(i, dut.io.peek))
            // but Y passes
            checkY()
        }

      }
    }
  }

  it should "drop Y if X turning" in  {
    test(new Switch(DIMX, DIMY, config)).withAnnotations(Seq(USE_VERILATOR)) { dut =>

      for (i <- Range(0, NUM_TESTS)) {
        val packetX = randomPacketXY
        val packetY = randomPacketY
        dut.io.xInput.poke(packetX)
        dut.io.yInput.poke(packetY)
        dut.io.lInput.poke(emptyPacket)

        dut.clock.step()
        routeFromX(packetX) match {
          case SwitchPort.Y =>
            // drop Y because X is turning
            dut.io.yOutput.expect(passY(packetX), s"[%d] Expected packet %s from X on Y got %s"
              .format(i, passY(packetX), dut.io.yOutput.peek))
            dut.io.xOutput.expect(emptyPacket, s"[%d] Expected empty packet on Y but got %s".format(
              i, dut.io.xOutput.peek
            ))
            dut.io.terminal.expect(false.B)
          case SwitchPort.L =>
            dut.io.yOutput.expect(invalidated(packetX), s"[%d] Expected packet %s from X on Y got %s"
              .format(i, invalidated(packetX), dut.io.yOutput.peek))
            dut.io.terminal.expect(true.B)
            dut.io.xOutput.expect(emptyPacket, s"[%d] Expected empty packet on Y but got %s".format(
              i, dut.io.xOutput.peek
            ))
          case SwitchPort.X =>
            // both packets pass
            dut.io.xOutput.expect(passX(packetX), s"[%d] Expected packet %s from X on Y got %s"
              .format(i, passX(packetX), dut.io.xOutput.peek))
            if (packetY.yHops.litValue() == 0) {
              dut.io.yOutput.expect(invalidated(packetY), s"[%d] Expected packet %s on Y but got %s".format(
                i, invalidated(packetY), dut.io.yOutput.peek
              ))
              dut.io.terminal.expect(true.B)
            } else {
              dut.io.yOutput.expect(passY(packetY), s"[%d] Expected packet %s on Y but got %s".format(
                i, passY(packetY), dut.io.yOutput.peek
              ))
              dut.io.terminal.expect(false.B)
            }

        }

      }
    }
  }

  it should "drop L if X is present" in {
    test(new Switch(DIMX, DIMY, config)).withAnnotations(Seq(USE_VERILATOR)) { dut =>

      for (i <- Range(0, NUM_TESTS)) {
        val packetX = randomPacketXY
        val packetL = randomPacketXY

        dut.io.xInput.poke(packetX)
        dut.io.yInput.poke(emptyPacket)
        dut.io.lInput.poke(packetL)
        dut.clock.step()
        (routeFromX(packetX), routeFromLocal(packetL)) match {
          case (SwitchPort.X, SwitchPort.X | SwitchPort.L) =>
            // drop L
            dut.io.xOutput.expect(passX(packetX), s"[%d] Expected packet %s on X but got %s"
              .format(i, passX(packetX), dut.io.xOutput.peek))
            dut.io.yOutput.expect(emptyPacket, s"[%d] Expected empty packet on %s on Y but " +
              s"got %s".format(i, dut.io.yOutput.peek))
          case (SwitchPort.X, SwitchPort.Y) =>
            // both X and L can pass
            dut.io.xOutput.expect(passX(packetX), s"[%d] Expected packet %s on X but got %s"
              .format(i, passX(packetX), dut.io.xOutput.peek))
            dut.io.yOutput.expect(passY(packetL), s"[%d] Expected packet %s on X but got %s"
              .format(i, passY(packetL), dut.io.yOutput.peek))
          case (SwitchPort.Y, SwitchPort.X) =>
            // X turns and L pass
            dut.io.xOutput.expect(passX(packetL), s"[%d] Expected packet %s on X but got %s"
              .format(i, passX(packetL), dut.io.xOutput.peek))
            dut.io.yOutput.expect(passY(packetX), s"[%d] Expected packet %s on X but got %s"
              .format(i, passY(packetX), dut.io.yOutput.peek))
          case (SwitchPort.Y, SwitchPort.L | SwitchPort.Y) =>
            // only x passes
            dut.io.yOutput.expect(passY(packetX), s"[%d] Expected packet %s on X but got %s"
              .format(i, passY(packetX), dut.io.yOutput.peek))
            dut.io.xOutput.expect(emptyPacket, s"[%d] Expected empty packet on %s on Y but " +
              s"got %s".format(i, dut.io.xOutput.peek))
          case (SwitchPort.L, SwitchPort.L | SwitchPort.Y) =>
            // only x passes
            dut.io.yOutput.expect(invalidated(packetX), s"[%d] Expected packet %s on X but got %s"
              .format(i, invalidated(packetX), dut.io.yOutput.peek))
            dut.io.xOutput.expect(emptyPacket, s"[%d] Expected empty packet on %s on Y but " +
              s"got %s".format(i, dut.io.xOutput.peek))
          case (SwitchPort.L, SwitchPort.X) =>
            // X turns and L pass
            dut.io.xOutput.expect(passX(packetL), s"[%d] Expected packet %s on X but got %s"
              .format(i, passX(packetL), dut.io.xOutput.peek))
            dut.io.yOutput.expect(invalidated(packetX), s"[%d] Expected packet %s on X but got %s"
              .format(i, invalidated(packetX), dut.io.yOutput.peek))

        }

      }

    }

  }
}
