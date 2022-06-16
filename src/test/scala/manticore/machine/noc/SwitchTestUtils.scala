package manticore.machine.noc

import Chisel._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import manticore.machine.ISA
import manticore.machine.core.NoCBundle

object SwitchTestUtils {

  def genData(config: ISA)(implicit rdgen: scala.util.Random): UInt = {
    rdgen.nextInt(1 << config.DataBits).U
  }

  def genAddress(config: ISA)(implicit rdgen: scala.util.Random): UInt = {
    rdgen.nextInt(1 << config.IdBits).U
  }

  def randomPacketXY(DimX: Int, DimY: Int, config: ISA)(implicit rdgen: scala.util.Random): NoCBundle = {
    NoCBundle(DimX, DimY, config).Lit(
      _.data -> genData(config),
      _.address -> genAddress(config),
      _.valid -> true.B,
      _.xHops -> rdgen.nextInt(1 << log2Ceil(DimX)).U,
      _.yHops -> rdgen.nextInt(1 << log2Ceil(DimY)).U
    )
  }

  def randomPacketY(DimX: Int, DimY: Int, config: ISA)(implicit rdgen: scala.util.Random): NoCBundle = {
    NoCBundle(DimX, DimY, config).Lit(
      _.data -> genData(config),
      _.address -> genAddress(config),
      _.valid -> true.B,
      _.xHops -> 0.U,
      _.yHops -> (rdgen.nextInt((1 << log2Ceil(DimX)) - 1)).U
    )
  }

  def randomPacketX(DimX: Int, DimY: Int, config: ISA)(implicit rdgen: scala.util.Random): NoCBundle = {
    NoCBundle(DimX, DimY, config).Lit(
      _.data -> genData(config),
      _.address -> genAddress(config),
      _.valid -> true.B,
      _.xHops -> (rdgen.nextInt((1 << log2Ceil(DimY)) - 1)).U,
      _.yHops -> 0.U
    )
  }

  def emptyPacket(DimX: Int, DimY: Int, config: ISA): NoCBundle = {
    NoCBundle(DimX, DimY, config).Lit(
      _.data -> 0.U,
      _.address -> 0.U,
      _.valid -> false.B,
      _.xHops -> 0.U,
      _.yHops -> 0.U
    )
  }
}
