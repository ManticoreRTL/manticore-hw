package manticore.processor

import Chisel._
import memory.CacheConfig
import org.scalatest.FlatSpec
import chisel3.tester.ChiselScalatestTester
import chisel3.tester.experimental.sanitizeFileName
import org.scalatest.Matchers
import manticore.ManticoreFullISA
import chisel3.SyncReadMem
import memory.CacheCommand
import chisel3.VecInit
import manticore.core.ProcessorInterface
import java.nio.file.Paths
import java.io.File

class UniProcessorPredicatedStoreTester
    extends FlatSpec
    with ChiselScalatestTester
    with Matchers {

  val Config = ManticoreFullISA

  val DimX, DimY = 4

  val rdgen = new scala.util.Random(2)

  class SmallCache(val init_values: Seq[Int]) extends Module {

    val io = IO(Flipped(CacheConfig.frontInterface()))

    val dirty_bits = Reg(Vec(init_values.size, Bool()))
    require(CacheConfig.DataBits == Config.DataBits)
    val memory = SyncReadMem(init_values.size, UInt(CacheConfig.DataBits))

    val init_mem = VecInit(init_values.map(_.U(CacheConfig.DataBits)))

    val idle = RegInit(Bool(), false.B)
    when(io.start) {
      switch(io.cmd) {
        is(CacheCommand.Read) {
          when(dirty_bits(io.addr)) {
            io.rdata := memory(io.addr)
          } otherwise {
            io.rdata := init_mem(io.addr)
          }
        }
        is(CacheCommand.Write) {
          dirty_bits(io.addr) := true.B
          init_mem(io.addr)   := io.wdata
        }
      }
      idle := false.B
    }

    when(idle === false.B) {
      idle    := true.B
      io.done := true.B
    } otherwise {
      io.done := false.B
    }

  }

  val equations = Seq.fill(1 << ManticoreFullISA.FunctBits) {
    Seq.fill(ManticoreFullISA.DataBits) {
      rdgen.nextInt(1 << 16)
    }
  }
  val rf = UniProcessorTestUtils.createMemoryDataFiles(
    Seq.tabulate(1 << ManticoreFullISA.IdBits) { i => 0 }
  ) {
    Paths
      .get(
        "test_data_dir" + File.separator +
          sanitizeFileName(
            scalaTestContext.value.get.name
          ) + File.separator + "rf.data"
      )
      .toAbsolutePath
  }

  val ra =
    UniProcessorTestUtils.createMemoryDataFiles(
      Seq.fill(1 << ManticoreFullISA.IdBits)(0)
    ) {
      Paths
        .get(
          "test_data_dir" + File.separator +
            sanitizeFileName(
              scalaTestContext.value.get.name
            ) + File.separator + "ra.data"
        )
        .toAbsolutePath
    }

  val address_range = 256
  val initial_cache_values =
    Seq.fill(address_range)(rdgen.nextInt(1 << CacheConfig.DataBits))

  class ProcessorWitthCache extends Module {

    val io = IO(new Bundle {
      val cache = new Bundle {
        val addr  = Input(UInt(CacheConfig.UsedAddressBits.W))
        val data  = Output(UInt(CacheConfig.DataBits))
        val start = Input(Bool())
        val done  = Output(Bool())
      }
      val processor    = Flipped(new ProcessorInterface(Config, DimX, DimY))
      val pause        = Input(Bool())
      val cache_access = Output(Bool())
      val read_access  = Output(Bool())
    })

    val clocked_processor = Module(
      new UniProcessorTestUtils.ClockedProcessor(
        Config,
        DimX,
        DimY,
        equations,
        rf,
        ra
      )
    )

    val cache = Module(
      new SmallCache(initial_cache_values)
    )
    clocked_processor.io.clock_enable_n := io.pause
    cache.io <> clocked_processor.io.periphery.cache

    io.cache_access := clocked_processor.io.periphery.cache.start
    io.read_access  := clocked_processor.io.rwb

  }

  def makeProgram = {
    import manticore.assembly.Instruction._

    val program = Array[Instruction](
        
    )
  }

}
