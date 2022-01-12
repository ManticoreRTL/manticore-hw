package manticore.control

import Chisel._
import chisel3.tester.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chisel3.tester.{ChiselScalatestTester, testableClock, testableData}
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import manticore.ManticoreBaseISA
import manticore.core.Programmer
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import chisel3.MultiIOModule
import manticore.core.MemoryReadWriteInterface
import manticore.ManticoreFullISA
import chisel3.VecInit
import chisel3.experimental.ChiselEnum
import manticore.core.NoCBundle
class ProgrammerTester
    extends FlatSpec
    with ChiselScalatestTester
    with Matchers {

  val rdgen = new scala.util.Random(0)
  class MemoryError extends Bundle {
    val multiple_access: Bool = Output(Bool())
    val out_of_range: Bool    = Output(Bool())
    val write_access: Bool    = Output(Bool())
  }
  class InstructionRom(content: Seq[Int]) extends MultiIOModule {

    val io    = IO(Flipped(new MemoryReadWriteInterface(ManticoreFullISA)))
    val error = IO(Output(new MemoryError))

    

    def stickySet(err: Bool, cond: => Bool) {
      val r = RegInit(Bool(), false.B)

      when(cond) {
        r := true.B
      }
      err := r
    }

    val rom     = VecInit(content.map(_.U))
    val latency = VecInit(content.map(_ => rdgen.nextInt(10) max 4).map(_.U))

    val counter = Reg(UInt(32.W))
    val data    = Reg(UInt(ManticoreFullISA.DataBits.W))
    object State extends ChiselEnum {
      val Idle, Busy, Done = Value
    }

    val state = RegInit(State.Type(), State.Idle)
    stickySet(error.write_access, io.wen === true.B)
    stickySet(
      error.multiple_access,
      io.start === true.B && state =/= State.Idle
    )
    stickySet(
      error.out_of_range,
      io.start === true.B && io.addr >= rom.length.U
    )
    io.rdata := 0.U
    io.done  := false.B
    switch(state) {
      is(State.Idle) {
        when(io.start) {
          counter := latency(io.addr)
          data    := rom(io.addr)
          state   := State.Busy
        }
      }
      is(State.Busy) {
        when(counter === 1.U) {
          state := State.Done
        }
        counter := counter - 1.U
      }
      is(State.Done) {
        state    := State.Idle
        io.rdata := data
        io.done  := true.B
      }
    }

  }

  class ProgrammerWithMemory(dimx: Int, dimy: Int, content: Seq[Int])
      extends Module {

    val io = IO(new Bundle {
      val errors     = Output(new MemoryError)
      val packet_out = Output(new NoCBundle(dimx, dimy, ManticoreFullISA))
      val instruction_base: UInt = Input(UInt(64.W))
      val running                = Output(Bool())
      val start                  = Input(Bool())
    })

    val programmer = Module(
      new Programmer(config = ManticoreFullISA, DimX = dimx, DimY = dimy)
    )
    val inst_rom = Module(new InstructionRom(content))

    io.packet_out := programmer.io.packet_out
    io.errors     := inst_rom.error

    inst_rom.io <> programmer.io.memory_backend
    inst_rom.io.addr := programmer.io.memory_backend.addr - io.instruction_base

    programmer.io.finish := false.B
    programmer.io.start  := io.start
    programmer.io.instruction_stream_base := io.instruction_base

  }

  val DimX = 3
  val DimY = 3
  val isa  = ManticoreBaseISA
  val memory_spec =
    new ProgrammerTestUtils.MemoryStreamSpec(DimX, DimY, isa, rdgen)
  behavior of "Programmer"

  it should "correctly read instruction stream from the cache or memory and stream it out" in {

    test(
      new ProgrammerWithMemory(
        content = memory_spec.content,
        dimx = DimX,
        dimy = DimY
      )
    )
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        @tailrec
        def validateStream(expected_stream: Seq[(Int, Int, Int)]): Unit = {
          dut.io.start.poke(false.B)
          dut.io.errors.multiple_access.expect(false.B)
          dut.io.errors.out_of_range.expect(false.B)
          dut.io.errors.write_access.expect(false.B)
          if (expected_stream.isEmpty) {

            dut.clock.step()
            println("Validated the instruction stream")
          } else {
            if (dut.io.packet_out.valid.peek.litToBoolean) {
              dut.io.packet_out.xHops.expect(expected_stream.head._2.U)
              dut.io.packet_out.yHops.expect(expected_stream.head._3.U)
              dut.io.packet_out.data.expect(expected_stream.head._1.U)
              // println(s"Validated ${expected_stream.head}")
              dut.clock.step()
              validateStream(expected_stream.tail)
            } else {
              dut.clock.step()
              validateStream(expected_stream)
            }
          }
          
        }

        dut.io.start.poke(true.B)
        dut.io.instruction_base.poke(memory_spec.base_address.U)
        dut.clock.step()
        dut.clock.setTimeout(1000 * DimX * DimY)
        dut.io.start.poke(false.B)
        // println(memory_spec.expected_stream)
//        dut.io.core_active.foreach{v => v.poke(false.B)}

        validateStream(memory_spec.expected_stream)

      }

  }

}
