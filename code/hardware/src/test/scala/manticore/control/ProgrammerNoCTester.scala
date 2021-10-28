package manticore.control

import chisel3.tester.{ChiselScalatestTester, testableClock, testableData}
import org.scalatest.{FlatSpec, Matchers}
import Chisel._
import chisel3.VecInit
import chisel3.experimental.ChiselEnum
import chisel3.tester.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chisel3.tester.experimental.sanitizeFileName
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import manticore.{ManticoreBaseISA, ManticoreFullISA}
import manticore.core.{BareNoC, BareNoCInterface, NamedError, NoCBundle, Processor, Programmer, ProgrammerInterface}
import manticore.processor.UniProcessorTestUtils
import memory.CacheConfig

import java.io.File
import java.nio.file.Paths
import scala.annotation.tailrec


class ProgrammerNoCTester extends FlatSpec with ChiselScalatestTester with Matchers {

  case class SimulatedMemoryWord(value: Int, latency: Int)

  val Config = ManticoreBaseISA
  val DimX = 2
  val DimY = 3

  val rdgen = new scala.util.Random(0)

  val equations = Seq.fill(1 << ManticoreFullISA.FunctBits) {
    Seq.fill(ManticoreFullISA.DataBits) {
      rdgen.nextInt(1 << 16)
    }
  }

  class MiniRomCache(val initial: Seq[SimulatedMemoryWord]) extends Module {
    val io = IO(CacheConfig.frontInterface())

    val storage: Vec[UInt] = VecInit(initial.map(_.value.U(Config.DataBits.W)))
    val delays: Vec[UInt] = VecInit(initial.map(_.latency.U(32.W)))
    val counter: UInt = Reg(UInt(32.W))

    object State extends ChiselEnum {
      val Idle, EmulateAccessLatency = Value
    }

    val addr_reg = Reg(io.addr.chiselCloneType)
    val state = RegInit(State.Type(), State.Idle)
    io.rdata := 0.U
    io.done := false.B
    switch(state) {
      is(State.Idle) {
        when(io.start) {
          addr_reg := io.addr
          state := State.EmulateAccessLatency
          counter := 0.U
        }
      }
      is(State.EmulateAccessLatency) {
        counter := counter + 1.U
        when(counter === delays(addr_reg)) {
          io.done := true.B
          io.rdata := storage(addr_reg)
          addr_reg := 0.U
          state := State.Idle
        }
      }
    }
  }

  class ComputeGridWithBootLoader(val initial: Seq[SimulatedMemoryWord]) extends Module {
    val io = IO(new Bundle {
      val programmer = new Bundle {
        val start = Input(Bool())
        val finish = Input(Bool())
        val running = Output(Bool())
        val sent = Output(new NoCBundle(DimX, DimY, Config))
      }
      val noc = new BareNoCInterface(DimX, DimY, Config)
      val core_active: Vec[Vec[Bool]] = Output(Vec(DimX, Vec(DimY, Bool())))
      val exception: Vec[Vec[NamedError]] = Output(Vec(DimX, Vec(DimY, new NamedError(Config.DataBits))))
      val schedule_length: UInt = Input(UInt((Config.NumPcBits + 1).W))
      val schedule_counter: UInt = Output(UInt((Config.NumPcBits + 1).W))
    })

    val rf = UniProcessorTestUtils.createMemoryDataFiles {
      Seq.tabulate(2048) { i =>
        if (i >= 500)
          0
        else
          i
      }
    } {
      Paths.get("test_data_dir" + File.separator +
        sanitizeFileName(scalaTestContext.value.get.name) + File.separator + "rf.data").toAbsolutePath
    }
    val ra = UniProcessorTestUtils.createMemoryDataFiles {
      Seq.fill(2048)(0)
    } {
      Paths.get("test_data_dir" + File.separator +
        sanitizeFileName(scalaTestContext.value.get.name) + File.separator + "ra.data").toAbsolutePath
    }

    val bootloader = Module(new Programmer(Config, DimX, DimY))
    val noc = Module(new BareNoC(DimX, DimY, Config))
    val cores = Range(0, DimX).map { x =>
      Range(0, DimY).map { y =>
        Module(new Processor(
          Config, DimX, DimY,
          equations, rf, ra
        ))
      }
    }

    noc.io.corePacketInput
      .flatten.zip(noc.io.corePacketOutput.flatten)
      .zip(cores.flatten)
      .foreach { case ((_inp, _out), core) =>
        _inp := core.io.packet_out
        core.io.packet_in := _out
      }

    val cache = Module(new MiniRomCache(initial))
    noc.io.configPacket := bootloader.io.packet_out
    noc.io.configEnable := bootloader.io.packet_out.valid
    //    noc.io.corePacketInput := io.noc.corePacketInput
    bootloader.io.start := io.programmer.start
    bootloader.io.finish := io.programmer.finish
    bootloader.io.cache_frontend <> cache.io
    io.noc.corePacketOutput := noc.io.corePacketOutput
    io.programmer.running := bootloader.io.running
    io.programmer.sent := bootloader.io.packet_out

    Range(0, DimX).foreach { x =>
      Range(0, DimY).foreach { y =>
        io.core_active(x)(y) := cores(x)(y).io.periphery.active
        io.exception(x)(y) := cores(x)(y).io.periphery.exception
      }
    }

    val schedule_counter: UInt = Reg(UInt((Config.NumPcBits + 1).W))
    when(bootloader.io.running) {
      schedule_counter := schedule_counter + 1.U
      when(schedule_counter === io.schedule_length - 1.U) {
        schedule_counter := 0.U
      }
    }
    io.schedule_counter := schedule_counter

  }


  def randomDelay(): Int = 10 max rdgen.nextInt(50)


  def makeProgram(x: Int, y: Int, num_nops: Int): Array[Long] = {
    import manticore.assembly.Instruction._
    val const_1 = R(1)
    val const_0 = R(0)
    val const_499 = R(499)
    val counter = R(501)
    val dest_header_x = R(600)
    val dest_header_y = R(601)
    val dest_counter = R(602)
    val const_x = R(x)
    val const_y = R(y)
    val initial_state = R(700)
    val stop_cond = R(800)
    val program: Array[Instruction] =
      Array[Instruction](
        Expect(dest_counter, counter, (y << 8 | x)), // expect the value sent from
        // last cycle to be equal to old value of the counter
        SetLessThanUnsigned(stop_cond, const_499, counter),
        Add2(counter, const_1, counter),
        Send(dest_header_x, const_x, 1, 0), // send the counter to the east
        // neighbor, with the header indicating the source of the message
        Send(dest_header_y, const_y, 1, 0),
        Nop(),
        Send(dest_counter, counter, 1, 0),
        Expect(stop_cond, const_0, 0xFFFF)
      ) ++ Array.fill(num_nops) { // fill up enough Nops
        Nop()
      }

    // assemble
    program.map { inst =>
      import manticore.assembly.Assembler
      Assembler.assemble(inst)(equations)
    }
  }

  val num_nops: Seq[Seq[Int]] = Range(0, DimX).map { x =>
    Range(0, DimY).map { y =>
      rdgen.nextInt(20) max 4
    }
  }

  val schedule_length: Int = num_nops.flatten.max + makeProgram(0, 0, 0).length + 3 + 5
  val initial_memory_content: Seq[Seq[Seq[SimulatedMemoryWord]]] = Range(0, DimX).map { x =>
    Range(0, DimY).map { y =>


      val binary = makeProgram(x, y, num_nops(x)(y))
      val body_length = binary.length
      val epilogue_length = 3 // expected number of messages that it receives
      //      val epilogue_length = 6
      //      val sleep_length = 7
      val sleep_length = schedule_length - (body_length + epilogue_length)
      if (sleep_length < 5) {
        fail(s"sleep length ${x}, ${y} should be greater than pipeline depth (${sleep_length} < 5}")
      }

      Seq(
        SimulatedMemoryWord(y << 8 | x, randomDelay()),
        SimulatedMemoryWord(body_length, randomDelay())
      ) ++ binary.flatMap { inst =>
        Seq(
          inst & 0x0000FFFF,
          (inst >> 16) & 0x0000FFFF,
          (inst >> 32) & 0x0000FFFF,
          (inst >> 48) & 0x0000FFFF
        ).zip(Seq.fill(4)(randomDelay()))
          .map { case (value, delay) => SimulatedMemoryWord(value.toInt, delay) }

      } ++ Seq(
        SimulatedMemoryWord(epilogue_length, randomDelay()),
        SimulatedMemoryWord(sleep_length, randomDelay())
      )
    }
  }

  behavior of "Programmer on the NoC"
  it should "correctly stream instructions and transition to running mode at the right time" in {

    test(new ComputeGridWithBootLoader(
      initial_memory_content.flatten.flatten
    )).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>

      dut.clock.step()

      val mem_pos: Array[Array[Int]] = Range(0, DimX).map { x =>
        Range(0, DimY).map { y =>
          1
        }.toArray
      }.toArray
      dut.io.noc.corePacketInput.flatten.foreach { p =>
        p.valid.poke(false.B)
      }
      var cycle = 1

      def checkPackets() = {
        if (dut.io.programmer.sent.valid.peek().litToBoolean) {
          println(s"[${cycle}]: Sent packet ${dut.io.programmer.sent.peek()}")
        }
        // when io.programmer.running is false, we should have core_active false for every core
        val running = dut.io.programmer.running.peek().litToBoolean
        val active = dut.io.core_active.flatten.map(_.peek().litToBoolean)
        // running => forall core: active(core)
        // !running => forall core: !active(core)
        if (running) {
          active.forall(_ == true) should be(true)
        } else {
          active.forall(_ == false) should be(true)
        }

        dut.io.noc.corePacketOutput.zipWithIndex.reverse.foreach { case (v, x) =>
          v.zipWithIndex.reverse.foreach { case (p, y) =>
            if (p.valid.peek().litToBoolean) {
              println(s"[${cycle}]: Received packet at (${x}, ${y}) with value ${p.data.peek().litValue().toInt}")
              if (mem_pos(x)(y) < initial_memory_content(x)(y).length) {
                p.data.expect(
                  initial_memory_content(x)(y)(mem_pos(x)(y)).value.U
                )
                mem_pos(x)(y) = mem_pos(x)(y) + 1
                println("validated")
              }
            }
          }
        }
      }

      @tailrec
      def evaluateUntilActivation(time_out: Int): Unit = {
        if (time_out != 0 && !dut.io.programmer.running.peek().litToBoolean) {
          checkPackets()
          dut.clock.step()
          cycle = cycle + 1
          evaluateUntilActivation(time_out - 1)
        } else {
          //          dut.io.programmer.running.expect(false.B, "Timed out before the cores started executing!")
        }
      }


      def checkExceptions(): Boolean = {
        Range(0, DimX).map { x =>
          Range(0, DimY).map { y =>
            if (dut.io.exception(x)(y).error.peek().litToBoolean) {
              println(s"[${cycle}] an exception in core_${x}_${y} occurred ${dut.io.exception(x)(y).peek}")
              false
            } else {
              true
            }
          }.forall(_ == true)
        }.forall(_ == true)
      }

      @tailrec
      def evaluateFor(num_cycles: Int): Unit = {
        if (num_cycles != 0) {
          val success: Boolean = checkExceptions()
          dut.clock.step()
          cycle = cycle + 1
          if (success)
            evaluateFor(num_cycles - 1)
          else {
            if (dut.io.exception.flatten.map(_.id.peek().litValue().toInt).forall(_ == 0xFFFF)) {
              println("Stop condition caught")
            } else {
              fail("Manticore exception caught!")
            }
          }
        }
      }

      dut.io.schedule_length.poke(schedule_length.U)
      dut.io.programmer.start.poke(true.B)
      dut.clock.step()
      dut.io.programmer.start.poke(false.B)
      dut.clock.setTimeout(initial_memory_content.flatten.flatten.length * DimX * DimY * 20 + 10000)

      // program the cores and evaluate until the cores become active
      evaluateUntilActivation(35000)

      // the cores are active, now snoop Send packets and ensure no exception happens
      println(s"Schedule length is ${schedule_length}")
      evaluateFor(schedule_length * 1000) // simulate 1000 virtual cycles
      dut.clock.step()

    }
  }

}
