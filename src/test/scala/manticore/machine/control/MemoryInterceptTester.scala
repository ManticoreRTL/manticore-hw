package manticore.machine.control

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import manticore.machine.memory.CacheConfig
import manticore.machine.core.ClockDistribution
import manticore.machine.memory.CacheCommand
import manticore.machine.core.MemoryIntercept

class MemoryInterceptTester extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  class CacheModel extends Module {

    val io = IO(new Bundle {
      val front = CacheConfig.frontInterface()
      val delay = Input(UInt(8.W))
    })

    val delay_counter = Counter(255)

    val delay = RegEnable(io.delay, io.front.start)

    val sIdle :: sDelay :: Nil = Enum(2)

    val state = RegInit(sIdle)

    io.front.rdata := Cat(
      RegEnable(io.front.addr, io.front.start)
    )

    io.front.idle := (state === sIdle)
    io.front.done := false.B
    switch(state) {
      is(sIdle) {
        state := Mux(io.front.start, sDelay, sIdle)
        delay_counter.reset()
      }
      is(sDelay) {
        state := Mux(delay_counter.value === delay, sIdle, sDelay)
        delay_counter.inc()
        io.front.done := (delay_counter.value === delay)
      }
    }

  }

  class KerneControlModel extends Module {
    val io = IO(new Bundle {
      val kill_clock   = Input(Bool())
      val revive_clock = Input(Bool())
      val clock_active = Output(Bool())

    })

    val clock_active = RegInit(true.B)

    io.clock_active := clock_active

    when(clock_active) {
      clock_active := !(io.kill_clock)
    } otherwise {
      clock_active := io.revive_clock
    }

  }
  class MemoryInstruction extends Bundle {
    val wen   = Bool()
    val addr  = UInt((3 * CacheConfig.DataBits).W)
    val wdata = UInt(CacheConfig.DataBits.W)
    val ren   = Bool()
  }

  class ProcessorModel extends Module {

    val Stages = 5

    val io = IO(new Bundle {
      val cache          = Flipped(CacheConfig.frontInterface())
      val instr          = Input(new MemoryInstruction)
      val write_back     = Output(UInt(CacheConfig.DataBits.W))
      val log            = Decoupled(UInt(CacheConfig.DataBits.W))
      val program_enable = Input(Bool())
    })

    val instruction_queue = Module(
      new Queue(
        gen = chiselTypeOf(io.instr),
        entries = 8192,
        pipe = false,
        flow = false,
        useSyncReadMem = false,
        hasFlush = false
      )
    )
    instruction_queue.io.enq.bits  := io.instr
    instruction_queue.io.enq.valid := io.program_enable

    instruction_queue.io.deq.ready := !io.program_enable

    val frontend = Pipe(
      enqValid = !io.program_enable && instruction_queue.io.deq.valid,
      enqBits = instruction_queue.io.deq.bits,
      latency = Stages - 2
    )

    val backend = Pipe(frontend, 2)

    io.cache.cmd   := Mux(frontend.bits.wen, CacheCommand.Write, CacheCommand.Read)
    io.cache.start := (frontend.bits.wen || frontend.bits.ren)
    io.cache.addr  := frontend.bits.addr
    io.cache.wdata := frontend.bits.wdata

    val write_log = Module(
      new Queue(
        gen = UInt(CacheConfig.DataBits.W),
        entries = 8192,
        pipe = false,
        flow = false,
        useSyncReadMem = true,
        hasFlush = false
      )
    )

    write_log.io.enq.valid := backend.valid
    write_log.io.enq.bits  := DontCare
    when(backend.bits.ren || backend.bits.wen) {
      io.write_back         := io.cache.rdata
      write_log.io.enq.bits := io.cache.rdata
    } otherwise {
      io.write_back         := backend.bits.wdata
      write_log.io.enq.bits := backend.bits.wdata
    }

    io.log <> write_log.io.deq

  }
  class DesignUnderTest extends Module {

    val io = IO(new Bundle {
      val instr          = Input(new MemoryInstruction)
      val program_enable = Input(Bool())
      val cache_delay    = Input(UInt(8.W))
      val write_back     = Output(UInt(CacheConfig.DataBits.W))
      val log            = Decoupled(UInt(CacheConfig.DataBits.W))
      val rdata          = Output(UInt(CacheConfig.DataBits.W))
    })

    val clock_dist = Module(new ClockDistribution)
    val control    = withClock(clock = clock_dist.io.control_clock) { Module(new KerneControlModel) }
    val cache      = withClock(clock = clock_dist.io.control_clock) { Module(new CacheModel) }
    val intercept  = withClock(clock = clock_dist.io.control_clock) { Module(new MemoryIntercept) }
    val core       = withClock(clock = clock_dist.io.compute_clock) { Module(new ProcessorModel) }
    clock_dist.io.root_clock       := clock
    clock_dist.io.root_rst_n       := !(reset.asBool)
    clock_dist.io.compute_clock_en := control.io.clock_active

    core.io.instr := io.instr
    io.write_back := core.io.write_back

    cache.io.delay := io.cache_delay

    core.io.cache <> intercept.io.core
    intercept.io.core_kill_clock <> control.io.kill_clock
    intercept.io.core_revive_clock <> control.io.revive_clock
    intercept.io.cache <> cache.io.front
    intercept.io.core_clock    := clock_dist.io.compute_clock
    intercept.io.boot.addr     := DontCare
    intercept.io.boot.wdata    := DontCare
    intercept.io.boot.cmd      := DontCare
    intercept.io.config_enable := false.B
    intercept.io.boot.start    := false.B

    io.rdata := cache.io.front.rdata

    io.log <> core.io.log
    core.io.program_enable := io.program_enable
  }

  behavior of "Memory request interceptor"

  it should "correctly handle back to back memory requests" in {

    test(new DesignUnderTest).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(10000)
      dut.io.cache_delay.poke(5.U)

      dut.io.log.setSinkClock(dut.clock)
      dut.io.log.ready.poke(false.B)
      dut.io.program_enable.poke(false.B)

      val expected = scala.collection.mutable.Queue.empty[UInt]
      val randGen  = new scala.util.Random(221)

      def rand16() = randGen.nextInt(1 << 16).U

      def doRead() = {
        val addr = rand16()
        dut.io.instr.addr.poke(addr)
        dut.io.instr.wdata.poke(rand16())
        dut.io.instr.ren.poke(true.B)
        dut.io.instr.wen.poke(false.B)
        dut.io.program_enable.poke(true.B)
        dut.clock.step()
        expected.enqueue(addr)
      }
      def doWrite() = {
        val addr = rand16()
        dut.io.instr.addr.poke(addr)
        dut.io.instr.wdata.poke(rand16())
        dut.io.instr.ren.poke(false.B)
        dut.io.instr.wen.poke(true.B)
        dut.io.program_enable.poke(true.B)
        dut.clock.step()
        expected.enqueue(addr)
      }

      def doNonMemory() = {
        val wdata = rand16()
        dut.io.instr.addr.poke(rand16())
        dut.io.instr.wdata.poke(wdata)
        dut.io.instr.ren.poke(false.B)
        dut.io.instr.wen.poke(false.B)
        dut.io.program_enable.poke(true.B)
        dut.clock.step()
        expected.enqueue(wdata)
      }
      // do a bunch of normal (static) instructions
      for (i <- 0 until 5) {
        doNonMemory()
      }

      // make sure a single stranded memory access works fine
      doRead()

      for (i <- 0 until 4) {
        doNonMemory()
      }

      for (i <- 0 until 3) {
        doRead()
      }

      // we can not finish up the pipeline with memory instructions, because of the
      // way the processor model in this test operates.
      for (i <- 0 until 12) {
        doNonMemory()
      }

      dut.io.program_enable.poke(false.B)

      // drain the pipeline
      dut.clock.step(1000)

      dut.io.log.ready.poke(true.B)
      println("Checking results")

      //   dut.io.log.expectDequeueSeq(expected.toSeq)

      while (dut.io.log.valid.peek().litToBoolean) {
        println(
          f"Got 0x${dut.io.log.bits.peek().litValue.toInt}%x (${dut.io.log.bits.peek().litValue.toInt}) expected 0x${expected.head.litValue.toInt}%x"
        )
        dut.io.log.bits.expect(expected.head)
        expected.dequeue()
        dut.clock.step()
      }
      println("Done")

      dut.clock.step(20)
    }
  }

}
