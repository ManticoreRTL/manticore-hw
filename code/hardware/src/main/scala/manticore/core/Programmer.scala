package manticore.core


import Chisel._
import chisel3.experimental.ChiselEnum
import memory.{Cache, CacheCommand, CacheConfig, CacheFrontInterface}
import manticore.ISA


class ProgrammerInterface(config: ISA, DimX: Int, DimY: Int) extends Bundle {

  val packet_out: NoCBundle = Output(new NoCBundle(config = config, DimX = DimX, DimY = DimY))
  // base byte-addressable address of the allocated instruction stream which has the following format
  // assuming a 16-bit data path if
  // [X0, Y0] (physical address of the first processor)
  // [PROGRAM_BODY_LENGTH0]
  // [INST0[15:0]]
  // [INST0[31:16]]
  // ...
  // [PROGRAM_EPILOGUE_LENGTH0] (expected number of receives)
  // [PROGRAM_SLEEP_LENGTH0]
  // [X1, X2]
  // ...
  // [X(DIMX-1), Y(DIMY-1]]
  // ...
  // [PROGRAM_SLEEP_LENGTH((DIMX-1) * (DIMY - 1))]

  val instruction_stream_base: UInt = Input(UInt(64.W))
  val start: Bool = Input(Bool())
  val finish: Bool = Input(Bool())
//  val core_active: Vec[Bool] = Input(Vec(DimY * DimX, Bool()))
  val running: Bool = Output(Bool())

//  val global_synch: Bool = Output(Bool())
  val cache_frontend: CacheFrontInterface = Flipped(CacheConfig.frontInterface())
}

class Programmer(config: ISA, DimX: Int, DimY: Int) extends Module {
  require(config.DataBits == 16, "Only 16-bit data width is supported with the cache implementation")
  val io: ProgrammerInterface = IO(new ProgrammerInterface(config, DimX, DimY))

  object Phase extends ChiselEnum {
    val Idle,
    StartCacheRead,
    StreamDest,
    StreamBodyLength,
    StreamInstruction,
    StreamEpilogueLength,
    StreamSleepLength,
    StreamCountDown,
    WaitForCountDownEnd1,
    WaitForCountDownEnd2,
    Running,
    Paused = Value
  }

  val phase: Phase.Type = RegInit(Phase.Type(), Phase.Idle)


  val instruction_stream_addr_reg: UInt = Reg(UInt(CacheConfig.UsedAddressBits.W))
  val enable_addr_increment: Bool = Wire(Bool())

  require(CacheConfig.UsedAddressBits > config.NumPcBits)
  val body_length_end: UInt = Reg(UInt(CacheConfig.UsedAddressBits.W))

  val delay_value: UInt = Reg(UInt(config.DataBits.W))

  when(io.start) {
    instruction_stream_addr_reg := io.instruction_stream_base(CacheConfig.UsedAddressBits, 1) // drop the byte offset
  }.elsewhen(enable_addr_increment) {
    instruction_stream_addr_reg := instruction_stream_addr_reg + 1.U
  }


  class WrappingCounter(bound: Int) extends Module {
    val io = IO(new Bundle {
      val value = Output(UInt(log2Ceil(bound).W))
      val en = Input(Bool())
      val wrap = Output(Bool())
    })
    val count_reg = RegInit(UInt(log2Ceil(bound).W), 0.U)
    val wrap: Bool = Wire(Bool())
    wrap := (count_reg === (bound - 1).U)
    io.wrap := wrap
    when(io.en) {
      when(wrap) {
        count_reg := 0.U
      } otherwise {
        count_reg := count_reg + 1.U
      }
    }
    io.value := count_reg
  }

  val x_counter = Module(new WrappingCounter(DimX))
  val y_counter = Module(new WrappingCounter(DimY))

  x_counter.io.en := false.B
  y_counter.io.en := false.B

  def xyCountUp(): Unit = {
    x_counter.io.en := true.B
    y_counter.io.en := x_counter.io.wrap
  }

  val dest_x: UInt = Reg(UInt(log2Ceil(DimX).W))
  val dest_y: UInt = Reg(UInt(log2Ceil(DimY).W))

  io.running := (phase === Phase.Running)
  io.packet_out.valid := false.B

  io.packet_out.data := io.cache_frontend.rdata
  io.packet_out.xHops := dest_x
  io.packet_out.yHops := dest_y
  io.packet_out.address := 0.U

  io.cache_frontend.start := false.B
  io.cache_frontend.wdata := 0.U
  io.cache_frontend.addr := instruction_stream_addr_reg
  io.cache_frontend.cmd := CacheCommand.Read


  enable_addr_increment := false.B

//  io.global_synch := io.core_active.forall(_ === false.B)


  val phase_next: Phase.Type = Reg(Phase.Type(), Phase.Idle)


  switch(phase) {
    is(Phase.Idle) {
      when(io.start) {
        phase := Phase.StartCacheRead
        phase_next := Phase.StreamDest
        delay_value := (DimX * DimY - (DimX + DimY - 1)).U
        instruction_stream_addr_reg := io.instruction_stream_base
      }
    }

    is(Phase.StartCacheRead) {
      phase := phase_next
      io.cache_frontend.start := true.B
      enable_addr_increment := true.B
    }

    is(Phase.StreamDest) {
      when(io.cache_frontend.done) {
        dest_x := io.cache_frontend.rdata.tail(config.DataBits / 2)
        dest_y := io.cache_frontend.rdata.head(config.DataBits / 2)

        xyCountUp()

        phase := Phase.StartCacheRead
        phase_next := Phase.StreamBodyLength
      }
    }

    is(Phase.StreamBodyLength) {
      when(io.cache_frontend.done) {
        io.packet_out.valid := true.B
        phase := Phase.StartCacheRead
        require(config.NumBits % config.DataBits == 0, "Instruction length should be aligned to data length")
        body_length_end :=
          instruction_stream_addr_reg + (io.cache_frontend.rdata << log2Ceil(config.NumBits / config.DataBits))
        when(io.cache_frontend.rdata === 0.U) {
          // no instructions
          phase_next := Phase.StreamEpilogueLength
        } otherwise {
          phase_next := Phase.StreamInstruction
        }
      }
    }

    is(Phase.StreamInstruction) {
      when(io.cache_frontend.done) {
        io.packet_out.valid := true.B
        when(instruction_stream_addr_reg === body_length_end) {
          phase := Phase.StartCacheRead
          phase_next := Phase.StreamEpilogueLength
        } otherwise {
          phase := Phase.StartCacheRead
          phase_next := Phase.StreamInstruction
        }
      }
    }

    is(Phase.StreamEpilogueLength) {
      when(io.cache_frontend.done) {
        io.packet_out.valid := true.B
        phase := Phase.StartCacheRead
        phase_next := Phase.StreamSleepLength
      }
    }

    is(Phase.StreamSleepLength) {
      when(io.cache_frontend.done) {
        io.packet_out.valid := true.B
        when(x_counter.io.value === 0.U && y_counter.io.value === 0.U) {
          // streamed to all PEs, now need to set the countdown!
          phase := Phase.StreamCountDown
        } otherwise {
          // still some PEs are not programmed
          phase := Phase.StartCacheRead
          phase_next := Phase.StreamDest
        }
      }
    }
    is(Phase.StreamCountDown) {
      xyCountUp()
      io.packet_out.xHops := (DimX - 1).U - x_counter.io.value
      io.packet_out.yHops := (DimY - 1).U - y_counter.io.value
      io.packet_out.valid := true.B
      io.packet_out.data := delay_value
//      delay_value := delay_value - 1.U

      when(x_counter.io.wrap) {
        delay_value := delay_value - (DimX - 1).U
      }
      when(x_counter.io.wrap && y_counter.io.wrap) {
        // streaming the last one
        phase := Phase.WaitForCountDownEnd1
      } otherwise {
        phase := Phase.StreamCountDown
      }
    }

    is (Phase.WaitForCountDownEnd1) {
      phase := Phase.WaitForCountDownEnd2
    }
    is (Phase.WaitForCountDownEnd2) {
      phase := Phase.Running
    }

    is(Phase.Running) {
      when(io.finish) {
        phase := Phase.Idle
      }
    }
  }
}
