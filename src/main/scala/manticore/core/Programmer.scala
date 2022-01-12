package manticore.core

import Chisel._
import chisel3.experimental.ChiselEnum
import memory.{Cache, CacheCommand, CacheConfig, CacheFrontInterface}
import manticore.ISA
import org.scalatest.run

class ProgrammerInterface(config: ISA, DimX: Int, DimY: Int) extends Bundle {

  val packet_out: NoCBundle = Output(
    new NoCBundle(config = config, DimX = DimX, DimY = DimY)
  )
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
  val start: Bool                   = Input(Bool())
  val finish: Bool                  = Input(Bool())
//  val core_active: Vec[Bool] = Input(Vec(DimY * DimX, Bool()))
  val running: Bool = Output(Bool())

//  val global_synch: Bool = Output(Bool())
  val memory_backend = new MemoryReadWriteInterface(config)
}

class Programmer(config: ISA, DimX: Int, DimY: Int) extends Module {
  require(
    config.DataBits == 16,
    "Only 16-bit data width is supported with the cache implementation"
  )
  val io: ProgrammerInterface = IO(new ProgrammerInterface(config, DimX, DimY))

  object Phase extends ChiselEnum {
    val Idle, StartCacheRead, StreamDest, StreamBodyLength, StreamInstruction,
        StreamEpilogueLength, StreamSleepLength, StreamCountDown,
        // WaitForCountDownEndn states are only there to ensure io.running
        // goes high at the same time processors become active. Notice that
        // if more pipeline register are added on the io.packet_out path,
        // more WaitForCountDownEndn states are needed!
        WaitForCountDownEnd1, 
        WaitForCountDownEnd2, 
        WaitForCountDownEnd3,
        Running, Paused = Value
  }



  val phase: Phase.Type = RegInit(Phase.Type(), Phase.Idle)

  val instruction_stream_addr_reg: UInt = Reg(
    UInt(CacheConfig.UsedAddressBits.W)
  )
  val enable_addr_increment: Bool = Wire(Bool())

  require(CacheConfig.UsedAddressBits > config.NumPcBits)
  val body_length_end: UInt = Reg(UInt(CacheConfig.UsedAddressBits.W))

  val delay_value: UInt = Reg(UInt(config.DataBits.W))

  when(io.start) {
    instruction_stream_addr_reg := io.instruction_stream_base(
      CacheConfig.UsedAddressBits,
      1
    ) // drop the byte offset
  }.elsewhen(enable_addr_increment) {
    instruction_stream_addr_reg := instruction_stream_addr_reg + 1.U
  }

  class WrappingCounter(bound: Int) extends Module {
    val io = IO(new Bundle {
      val value = Output(UInt(log2Ceil(bound).W))
      val en    = Input(Bool())
      val wrap  = Output(Bool())
    })
    val count_reg  = RegInit(UInt(log2Ceil(bound).W), 0.U)
    val wrap: Bool = Wire(Bool())
    wrap    := (count_reg === (bound - 1).U)
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

  // register memory response for better timing closure
  val mem_resp_done = Reg(Bool())
  val mem_rdata     = Reg(io.memory_backend.rdata.cloneType)
  mem_rdata     := io.memory_backend.rdata
  mem_resp_done := io.memory_backend.done

  // register the output packet as well
  val packet_out = Reg(new NoCBundle(DimX, DimY, config))

  io.packet_out := packet_out

  io.running       := (phase === Phase.Running)
  packet_out.valid := false.B

  packet_out.data    := mem_rdata
  packet_out.xHops   := dest_x
  packet_out.yHops   := dest_y
  packet_out.address := 0.U

  io.memory_backend.start := false.B
  io.memory_backend.wdata := 0.U
  io.memory_backend.addr  := instruction_stream_addr_reg
  io.memory_backend.wen   := false.B

  enable_addr_increment := false.B

//  io.global_synch := io.core_active.forall(_ === false.B)

  val phase_next: Phase.Type = Reg(Phase.Type(), Phase.Idle)
  val running: Bool = Reg(Bool())
  switch(phase) {
    is(Phase.Idle) {
      
      when(io.start) {
        phase      := Phase.StartCacheRead
        phase_next := Phase.StreamDest
        // set the initial delay to be DimX * DimY  - (DimX + DimY - 1)
        // and first send it to the furthest processor, then go back in X
        // direction and give the same value because the packets at (x, y)
        // and (x - 1, y) arrive at the same time, however, when sending
        // delay D to (0, y) to (DimX - 1, y - 1) the delay should be
        // be decremented by DimX - 1 because the packet will arrive much
        // later (DimX - 1 cycles later) at (DimX - 1, y - 1). Note that
        // twe should fix the initial delay to a value such that
        // the last processors receives the delay value when the programmer
        // transitions to State.Running.
        delay_value := (DimX * DimY - (DimX + DimY - 1)).U // magic formula
        instruction_stream_addr_reg := io.instruction_stream_base
        
      }
      running := false.B
    }

    is(Phase.StartCacheRead) {
      phase                   := phase_next
      io.memory_backend.start := true.B
      enable_addr_increment   := true.B
    }

    is(Phase.StreamDest) {
      when(mem_resp_done) {
        dest_x := mem_rdata.tail(config.DataBits / 2)
        dest_y := mem_rdata.head(config.DataBits / 2)

        xyCountUp()

        phase      := Phase.StartCacheRead
        phase_next := Phase.StreamBodyLength
      }
    }

    is(Phase.StreamBodyLength) {
      when(mem_resp_done) {
        packet_out.valid := true.B
        phase            := Phase.StartCacheRead
        require(
          config.NumBits % config.DataBits == 0,
          "Instruction length should be aligned to data length"
        )
        body_length_end :=
          instruction_stream_addr_reg + (mem_rdata << log2Ceil(
            config.NumBits / config.DataBits
          ))
        when(mem_rdata === 0.U) {
          // no instructions
          phase_next := Phase.StreamEpilogueLength
        } otherwise {
          phase_next := Phase.StreamInstruction
        }
      }
    }

    is(Phase.StreamInstruction) {
      when(mem_resp_done) {
        packet_out.valid := true.B
        when(instruction_stream_addr_reg === body_length_end) {
          phase      := Phase.StartCacheRead
          phase_next := Phase.StreamEpilogueLength
        } otherwise {
          phase      := Phase.StartCacheRead
          phase_next := Phase.StreamInstruction
        }
      }
    }

    is(Phase.StreamEpilogueLength) {
      when(mem_resp_done) {
        packet_out.valid := true.B
        phase            := Phase.StartCacheRead
        phase_next       := Phase.StreamSleepLength
      }
    }

    is(Phase.StreamSleepLength) {
      when(mem_resp_done) {
        packet_out.valid := true.B
        when(x_counter.io.value === 0.U && y_counter.io.value === 0.U) {
          // streamed to all PEs, now need to set the countdown!
          phase := Phase.StreamCountDown
        } otherwise {
          // still some PEs are not programmed
          phase      := Phase.StartCacheRead
          phase_next := Phase.StreamDest
        }
      }
    }
    is(Phase.StreamCountDown) {
      xyCountUp()
      packet_out.xHops := (DimX - 1).U - x_counter.io.value
      packet_out.yHops := (DimY - 1).U - y_counter.io.value
      packet_out.valid := true.B
      packet_out.data  := delay_value
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

    // these are "wait" states so that the delay packets arrive at the processor
    // and only when all processors are active
    is(Phase.WaitForCountDownEnd1) {
      phase := Phase.WaitForCountDownEnd2
    }
    is(Phase.WaitForCountDownEnd2) {
      phase := Phase.WaitForCountDownEnd3
      
    }
    is(Phase.WaitForCountDownEnd3) {
      phase := Phase.Running
      running := true.B
    }

    is(Phase.Running) {
      when(io.finish) {
        phase := Phase.Idle
      }
    }
  }
}
