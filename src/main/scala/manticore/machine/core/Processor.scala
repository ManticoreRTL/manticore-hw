package manticore.machine.core

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum
import chisel3.stage.ChiselStage
import manticore.machine.ISA
import manticore.machine.ManticoreBaseISA
import manticore.machine.ManticoreFullISA
import manticore.machine.memory.CacheConfig
import manticore.machine.memory.CacheFrontInterface
import manticore.machine.memory.MemStyle
import manticore.machine.memory.SimpleDualPortMemory

import scala.util.Random
import manticore.machine.Helpers
import manticore.machine.memory.CacheCommand

class NamedError(nameBits: Int) extends Bundle {
  val error: Bool = Bool()
  val id: UInt    = UInt(nameBits.W)
}

class PeripheryProcessorInterface(config: ISA) extends Bundle {

  val active: Bool = Output(
    Bool()
  ) // high if the processor is in the execution phase
  val cache: CacheFrontInterface = Flipped(CacheConfig.frontInterface())
  val exception: NamedError      = Output(new NamedError(config.DataBits))
  val debug_time: UInt           = Input(UInt(64.W))
  val dynamic_cycle: Bool        = Output(Bool())
}
class ProcessorInterface(config: ISA, DimX: Int, DimY: Int) extends Bundle {
  val packet_in  = Input(new BareNoCBundle(config))
  val packet_out = Output(NoCBundle(DimX, DimY, config))
  val periphery: PeripheryProcessorInterface = new PeripheryProcessorInterface(
    config
  )
}

class ProcessorWithSendRecvPipe(
    config: ISA,
    DimX: Int,
    DimY: Int,
    x: Int,
    y: Int,
    equations: Seq[Seq[BigInt]],
    initial_registers: String = "",
    initial_array: String = "",
    name_tag: String = "core",
    debug_enable: Boolean = false,
    debug_level: Int = 0,
    enable_custom_alu: Boolean = true
) extends Module {
  val io: ProcessorInterface = IO(new ProcessorInterface(config, DimX, DimY))

  val processor = Module(
    new Processor(
      config,
      DimX,
      DimY,
      equations,
      initial_registers,
      initial_array,
      name_tag,
      debug_enable,
      debug_level,
      enable_custom_alu
    )
  )
  processor.suggestName("processor")

  val sendRecvPipe = Module(
    new SendRecvPipe(
      config,
      DimX,
      DimY,
      x,
      y
    )
  )
  sendRecvPipe.suggestName("sendRecvPipe")

  // sendRecvPipe inputs.
  sendRecvPipe.io.send.in := processor.io.packet_out
  sendRecvPipe.io.recv.in := io.packet_in

  // sendRecvPipe outputs.
  io.packet_out          := sendRecvPipe.io.send.out
  processor.io.packet_in := sendRecvPipe.io.recv.out

  io.periphery <> processor.io.periphery

}

class SendRecvPipe(
    config: ISA,
    DimX: Int,
    DimY: Int,
    x: Int,
    y: Int
) extends Module {

  val io = IO(new Bundle {
    val send = new Bundle {
      val in  = Input(NoCBundle(DimX, DimY, config))
      val out = Output(NoCBundle(DimX, DimY, config))
    }
    val recv = new Bundle {
      val in  = Input(new BareNoCBundle(config))
      val out = Output(new BareNoCBundle(config))
    }
  })

  // We removed 7 pipeline stages related to sending packets out from inside the core.
  // These stages have been added outside the core such that they allow vivado to have
  // flexibility in placing switches on the chip.
  val latency            = 7
  val procSideLatency    = 3
  val slrCrossingLatency = 2
  val switchSideLatency  = 2
  require(latency == procSideLatency + slrCrossingLatency + switchSideLatency)

  class ProcessorToSwitchPipe extends Module {
    val io = IO(new Bundle {
      val from_proc = Input(NoCBundle(DimX, DimY, config))
      val to_switch = Output(NoCBundle(DimX, DimY, config))
    })

    if (x == 0 && y == 0) {
      // Privileged core is placed close to the privileged switch, so no need for pure registers as pipeline registers.
      // We use an SRL alternative to save space.
      val srlStyle = Helpers.SrlStyle.RegSrl
      io.to_switch := Helpers.InlinePipeWithStyle(io.from_proc, latency, srlStyle)
    } else {
      val srlStyle              = Helpers.SrlStyle.Reg
      val procSide              = Helpers.WrappedPipeWithStyle(io.from_proc, procSideLatency, srlStyle)
      val slrCrossingProcSide   = Helpers.WrappedPipeWithStyle(procSide, 1, srlStyle)
      val slrCrossingSwitchSide = Helpers.WrappedPipeWithStyle(slrCrossingProcSide, 1, srlStyle)
      val switchSide            = Helpers.WrappedPipeWithStyle(slrCrossingSwitchSide, switchSideLatency, srlStyle)
      io.to_switch := switchSide
    }
  }

  class SwitchToProcessorPipe extends Module {
    val io = IO(new Bundle {
      val from_switch = Input(new BareNoCBundle(config))
      val to_proc     = Output(new BareNoCBundle(config))
    })

    if (x == 0 && y == 0) {
      // Privileged switch is placed close to the privileged core, so no need fore pure registers as pipeline registers.
      // We use an SRL alternative to save space.
      val srlStyle = Helpers.SrlStyle.RegSrl
      io.to_proc := Helpers.InlinePipeWithStyle(io.from_switch, latency, srlStyle)
    } else {
      val srlStyle              = Helpers.SrlStyle.Reg
      val switchSide            = Helpers.WrappedPipeWithStyle(io.from_switch, switchSideLatency, srlStyle)
      val slrCrossingSwitchSide = Helpers.WrappedPipeWithStyle(switchSide, 1, srlStyle)
      val slrCrossingProcSide   = Helpers.WrappedPipeWithStyle(slrCrossingSwitchSide, 1, srlStyle)
      val procSide              = Helpers.WrappedPipeWithStyle(slrCrossingProcSide, procSideLatency, srlStyle)
      io.to_proc := procSide
    }
  }

  val procToSwitchPipe = Module(new ProcessorToSwitchPipe)
  val switchToProcPipe = Module(new SwitchToProcessorPipe)

  // inputs
  procToSwitchPipe.io.from_proc   := io.send.in
  switchToProcPipe.io.from_switch := io.recv.in

  // outputs
  io.send.out := procToSwitchPipe.io.to_switch
  io.recv.out := switchToProcPipe.io.to_proc
}

class Processor(
    config: ISA,
    DimX: Int,
    DimY: Int,
    equations: Seq[Seq[BigInt]],
    initial_registers: String = "",
    initial_array: String = "",
    name_tag: String = "core",
    debug_enable: Boolean = false,
    debug_level: Int = 0,
    enable_custom_alu: Boolean = true
) extends Module {
  val io: ProcessorInterface = IO(new ProcessorInterface(config, DimX, DimY))

  def RegNext3[T <: Data](src: T): T = {
    RegNext(RegNext(RegNext(src)))
  }
  def RegNext2[T <: Data](src: T): T = RegNext(RegNext(src))

  object ProcessorPhase extends ChiselEnum {
    val DynamicReceiveProgramLength, // wait for the first message that indicates the length of the program
    DynamicReceiveInstruction,       // wait for a dynamic number of cycles to receive all instructions
    DynamicReceiveEpilogueLength,    // wait for a message that contains the epilogue length of the program
    DynamicReceiveSleepLength,       // wait for dynamic number of cycles to receive the sleep period after
    // execution which is essentially the number of messages the processor is expected to receive from other processors
    DynamicReceiveCountDown, // wait for the last message that determines the count-down period
    StaticCountDown,         // count down to start the processor execution
    StaticExecutionPhase,    // static execution phase, the processor runs the instructions
    StaticSleepPhase = Value

  }

  def dprintf(fmt: String, data: Bits*) =
    if (debug_enable) {
      printf(
        s"[%d : ${name_tag}] " + fmt,
        (io.periphery.debug_time +: data): _*
      )
    }

  val state =
    RegInit(ProcessorPhase(), ProcessorPhase.DynamicReceiveProgramLength)
  val soft_reset = Wire(Bool())

  // the timer should count at least up to 2^12, because there could be 2^12 instructions in this processor, however,
  // since other processors should initialize as well, and there are DIMX * DIMY of them, we need 256 * 2 ^ 12 for
  // initialization (upper bound). But let's go with 2^22 reading since streaming form the DRAM could take longer
  val countdown_timer = Reg(UInt(config.DataBits.W))

  val program_body_length     = Reg(UInt(config.NumPcBits.W))
  val program_epilogue_length = Reg(UInt(config.NumPcBits.W))
  val program_sleep_length    = Reg(UInt(config.NumPcBits.W))

  val program_pointer = Reg(UInt(config.NumPcBits.W))

  require(
    config.NumPcBits / 8 <= 8,
    "Can only support up to 64-bit instructions"
  )
  require(
    config.DataBits == 16,
    "Not sure if can do other than 16-bit data path"
  )
  val NumInstructionChunks: Int = config.NumBits / config.DataBits
  val inst_builder_pos: UInt    = Reg(UInt(NumInstructionChunks.W))
  val inst_builder_reg: Vec[UInt] = Reg(
    Vec(NumInstructionChunks - 1, UInt(config.DataBits.W))
  )

  val fetch_stage  = Module(new Fetch(config))
  val decode_stage = Module(new Decode(config))
  val execute_stage = Module(
    new ExecuteComb(config, equations, name_tag + "::exec", debug_enable, enable_custom_alu)
  )

  if (debug_enable) {
    execute_stage.io.debug_time := io.periphery.debug_time
  } else {
    execute_stage.io.debug_time := 0.U
  }

  val memory_stage = Module(new MemoryAccess(config, DimX, DimY))

  val register_file = Module(new RegisterFile(config, initial_registers, enable_custom_alu))
  // val carry_register_file = Module(new CarryRegisterFile(config))

  val lut_load_regs = Module(new LutLoadDataRegisterFile(config, enable_custom_alu))

  val array_memory = Module(
    new SimpleDualPortMemory(
      ADDRESS_WIDTH = 14,
      DATA_WIDTH = config.DataBits,
      STYLE = MemStyle.URAMReal,
      INIT = initial_array
    )
  )

  val skip_exec            = Wire(Bool())
  val skip_sleep           = Wire(Bool())
  val total_program_length = Wire(UInt(config.NumPcBits.W))

  total_program_length := (program_epilogue_length + program_body_length)
  skip_exec            := (total_program_length === 0.U)
  skip_sleep           := (program_sleep_length === 0.U)

  fetch_stage.io.programmer.enable := false.B
  fetch_stage.io.programmer.instruction := DontCare

  fetch_stage.io.programmer.address := program_pointer

  soft_reset := false.B
  switch(state) {
    is(ProcessorPhase.DynamicReceiveProgramLength) {
      soft_reset := true.B
      when(io.packet_in.valid) {
        program_body_length := io.packet_in.data
        when(io.packet_in.data === 0.U) {
          dprintf(
            "\tState <= DynamicReceiveEpilogueLength\tprogram_body_length <= %d\n",
            io.packet_in.data
          )
          state := ProcessorPhase.DynamicReceiveEpilogueLength
        } otherwise {
          dprintf(
            "\tState <= DynamicReceiveInstruction\tprogram_body_length <= %d\n",
            io.packet_in.data
          )
          state            := ProcessorPhase.DynamicReceiveInstruction
          inst_builder_pos := 1.U
        }
        program_pointer := 0.U
      }
    }
    is(ProcessorPhase.DynamicReceiveInstruction) {

      when(io.packet_in.valid) {
        when(inst_builder_pos.head(1) === 1.U) {
          // received all the pieces
          fetch_stage.io.programmer.enable := true.B
          fetch_stage.io.programmer.instruction := Cat(
            io.packet_in.data +: inst_builder_reg
          )
          program_pointer := program_pointer + 1.U
          when(program_pointer + 1.U === program_body_length) {
            state := ProcessorPhase.DynamicReceiveEpilogueLength
          } otherwise {
            state := ProcessorPhase.DynamicReceiveInstruction
          }

        }
        // shift left rotate the position by one bit
        inst_builder_pos := inst_builder_pos.tail(1) ## inst_builder_pos.head(1)
        // shift the new chunk in
        inst_builder_reg(0) := io.packet_in.data
        for (i <- 1 until inst_builder_reg.size) {
          inst_builder_reg(i) := inst_builder_reg(i - 1)
        }
      }

    }

    is(ProcessorPhase.DynamicReceiveEpilogueLength) {
      when(io.packet_in.valid) {
        program_epilogue_length := io.packet_in.data
        state                   := ProcessorPhase.DynamicReceiveSleepLength
        dprintf("\tprogram_epilogue_length <= %d\n", io.packet_in.data)
      } otherwise {
        state := ProcessorPhase.DynamicReceiveEpilogueLength
      }
    }

    is(ProcessorPhase.DynamicReceiveSleepLength) {
      when(io.packet_in.valid) {
        program_sleep_length := io.packet_in.data
        state                := ProcessorPhase.DynamicReceiveCountDown
        dprintf("\tprogram_sleep_length <= %d\n", io.packet_in.data)
      } otherwise {
        state := ProcessorPhase.DynamicReceiveSleepLength
      }
    }

    is(ProcessorPhase.DynamicReceiveCountDown) {
      when(io.packet_in.valid) {
        countdown_timer := io.packet_in.data
        state           := ProcessorPhase.StaticCountDown
        dprintf("\tcountdown_timer <= %d\n", io.packet_in.data)
      } otherwise {
        state := ProcessorPhase.DynamicReceiveCountDown
      }
    }

    is(ProcessorPhase.StaticCountDown) {
      when(countdown_timer === 0.U) {
        when(skip_exec) {
          state           := ProcessorPhase.StaticSleepPhase
          countdown_timer := program_sleep_length
          dprintf(
            "\tsleep phase begins next cycle (processor has no program)\n"
          )
        } otherwise {
          state           := ProcessorPhase.StaticExecutionPhase
          countdown_timer := total_program_length
          dprintf("\texecution phase begins next cycle\n")
        }
      } otherwise {
        state           := ProcessorPhase.StaticCountDown
        countdown_timer := countdown_timer - 1.U
      }
    }

    is(ProcessorPhase.StaticExecutionPhase) {

      when(countdown_timer === 1.U) {
        when(skip_sleep) {
          state := ProcessorPhase.StaticExecutionPhase
          dprintf(
            "\tWARNING! execution phase begins next cycle (processor does not sleep)\n"
          )
          countdown_timer := total_program_length
        } otherwise {
          state           := ProcessorPhase.StaticSleepPhase
          countdown_timer := program_sleep_length
          dprintf("\tsleep phase begins next cycle\n")
        }
      } otherwise {
        state           := ProcessorPhase.StaticExecutionPhase
        countdown_timer := countdown_timer - 1.U
      }

      when(io.packet_in.valid) {
        program_pointer                  := program_pointer + 1.U
        fetch_stage.io.programmer.enable := true.B
        fetch_stage.io.programmer.instruction :=
          Cat(
            Seq(
              io.packet_in.data,
              0.U((config.IdBits * 4 - config.DataBits).W),
              0.U(config.FunctBits.W),
              io.packet_in.address,
              config.SetValue.value.U(config.OpcodeBits.W)
            )
          )
      } otherwise {
        fetch_stage.io.programmer.enable := false.B
      }
    }

    is(ProcessorPhase.StaticSleepPhase) {
      when(countdown_timer === 1.U) {
        when(skip_exec) {
          state           := ProcessorPhase.StaticSleepPhase
          countdown_timer := program_sleep_length
        } otherwise {
          state           := ProcessorPhase.StaticExecutionPhase
          countdown_timer := total_program_length
          dprintf("\texecution phase begins next cycle (wake-up)\n")
        }
        program_pointer := program_body_length
      } otherwise {
        state           := ProcessorPhase.StaticSleepPhase
        countdown_timer := countdown_timer - 1.U
      }
    }
  }

  fetch_stage.io.execution_enable     := (state === ProcessorPhase.StaticExecutionPhase)
  fetch_stage.io.is_final_instruction := (countdown_timer === 1.U)
  io.periphery.active                 := (state === ProcessorPhase.StaticExecutionPhase)

  class RegisterWriteByPass extends Bundle {
    val value   = UInt(config.DataBits.W)
    val address = UInt(config.IdBits.W)
    val en      = Bool()
  }
  val register_bypass = Reg(new RegisterWriteByPass)
  register_bypass.value   := memory_stage.io.pipe_out.result
  register_bypass.address := memory_stage.io.pipe_out.rd
  register_bypass.en      := memory_stage.io.pipe_out.write_back
  val forwarding_signals =
    if (config.forwarding) {
      Seq(
        ForwardingTuple(
          execute_stage.io.pipe_out.result,
          execute_stage.io.pipe_out.rd,
          execute_stage.io.pipe_out.opcode.arith || execute_stage.io.pipe_out.opcode.cust
        ),
        ForwardingTuple(
          memory_stage.io.pipe_out.result,
          memory_stage.io.pipe_out.rd,
          memory_stage.io.pipe_out.write_back
        ),
        ForwardingTuple(
          register_bypass.value,
          register_bypass.address,
          register_bypass.en
        )
      )
    } else {
      Seq.empty
    }

  // fetch --> decode
  decode_stage.io.instruction := fetch_stage.io.instruction

  // decode --> exec
  execute_stage.io.pipe_in := decode_stage.io.pipe_out

  execute_stage.io.regs_in.rs1 := ForwardPath(
    register_file.io.rs1.dout,
    decode_stage.io.pipe_out.rs1,
    forwarding_signals
  )
  execute_stage.io.regs_in.rs2 := ForwardPath(
    register_file.io.rs2.dout,
    decode_stage.io.pipe_out.rs2,
    forwarding_signals
  )
  execute_stage.io.regs_in.rs3 := ForwardPath(
    register_file.io.rs3.dout(config.DataBits - 1, 0),
    decode_stage.io.pipe_out.rs3,
    forwarding_signals
  )
  execute_stage.io.regs_in.rs4 := ForwardPath(
    register_file.io.rs4.dout,
    decode_stage.io.pipe_out.rs4,
    forwarding_signals
  )
  execute_stage.io.carry_in := RegNext(register_file.io.rs3.dout(config.DataBits))
  register_file.io.rs1.addr := decode_stage.io.pipe_out.rs1
  register_file.io.rs2.addr := decode_stage.io.pipe_out.rs2
  register_file.io.rs3.addr := decode_stage.io.pipe_out.rs3
  register_file.io.rs4.addr := decode_stage.io.pipe_out.rs4

  register_file.io.w.addr := memory_stage.io.pipe_out.rd

  register_file.io.w.din := Cat(
    RegNext3(execute_stage.io.carry_wen & execute_stage.io.carry_out),
    memory_stage.io.pipe_out.result
  )

  register_file.io.w.en := memory_stage.io.pipe_out.write_back

  lut_load_regs.io.din         := decode_stage.io.pipe_out.immediate
  lut_load_regs.io.wen         := decode_stage.io.pipe_out.opcode.config_cfu
  execute_stage.io.lutdata_din := lut_load_regs.io.dout

  // exec --> memory and write back implementation
  memory_stage.io.local_memory_interface <> array_memory.io
  memory_stage.io.local_memory_interface.dout := array_memory.io.dout
  memory_stage.io.pipe_in                     := execute_stage.io.pipe_out

  register_file.io.w.addr := memory_stage.io.pipe_out.rd

  // io.packet_out := memory_stage.io.pipe_out.packet
  val hop_bits: Int = decode_stage.io.pipe_out.immediate.getWidth / 2
  io.packet_out.xHops   := RegNext(decode_stage.io.pipe_out.immediate.tail(hop_bits))
  io.packet_out.yHops   := RegNext(decode_stage.io.pipe_out.immediate.head(hop_bits))
  io.packet_out.data    := register_file.io.rs2.dout
  io.packet_out.address := RegNext(decode_stage.io.pipe_out.rd)
  io.packet_out.valid   := RegNext(decode_stage.io.pipe_out.opcode.send)

  if (config.WithGlobalMemory) {
    memory_stage.io.global_memory_interface <> io.periphery.cache
  } else {
    io.periphery.cache <> DontCare
    memory_stage.io.global_memory_interface <> DontCare
  }

  // error bit to check whether a memory response came back
  val gmem_expect_response = Reg(Bool())
  gmem_expect_response := memory_stage.io.global_memory_interface.start
  val gmem_failure = withReset(soft_reset) { RegInit(false.B) }

  gmem_failure := (gmem_expect_response && !io.periphery.cache.done) || gmem_failure

  val exception_occurred: Bool = withReset(soft_reset) { RegInit(false.B) }
  val exception_id: UInt       = Reg(UInt(config.DataBits.W))
  val exception_cond =
    RegNext(decode_stage.io.pipe_out.opcode.expect) && !RegNext(register_file.io.rs1.dout === register_file.io.rs2.dout)

  exception_occurred           := RegNext3(exception_cond)
  io.periphery.exception.id    := RegNext2(RegEnable(RegNext(decode_stage.io.pipe_out.immediate), exception_cond))
  io.periphery.exception.error := exception_occurred

  io.periphery.dynamic_cycle := execute_stage.io.pipe_out.gmem.start

}

object ProcessorEmitter extends App {

  val rgen = Random

  val equations = Seq.fill(1 << ManticoreFullISA.FunctBits) {
    Seq.fill(ManticoreFullISA.DataBits) { BigInt(rgen.nextInt(1 << 16)) }
  }

  def makeProcessor() =
    new Processor(
      config = ManticoreFullISA,
      equations = equations,
      DimX = 16,
      DimY = 16,
      enable_custom_alu = true
    )

  new ChiselStage().emitVerilog(
    makeProcessor(),
    Array("--target-dir", "gen-dir/processor/")
  )

}
