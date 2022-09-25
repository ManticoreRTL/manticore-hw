package manticore.machine.core

import Chisel._
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

class NamedError(nameBits: Int) extends Bundle {
  val error: Bool = Bool()
  val id: UInt    = UInt(nameBits.W)
}

class PeripheryProcessorInterface(config: ISA) extends Bundle {

  val active: Bool = Output(
    Bool()
  ) // high if the processor is in the execution phase
  val cache: CacheFrontInterface      = Flipped(CacheConfig.frontInterface())
  val gmem_access_failure_error: Bool = Output(Bool())
  val exception: NamedError           = Output(new NamedError(config.DataBits))
  val debug_time: UInt                = Input(UInt(64.W))
  val dynamic_cycle: Bool             = Output(Bool())
}
class ProcessorInterface(config: ISA, DimX: Int, DimY: Int) extends Bundle {

  val packet_in  = Input(new BareNoCBundle(config))
  val packet_out = Output(NoCBundle(DimX, DimY, config))
  val periphery: PeripheryProcessorInterface = new PeripheryProcessorInterface(
    config
  )

}

class ProcessorWithSendPipe(
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

  io.periphery <> processor.io.periphery

  // We removed 7 pipeline stages related to sending packets out from inside the core.
  // These stages have been added outside the core such that they allow vivado to have
  // flexibility in placing switches on the chip.

  // These go from the cores to the switch island.
  // 1-3 are in the core SLR.
  // 4   is in the core SLR LAGUNA cell.
  // 5   is in the switch SLR LAGUNA cell.
  // 6-7 are in the switch SLR.
  io.packet_out := Helpers.SlrCrossing(processor.io.packet_out, 7, Set(4, 5))

  // These come from the switch island to the cores.
  // 1-2 are in the switch SLR.
  // 3   is in the switch SLR LAGUNA cell.
  // 4   is in the core SLR LAGUNA cell.
  // 5-7 are in the core SLR.
  processor.io.packet_in := Helpers.SlrCrossing(io.packet_in, 7, Set(3, 4))

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

  val multiplier_res_high = Wire(UInt(config.DataBits.W))
  val multiplier_res_low  = Wire(UInt(config.DataBits.W))

  val skip_exec            = Wire(Bool())
  val skip_sleep           = Wire(Bool())
  val total_program_length = Wire(UInt(config.NumPcBits.W))

  total_program_length := (program_epilogue_length + program_body_length)
  skip_exec            := (total_program_length === 0.U)
  skip_sleep           := (program_sleep_length === 0.U)

  fetch_stage.io.programmer.enable := false.B

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
          fetch_stage.io.programmer.address := program_pointer
          program_pointer                   := program_pointer + 1.U
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
  execute_stage.io.valid_in := decode_stage.io.pipe_out.opcode.mul || decode_stage.io.pipe_out.opcode.mulh
  register_file.io.rs1.addr := decode_stage.io.pipe_out.rs1
  register_file.io.rs2.addr := decode_stage.io.pipe_out.rs2
  register_file.io.rs3.addr := decode_stage.io.pipe_out.rs3
  register_file.io.rs4.addr := decode_stage.io.pipe_out.rs4

  multiplier_res_high := memory_stage.io.pipe_out.result_mul(2 * config.DataBits - 1, config.DataBits)
  multiplier_res_low  := memory_stage.io.pipe_out.result_mul(config.DataBits - 1, 0)

  register_file.io.w.addr := memory_stage.io.pipe_out.rd
  when(memory_stage.io.valid_out) {
    register_file.io.w.din := Cat(
      0.U(1.W),
      Mux(memory_stage.io.pipe_out.mulh, multiplier_res_high, multiplier_res_low)
    )
  } otherwise {
    register_file.io.w.din := Cat(
      RegNext3(execute_stage.io.carry_wen & execute_stage.io.carry_out),
      memory_stage.io.pipe_out.result
    )
  }
  register_file.io.w.en := memory_stage.io.pipe_out.write_back

  lut_load_regs.io.din         := decode_stage.io.pipe_out.immediate
  lut_load_regs.io.wen         := decode_stage.io.pipe_out.opcode.config_cfu
  execute_stage.io.lutdata_din := lut_load_regs.io.dout

  // exec --> memory and write back implementation
  memory_stage.io.local_memory_interface <> array_memory.io
  memory_stage.io.local_memory_interface.dout := array_memory.io.dout
  memory_stage.io.pipe_in                     := execute_stage.io.pipe_out
  memory_stage.io.valid_in                    := execute_stage.io.valid_out

  register_file.io.w.addr := memory_stage.io.pipe_out.rd

  // io.packet_out := memory_stage.io.pipe_out.packet
  val hop_bits: Int = decode_stage.io.pipe_out.immediate.getWidth / 2
  io.packet_out.xHops   := RegNext(decode_stage.io.pipe_out.immediate.tail(hop_bits))
  io.packet_out.yHops   := RegNext(decode_stage.io.pipe_out.immediate.head(hop_bits))
  io.packet_out.data    := register_file.io.rs2.dout
  io.packet_out.address := RegNext(decode_stage.io.pipe_out.rd)
  io.packet_out.valid   := RegNext(decode_stage.io.pipe_out.opcode.send)

  if (config.WithGlobalMemory) {
    memory_stage.io.global_memory_interface ==> io.periphery.cache
  }

  // error bit to check whether a memory response came back
  val gmem_expect_response = Reg(Bool())
  gmem_expect_response := memory_stage.io.global_memory_interface.start
  val gmem_failure = Reg(Bool())
  gmem_failure := (gmem_expect_response && !io.periphery.cache.done) || gmem_failure

  val exception_occurred: Bool = Reg(Bool())
  val exception_id: UInt       = Reg(UInt(config.DataBits.W))

  val exception_cond: Bool = Wire(Bool())

  // Expect instruction should throw an exception if the result of SetEqual is false.
  // Using functionality related to the custom ALU is an error if the custom ALU has been disabled.
  exception_cond := (execute_stage.io.pipe_out.opcode.expect && execute_stage.io.pipe_out.result === 0.U) ||
    (!enable_custom_alu.B &&
      (decode_stage.io.pipe_out.opcode.config_cfu ||
        decode_stage.io.pipe_out.opcode.cust))

  exception_occurred := exception_cond

  when(exception_cond) {
    exception_id := execute_stage.io.pipe_out.immediate
  }

  io.periphery.exception.id    := exception_id
  io.periphery.exception.error := exception_occurred
  when(soft_reset) {
    gmem_failure       := false.B
    exception_occurred := false.B
  }

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
