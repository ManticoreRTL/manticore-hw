package manticore.machine.core

import Chisel._
import chisel3.experimental.ChiselEnum
import manticore.machine.memory.{
  CacheConfig,
  CacheFrontInterface,
  MemStyle,
  SimpleDualPortMemory
}
import manticore.machine.{ISA, ManticoreBaseISA}

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

class Processor(
    config: ISA,
    DimX: Int,
    DimY: Int,
    equations: Seq[Seq[Int]],
    initial_registers: String = "",
    initial_array: String = "",
    name_tag: String = "core",
    debug_enable: Boolean = false,
    debug_level: Int = 0
) extends Module {
  //  val EQUATIONS = Seq.fill(1 << config.FUNCT_BITS)(Seq.fill(config.DATA_BITS)(rdgen.nextInt(16)))

  val io: ProcessorInterface = IO(new ProcessorInterface(config, DimX, DimY))

  object ProcessorPhase extends ChiselEnum {
    val DynamicReceiveProgramLength, // wait for the first message that indicates the length of the program
    DynamicReceiveInstruction, // wait for a dynamic number of cycles to receive all instructions
    DynamicReceiveEpilogueLength, // wait for a message that contains the epilogue length of the program
    DynamicReceiveSleepLength, // wait for dynamic number of cycles to receive the sleep period after
    // execution which is essentially the number of messages the processor is expected to receive from other processors
    DynamicReceiveCountDown, // wait for the last message that determines the count-down period
    StaticCountDown, // count down to start the processor execution
    StaticExecutionPhase, // static execution phase, the processor runs the instructions
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
  val execute_stage: ExecuteBase = Module(
    new ExecuteComb(config, equations, name_tag + "::exec", debug_enable)
  )

  if (debug_enable)
    execute_stage.io.debug_time := io.periphery.debug_time

  val memory_stage = Module(new MemoryAccess(config, DimX, DimY))

  val register_file       = Module(new RegisterFile(config, initial_registers))
  val carry_register_file = Module(new CarryRegisterFile(config))

  val array_memory = Module(
    new SimpleDualPortMemory(
      ADDRESS_WIDTH = 12,
      DATA_WIDTH = config.DataBits,
      STYLE = MemStyle.BRAM,
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

  fetch_stage.io.execution_enable := (state === ProcessorPhase.StaticExecutionPhase)
  io.periphery.active := (state === ProcessorPhase.StaticExecutionPhase) // || (
  //   state === ProcessorPhase.StaticSleepPhase &&
  //     (
  //       decode_stage.io.instruction =/= 0.U ||
  //         execute_stage.io.pipe_in.opcode.nop === false.B ||
  //         memory_stage.io.pipe_in.opcode.nop === false.B ||
  //         memory_stage.io.pipe_out.nop === false.B
  //     )
  // )

  // fetch --> decode
  decode_stage.io.instruction := fetch_stage.io.instruction

  // decode --> exec
  execute_stage.io.pipe_in   := decode_stage.io.pipe_out
  execute_stage.io.regs_in.x := register_file.io.rx.dout
  execute_stage.io.regs_in.y := register_file.io.ry.dout
  execute_stage.io.regs_in.u := register_file.io.ru.dout
  execute_stage.io.regs_in.v := register_file.io.rv.dout
  register_file.io.rx.addr   := decode_stage.io.pipe_out.rs1
  register_file.io.ry.addr   := decode_stage.io.pipe_out.rs2
  register_file.io.ru.addr   := decode_stage.io.pipe_out.rs3
  register_file.io.rv.addr   := decode_stage.io.pipe_out.rs4

  carry_register_file.io.raddr := decode_stage.io.pipe_out.rs3
  execute_stage.io.carry_in    := carry_register_file.io.dout
  carry_register_file.io.wen   := execute_stage.io.pipe_out.carry_wen
  carry_register_file.io.waddr := execute_stage.io.pipe_out.carry_rd
  carry_register_file.io.din   := execute_stage.io.pipe_out.carry_din

  // exec --> memory and write back implementation
  memory_stage.io.local_memory_interface <> array_memory.io
  memory_stage.io.local_memory_interface.dout := array_memory.io.dout

  memory_stage.io.pipe_in := execute_stage.io.pipe_out
  register_file.io.w.en := memory_stage.io.pipe_out.write_back & (memory_stage.io.pipe_out.rd =/= 0.U)

  def writeback_message(msg: String, value: UInt) = {
    when(memory_stage.io.pipe_out.write_back) {
      assert(
        memory_stage.io.pipe_out.rd =/= 0.U,
        s"\tError ignored R(0) <= %d (${msg})\n",
        value
      )
    }

  }

  if (config.WithGlobalMemory) {
    when(memory_stage.io.pipe_out.lload) {
      register_file.io.w.din := memory_stage.io.pipe_out.lmem_data
      writeback_message("LocalLoad", memory_stage.io.pipe_out.lmem_data)
    }.elsewhen(memory_stage.io.pipe_out.gload) {
      register_file.io.w.din := memory_stage.io.pipe_out.gmem_data
      writeback_message("GlobalLoad", memory_stage.io.pipe_out.gmem_data)
    } otherwise {
      writeback_message("WB", memory_stage.io.pipe_out.gmem_data)
      register_file.io.w.din := memory_stage.io.pipe_out.result
    }
  } else {
    when(memory_stage.io.pipe_out.lload) {
      register_file.io.w.din := memory_stage.io.pipe_out.lmem_data
      writeback_message("LocalLoad", memory_stage.io.pipe_out.lmem_data)
    } otherwise {
      register_file.io.w.din := memory_stage.io.pipe_out.result
      writeback_message("WB", memory_stage.io.pipe_out.lmem_data)
    }
  }

  register_file.io.w.addr := memory_stage.io.pipe_out.rd

  io.packet_out := memory_stage.io.pipe_out.packet

  if (config.WithGlobalMemory) {
    memory_stage.io.global_memory_interface ==> io.periphery.cache
  }

  // error bit to check whether a memory response came back
  val gmem_expect_response = Reg(Bool())
  gmem_expect_response := memory_stage.io.global_memory_interface.start
  val gmem_failure = Reg(Bool())
  gmem_failure := (gmem_expect_response && !io.periphery.cache.done) | gmem_failure

  val exception_occurred: Bool = Reg(Bool())
  val exception_id: UInt       = Reg(UInt(config.DataBits.W))

  val exception_cond: Bool = Wire(Bool())

  // Expect instruction should throw an exception if the result of SetEqual is false
  exception_cond := (execute_stage.io.pipe_out.opcode.expect && execute_stage.io.pipe_out.result === 0.U)

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
  val rdgen = new scala.util.Random(0)
  val equations: Seq[Seq[Int]] =
    Seq.fill(32)(Seq.fill(16)(rdgen.nextInt(1 << 16)))
//
//  def makeProcessor() =
//    new Processor(config = ThyrioISA, DimX = 16, DimY = 16,
//      equations = equations
//    )
//
//  new ChiselStage().emitVerilog(
//    makeProcessor(), Array("--target-dir", "gen-dir")
//  )

//  new ChiselStage().emitVerilog(
//    new ClockedProcessor(config = ThyrioISA, DimX = 2, DimY = 2,
//      equations = equations, "rf.dat", "ra.dat"
//    ), Array("--target-dir", "gen-dir")
//  )
}
