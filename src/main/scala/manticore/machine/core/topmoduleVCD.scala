package manticore.machine.core

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util.is
import chisel3.util._
import chisel3.util.switch
import chisel3.stage.ChiselStage
import manticore.machine.ISA
import manticore.machine.ManticoreBaseISA
import manticore.machine.ManticoreFullISA
import manticore.machine.memory.SimpleDualPortMemory
import manticore.machine.xrt.AxiMemoryModelSimInterface
import manticore.machine.memory.MemStyle
import manticore.machine.xrt.AxiParameters
import manticore.machine.xrt.AxiMasterIF
import manticore.machine.memory.CacheConfig
import manticore.machine
import scala.math


class topmoduleVCD(config: ISA) extends Module {
  val MEM_SIZE = 0x100000

  val io = IO(new Bundle {
    val sim = new AxiMemoryModelSimInterface(16)
    val id_in        = Input(UInt(16.W))
    val data_in      = Input(UInt(16.W))
    val valid_in     = Input(Bool())
    val kill_clock   = Output(Bool())
  })

  val axiParams = new AxiParameters {
    override val IdWidth: Int   = 1
    override val AddrWidth: Int = log2Ceil(MEM_SIZE)
    override val DataWidth: Int = 64
  }

  val clock_distribution = withClockAndReset(clock, reset) { Module(new ClockDistribution) }

  clock_distribution.io.root_clock := clock
  val vcd = withClockAndReset(clock, reset) { Module(new VcdEngine(config = ManticoreFullISA)) }

  when(vcd.io.kill_clock === 1.B) {
    clock_distribution.io.compute_clock_en := withClock(clock) { 0.B }
    io.kill_clock                          := withClock(clock) { 1.B }
  }.otherwise {
    clock_distribution.io.compute_clock_en := withClock(clock) { 1.B }
    io.kill_clock                          := withClock(clock) { 0.B }
  }

  val s_axi_memory = Module(
    (new machine.xrt.AxiMemoryModel(
      axiParams,
      MEM_SIZE,
      16
    ))
  )


  vcd.io.id_in   := io.id_in
  vcd.io.data_in := io.data_in
  vcd.io.valid   := io.valid_in
  vcd.io.root_clock := clock


  vcd.io.m_axi <> s_axi_memory.io.axi  
  io.sim <> s_axi_memory.io.sim
}
