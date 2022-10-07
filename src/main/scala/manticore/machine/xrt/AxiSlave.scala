package manticore.machine.xrt

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.stage.ChiselStage
import chisel3.util._

import manticore.machine.ISA
import manticore.machine.ManticoreFullISA
import manticore.machine.core.DeviceRegisters
import manticore.machine.core.HostRegisters
import manticore.machine.memory.CacheCounterInterface

object AxiSlave {
  sealed trait RegPort
  case object SlavePort extends RegPort {
    override def toString = "s_axi_control"
  }
  case class MasterPort(port: String) extends RegPort {
    override def toString = port
  }

  sealed trait RegWriter
  case object HostWriter extends RegWriter
  case object DevWriter  extends RegWriter

  sealed trait SlaveReg {
    val width: Int
    val port: RegPort
    val name: String = getClass().getSimpleName().takeWhile(_ != '$')
    val writer: RegWriter
  }

  sealed abstract trait DevReg extends SlaveReg {
    val writer = DevWriter
    val port   = SlavePort
  }
  sealed trait HostReg extends SlaveReg {
    val writer: RegWriter = HostWriter
  }
  sealed trait DevReg32 extends DevReg {
    val width: Int = 32

  }

  sealed trait DevReg64 extends DevReg {
    val width: Int = 64

  }

  sealed trait HostReg64 extends HostReg {
    val width: Int = 64
    val port       = SlavePort
  }

  sealed trait DramPointerReg extends HostReg {
    val width: Int = 64
    val axiDataWidth: Int
  }

  // All the available register are listed below
  case object ScheduleConfig              extends HostReg64
  case object TraceDumpBase               extends HostReg64
  case object GlobalMemoryInstructionBase extends HostReg64
  case object VirtualCycleCount           extends DevReg64
  case object BootloaderCycleCount        extends DevReg32
  case object ExceptionId                 extends DevReg32
  case object CycleCount                  extends DevReg64
  case object TraceDumpHead               extends DevReg64
  case object DeviceInfo                  extends DevReg32

  case object CacheHits   extends DevReg32
  case object CacheMisses extends DevReg32
  case object CacheStalls extends DevReg32

  case object ClockStalls extends DevReg32

  case object DramBank0Base extends DramPointerReg {

    val port              = MasterPort("m_axi_bank_0")
    val axiDataWidth: Int = 256
  }
  case object DramBank1Base extends DramPointerReg {
    val port              = MasterPort("m_axi_bank_1")
    val axiDataWidth: Int = 64
  }

  class AxiSlaveCoreInterface(
      AxiSlaveAddrWidth: Int = 8,
      AxiSlaveDataWidth: Int = 32
  ) extends Bundle {

    val AWADDR  = Input(UInt(AxiSlaveAddrWidth.W))
    val AWVALID = Input(Bool())
    val AWREADY = Output(Bool())
    val WDATA   = Input(UInt(AxiSlaveDataWidth.W))
    val WSTRB   = Input(UInt((AxiSlaveDataWidth / 8).W))
    val WVALID  = Input(Bool())
    val WREADY  = Output(Bool())
    val BRESP   = Output(UInt(2.W))
    val BVALID  = Output(Bool())
    val BREADY  = Input(Bool())
    val ARADDR  = Input(UInt(AxiSlaveAddrWidth.W))
    val ARVALID = Input(Bool())
    val ARREADY = Output(Bool())
    val RDATA   = Output(UInt(AxiSlaveDataWidth.W))
    val RRESP   = Output(UInt(2.W))
    val RVALID  = Output(Bool())
    val RREADY  = Input(Bool())

  }

  class AxiSlaveInterface(
      AxiSlaveAddrWidth: Int = 8,
      AxiSlaveDataWidth: Int = 32
  ) extends AxiSlaveCoreInterface(AxiSlaveAddrWidth, AxiSlaveDataWidth) {

    val ACLK    = Input(Clock()) // implicit
    val ARESETN = Input(Bool())  // implicit

  }
  class AxiSlaveControlInterface extends Bundle {

    val ap_start  = Output(Bool())
    val ap_ready  = Input(Bool())
    val ap_done   = Input(Bool())
    val ap_idle   = Input(Bool())
    val interrupt = Output(Bool())

  }
  class AxiSlaveManticoreInterface(config: ISA) extends Bundle {

    val host_regs    = Output(new HostRegisters)
    val pointer_regs = Output(new MemoryPointers())
    val dev_regs     = Input(new DeviceRegisters)
    val cache_regs   = Flipped(new CacheCounterInterface())

    val AxiSlaveAddrWidth = log2Ceil(
      listRegs.map(addressOf).max + 12
    )

    val core = new AxiSlaveCoreInterface(
      AxiSlaveAddrWidth = AxiSlaveAddrWidth,
      AxiSlaveDataWidth = 32
    )

    val control = new AxiSlaveControlInterface()

  }

  //------------------------Address Info-------------------
  // 0x00 : Control signals
  //        bit 0  - ap_start (Read/Write/COH)
  //        bit 1  - ap_done (Read/COR)
  //        bit 2  - ap_idle (Read)
  //        bit 3  - ap_ready (Read)
  //        others - reserved
  // 0x04 : Global Interrupt Enable Register
  //        bit 0  - Global Interrupt Enable (Read/Write)
  //        others - reserved
  // 0x08 : IP Interrupt Enable Register (Read/Write)
  //        bit 0  - enable ap_done interrupt (Read/Write)
  //        bit 1  - enable ap_ready interrupt (Read/Write)
  //        others - reserved
  // 0x0c : IP Interrupt Status Register (Read/TOW)
  //        bit 0  - ap_done (COR/TOW)
  //        bit 1  - ap_ready (COR/TOW)
  //        others - reserved
  // 0x10: start of user register
  // host_regs: host writable, device readable
  // pointer_regs: host writeable, device readable
  // dev_regs: host readable, device writeable
  // (SC = Self Clear, COR = Clear on Read, TOW = Toggle on Write, COH = Clear on Handshake)
  val ApControlAddress             = 0x00
  val GlobalInterruptEnableAddress = 0x04
  val IpInterruptEnableRegister    = 0x08
  val IpInterruptStatusRegister    = 0x0c
  val UserAddressBase              = 0x10 // from Xilinx Documentation

  def listRegs: List[SlaveReg] = List(
    // write by the device
    ExceptionId,
    TraceDumpHead,
    DeviceInfo,
    // profiling
    CycleCount,
    VirtualCycleCount,
    BootloaderCycleCount,
    ClockStalls,
    CacheHits,
    CacheMisses,
    CacheStalls,
    // written by the host
    ScheduleConfig,
    TraceDumpBase,
    GlobalMemoryInstructionBase,
    DramBank0Base
    // DramBank1Base
  )
  val addressOf: SlaveReg => Int = {
    val allRegs = scala.collection.mutable.Queue.empty[SlaveReg] ++ listRegs

    var offset  = UserAddressBase
    val mapping = scala.collection.mutable.Map.empty[SlaveReg, Int]
    while (allRegs.nonEmpty) {
      val r = allRegs.dequeue()
      val incr = r match {
        case _: DevReg64 | _: HostReg64 | _: DramPointerReg => 12
        case _: DevReg32                                    => 8
      }
      mapping += (r -> offset)
      offset += incr
    }
    mapping.toMap
  }

  def kernelXml = {

    val dramPorts = listRegs.collect { case ptr: DramPointerReg => ptr }.map { ptr =>
      <port name={
        s"${ptr.port}"
      } portType="addressable" mode="master" base="0x0" range="0xFFFFFFFFFFFFFFFF" dataWidth={
        s"${ptr.axiDataWidth}"
      }/>
    }

    val args = listRegs.zipWithIndex.map { case (r, ix) =>
      val tpe = if (r.isInstanceOf[DramPointerReg]) {
        "unsigned int*"
      } else if (r.width == 32) {
        "unsigned int"
      } else if (r.width == 64) {
        "unsigned long"
      } else {
        throw new UnsupportedOperationException(s"Invalid width in ${r}")
      }
      <arg id={s"${ix}"} name={s"${r.name}"} addressQualifier={
        if (r.isInstanceOf[DramPointerReg]) "1" else "0"
      } port={s"${r.port}"} hostOffset="0x0" hostSize={f"0x${r.width / 8}%x"} offset={
        f"0x${addressOf(r)}%x"
      } type={s"$tpe"} />
    }
    <root versionMajor="1" versionMinor="0">
      <kernel name="ManticoreKernel" language="ip_c" vlnv="vlsc.epfl.ch:RTLKernel:ManticoreKernel:1.0" attributes="" preferredWorkGroupSizeMultiple="0" workGroupSize="1" interrupt="true">
        <ports>
          {
      dramPorts :+
        <port name="s_axi_control" portType="addressable" mode="slave" base="0x0" range="0x1000" dataWidth="32" />
    }
        </ports>
        <args>
          {args}
        </args>
        <compileWorkGroupSize x="1" y="1" z="1" />
        <maxWorkGroupSize x="1" y="1" z="1" />
      </kernel>
    </root>

  }

  def header: String = {

    val instances = listRegs
      .map { r =>
        s"\tstruct ${r.name} { static constexpr int base = ${addressOf(r)}; static constexpr int width = ${r.width}; };"
      }
      .mkString("\n")
    instances

  }

}

class AxiSlave(config: ISA) extends Module {

  import AxiSlave._

  val io = IO(new AxiSlaveManticoreInterface(config))

  val registers = AxiSlave.listRegs.map(sr =>
    sr ->
      (if (sr.width == 64) {
         Seq.fill(2) { Reg(UInt(32.W)) }
       } else if (sr.width == 32) {
         Seq.fill(1) { Reg(UInt(32.W)) }
       } else {
         throw new UnsupportedOperationException(
           s"Can not handle ${sr}, only 32 and 64-bit slave registers are supported!"
         )
       })
  )

  val rdata = Reg(UInt(32.W))

  object ChannelState extends ChiselEnum {
    val Idle, Data, Resp = Value
  }

  // read and write channel states
  val wstate = RegInit(ChannelState.Type(), ChannelState.Idle)
  val rstate = RegInit(ChannelState.Type(), ChannelState.Idle)

  // val NumResRegs =
  // val regfile = Wire(Vec(AddressRange, UInt(32.W)))

  // -- axi write fsm
  io.core.AWREADY := (wstate === ChannelState.Idle)
  io.core.WREADY  := (wstate === ChannelState.Data)
  io.core.BRESP   := 0.U
  io.core.BVALID  := (wstate === ChannelState.Resp)

  val wmask = Wire(Vec(32, Bool()))
  wmask.zipWithIndex.foreach { case (bit, i) =>
    bit := io.core.WSTRB(i / 8)
  }

  val waddr = RegEnable(io.core.AWADDR, wstate === ChannelState.Idle && io.core.AWVALID)
  val w_hs  = wstate === ChannelState.Data && io.core.WVALID
  // write channel state machine
  switch(wstate) {
    is(ChannelState.Idle) {
      when(io.core.AWVALID) {
        wstate := ChannelState.Data
      }
    }
    is(ChannelState.Data) {
      when(io.core.WVALID) {
        wstate := ChannelState.Resp
      }
    }
    is(ChannelState.Resp) {
      when(io.core.BREADY) {
        wstate := ChannelState.Idle
      }
    }
  }

  // register the write address when both AWVALID and AWREADY are high

  //--- axi read fsm

  io.core.ARREADY := (rstate === ChannelState.Idle)
  io.core.RDATA   := rdata
  io.core.RRESP   := 0.U
  io.core.RVALID  := (rstate === ChannelState.Data)

  val ar_hs = io.core.ARVALID & io.core.ARREADY

  switch(rstate) {

    is(ChannelState.Idle) {
      when(io.core.ARVALID) {
        rstate := ChannelState.Data
      }
    }
    is(ChannelState.Data) {
      when(io.core.RREADY && io.core.RVALID) {
        rstate := ChannelState.Idle
      }
    }
    is(ChannelState.Resp) {
      // should not happen
      rstate := ChannelState.Idle
    }
  }

  // --- Special "bit-level" registers

  val int_ap_idle  = Reg(Bool())
  val int_ap_ready = Reg(Bool())
  val int_ap_done  = RegInit(false.B)
  val int_ap_start = RegInit(false.B)

  val int_gie = RegInit(Bool(), false.B)
  val int_ier = RegInit(VecInit.fill(2)(false.B))
  val int_isr = RegInit(VecInit.fill(2)(false.B))

  io.control.interrupt := int_gie & (int_isr.reduce(_ | _))
  io.control.ap_start  := int_ap_start
  int_ap_idle          := io.control.ap_idle
  int_ap_ready         := io.control.ap_ready

  // handle ap_start
  when(
    w_hs && waddr === ApControlAddress.U && io.core.WSTRB(0) && io.core.WDATA(0)
  ) {
    int_ap_start := true.B
  }.elsewhen(int_ap_ready) {
    int_ap_start := false.B // clear on handshake
  }

  // handle ap_done
  when(io.control.ap_done) {
    int_ap_done := true.B
  }.elsewhen(ar_hs && io.core.ARADDR === ApControlAddress.U) {
    int_ap_done := false.B // clear on read
  }

  // handle interrupt enable

  when(w_hs && waddr === GlobalInterruptEnableAddress.U && io.core.WSTRB(0)) {
    // enable/disable global interrupt
    int_gie := io.core.WDATA(0)
  }

  // handle ip interrupt enable
  when(w_hs && waddr === IpInterruptEnableRegister.U && io.core.WSTRB(0)) {
    int_ier(0) := io.core.WDATA(0)
    int_ier(1) := io.core.WDATA(1)
  }

  // handle interrupt status bits
  when(int_ier(0) && io.control.ap_done) {
    int_isr(0) := true.B
  }.elsewhen(
    w_hs && waddr === IpInterruptStatusRegister.U && io.core.WSTRB(0)
  ) {
    int_isr(0) := int_isr(0) ^ io.core.WDATA(0) // toggle status on write
  }
  when(int_ier(0) && io.control.ap_ready) {
    int_isr(1) := true.B
  }.elsewhen(
    w_hs && waddr === IpInterruptStatusRegister.U && io.core.WSTRB(0)
  ) {
    int_isr(1) := int_isr(1) ^ io.core.WDATA(1) // toggle status on write
  }

  // handle user register writes, though device registers can not be written using
  // the axi slave interface
  for ((desc, sreg) <- registers) {
    sreg.zipWithIndex.foreach { case (r, i) => r.suggestName(s"${desc}_${i}") }
    val mask = wmask.asUInt
    desc match {
      case hr: HostReg =>
        sreg match {
          case Seq(low, high) =>
            when(w_hs) {
              when(waddr === AxiSlave.addressOf(desc).U) {
                low := (io.core.WDATA & mask) | (low & !mask)

              }.elsewhen(waddr === (AxiSlave.addressOf(desc) + 4).U) {
                high := (io.core.WDATA & mask) | (high & !mask)

              }
            }
          case Seq(r) =>
            when(w_hs && waddr === AxiSlave.addressOf(desc).U) {
              r := (io.core.WDATA & mask) | (r & !mask)
            }
        }
        hr match {
          case ScheduleConfig =>
            io.host_regs.schedule_config := Cat(sreg.reverse)
          case TraceDumpBase =>
            io.host_regs.trace_dump_base := Cat(sreg.reverse)
          case GlobalMemoryInstructionBase =>
            io.host_regs.global_memory_instruction_base := Cat(sreg.reverse)
          case DramBank0Base =>
            io.pointer_regs.pointer_0 := Cat(sreg.reverse)

        }
      case VirtualCycleCount =>
        sreg(0) := io.dev_regs.virtual_cycles
        sreg(1) := io.dev_regs.virtual_cycles >> 32.U
      case BootloaderCycleCount =>
        sreg(0) := io.dev_regs.bootloader_cycles
      case CycleCount =>
        sreg(0) := io.dev_regs.execution_cycles
        sreg(1) := io.dev_regs.execution_cycles >> 32.U
      case ExceptionId =>
        sreg(0) := io.dev_regs.exception_id
      case TraceDumpHead =>
        sreg(0) := io.dev_regs.trace_dump_head
        sreg(1) := io.dev_regs.trace_dump_head >> 32.U
      case DeviceInfo =>
        sreg(0) := io.dev_regs.device_info
      case ClockStalls =>
        sreg(0) := io.dev_regs.clock_stalls
      case CacheHits =>
        sreg(0) := io.cache_regs.hit
      case CacheMisses =>
        sreg(0) := io.cache_regs.miss
      case CacheStalls =>
        sreg(0) := io.cache_regs.stall

    }
  }

  val ctrl_vec = Cat(0.U(28), int_ap_ready, int_ap_idle, int_ap_done, int_ap_start)


    // handle reading
  val addressMap = List(
    ApControlAddress             -> ctrl_vec,
    GlobalInterruptEnableAddress -> int_gie.asUInt,
    IpInterruptEnableRegister    -> int_ier.asUInt,
    IpInterruptStatusRegister    -> int_isr.asUInt
  ) ++ registers.flatMap {
    case (desc, Seq(low, high)) =>
      Seq(
        AxiSlave.addressOf(desc)       -> low,
        AxiSlave.addressOf(desc) + 4 -> high
      )
    case (desc, Seq(low)) =>
      Seq(
        AxiSlave.addressOf(desc) -> low
      )
  }

  // print(addressMap)

  when(ar_hs) {
    rdata := MuxLookup(
      io.core.ARADDR,
      0.U(32.W),
      addressMap.map { case (a, v) => a.U -> v }
    )
  }

}

// object Motherfucker extends App {

//   // new ChiselStage().emitVerilog(new AxiSlave(ManticoreFullISA), Array("-td", "gen-dir"))
//   val printer = new scala.xml.PrettyPrinter(32, 1)

//   println(printer.format(AxiSlave.kernelXml))
//   println(AxiSlave.header)
// }
