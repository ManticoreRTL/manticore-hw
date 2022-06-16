package manticore.machine.xrt

import chisel3._
import manticore.machine.core.HostRegisters
import manticore.machine.core.DeviceRegisters
import manticore.machine.ISA
import chisel3.util.log2Ceil

import scala.annotation.tailrec
import chisel3.stage.ChiselStage
import manticore.machine.ManticoreFullISA
import chisel3.experimental.ChiselEnum
import chisel3.util.switch
import chisel3.util.is
import chisel3.util.Cat
import manticore.machine.ISA

object AxiSlave {

  class AxiSlaveCorenterface(
      AxiSlaveAddrWidth: Int = 8,
      AxiSlaveDataWidth: Int = 32
  ) extends Bundle {

    //val ACCLK    = Input(Clock()) // implicit
    //val ARESET   = Input(Bool()) // implicit
    // val ACLK_EN = Input(Bool()) // always enabled
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
  class AxiSlaveControlInterface extends Bundle {

    val ap_start  = Output(Bool())
    val ap_ready  = Input(Bool())
    val ap_done   = Input(Bool())
    val ap_idle   = Input(Bool())
    val interrupt = Output(Bool())

  }
  class AxiSlaveManticoreInterface(config: ISA) extends Bundle {

    val host_regs    = Output(new HostRegisters(config))
    val pointer_regs = Output(new MemoryPointers())
    val dev_regs     = Input(new DeviceRegisters(config))

    val AxiSlaveAddrWidth = log2Ceil(
      (
        host_regs.elements ++ pointer_regs.elements ++ dev_regs.elements
      ).map { case (_, data) =>
        require(
          data.isInstanceOf[UInt],
          "Only UInt data types are accepted as slave regs"
        )
        data.getWidth / 8 + 4 // 3 registers for 64 bit data and 2 for 32 bit data

      }.sum + 0x10
    )

    val core = new AxiSlaveCorenterface(
      AxiSlaveAddrWidth = AxiSlaveAddrWidth,
      AxiSlaveDataWidth = 32
    )

    val control = new AxiSlaveControlInterface()

    def createUserAddressMap() = {

      val regs = (
        host_regs.elements.toSeq ++
          pointer_regs.elements.toSeq ++
          dev_regs.elements.toSeq
      )

      @tailrec
      def construct(
          base: Int,
          elems_left: Seq[(String, Data)],
          offset_map: Map[(String, Data), Int]
      ): Map[(String, Data), Int] = {

        elems_left match {
          case Nil => offset_map
          case (name, data) +: rest =>
            require(
              data.isInstanceOf[UInt],
              "Only UInt registers are supported"
            )
            require(
              data.getWidth == 32 || data.getWidth == 64,
              "Only 32 or 64 bit registers are supported!"
            )
            val next_base = base + (data.getWidth / 8) + 4
            construct(
              next_base,
              rest,
              offset_map + (
                (name, data) -> base
              )
            )
        }
      }
      construct(UserAddressBase, regs, Map())
    }
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

  def createUserAddressMap() = {
    val io = new AxiSlaveManticoreInterface(ManticoreFullISA)
    io.createUserAddressMap()
  }
  def getUserPointers() = {
    val io = new MemoryPointers
    io.elements
  }

}

class AxiSlave(config: ISA) extends Module {

  import AxiSlave._

  val io = IO(new AxiSlaveManticoreInterface(config))

  val AddressBits = io.AxiSlaveAddrWidth


  case class BitRange(high: Int, low: Int)
  case class AddressMapEntry(
      offset: Int,
      chisel_name: String,
      chisel_data: UInt,
      range: BitRange,
      vector_index: Option[Int] = None
  )

  val AddressMap = io.createUserAddressMap()

  AddressMap.toSeq.sortBy(_._2).foreach { case ((name, _), offset) =>
    println(f"0x${offset}%02x -> ${name}")
  }
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


  val waddr = Reg(UInt(AddressBits.W))
  val w_hs = Wire(Bool())
  // write channel state machine
  w_hs := false.B
  switch(wstate) {
    is(ChannelState.Idle) {
      when(io.core.AWVALID) {
        wstate := ChannelState.Data
        waddr := io.core.AWADDR(AddressBits - 1, 0)
      }
    }
    is(ChannelState.Data) {
      when(io.core.WVALID) {
        wstate := ChannelState.Resp
        w_hs := true.B
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

  val ar_hs = Wire(Bool())
  ar_hs := io.core.ARVALID & io.core.ARREADY

  val raddr = Wire(UInt(AddressBits.W))
  raddr := io.core.ARADDR

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
  val int_ap_done  = RegInit(Bool(), false.B)
  val int_ap_start = RegInit(Bool(), false.B)

  val int_gie = RegInit(Bool(), false.B)
  val int_ier = RegInit(Vec(2, Bool()), VecInit(false.B, false.B))
  val int_isr = RegInit(Vec(2, Bool()), VecInit(false.B, false.B))

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
  }.elsewhen(ar_hs && raddr === ApControlAddress.U) {
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

  //handle user register writes, device registers can not be written using
  //the axi slave, the manitcore array writes them externally

  val writables = (io.host_regs.elements ++ io.pointer_regs.elements)
    .map { k =>
      k -> AddressMap(k)
    }
    .toSeq
    .sortBy { case (k, offset) => offset }
  val writeable_address_start = writables.head._2
  val writeable_address_end = writables.last match {
    case ((_, data: UInt), offset) if data.getWidth == 32 => offset
    case ((_, data: UInt), offset) if data.getWidth == 64 => offset + 4
    case _ =>
      throw new UnsupportedOperationException(
        "slave registers should be either UInt<32> or UInt<64>"
      )
  }
  val num_writable =
    (writeable_address_end / 4) - (writeable_address_start / 4) + 1
  println(
    f"Writeable address range ${writeable_address_start} -> ${writeable_address_end} (total ${num_writable})"
  )
  val host_controlled_regs = Reg(
    Vec(
      num_writable,
      UInt(32.W)
    )
  )

  when(w_hs) {
    when(
      waddr >= writeable_address_start.U && waddr <= writeable_address_end.U
    ) {
      val old_value = Wire(UInt(32.W))
      old_value := host_controlled_regs((waddr >> 2) - (UserAddressBase >> 2).U)
      host_controlled_regs((waddr >> 2) - (UserAddressBase >> 2).U) :=
        (io.core.WDATA & wmask.asUInt()) | (old_value & !(wmask.asUInt()))
    
  }
  }
  // when(io.host_regs.ele)
  /** Handling rdata for ApControlAddress = 0x00 GlobalInterruptEnableAddress =
    * 0x04 IpInterruptEnableRegister = 0x08 IpInterruptStatusRegister = 0x0c We
    * have special treatments since bits are programmable in those registers but
    * anything above UserAddressBase is treated uniformly
    */

  val readables = io.dev_regs.elements
  val ctrl_vec = Wire(Vec(32, UInt(1.W)))
  ctrl_vec.foreach(_ := 0.U)
  ctrl_vec(0)        := int_ap_start
  ctrl_vec(1)        := int_ap_done
  ctrl_vec(2)        := int_ap_idle
  ctrl_vec(3)        := int_ap_ready
  when(ar_hs) {

    rdata := 0.U

    when(raddr === ApControlAddress.U) {
      rdata               := ctrl_vec.asUInt()
    }.elsewhen(raddr === GlobalInterruptEnableAddress.U) {
      rdata := int_gie.asUInt()
    }.elsewhen(raddr === IpInterruptEnableRegister.U) {
      rdata := int_ier.asUInt()
    }.elsewhen(raddr === IpInterruptStatusRegister.U) {
      rdata := int_isr.asUInt()
    }.otherwise {

      when(
        raddr >= writeable_address_start.U && raddr <= writeable_address_end.U
      ) {
        rdata := host_controlled_regs((raddr >> 2) - (UserAddressBase >> 2).U)
      } otherwise {

        readables.foreach { case r @ (_, data: UInt) =>
          val offset = AddressMap(r)
          if (data.getWidth == 64) {
            when(raddr === offset.U) {
              rdata := data(31, 0)
            }.elsewhen(raddr === (offset + 4).U) {
              rdata := data(63, 32)
            }
          } else {
            // 32 bit reg
            when(raddr === offset.U) {
              rdata := data(31, 0)
            }
          }

        }
      }
    }
  }

  writables.foreach { case r @ ((_, data: UInt), offset) =>
    val ix = (offset >> 2) - (UserAddressBase >> 2)
    if (data.getWidth == 64) {
      data := Cat(host_controlled_regs(ix + 1), host_controlled_regs(ix))
    } else {
      data := host_controlled_regs(ix)
    }

  }
}

object MyAxiSlaveApplication extends App {

  new ChiselStage().emitVerilog(
    new AxiSlave(ManticoreFullISA),
    Array("-td", "gen-dir/myslave")
  )
}
