package manticore.machine.memory

import chisel3._
import chisel3.experimental.Param
import chisel3.stage.ChiselStage
import chisel3.util.HasBlackBoxInline
import chisel3.util.HasBlackBoxResource

class GenericMemoryInterface(val ADDRESS_WIDTH: Int, val DATA_WIDTH: Int)
    extends Bundle {
  private def dataInPort  = Input(UInt(DATA_WIDTH.W))
  private def controlPort = Input(Bool())
  private def addressPort = Input(UInt(ADDRESS_WIDTH.W))
  private def dataOutPort = Output(UInt(DATA_WIDTH.W))
  val wea                 = controlPort
  val addra               = addressPort
  val dina                = dataInPort
  val douta               = dataOutPort
  val web                 = controlPort
  val addrb               = Input(UInt(ADDRESS_WIDTH.W))
  val dinb                = dataInPort
  val doutb               = dataOutPort

}

class GenericMemory(
    val ADDRESS_WIDTH: Int,
    val DATA_WIDTH: Int,
    val INIT: String = ""
) extends Module {

  val io = IO(
    new GenericMemoryInterface(
      ADDRESS_WIDTH = ADDRESS_WIDTH,
      DATA_WIDTH = DATA_WIDTH
    )
  )

  val memory = Module(
    if (INIT.isEmpty) {
      new GenericChiselMemory(ADDRESS_WIDTH, DATA_WIDTH)
    } else {
      new GenericVerilogMemoryFromFileWrapper(ADDRESS_WIDTH, DATA_WIDTH, INIT)
    }
  )

  memory.io <> io

}

abstract class AbstractGenericMemory(
    val ADDRESS_WIDTH: Int,
    val DATA_WIDTH: Int
) extends Module {
  val io = IO(
    new GenericMemoryInterface(
      ADDRESS_WIDTH = ADDRESS_WIDTH,
      DATA_WIDTH = DATA_WIDTH
    )
  )
}

class GenericChiselMemory(ADDRESS_WIDTH: Int, DATA_WIDTH: Int)
    extends AbstractGenericMemory(ADDRESS_WIDTH, DATA_WIDTH) {
  val memory = SyncReadMem(1 << ADDRESS_WIDTH, UInt(DATA_WIDTH.W))
  when(io.wea) {
    memory(io.addra) := io.dina
  }
  when(io.web) {
    memory(io.addrb) := io.dinb
  }
  io.douta := memory(io.addra)
  io.doutb := memory(io.addrb)

}

class GenericVerilogMemoryFromFileWrapper(
    ADDRESS_WIDTH: Int,
    DATA_WIDTH: Int,
    val INIT: String
) extends AbstractGenericMemory(ADDRESS_WIDTH, DATA_WIDTH) {

  val impl = Module(
    new GenericVerilogMemoryFromFile(ADDRESS_WIDTH, DATA_WIDTH, INIT)
  )
  impl.io.wea   := io.wea
  impl.io.web   := io.web
  impl.io.clock := clock
  impl.io.addra := io.addra
  impl.io.addrb := io.addrb
  impl.io.dina  := io.dina
  impl.io.dinb  := io.dinb
  io.douta      := impl.io.douta
  io.doutb      := impl.io.doutb

}

class GenericVerilogMemoryFromFile(
    val ADDRESS_WIDTH: Int,
    val DATA_WIDTH: Int,
    val INIT: String
) extends BlackBox(
      Map(
        "ADDRESS_WIDTH" -> ADDRESS_WIDTH,
        "DATA_WIDTH"    -> DATA_WIDTH,
        "filename"      -> INIT
      )
    )
    with HasBlackBoxResource {

  val io = IO(
    new Bundle {
      val wea   = Input(Bool())
      val web   = Input(Bool())
      val clock = Input(Clock())
      val addra = Input(UInt(ADDRESS_WIDTH.W))
      val addrb = Input(UInt(ADDRESS_WIDTH.W))
      val dina  = Input(UInt(DATA_WIDTH.W))
      val dinb  = Input(UInt(DATA_WIDTH.W))
      val douta = Output(UInt(DATA_WIDTH.W))
      val doutb = Output(UInt(DATA_WIDTH.W))
    }
  )

  require(INIT.size <= (1 << ADDRESS_WIDTH), "Too many initial values!")
  addResource("/verilog/GenericVerilogMemoryFromFile.v")
}

object MemStyle extends Enumeration {
  type MemSyle = Value
  val BRAM, URAM, URAMReal = Value
}

class SimpleDualPortMemoryInterface(val ADDRESS_WIDTH: Int, val DATA_WIDTH: Int)
    extends Bundle {
  private def dataInPort  = Input(UInt(DATA_WIDTH.W))
  private def controlPort = Input(Bool())
  private def addressPort = Input(UInt(ADDRESS_WIDTH.W))
  private def dataOutPort = Output(UInt(DATA_WIDTH.W))
  val raddr               = addressPort
  val dout                = dataOutPort
  val wen                 = controlPort
  val waddr               = Input(UInt(ADDRESS_WIDTH.W))
  val din                 = dataInPort

}

class AbstractDualPortMemory(
    ADDRESS_WIDTH: Int,
    DATA_WIDTH: Int,
    READ_LATENCY: Int = 1,
    STYLE: MemStyle.MemSyle = MemStyle.BRAM,
    INIT: String = ""
) extends Module {
  val io = IO(new SimpleDualPortMemoryInterface(ADDRESS_WIDTH, DATA_WIDTH))
}

class SimpleDualPortMemory(
    val ADDRESS_WIDTH: Int,
    val DATA_WIDTH: Int,
    val READ_LATENCY: Int = 1,
    val STYLE: MemStyle.MemSyle = MemStyle.BRAM,
    val INIT: String = ""
) extends AbstractDualPortMemory(ADDRESS_WIDTH, DATA_WIDTH, READ_LATENCY, STYLE, INIT) {
  abstract class VerilogMemory(params: Map[String, Param])
      extends BlackBox(params) {
    class VerilogInternalInterface extends Bundle {
      val wen   = Input(Bool())
      val clock = Input(Clock())
      val raddr = Input(UInt(ADDRESS_WIDTH.W))
      val waddr = Input(UInt(ADDRESS_WIDTH.W))
      val din   = Input(UInt(DATA_WIDTH.W))
      val dout  = Output(UInt(DATA_WIDTH.W))
    }
    val io = IO(new VerilogInternalInterface)
  }
  class BRAMLike
      extends VerilogMemory(
        Map(
          "ADDRESS_WIDTH" -> ADDRESS_WIDTH,
          "DATA_WIDTH"    -> DATA_WIDTH,
          "READ_LATENCY"  -> READ_LATENCY,
          "filename"      -> INIT
        )
      )
      with HasBlackBoxResource {
    addResource("/verilog/BRAMLike.v")
  }
  class URAMLike extends VerilogMemory(
    Map(
      "ADDRESS_WIDTH" -> ADDRESS_WIDTH,
      "DATA_WIDTH" -> DATA_WIDTH,
      "READ_LATENCY" -> READ_LATENCY
    )
  ) with HasBlackBoxResource {
    addResource("/verilog/URAMLike.v")
  }
  
  class URAMReal extends VerilogMemory(
    Map(
      "ADDRESS_WIDTH" -> ADDRESS_WIDTH,
      "DATA_WIDTH" -> DATA_WIDTH
    )
  ) with HasBlackBoxResource {
    addResource("/verilog/URAMReal.v")
  }

  val impl = Module(STYLE match {
    case MemStyle.BRAM => new BRAMLike
    case MemStyle.URAM => new URAMLike
    case MemStyle.URAMReal => new URAMReal
  })

  impl.io.clock := clock
  impl.io.din   := io.din
  impl.io.raddr := io.raddr
  impl.io.wen   := io.wen
  impl.io.waddr := io.waddr
  io.dout       := impl.io.dout

}

class VcdDualPortMemory(
    val ADDRESS_WIDTH: Int,
    val DATA_IN_WIDTH: Int,
    val DATA_OUT_WIDTH:Int,
    val READ_LATENCY: Int = 1,
    val STYLE: MemStyle.MemSyle = MemStyle.BRAM,
    val INIT: String = ""
) extends Module {

      val io = IO (new Bundle {
      val wen   = Input(Bool())
      val clock = Input(Clock())
      val raddr = Input(UInt(ADDRESS_WIDTH.W))
      val waddr = Input(UInt(ADDRESS_WIDTH.W))
      val din   = Input(UInt(DATA_IN_WIDTH.W))
      val dout  = Output(UInt(DATA_OUT_WIDTH.W))
    })

  class BRAMLike
      extends BlackBox(
        Map(
          "ADDRESS_WIDTH" -> ADDRESS_WIDTH,
          "DATA_IN_WIDTH"    -> DATA_IN_WIDTH,
          "DATA_OUT_WIDTH"    -> DATA_OUT_WIDTH,
          "filename"      -> INIT
        )
      )
      with HasBlackBoxResource {
        val io = IO(new Bundle {
          val wen   = Input(Bool())
      val clock = Input(Clock())
      val raddr = Input(UInt(ADDRESS_WIDTH.W))
      val waddr = Input(UInt(ADDRESS_WIDTH.W))
      val din   = Input(UInt(DATA_IN_WIDTH.W))
      val dout  = Output(UInt(DATA_OUT_WIDTH.W))
        })


    addResource("/verilog/BRAMLike.v")
  }


  val impl = Module(new BRAMLike)

  impl.io.clock := io.clock
  impl.io.din   := io.din
  impl.io.raddr := io.raddr
  impl.io.wen   := io.wen
  impl.io.waddr := io.waddr
  io.dout       := impl.io.dout

}


class DummyDualPortMemory(
    ADDRESS_WIDTH: Int,
    DATA_WIDTH: Int,
    READ_LATENCY: Int = 1,
    STYLE: MemStyle.MemSyle = MemStyle.BRAM,
    INIT: String = ""
) extends AbstractDualPortMemory(ADDRESS_WIDTH, DATA_WIDTH, READ_LATENCY, STYLE, INIT) {
  io.dout := 0.U
}

object MemGen extends App {

  new ChiselStage().emitVerilog(new GenericMemory(11, 16))
  new ChiselStage().emitVerilog(
    new GenericVerilogMemoryFromFileWrapper(11, 16, "array.data")
  )
}
object BRAM2Kx16 {
  def apply(INIT: String = "") =
    new GenericMemory(DATA_WIDTH = 16, ADDRESS_WIDTH = 11, INIT = INIT)

//  def emitVerilog() = new chisel3.stage.ChiselStage().emitVerilog(apply())
}
