package manticore.machine.memory

import chisel3._
import chisel3.stage.ChiselStage

class FifoWriterInterface(dataBits: Int) extends Bundle {
  val dataIn  = Input(UInt(dataBits.W))
  val writeEn = Input(Bool())
  val nonFull = Output(Bool())
}

class FifoReaderInterface(dataBits: Int) extends Bundle {
  val dataOut  = Output(UInt(dataBits.W))
  val readEn   = Input(Bool())
  val nonEmpty = Output(Bool())
}

class FifoInterface(dataBits: Int) extends Bundle {
    val in  = new FifoWriterInterface(dataBits)
    val out = new FifoReaderInterface(dataBits)
}
class SimpleFifo(dataBits: Int, forcedWrites: Boolean) extends Module {

  val io = IO(new FifoInterface(dataBits))

  
  val storage = Mem(8, UInt(dataBits.W))

  val count = RegInit(UInt(4.W), 0.U)
  val headPtr = RegInit(UInt(3.W), 0.U)
  val tailPtr = RegInit(UInt(3.W), 0.U)

  val isFull = Wire(Bool())

  isFull := (count === 8.U)
  
  io.in.nonFull := !isFull

  val isEmpty = Wire(Bool())

  isEmpty := (count === 0.U)

  io.out.nonEmpty := !isEmpty


  when(io.in.writeEn) {
    if (forcedWrites) {
        storage(headPtr) := io.in.dataIn
        headPtr := headPtr + 1.U   
         
    } else {
        when(!isFull) {
            storage(headPtr) := io.in.dataIn
            headPtr := headPtr + 1.U
            count := count + 1.U
        }
    }
  }

  when(io.out.readEn && !isEmpty) {
    tailPtr := tailPtr + 1.U
    io.out.dataOut := storage(tailPtr)
    count := count - 1.U
  } otherwise {
    io.out.dataOut := DontCare
  }

}

class Fifo32 extends Module {

    val io = IO(new FifoInterface(32))

    val impl = Module(new SimpleFifo(32, false))

    io <> impl.io
    
}
class Slicer extends Module {

  val io = IO(new Bundle {
    val wordIn  = Input(UInt(32.W))
    val offset  = Input(UInt(5.W))
    val length  = Input(UInt(5.W))
    val wordOut = Output(UInt(8.W))
  })

  io.wordOut := io.wordIn(23, 16)

}

object FifoGenerator extends App {

    // new ChiselStage().emitVerilog(new SimpleFifo(4, false), Array("-td", "dumps/fifo", "--no-dce"))
    // new ChiselStage().emitVerilog(new SimpleFifo(4, true), Array("-td", "dumps/fifo_forced"))
    new ChiselStage().emitVerilog(new Fifo32(), Array("-td", "dumps/fifo32"))

}
