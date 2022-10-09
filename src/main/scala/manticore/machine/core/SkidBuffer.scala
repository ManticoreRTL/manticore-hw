package manticore.machine.core

import chisel3._
import chisel3.util._

class SkidBufferInterface[T <: Data](gen: T) extends Bundle {
  val enq = Flipped(Decoupled(gen.cloneType))
  val deq = Decoupled(gen.cloneType)
}

class SkidBuffer[T <: Data](gen: T) extends Module {

  val io = IO(new SkidBufferInterface(gen))

  val data_buffer = Reg(gen.cloneType)
  val skid_buffer = Reg(gen.cloneType)

  val sEmpty :: sHalf :: sFull :: Nil = Enum(3)

  val state_next = WireDefault(sEmpty)
  val state      = RegNext(state_next, sEmpty)

  io.enq.ready := RegNext(state_next =/= sFull, true.B)
  io.deq.valid := RegNext(state_next =/= sEmpty, false.B)

  switch(state) {
    is(sEmpty) {
      when(io.enq.fire) {
        state_next := sHalf
      } otherwise {
        state_next := sEmpty
      }
      // broadcast data to both buffers to enable single-cycle latency transfers
      skid_buffer := io.enq.bits
    }
    is(sHalf) {
      when(io.deq.fire) {
        when(io.enq.fire) {
          // flow the data
          skid_buffer := io.enq.bits
          state_next  := sHalf
        } otherwise {
          // unload the data
          state_next := sEmpty
        }
      }.elsewhen(io.enq.fire) {
        // fill
        data_buffer := io.enq.bits
        state_next  := sFull
      } otherwise {
        state_next := sHalf
      }
    }
    is(sFull) {
      when(io.deq.fire) {
        // flush the skid buffer
        skid_buffer := data_buffer
        state_next  := sHalf
      } otherwise {
        state_next := sFull
      }
    }
  }

  io.deq.bits := skid_buffer

}

object SkidBuffer {
  def apply[T <: Data](enq: DecoupledIO[T]): DecoupledIO[T] = {
    val buffer = Module(new SkidBuffer(enq.bits))
    buffer.io.enq <> enq
    buffer.io.deq
  }
}
