package manticore.machine.core

import Chisel._
import chisel3.experimental.ChiselEnum


class ExceptionInterface(NameBits: Int) extends Bundle {
  val exception: NamedError = Input(new NamedError(NameBits))
  val clear: Bool = Input(Bool())
  val clock_enable: Bool = Input(Bool())
  val caught: Bool = Output(Bool())
  val gate_request_start: Bool = Output(Bool())
  val gate_request_end: Bool = Output(Bool())

}

class ExceptionHandler(NameBits: Int) extends Module {


  val io = IO(new ExceptionInterface(NameBits))


  object State extends ChiselEnum {
    val Listening, Handling, Clearing = Value
  }
  val state = RegInit(State.Type(), State.Listening)

  io.gate_request_end := false.B
  io.gate_request_start := false.B
  io.caught := false.B
  switch(state) {
    is (State.Listening) {
      when (io.exception.error) {
        io.gate_request_start := true.B
        state := State.Handling
      }
    }
    is (State.Handling) {
      io.caught := true.B
      when (io.clear) {
        io.gate_request_end := true.B
        state := State.Clearing
      }
    }
    is (State.Clearing) {
      when (io.clock_enable) {
        state := State.Listening
      }
    }
  }


}
