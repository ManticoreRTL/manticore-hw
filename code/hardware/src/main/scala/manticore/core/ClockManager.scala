package manticore.core

import Chisel._
import chisel3.VecInit
import chisel3.experimental.ChiselEnum
import chisel3.util.HasBlackBoxResource


class ClockManagerInterface(NumUsers: Int = 4)
  extends Bundle {

  /**
   * To issue a clock gate request, a user should pull the dynamic_start_request
   * signal high, at the next cycle, the user should check the value of clock_enable
   * and pull down the dynamic_start_request if clock_enable is deasserted.
   * assert request -> check clock enable -> deassert request if clock is not enabled
   *
   * Note that it is possible to assert a request even though the clock_enable is
   * low. In fact, the user should NEVER check clock enable before issuing a request.
   * Doing so could result in a dead lock.
   */
  val start_request: Vec[Bool] = Input(Vec(NumUsers, Bool()))
  /**
   * After pulling down the request, a user must pull up done to notify the
   * clock manger it no longer needs the clock gated. Every start should be
   * followed by a done (with arbitrary number of cycles between them).
   * If the done is eventually pulled high, the clock remains gated.
   */
  val done_request: Vec[Bool] = Input(Vec(NumUsers, Bool()))

  /**
   * Clock enable signal to be given to a glitchless clock buffer, e.g.,
   * BUFGCE in Vivado (see page 28 in ug572).
   */
  val clock_enable: Bool = Output(Bool())

  /**
   * Sticky error checking indicator (only clears on reset).
   */
  val invalid_done_error: Vec[Bool] = Output(Vec(NumUsers, Bool()))
  val invalid_request_error: Vec[Bool] = Output(Vec(NumUsers, Bool()))
}

class ClockManager(NumUsers: Int = 4) extends Module {
  val io: ClockManagerInterface = IO(new ClockManagerInterface(NumUsers))

  object GatedState extends ChiselEnum {
    val ClockActive, ClockGated = Value
  }

  val request_captured: Vec[Bool] = Reg(Vec(NumUsers, Bool()))
  val done_captured: Vec[Bool] = Reg(Vec(NumUsers, Bool()))
  val request_served: Vec[Bool] = Wire(Vec(NumUsers, Bool()))
  val new_request: Vec[Bool] = Wire(Vec(NumUsers, Bool()))

  new_request := request_captured.zip(io.start_request).map { case (req_capture, req) =>
    val new_req = Wire(Bool())
    when(req_capture === false.B && req === true.B) {
      new_req := true.B
    } otherwise {
      new_req := false.B
    }
    new_req
  }
  request_served := request_captured.zip(done_captured).map { case (r: Bool, d: Bool) =>
    val request_implies_done = Wire(Bool())
    request_implies_done := !r | (r & d)
    request_implies_done
  }


  def makeErrorVector(): Vec[Bool] = RegInit(Vec(NumUsers, Bool()), VecInit(Seq.fill(NumUsers)(false.B)))

  val invalid_done_err: Vec[Bool] = makeErrorVector()
  val invalid_request_err: Vec[Bool] = makeErrorVector()


  val state = RegInit(GatedState.Type(), GatedState.ClockActive)
  io.clock_enable := state === GatedState.ClockActive

  def setOnceError(error_bit: Bool, cond: Bool): Unit = {
    when(cond === true.B) {
      error_bit := true.B
    } otherwise {
      error_bit := error_bit // don't change the value
    }
  }

  def captureNewRequests(): Unit = {
    request_captured := request_captured.zip(io.start_request).map { case (req_capture, req) =>
      val new_req = Wire(Bool())
      when(req_capture =/= true.B) {
        new_req := req
      } otherwise {
        new_req := req_capture
      }
      new_req
    }
  }

  def exists[T <: Data](elems: Vec[T])(cond: T => Bool): Bool = {
    val quantification = Wire(Bool())
    quantification := elems.map{ e =>
      val wire_cond = Wire(Bool())
      wire_cond := cond(e)
      wire_cond
    }.reduce(_ | _)
    quantification
  }

  def forall[T <: Data](elems: Vec[T])(cond: T => Bool): Bool = {
    val quantification = Wire(Bool())
    quantification := !exists(elems)(cond)
    quantification
  }

  switch(state) {
    is(GatedState.ClockActive) {

      // no done should come when the clock is active
      invalid_done_err.zip(io.done_request).foreach { case (e, c) =>
        setOnceError(e, c)
      }
      captureNewRequests()
      // reset done
      done_captured := VecInit(Seq.fill(NumUsers)(false.B))
      when(io.start_request.exists(_ === true.B)) {
        state := GatedState.ClockGated
      } otherwise {
        state := GatedState.ClockActive
      }

    }
    is(GatedState.ClockGated) {
      done_captured := io.done_request
      // check if there are any new request, update the request_captured bit vector
      captureNewRequests()
      // we are done when there are no new requests and all the old ones are served
      when(
        forall(request_served) {
          _ === true.B
        } &&
          forall(new_request) {
            _ === false.B
          }) {
        request_captured := VecInit(Seq.fill(NumUsers)(false.B))
        state := GatedState.ClockActive
      } otherwise {
        state := GatedState.ClockGated
      }

      request_captured.zip(io.start_request).
        map { case (req_capture, req) =>
          val double_req = Wire(Bool())
          double_req := (req_capture === true.B && req === true.B)
          double_req
        }.zip(invalid_request_err).
        foreach { case (c, e) =>
          setOnceError(e, c)
        }
    }
  }

  io.invalid_done_error := invalid_done_err
  io.invalid_request_error :=invalid_request_err

}



class ClockBuffer extends BlackBox() with HasBlackBoxResource {

  val io = IO(new Bundle {
    val I = Input(Clock())
    val CE = Input(Bool())
    val O = Output(Clock())
  })

  addResource("/verilog/ClockBuffer.v")

}

