package thyrio

import Chisel._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.tester.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chisel3.tester.experimental.sanitizeFileName
import chisel3.tester.{ChiselScalatestTester, fork, testableClock, testableData}
import org.scalatest.{FlatSpec, Matchers, ScalaTestVersion}
import thyrio.assembly.Instruction.{Instruction, Nop, R, Register, Send}
import thyrio.core.{BareNoCBundle, NoCBundle, Processor}
import thyrio.assembly.{Assembler, Interpreter}
import chiseltest.internal.{VerilatorBackendAnnotation => USE_VERILATOR, WriteVcdAnnotation => DUMP_VCD}

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path, Paths}
import scala.annotation.tailrec
class UniProcessorCoupledCountersTester extends FlatSpec with Matchers with ChiselScalatestTester{



  val rdgen = new scala.util.Random(0)


  def initEnvironment: (Array[Instruction], Map[String, Register], Interpreter) = {

    val interp = new Interpreter
    val TEST_DATA = Range(1, 101)
    val COEFFS = Seq(187, 212, 61)

    import thyrio.assembly.Instruction._

    /// Register containing constant
    val const_0 = R(0)
    interp.env.register_file(const_0) = 0
    val const_1 = R(1)
    interp.env.register_file(const_1) = 1
    val const_coeffs = R(2)
    interp.env.register_file(const_coeffs) = COEFFS.size
    val const_FFFF = R(3)
    interp.env.register_file(const_FFFF) = 0xFFFF
    val const_x_mem_size = R(4)
    interp.env.register_file(const_x_mem_size) = TEST_DATA.size
    val const_y_mem_size = R(5)
    interp.env.register_file(const_y_mem_size) = TEST_DATA.size - COEFFS.size

    /// make a reset pulse
    interp.env.register_array(0) = 1

    var ix: Int = 20
    def freshReg = {
      val reg = R(ix)
      ix = ix + 1
      reg
    }
    // registers with dynamic values
    val reset_ptr = freshReg
    interp.env.register_file(reset_ptr) = 0

    val y_ptr = freshReg
    interp.env.register_file(y_ptr) = 0
    val x_ptr = freshReg
    interp.env.register_file(x_ptr) = 0

    val y_ptr_next_0 = freshReg
    val y_ptr_plus_1 = freshReg
    val reset_value = freshReg
    val x_ptr_next = freshReg
    val x_ptr_next_0 = freshReg
    val x_ptr_plus_1 = freshReg
    val cond_x = freshReg
    val cond_y_1 = freshReg
    val cond_y_0 = freshReg
    val cond_y = freshReg
    val cond_y_as_sel = freshReg

    val reset_cond = freshReg
    val mux_eq = CustomFunction(Seq.fill(ThyrioISA.DATA_BITS){0xcaca})

    /**
     * The program does the following at every virtual cycle:
     *
     * y_ptr =
     *    if (reset_value)
     *      0
     *    else if (x_ptr > const_coeffs && y_ptr < const_y_mem_size)
     *      y_ptr + 1
     *    else
     *      y_ptr
     * x_ptr =
     *    if (reset_value)
     *      0
     *    else if (x_ptr < const_x_mem_size)
     *      x_ptr + 1
     *    else
     *      x_ptr
     * reset_value = register_array[reset_ptr]
     * reset_ptr = reset_ptr + 1
     */
    val program = Array[Instruction](
      // compute the condition for guarding increment of y_ptr
      SetLessThanUnsigned(cond_y_0, y_ptr, const_y_mem_size),
      SetGreaterThanUnsigned(cond_y_1, x_ptr, const_coeffs),
      SetLessThanUnsigned(cond_x, x_ptr, const_x_mem_size),
      Add2(x_ptr_plus_1, x_ptr, const_1),
      And2(cond_y, cond_y_0, cond_y_1),
      Add2(y_ptr_plus_1, y_ptr, const_1),
      LocalLoad(reset_value, reset_ptr, 0),
      Add2(cond_y_as_sel, cond_y, const_FFFF), // we want to create a fanout from cond_y, to do so we
      // first add it to 0xFFFF
      Mux2(x_ptr_next_0, x_ptr, x_ptr_plus_1), // another way of muxing is using custom functions
      SetEqual(reset_cond, reset_value, const_1),
      Xor2(cond_y_as_sel, cond_y_as_sel, const_FFFF), // then perform a not (through xor with 1 is not)
      Mux2(x_ptr_next, x_ptr_next_0, const_0),
      Add2(reset_ptr, reset_ptr, const_1),
      Custom0(y_ptr_next_0, mux_eq, const_0, cond_y_as_sel, y_ptr_plus_1, y_ptr), // muxing with custom function requires a fanout operation
      Add2(x_ptr, x_ptr_next, const_0),
      Nop(),
      Mux2(y_ptr, y_ptr_next_0, const_0),
////      Send(x_ptr, x_ptr, 4, 4),
////      Send(y_ptr, y_ptr, 4, 4),
//      Nop(),
//      Nop()
    )
    (program, Map("y_ptr" -> y_ptr, "x_ptr" -> x_ptr), interp)
  }


  // create MUX LUT equations
  val equations: Seq[Seq[Int]] = Seq.fill(32)(Seq.fill(16)(0xcaca))



  behavior of "Processor"

  it should "match interpretation instruction by instruction in non-pipelined mode" in {
    // insert extra NoPs to break any RAW deps
    val (program: Array[Instruction], vars: Map[String, Register], interp: Interpreter) = initEnvironment
    def transformInst[T](inst: Instruction)(transform: Register => T): T = {
      import thyrio.assembly.Instruction._
      inst match {
        case Custom0(rd, func, rs1, rs2, rs3, rs4) =>
          transform(rd)
        case Add2(rd, rs1, rs2) =>
          transform(rd)
        case Or2(rd, rs1, rs2) =>
          transform(rd)
        case And2(rd, rs1, rs2) =>
          transform(rd)
        case Xor2(rd, rs1, rs2) =>
          transform(rd)
        case Mult2(rd, rs1, rs2) =>
          transform(rd)
        case Mux2(rd, rs1, rs2) =>
          transform(rd)
        case SetLessThanSigned(rd, rs1, rs2) =>
          transform(rd)
        case SetLessThanUnsigned(rd, rs1, rs2) =>
          transform(rd)
        case SetGreaterThanUnsigned(rd, rs1, rs2) =>
          transform(rd)
        case SetGreaterThanSigned(rd, rs1, rs2) =>
          transform(rd)
        case SetEqual(rd, rs1, rs2) =>
          transform(rd)
        case LocalLoad(rd, base, offset) =>
          transform(rd)
        case _ =>
          transform(R(0))
      }
    }
    val non_piped_program = program.map{inst =>
      val send_inst = transformInst(inst){ rd =>
        Send(rd, rd, 4, 4)
      }
      inst match {
        case Nop() | Send(_, _, _, _) =>
          Array(inst)
        case _ =>
          Array(inst, Nop(), Nop(), send_inst, Nop(), Nop(), Nop(), Nop(), Nop(), Nop())
      }

    }

    test{
      new Processor(config = ThyrioISA,
        EQUATIONS = equations,
        INITIAL_REGISTERS = UniProcessorTestUtils.createMemoryDataFiles{
          interp.env.register_file.toSeq
        }{
          Paths.get("test_data_dir" + File.separator +
            sanitizeFileName(scalaTestContext.value.get.name) + File.separator + "rf.data").toAbsolutePath
        },
        INITIAL_ARRAY = UniProcessorTestUtils.createMemoryDataFiles{
          interp.env.register_array.toSeq
        }{
          Paths.get("test_data_dir" + File.separator +
            sanitizeFileName(scalaTestContext.value.get.name) + File.separator + "ra.data").toAbsolutePath
        }
    )}.withAnnotations(Seq(USE_VERILATOR)) { implicit dut =>

      UniProcessorTestUtils.programProcessor(
        non_piped_program.flatten.map(inst => Assembler.assemble(inst)(equations)),
        4, 30, 20, dut
      ){
        rdgen.nextInt(2) == 0
      }

//      streamInstructions(non_piped_program.flatten)
      val num_vcycles = 2060
      val expected = Range(0, num_vcycles).flatMap { i =>
        non_piped_program.toSeq.map { b =>
          interp.run(b)
          val reg_rd = transformInst(b(0)) { rd => rd }
          (reg_rd, interp.env.register_file(reg_rd), b(0))
        }.filter(_._1.index > 0)
      }
      println(interp.env.register_file(vars("x_ptr")))
      dut.clock.setTimeout((non_piped_program.flatten.length + 30) * num_vcycles + 10000)
      @tailrec
      def executeFor(gold: Seq[(Register, Int, Instruction)]): Unit = {
        if (gold.nonEmpty) {
          dut.clock.step(1)
          if (dut.io.packet_out.valid.peek().litToBoolean) {
            val reg = (dut.io.packet_out.address.peek().litValue().toInt)
            val value = dut.io.packet_out.data.peek().litValue().toInt
            println(s"Finished ${gold.head._3}")
            println(s"expected ${gold.head._1} = ${gold.head._2}")
            println(s"got ${R(reg)} = ${value}")
            dut.io.packet_out.address.expect(gold.head._1.index.U)
            dut.io.packet_out.data.expect(gold.head._2.U)
            executeFor(gold.tail)
          } else {
            executeFor(gold)
          }
        }
      }

      executeFor(expected)

    }
  }

  it should "match the interpretation in pipelined execution" in {
    val (program: Array[Instruction], vars: Map[String, Register], interp: Interpreter) = initEnvironment

    val program_with_send = program ++ Array(
      Send(vars("x_ptr"), vars("x_ptr"), 4, 4),
      Nop(),
      Send(vars("y_ptr"), vars("y_ptr"), 4, 4),
      Nop(),
      Nop(),
      Nop(),
      Nop(),
      Nop(),
    )

    test{
      new Processor(config = ThyrioISA,
        EQUATIONS = equations,
        INITIAL_REGISTERS = UniProcessorTestUtils.createMemoryDataFiles{
          interp.env.register_file.toSeq
        }{
          Paths.get("test_data_dir" + File.separator +
            sanitizeFileName(scalaTestContext.value.get.name) + File.separator + "rf.data").toAbsolutePath
        },
        INITIAL_ARRAY = UniProcessorTestUtils.createMemoryDataFiles{
          interp.env.register_array.toSeq
        }{
          Paths.get("test_data_dir" + File.separator +
            sanitizeFileName(scalaTestContext.value.get.name) + File.separator + "ra.data").toAbsolutePath
        }
      )}.withAnnotations(Seq(USE_VERILATOR)){ implicit dut =>

      val num_vcycles = 2050
      val expected = Range(0, num_vcycles).flatMap { i =>
        interp.run(program)
        Seq(
          (vars("x_ptr"), interp.env.register_file(vars("x_ptr"))),
          (vars("y_ptr"), interp.env.register_file(vars("y_ptr")))
        )
      }
      dut.clock.setTimeout((program_with_send.length + 30) * num_vcycles + 10000)

      UniProcessorTestUtils.programProcessor(
        program_with_send.map(inst => Assembler.assemble(inst)(equations)),
        4, 30, 20, dut
      ){
        rdgen.nextInt(2) == 0
      }

      @tailrec
      def executeChecked(gold: Seq[(Register, Int)]): Unit = {
        if (gold.nonEmpty) {
          dut.clock.step(1)
          if (dut.io.packet_out.valid.peek().litToBoolean) {
            val reg = (dut.io.packet_out.address.peek().litValue().toInt)
            val value = dut.io.packet_out.data.peek().litValue().toInt
            println(s"expected ${gold.head._1} = ${gold.head._2}")
            println(s"got ${R(reg)} = ${value}")
            dut.io.packet_out.address.expect(gold.head._1.index.U)
            dut.io.packet_out.data.expect(gold.head._2.U)
            executeChecked(gold.tail)
          } else {
            executeChecked(gold)
          }
        }
      }

      executeChecked(expected)
    }

  }

}
