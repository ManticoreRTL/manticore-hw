package manticore.machine

import org.scalatest.{FlatSpec, Matchers}
import manticore.machine.assembly._





class InterpreterTester extends FlatSpec with Matchers{


  behavior of "assembly interpreter"
  it should "interpret the instructions correctly" in {
    import manticore.machine.assembly.Instruction._

    val interp = new Interpreter

    interp.run(
      Array(
        SetValue(R(0), 0), // literal 0
        SetValue(R(1), 0xFFFF.toShort), // literal -1
        SetValue(R(2), 1231),
        SetValue(R(3), 9124),
        Add2(R(4), R(2), R(3)),
        // multiplex
        Custom0(R(5),
          CustomFunction(Array.fill(16)(0xcaca)),
          R(0), R(1), R(2), R(3)),
        Custom0(R(6),
          CustomFunction(Array.fill(16)(0xcaca)),
          R(5), R(0), R(2), R(3)),
        SetValue(R(7), 0x1234),
        SetValue(R(8), 0x4567),
        SetValue(R(9), 0x89AB.toShort),
        SetValue(R(10), 0xCDEF.toShort),
        Custom0(R(11), // select the first 4 bits from rs4, the next from rs3 and so on
          CustomFunction(Array(
            0xAAAA,
            0xAAAA,
            0xAAAA,
            0xAAAA,

            0xCCCC,
            0xCCCC,
            0xCCCC,
            0xCCCC,

            0xF0F0,
            0xF0F0,
            0xF0F0,
            0xF0F0,

            0xFF00,
            0xFF00,
            0xFF00,
            0xFF00

          )), R(10), R(9), R(8), R(7)
        )
      )
    )

    interp.env.register_file(4) should be (1231 + 9124)
    interp.env.register_file(5) should be (1231)
    interp.env.register_file(6) should be (9124)
    interp.env.register_file(11) should be (0xC964)

  }

}
