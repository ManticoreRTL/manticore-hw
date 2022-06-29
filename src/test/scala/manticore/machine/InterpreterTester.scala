package manticore.machine

import manticore.machine.assembly._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers



class InterpreterTester extends AnyFlatSpec with Matchers{


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
        Custom(R(5),
          CustomFunction(Array.fill(16)(BigInt(0xcaca)).toIndexedSeq),
          R(0), R(1), R(2), R(3)),
        Custom(R(6),
          CustomFunction(Array.fill(16)(BigInt(0xcaca)).toIndexedSeq),
          R(5), R(0), R(2), R(3)),
        SetValue(R(7), 0x1234),
        SetValue(R(8), 0x4567),
        SetValue(R(9), 0x89AB.toShort),
        SetValue(R(10), 0xCDEF.toShort),
        Custom(R(11), // select the first 4 bits from rs4, the next from rs3 and so on
          CustomFunction(Array(
            BigInt(0xAAAA),
            BigInt(0xAAAA),
            BigInt(0xAAAA),
            BigInt(0xAAAA),

            BigInt(0xCCCC),
            BigInt(0xCCCC),
            BigInt(0xCCCC),
            BigInt(0xCCCC),

            BigInt(0xF0F0),
            BigInt(0xF0F0),
            BigInt(0xF0F0),
            BigInt(0xF0F0),

            BigInt(0xFF00),
            BigInt(0xFF00),
            BigInt(0xFF00),
            BigInt(0xFF00)
          ).toIndexedSeq), R(10), R(9), R(8), R(7)
        )
      )
    )

    interp.env.register_file(4) should be (1231 + 9124)
    interp.env.register_file(5) should be (1231)
    interp.env.register_file(6) should be (9124)
    interp.env.register_file(11) should be (0xC964)

  }

}
