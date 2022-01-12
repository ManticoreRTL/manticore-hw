package manticore.assembly

import manticore.assembly.Instruction._
import scala.language.implicitConversions

case class Environment(
    register_file: Array[Int],
    register_array: Array[Int],
    memory: Array[Int],
    var select: Boolean,
    var predicate: Boolean
)

class Interpreter {

  val env = Environment(
    register_file = Array.fill(2048)(0),
    register_array = Array.fill(2048)(0),
    memory = Array.fill(1048576)(0),
    select = false,
    predicate = false
  )

  private def clipped(x: Int, bits: Int = 16) = x & ((1 << bits) - 1)
  def run(program: Array[Instruction]): Environment = {
    var pc: Int = 0
    while (pc < program.size) {
      eval(program(pc))
      pc = pc + 1
    }
    env
  }

  def eval(instruction: Instruction): Unit = {

    instruction match {
      case Custom0(rd, func, rs1, rs2, rs3, rs4) =>
        def asBitSeq(v: Int): Seq[Int] = {
          for (i <- 0 to 15) yield {
            val mask: Int = 0x00000001
            (v & (mask << i)) >> i
          }
        }
        val x = asBitSeq(env.register_file(rs1))
        val y = asBitSeq(env.register_file(rs2))
        val u = asBitSeq(env.register_file(rs3))
        val v = asBitSeq(env.register_file(rs4))

        val res = {
          for (i <- 0 to 15) yield {
            val o = (func.equation(i) >> (
              (x(i) << 3) | (y(i) << 2) | (u(i) << 1) | v(i)
            ) & 0x01) << i
            o
          }
        }.reduce(_ | _)

        env.register_file(rd) = clipped(res)

      case Add2(rd, rs1, rs2) =>
        val res: Int = clipped(env.register_file(rs1) + env.register_file(rs2))
        env.register_file(rd) = res

      case Or2(rd, rs1, rs2) =>
        env.register_file(rd) = clipped(
          env.register_file(rs1) | env.register_file(rs2)
        )

      case And2(rd, rs1, rs2) =>
        env.register_file(rd) = clipped(
          env.register_file(rs1) & env.register_file(rs2)
        )

      case Xor2(rd, rs1, rs2) =>
        env.register_file(rd) = clipped(
          env.register_file(rs1) ^ env.register_file(rs2)
        )

      case Mult2(rd, rs1, rs2) =>
        env.register_file(rd) = clipped(
          env.register_file(rs1) * env.register_file(rs2)
        )

      case Mux2(rd, rs1, rs2) =>
        env.register_file(rd) =
          if (env.select) env.register_file(rs2) else env.register_file(rs1)

      case SetLessThanSigned(rd, rs1, rs2) =>
        val v1: Short = env.register_file(rs1).toShort
        val v2: Short = env.register_file(rs2).toShort
        env.register_file(rd) = if (v1 < v2) 1 else 0
        env.select = v1 < v2

      case SetLessThanUnsigned(rd, rs1, rs2) =>
        env.register_file(rd) =
          if (env.register_file(rs1) < env.register_file(rs2)) 1 else 0
        env.select = env.register_file(rs1) < env.register_file(rs2)

      case SetGreaterThanUnsigned(rd, rs1, rs2) =>
        env.register_file(rd) =
          if (env.register_file(rs1) > env.register_file(rs2)) 1 else 0
        env.select = env.register_file(rs1) > env.register_file(rs2)

      case SetGreaterThanSigned(rd, rs1, rs2) =>
        val v1: Short = env.register_file(rs1).toShort
        val v2: Short = env.register_file(rs2).toShort
        env.register_file(rd) = if (v1 > v2) 1 else 0
        env.select = v1 > v2

      case SetEqual(rd, rs1, rs2) =>
        env.register_file(rd) =
          if (env.register_file(rs1) == env.register_file(rs2)) 1 else 0
        env.select = env.register_file(rs1) == env.register_file(rs2)

      case LocalLoad(rd, base, offset) =>
        val addr: Int = clipped((env.register_file(base) + offset).toInt, 11)

        val value: Int = env.register_array(addr)
        env.register_file(rd) = value

      case LocalStore(rs, base, offset) =>
        val addr: Int = clipped(
          env.register_file(base) + clipped(offset.toInt, 11)
        )
        val value: Int = env.register_file(rs)
        env.register_array(addr) = value

      case SetValue(rd, value) =>
        env.register_file(rd) = clipped(value)
      case Predicate(rs) =>

      case Send(target, rs, addressX, addressY)     =>
      case Expect(value, expected, message)         => ???
      case GlobalLoad(rd, addrlo, addrmid, addrhi)  => ???
      case GlobalStore(rs, addrlo, addrmid, addrhi) => ???
      case Nop()                                    =>
    }
  }

}
