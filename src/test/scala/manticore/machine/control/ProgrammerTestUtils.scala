package manticore.machine.control

import manticore.machine.ISA
import manticore.machine.ISA

object ProgrammerTestUtils {

  class MemoryStreamSpec(
      val DimX: Int,
      val DimY: Int,
      val isa: ISA,
      rdgen: scala.util.Random
  ) {

    val xy: Seq[(Int, Int)] = rdgen.shuffle(
      Range(0, DimX).flatMap(x => Range(0, DimY).map(y => (x, y)))
    )

    val streams: Seq[Seq[Int]] = xy.map { _ =>
      val body_length     = rdgen.nextInt(20)
      val epilogue_length = rdgen.nextInt(20)
      val sleep_length    = rdgen.nextInt(300)
      (body_length +: Seq.fill(body_length * (isa.NumBits / isa.DataBits)) {
        rdgen.nextInt(1 << isa.DataBits)
        //            0
      } :+
        epilogue_length :+ sleep_length)
    }

    val content: Seq[Int] = streams
      .zip(xy)
      .flatMap { case (s, (x, y)) =>
        val dest = (y << (isa.DataBits / 2)) | x
        dest +: s
      }

    val expected_stream: Seq[(Int, Int, Int)] =
      streams.zip(xy).flatMap { case (s, (x, y)) =>
        s.map { value => (value, x, y) }
      } ++ Seq.tabulate(DimX) { y =>
        Seq.tabulate(DimY) { x =>
          // the first delay value is (DimX * DimY) - (DimX * DimY - 1)
          // that is fed to (DimX - 1, DimY - 1), then the next processor is
          // (DimX - 2, DimY - 1) which receives the same delay value because it
          // recieves it at the same time as (DimX - 1, DimY - 1). When the row
          // is fed, the the delay value is decreased by (DimX - 1) so that the
          // last row (corrsponding to y = 0) receives delay = 0
          val delay =
            (DimX * DimY - (DimX + DimY - 1)) - (DimX - 1) * (DimY - 1 - y)
          println(s"delay($x, $y) = $delay")
          (delay, x, y)
        }.reverse
      }.reverse.flatten

    val base_address: Int = rdgen.nextInt(100)

    def apply(address: Int): Int = {
      if (!checkAddress(address)) {
        throw new IndexOutOfBoundsException(
          s"${address} not in  [${base_address}, ${base_address + content.length}"
        )
      } else {
        content(address - base_address)
      }
    }

    def checkAddress(address: Int): Boolean =
      (base_address < address || address < base_address + content.length)

  }
}
