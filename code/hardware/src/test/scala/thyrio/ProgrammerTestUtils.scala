package thyrio



object ProgrammerTestUtils {

  class MemoryStreamSpec(val DimX: Int, val DimY: Int, val isa: ISA, rdgen: scala.util.Random) {

    val xy: Seq[(Int, Int)] = rdgen.shuffle(Range(0, DimX).flatMap(x => Range(0, DimY).map(y => (x, y))))

    val streams: Seq[Seq[Int]] = xy.map { _ =>
      val body_length = rdgen.nextInt(20)
      val epilogue_length = rdgen.nextInt(20)
      val sleep_length = rdgen.nextInt(300)
      (body_length +: Seq.fill(body_length * (isa.NUM_BITS / isa.DATA_BITS)) {
        rdgen.nextInt(1 << isa.DATA_BITS)
        //            0
      } :+
        epilogue_length :+ sleep_length)
    }

    val content: Seq[Int] = streams.zip(xy).flatMap{ case(s, (x, y)) =>
      val dest = (y << (isa.DATA_BITS / 2)) | x
      dest +: s
    }.toArray

    val expected_stream: Seq[(Int, Int, Int)] = streams.zip(xy).flatMap { case (s, (x, y)) =>
      s.map{value  => (value, x, y) }
    } ++ xy.map{case(x, y) => (y, x)}.sorted.map { case (y, x) =>
      (DimX * DimY + 2 - (x + y * DimX), DimX - 1 - x, DimY - 1 -y)
    }

    val base_address: Int = rdgen.nextInt(100)

    def apply(address: Int): Int = {
      if (!checkAddress(address)) {
        throw new IndexOutOfBoundsException(s"${address} not in  [${base_address}, ${base_address + content.length}")
      } else {
        content(address - base_address)
      }
    }
    def checkAddress(address: Int): Boolean = (base_address < address || address < base_address + content.length)

  }
}
