package memory


import Chisel._
import chisel3.experimental.ChiselEnum
import chisel3.stage.ChiselStage


/**
 * Cache back-end interface. The backend interface connects to a module that talks to the memory through a bus (e.g.,
 * AXI). Such a back-end should be able to perform a single read or a write-back accompanied by a read.
 *
 * @param CacheLineBits the number of bits in a cache line, e.g., 256
 * @param AddressBits   the number of bits in the "half-word address" aligned to the cache-line size
 *                      (i.e., not byte-addressable)
 */

object CacheBackendCommand extends Enumeration {
  type Type = Value
  val Read, Write, WriteBack = Value

  def instance() = UInt(log2Ceil(this.maxId).W)

}

class CacheBackInterface(CacheLineBits: Int, AddressBits: Int) extends Bundle {


  /**
   * a cache-line aligned half-word address to read
   */
  val raddr = Output(UInt(AddressBits.W))

  /**
   * write-back address, only used if the `write_back` signal is pulled high
   */

  val waddr = Output(UInt(AddressBits.W))

  /**
   * input signal to start the read or write-back-then-read operation
   */
  val start = Output(Bool())
  /**
   * output signal indicating the memory operation is finished
   */
  val done = Input(Bool())

  /**
   * input control signal, when pulled high, the value in `wline` is written to the address `waddr` a the line
   * addressed by `raddr` is read into `rline`
   */
  val cmd = Output(CacheBackendCommand.instance())

  /**
   * Read cache line from `raddr`
   */
  val rline = Input(UInt(CacheLineBits.W))

  /**
   * Cache line to be written at `waddr`
   */
  val wline = Output(UInt(CacheLineBits.W))

  /**
   * Disable the back-end interface
   */
  def disabled(): Unit = {
    start := false.B
    waddr := 0.U
    raddr := 0.U
    wline := 0.U
    cmd := CacheBackendCommand.Read.id.U

  }


  /**
   * start a write-back-then-read operation
   *
   * @param dirty_line    the dirty line to write back
   * @param write_address address of the dirty line in the memory (cache aligned half-word address)
   * @param read_address  address of the read location
   */
  def startWriteBackLine(dirty_line: UInt, write_address: UInt, read_address: UInt): Unit = {
    start := true.B
    wline := dirty_line
    cmd := CacheBackendCommand.WriteBack.id.U
    waddr := write_address
    raddr := read_address
  }

  /**
   * Start a read operation
   *
   * @param read_addr address to read from
   */
  def startReadLine(read_addr: UInt) = {
    start := true.B
    raddr := read_addr
    cmd := CacheBackendCommand.Read.id.U
  }

  def startWriteLine(flush_line: UInt, write_address: UInt): Unit = {
    start := true.B
    raddr := 0.U
    waddr := write_address
    wline := flush_line
    cmd := CacheBackendCommand.Write.id.U
  }


}


/**
 * Cache sub-system command types
 */
object CacheCommand extends ChiselEnum {
  val Reset, Read, Write, Flush = Value

}


/**
 * Cache front-end interface
 *
 * @param DataBits    number of bits in a half word (e.g., 16 bits)
 * @param AddressBits number of bits in a half-word address
 */
class CacheFrontInterface(DataBits: Int, AddressBits: Int) extends Bundle {

  val addr = Input(UInt(AddressBits.W))
  val wdata = Input(UInt(DataBits.W))
  val start = Input(Bool())
  val cmd = Input(CacheCommand.Type())
  val rdata = Output(UInt(DataBits.W))
  val done = Output(Bool())

  def ==>(that: CacheFrontInterface): Unit = {
    this.addr.dir match {
      case INPUT =>
        throwException("Cannot bind, invalid direction")
      case OUTPUT =>
        that.addr := this.addr
        that.wdata := this.wdata
        that.start := this.start
        that.cmd := this.cmd
        this.rdata := that.rdata
        this.done := that.done
    }
  }

  def bindToFlipped(thatFlipped: CacheFrontInterface): Unit = {
    require(thatFlipped.addr.dir == OUTPUT)
    this.addr := thatFlipped.addr
    this.wdata := thatFlipped.wdata
    this.start := thatFlipped.start
    this.cmd := thatFlipped.cmd
    thatFlipped.rdata := this.rdata
    thatFlipped.done := this.done
  }

}

/**
 * Cache front- and back-end interfaces
 *
 * @param DataBits      number of bits in a half-word
 * @param CacheLineBits number of bits in a cache line
 * @param AddressBits   number of bits in a half-word address
 */
class CacheInterface(DataBits: Int, CacheLineBits: Int, AddressBits: Int) extends Bundle {
  val front = new CacheFrontInterface(DataBits, AddressBits)
  val back = new CacheBackInterface(CacheLineBits, AddressBits)
}

object CacheConfig {

  val DataBits = 16
  // number of bits in a cache line
  val CacheLineBits = 256
  // number of bits of data in cache line per cache bank
  val BankLineBits = 64
  // total number of bits in a cache bank (includes data and tag)
  val BankLineTaggedBits = 72
  // number of tag bits in each cache bank, total tag bits are TagBits * NumBanks - 2, because 2 bits are used for valid
  // and dirty status bits in the last bank
  val TagBits: Int = BankLineTaggedBits - BankLineBits

  // number of address bits actually used by the front-end interface
  val UsedAddressBits = 40

  /**
   * The address provided by the front-end is a half-word address, that is zero-extended to a larger address space
   * with the following format
   * ----------------------------------------
   * | Tag Bits  | Index Bits | Offset Bits |
   * ----------------------------------------
   *
   * Each cache line is spread out among multiple banks, each bank then store the following piece of information:
   * -----------------------------------------------------------------------------------------------
   * | Tag (8-bit) | data (4 * id + 3) | data (4 * id + 2) | data (4 * id + 1) | data (4 * id + 0) |
   * -----------------------------------------------------------------------------------------------
   * Each data(i) is a half word, and id is the bank id. The tag part of the last bank is
   * ------------------------------
   * | Valid | Dirty | Tag (6-bit)|
   * ------------------------------
   *
   */


  // number of banks required to implement the cache
  val NumBanks: Int = CacheLineBits / BankLineBits


  // number of offset bits in the address
  val OffsetBits = log2Ceil(CacheLineBits / DataBits)

  // number of index bits in an address
  val IndexBits = 12

  val AddressBits: Int = (OffsetBits + IndexBits + TagBits * NumBanks)

  val CacheLineHalfWords: Int = CacheLineBits / DataBits
  val BankLineHalfWords: Int = BankLineBits / DataBits

  def frontInterface() = new CacheFrontInterface(DataBits, UsedAddressBits)

  def backInterface() = new CacheBackInterface(DataBits, UsedAddressBits)

  def interface() = new CacheInterface(DataBits, CacheLineBits, UsedAddressBits)

}

/**
 * Cache module
 */
class Cache extends Module {

  import CacheConfig._

  /**
   * The cache controller has 3 states `Idle`, `HitCheck`, and `WaitResponse`.
   * In the `Idle` state, the cache waits for the `io.front.start` signal to initiate either a read or write
   * (`io.front.wen`` is high) to the cache. This causes a transition to `HitCheck`, where a line from the cache is
   * fetched and its tag is compared against the provided address (`io.front.addr`). If the tags match and the
   * line is valid, we have a hit and the read data is returned or the provided `io.front.wdata` is written to the
   * cache line and the cache line is marked dirty.
   * In case of a miss, the memory back side interface is invoked. When a read miss to __clean__ line occurs, the memory
   * back-side interface only fetches the missing line and replaces the valid/invalid line in the cache. In case of
   * a read miss to a __dirty__ line, the line is first written back and then the newly read line is placed in the
   * cache.
   * A write miss to a dirty line also initiates a write back, but a write miss to a clean line only reads the missing
   * line from the memory, dirties it immediately and writes it to the cache.
   * against
   */
  object StateValue extends ChiselEnum {
    val Idle, HitCheck, WaitResponse, Flush, FlushCheck, FlushResponse, Reset = Value
  }

  require(DataBits == 16, "only 16-bit data width is implemented!")
  val io = IO(interface())

  val pstate = RegInit(StateValue.Type(), StateValue.Idle)


  case class BankCollection(module: URAM4kx72, id: Int) {
    val loaded_hwords = Wire(Vec(BankLineHalfWords, UInt(DataBits.W)))
    val cached_hwords = Wire(Vec(BankLineHalfWords, UInt(DataBits.W)))
    val cached_tag = Wire(UInt(TagBits.W))

    val write_hwords = Wire(Vec(BankLineHalfWords, UInt(DataBits.W)))
    val write_tag = Wire(UInt(TagBits.W))
    val write_addr = Wire(UInt(IndexBits.W))

    cached_tag := module.io.douta.head(TagBits)
    cached_hwords.zipWithIndex.foreach { case (chw, i) =>
      chw := module.io.douta((i + 1) * DataBits - 1, i * DataBits)
    }

    loaded_hwords.zipWithIndex.foreach { case (lhw, i) =>
      val flat_ix = id * BankLineHalfWords + i
      lhw := io.back.rline((flat_ix + 1) * DataBits - 1, flat_ix * DataBits)
    }

    //    cached_tag := module.io.douta.head(TagBits)


    // disable writing by default
    module.io.bweb := UInt("b" + "1" * (BankLineTaggedBits / 8))
    module.io.web := false.B

    //    module.io.addra := io.front.addr(IndexBits + OffsetBits - 1, OffsetBits)

    module.io.dinb := Cat(
      write_tag, Cat(write_hwords.reverse)
    )
    //    module.io.addrb := write_addr


  }

  val banks = Range(0, NumBanks).map { i =>
    BankCollection(Module(new URAM4kx72), i)
  }

  banks.last.cached_tag := banks.last.module.io.douta.head(TagBits).tail(2)


  val dirty_bit = Wire(Bool())
  val valid_bit = Wire(Bool())

  dirty_bit := banks.last.module.io.douta.head(2).tail(1)
  valid_bit := banks.last.module.io.douta.head(1)


  val addr = Reg(UInt(AddressBits.W))
  addr := io.front.addr
  val addr_reg = Reg(UInt(AddressBits.W))
  val wdata_reg = Reg(UInt(DataBits.W))
  val cmd_reg = Reg(CacheCommand.Type())
  // pointer to the next line to flush
  val flush_pointer = Reg(UInt(IndexBits.W))
  val flush_index = Reg(UInt(IndexBits.W))
  val hit = Wire(Bool())

  hit := (Cat(banks.map(_.cached_tag).reverse) === addr.head(TagBits * NumBanks)) & valid_bit

  // read cache line (excluding tag)
  val rdata_cache_line = Wire(Vec(CacheLineHalfWords, UInt(DataBits.W)))
  // loaded data cache line
  val ldata_cache_line = Wire(Vec(CacheLineHalfWords, UInt(DataBits.W)))
  banks.foreach { b =>
    val half_word_base = b.id * BankLineHalfWords
    b.cached_hwords.zipWithIndex.foreach { case (chword, hword_ix) =>
      rdata_cache_line(half_word_base + hword_ix) := chword

    }
    b.loaded_hwords.zipWithIndex.foreach { case (lword, hword_ix) =>
      ldata_cache_line(half_word_base + hword_ix) := lword
    }

    b.module.io.addrb := addr_reg(IndexBits + OffsetBits - 1, OffsetBits)
    b.module.io.addra := io.front.addr(IndexBits + OffsetBits - 1, OffsetBits)
    if (b == banks.last)
      b.cached_tag := Cat(UInt("b00"), b.module.io.douta.head(TagBits).tail(2))
    else
      b.cached_tag := b.module.io.douta.head(TagBits)

  }

  // read data from the cache
  val rdata = Wire(UInt(DataBits.W))
  // loaded data from memory
  val ldata = Wire(UInt(DataBits.W))
  rdata := rdata_cache_line(addr_reg(OffsetBits - 1, 0))
  ldata := ldata_cache_line(addr_reg(OffsetBits - 1, 0))

  val rdata_reg = Reg(UInt(DataBits.W))


  val write_back_addr: UInt = Wire(UInt(AddressBits.W))
  write_back_addr := Cat(banks.map(_.cached_tag).reverse ++ Seq(addr_reg.tail(TagBits * NumBanks)))
  // default values
  addr_reg := addr_reg
  val addr_tag = Wire(UInt((TagBits * NumBanks).W))
  addr_tag := addr_reg.head(TagBits * NumBanks)

  io.front.done := false.B

  io.back.disabled() // default values, disable the backend
  io.front.rdata := rdata

  switch(pstate) {


    is(StateValue.Idle) {
      when(io.front.start) {
        addr_reg := io.front.addr
        wdata_reg := io.front.wdata
        cmd_reg := io.front.cmd
        flush_pointer := 0.U
        flush_index := 0.U
        val decoded = Wire(StateValue.Type())
        when(io.front.cmd === CacheCommand.Read || io.front.cmd === CacheCommand.Write) {
          addr_reg := io.front.addr
          banks.foreach(b => b.module.io.addra := io.front.addr(IndexBits + OffsetBits - 1, OffsetBits))
          // read the requested line from the banks
          decoded := StateValue.HitCheck
        }.elsewhen(io.front.cmd === CacheCommand.Flush) {

          decoded := StateValue.Flush
        }.elsewhen(io.front.cmd === CacheCommand.Reset) {

          decoded := StateValue.Reset
        }.otherwise {
          decoded := StateValue.Idle
        }
        pstate := decoded

      }
    }
    is(StateValue.HitCheck) {

      rdata_reg := rdata
      when(hit) {
        when(cmd_reg === CacheCommand.Write) {
          // write hit
          banks.foreach { b =>
            b.module.io.web := true.B
            // the cachline is valid and dirty
            if (b == banks.last)
              b.write_tag := Cat(UInt("b11"), b.cached_tag.tail(2))
            else
              b.write_tag := b.cached_tag
            b.write_hwords := b.cached_hwords
            b.write_hwords.zipWithIndex.foreach { case (hword, hword_ix) =>
              val hword_offset = b.id * BankLineHalfWords + hword_ix
              when(addr_reg(OffsetBits - 1, 0) === hword_offset.U) {
                hword := wdata_reg
              }
              // otherwise gets the cached half-word
            }
          }
          io.front.done := true.B
        } otherwise {
          // read hit
          io.front.rdata := rdata
          io.front.done := true.B
        }
        pstate := StateValue.Idle
      } otherwise {
        val offset_mask = 0.U(OffsetBits.W)
        when(dirty_bit) {
          // the missed line is dirty, so we need to flush the cache line to the memory
          io.back.startWriteBackLine(
            Cat(rdata_cache_line.reverse),
            Cat(write_back_addr.head(AddressBits - OffsetBits), offset_mask),
            Cat(addr_reg.head(AddressBits - OffsetBits), offset_mask))
        } otherwise {
          // fetch a line from the memory and replace the existing "clean" line
          io.back.startReadLine(
            Cat(addr_reg.head(AddressBits - OffsetBits), offset_mask))
        }
        pstate := StateValue.WaitResponse
      }
    }
    is(StateValue.WaitResponse) {
      // wait for the "done" signal from the backend
      when(io.back.done) {

        when(cmd_reg === CacheCommand.Write) {
          banks.foreach { b =>
            b.module.io.web := true.B
            if (b == banks.last)
              b.write_tag := Cat(
                UInt("b11"), // make the line dirty
                addr_tag((b.id + 1) * TagBits - 1, b.id * TagBits).tail(2))
            else
              b.write_tag := addr_tag((b.id + 1) * TagBits - 1, b.id * TagBits)
            b.write_hwords := b.loaded_hwords
            b.write_hwords.zipWithIndex.foreach { case (hword, hword_ix) =>
              val hword_offset = b.id * BankLineHalfWords + hword_ix
              when(addr_reg(OffsetBits - 1, 0) === hword_offset.U) {
                hword := wdata_reg
              }
              // otherwise gets the loaded half-word
            }

          }

          io.front.done := true.B
        } otherwise {
          // write the missed read line and finish
          banks.foreach { b =>
            b.module.io.web := true.B
            if (b == banks.last)
              b.write_tag := Cat(
                UInt("b10"),
                addr_tag((b.id + 1) * TagBits - 1, b.id * TagBits).tail(2)
              )
            else
              b.write_tag := addr_tag((b.id + 1) * TagBits - 1, b.id * TagBits)
            b.write_hwords := b.loaded_hwords
          }
          io.front.rdata := ldata
          io.front.done := true.B
        }
        pstate := StateValue.Idle
      }
    }

    is(StateValue.Flush) {

      banks.foreach(b => b.module.io.addra := flush_pointer)
      pstate := StateValue.FlushCheck
    }

    is(StateValue.FlushCheck) {

      when(valid_bit && dirty_bit) {
        val offset_mask = 0.U(OffsetBits.W)
        io.back.startWriteLine(
          Cat(rdata_cache_line.reverse),
          Cat(banks.map(_.cached_tag).reverse :+ flush_pointer :+ offset_mask))
        pstate := StateValue.FlushResponse
      } otherwise {
        when(flush_pointer === ((1 << IndexBits) - 1).U) {
          pstate := StateValue.Idle
          flush_pointer := 0.U
          io.front.done := true.B
        } otherwise {
          banks.foreach { b => b.module.io.addra := flush_pointer + 1.U }
          flush_pointer := flush_pointer + 1.U
          pstate := StateValue.FlushCheck
        }
      }
    }

    is(StateValue.FlushResponse) {

      when(io.back.done) {

        when(flush_pointer === ((1 << IndexBits) - 1).U) {
          pstate := StateValue.Idle
          flush_pointer := 0.U
          io.front.done := true.B
        } otherwise {
          banks.foreach { b => b.module.io.addra := flush_pointer + 1.U }
          flush_pointer := flush_pointer + 1.U
          pstate := StateValue.FlushCheck
        }

      }
    }

    is(StateValue.Reset) {

      when(flush_pointer === ((1 << IndexBits) - 1).U) {
        pstate := StateValue.Idle
        io.front.done := true.B
      } otherwise {
        banks.foreach { b =>
          b.module.io.addrb := flush_pointer
          b.module.io.web := true.B
          b.module.io.dinb := 0.U
        }
        flush_pointer := flush_pointer + 1.U
        pstate := StateValue.Reset
      }

    }

  }


}


class TailTest extends Module {

  val io = IO(
    new Bundle {
      val in = Input(UInt(32.W))
      val out = Output(UInt(32.W))
    }
  )

  io.out := Cat(UInt("b10"), io.in.tail(2))

}

object TailTestGen extends App {

  new ChiselStage().emitVerilog(new TailTest, Array("--target-dir", "gen-dir"))
  new ChiselStage().emitVerilog(new Cache(), Array("--target-dir", "gen-dir"))
}
