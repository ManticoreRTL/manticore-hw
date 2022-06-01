package manticore.machine.memory

import Chisel._
import chisel3.tester.testableClock
import chisel3.tester.testableData
import chiseltest.ChiselScalatestTester
import chiseltest.VerilatorBackendAnnotation
import chiseltest.WriteVcdAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import scala.collection.mutable


class CacheTester extends AnyFlatSpec with ChiselScalatestTester with Matchers {


  val rdgen = new scala.util.Random(0)

  case class MemoryInputIF(raddr: Int, waddr: Int, wline: BigInt, start: Boolean, cmd: CacheBackendCommand.Type)
  case class MemoryOutputIF(done: Boolean, rline: BigInt)
  class MemorySpec {
    println("Initializing memory...")

    var mem: Array[BigInt] = Array.fill(1 << 20){
      // cache line is 256 bits, or 4 long words (64 * 4)

//      val vs = Seq.fill(256 / 64)(rdgen.nextLong & Long.MaxValue).map(BigInt(_))

      val v = Seq.fill(4)(rdgen.nextLong() & Long.MaxValue).zipWithIndex.foldRight(BigInt(0)){
        case ((word, index), res) => res + (BigInt(word) << (index * 64))
      }
      v
    }
    var latency = 0
    var idle = true
    var rline: BigInt = 0
    def evaluate(input: MemoryInputIF): MemoryOutputIF = {
      if (idle) {
        if(input.start) {
          idle = false
          // pick a random memory access latency
          latency = 8 max rdgen.nextInt(40)
          if (input.cmd == CacheBackendCommand.WriteBack) {
            mem(input.waddr) = input.wline
            rline = mem(input.raddr)
          } else if (input.cmd == CacheBackendCommand.Read) {
            rline = mem(input.raddr)
          } else if (input.cmd == CacheBackendCommand.Write) {
            mem(input.waddr) = input.wline
            rline = 0
          }
        }
        MemoryOutputIF(done = false, rline = 0)
      } else {
        latency = latency - 1
        if (latency == 0) {
          idle = true
          MemoryOutputIF(done = true, rline)
        } else {
          MemoryOutputIF(done = false, 0)
        }

      }
    }
    def read(addr: Int): Int = {
      val word_addr = addr & (~0x0F)
      val line = mem(word_addr)
      val offset =  addr & 0x0F
      val word = line >> (16 * offset)
      (word & 0x000FFFF).toInt
    }
    def createRequest(dut: Cache): MemoryInputIF = {
      MemoryInputIF(
        dut.io.back.raddr.peek().litValue.toInt,
        dut.io.back.waddr.peek().litValue.toInt,
        dut.io.back.wline.peek().litValue,
        dut.io.back.start.peek().litToBoolean,
        CacheBackendCommand(dut.io.back.cmd.peek().litValue.toInt)
      )
    }

  }

  case class Access(is_write: Boolean, addr: Int, value: Int)
  //    case class SimState(outstanding: Option[Access], )


  // warm up the cache by reading the loading values in the addr_list
  def loadAddresses(addr_list: Seq[Int], mem_spec: MemorySpec)(implicit dut: Cache): Unit = {
    def evaluteMemorySpec = {
      val mem_req = mem_spec.createRequest(dut)
      val mem_resp = mem_spec.evaluate(mem_req)
      dut.io.back.done.poke(mem_resp.done.B)
      dut.io.back.rline.poke(mem_resp.rline.U)
    }
    @tailrec
    def checkedRead(raccess: Option[Access])(reads_left: Seq[Int]): Unit = {
      if (reads_left.nonEmpty || raccess.nonEmpty) {

        val n = raccess match {
          // no pending loads, create one
          case None =>
            val addr = reads_left.head
            dut.io.front.addr.poke(addr.U)
            dut.io.front.start.poke(true.B)
            dut.io.front.cmd.poke(CacheCommand.Read)
            evaluteMemorySpec
            (Some(Access(false, addr, mem_spec.read(addr))), reads_left.tail)
          case a @ Some(Access(_, addr, value)) =>
            dut.io.front.start.poke(false.B)
            dut.io.front.addr.poke(0.U)
            dut.io.front.wdata.poke(0.U)
            evaluteMemorySpec
            if (dut.io.front.done.peek().litToBoolean) {
              dut.io.front.rdata.expect(value.U, s"incorrect load from address 0x$addr%0x")
              (None, reads_left)
            } else {
              (a, reads_left)
            }
        }
        dut.clock.step()
        checkedRead(n._1)(n._2)
      }
    }

    checkedRead(None)(addr_list)
  }

  // perform a write and then read the same value from the cache, ensure that the cache does not go to memory during
  // this process and ensure the loaded value is the stored one
  def checkReadAfterWrite(hit_addr: Seq[(Int, Int)])(implicit dut: Cache): Unit = {
    @tailrec
    def loopSingleWrite(access_address : (Option[Access], Seq[(Int, Int)])): Unit = {
      val access = access_address._1
      val addr_list = access_address._2
      if (access.nonEmpty || addr_list.nonEmpty) {
        val x: (Option[Access], Seq[(Int, Int)]) = access match {
          case None =>
            /// perform a write
            dut.clock.step()
            val addr = addr_list.head._1
            val value = addr_list.head._2
            dut.io.front.addr.poke(addr.U)
            dut.io.front.wdata.poke(value.U)
            dut.io.front.start.poke(true.B)
            dut.io.front.cmd.poke(CacheCommand.Write)
            //              println(s"Writing mem[${addr}] = ${value}")
            dut.io.back.start.expect(false.B, "Write should be a hit!")
            dut.clock.step()
            (Some(Access(true, addr, value)), addr_list.tail)
          case a @ Some(Access(true, addr, value)) =>
            dut.io.back.start.expect(false.B, "Write should be a hit!")
            if (dut.io.front.done.peek().litToBoolean) {
              // store is finished, now load the stored value..
              dut.clock.step()
              dut.io.front.start.poke(true.B)
              dut.io.front.addr.poke(addr.U)
              dut.io.front.wdata.poke(0.U)
              dut.io.front.cmd.poke(CacheCommand.Read)
              dut.clock.step()
              (Some(Access(false, addr, value)), addr_list)
            } else {
              // do nothing, wait for store to finish
              dut.io.front.start.poke(false.B)
              dut.clock.step()
              (a, addr_list)
            }
          case a @ Some(Access(false, addr, value)) =>
            // check if the read is finished
            dut.io.back.start.expect(false.B, "Read should be a hit!")
            if (dut.io.front.done.peek().litToBoolean) {
              // read is finished, check the value
              dut.io.front.rdata.expect(value.U)
              dut.io.front.start.poke(false.B)

              (None, addr_list)
            } else {
              dut.io.front.start.poke(false.B)
              dut.clock.step()
              (a, addr_list)
            }
        }
        loopSingleWrite(x)
      }
    }
    loopSingleWrite((None, hit_addr))
    dut.clock.step()
  }

  def resetCache(implicit dut: Cache) = {
    @tailrec
    def wait(): Unit = {
      dut.clock.step()
      dut.io.back.start.expect(false.B)
      if (dut.io.front.done.peek().litToBoolean) {
        println("Cache reset complete")
        dut.io.front.start.poke(false.B)
      } else {
        dut.io.front.start.poke(false.B)
        wait()
      }
    }
    dut.clock.setTimeout(1 << 13)
    dut.io.front.start.poke(true.B)
    dut.io.front.cmd.poke(CacheCommand.Reset)
    wait()
    dut.clock.step()

  }

  def flushCache(mem_spec: MemorySpec)(implicit dut: Cache): Unit = {
    def evaluteMemorySpec() = {
      val mem_req = mem_spec.createRequest(dut)
      val mem_resp = mem_spec.evaluate(mem_req)
      dut.io.back.done.poke(mem_resp.done.B)
      dut.io.back.rline.poke(mem_resp.rline.U)
      //      dut.io.back
    }
    @tailrec
    def wait(): Unit = {
      dut.clock.step()
      evaluteMemorySpec()
      if (dut.io.front.done.peek().litToBoolean) {
        println("Cache reset complete")
        dut.io.front.start.poke(false.B)
      } else {
        dut.io.front.start.poke(false.B)
        wait()
      }
    }
    dut.clock.setTimeout(1 << 13)
    dut.io.front.start.poke(true.B)
    dut.io.front.cmd.poke(CacheCommand.Flush)
    wait()
    dut.clock.step()
  }

  def writeToCache(writes: Seq[(Int, Int)], mem_spec: MemorySpec)(implicit dut: Cache): Unit = {

    def evaluteMemorySpec = {
      val mem_req = mem_spec.createRequest(dut)
      val mem_resp = mem_spec.evaluate(mem_req)
      dut.io.back.done.poke(mem_resp.done.B)
      dut.io.back.rline.poke(mem_resp.rline.U)
//      dut.io.back
    }
    @tailrec
    def loop(waccess: Option[Access])(writes_left: Seq[(Int, Int)]): Unit = {
      if (writes_left.nonEmpty || waccess.nonEmpty) {

        val n = waccess match {
          // no pending loads, create one
          case None =>
            val addr = writes_left.head._1
            val value = writes_left.head._2
            dut.io.front.addr.poke(addr.U)
            dut.io.front.wdata.poke(value.U)
            dut.io.front.start.poke(true.B)
            dut.io.front.cmd.poke(CacheCommand.Write)
            evaluteMemorySpec
            (Some(Access(true, addr, value)), writes_left.tail)
          case a@Some(Access(_, addr, value)) =>
            dut.io.front.start.poke(false.B)
            dut.io.front.addr.poke(0.U)
            dut.io.front.wdata.poke(0.U)
            evaluteMemorySpec
            if (dut.io.front.done.peek().litToBoolean) {
              (None, writes_left)
            } else {
              (a, writes_left)
            }
        }
        dut.clock.step()
        loop(n._1)(n._2)
      }
    }

    loop(None)(writes)
  }
  behavior of "Cache"
  it should "respect raw dependencies" in {
    test(new Cache()).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { implicit dut =>
      val mem_spec = new MemorySpec
      dut.clock.step()
      println("Resetting cache")
      resetCache

      println("Warming up the cache with sequential loads")

      val hit_addr = Range(0, 4096).map(_ << 4)
      loadAddresses(hit_addr, mem_spec)
      println("Dirtying the loaded values")
      // an address to every half-word in the cache
      val whit_addr = hit_addr.flatMap{ addr => Range(0, 1 << 4).map(addr + _)}
      val whit_val = whit_addr.map(_ => rdgen.nextInt(1 << 16))

      checkReadAfterWrite(whit_addr.zip(whit_val))
      // all the cache line are dirty now, any read miss should result in a write back
      println("Testing capacity read miss and write-back")
      val miss_addr = hit_addr.map(_ + ( 4096 << 4 ) + rdgen.nextInt(1 << 4))

      dut.clock.step(20)

      loadAddresses(miss_addr, mem_spec)
      println("Checking read values")
      // assert that the values have been written back to memory
      whit_addr.zip(whit_val).foreach{ case (addr, value) =>
        mem_spec.read(addr) should be (value)
      }
      dut.clock.step()
    }
  }

  it should "be able to flush dirty lines" in {

    test(new Cache).withAnnotations(Seq(VerilatorBackendAnnotation)) { implicit dut =>

      val mem_spec = new MemorySpec
      dut.clock.step()
      println("Resetting cache")
      resetCache

      val dirty_addresses = Range(0, 4096).map(index => (index << 4) + rdgen.nextInt(1 << 4))
      val dirty_values = dirty_addresses.map(_ => rdgen.nextInt(1 << 16))


      println("Dirtying cache lines")
      writeToCache(dirty_addresses zip dirty_values, mem_spec)

      dut.clock.step()

      println("Flushing dirty lines")
      flushCache(mem_spec)

      println("Checking flushed values")
      dirty_addresses zip dirty_values foreach { case (addr, value) =>
        mem_spec read addr should be (value)
      }
    }
  }


}
