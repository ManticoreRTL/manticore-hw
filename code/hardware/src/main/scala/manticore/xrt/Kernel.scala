package manticore.xrt

import chisel3._
import manticore.core.HostRegisters
import manticore.ManticoreFullISA
import manticore.core.DeviceRegisters

class MemoryPointers extends Bundle {
  val pointer: Vec[UInt] = Vec(4, UInt(64.W))
}

object KernelInfo {
  case class KernelMemoryInterface(
      name: String,
      width: Int = 256,
      bundle: String
  ) {
    def portName = name + "_" + bundle
  }
  case class KernelSlaveRegister(
      id: Int,
      name: String,
      offset: Int,
      cpp_type: String,
      is_pointer: Boolean,
      port_interface: String
  ) {
    def withId(new_id: Int) = KernelSlaveRegister(
      new_id,
      this.name,
      this.offset,
      this.cpp_type,
      this.is_pointer,
      this.port_interface
    )

  }
}
object KernelGenerator extends App {

  val out_dir = "gen-dir"
  val part_num = "xcu250-figd2104-2l-e"
  def lowerType(data: Data): Seq[String] = {

    data match {
      case vec: Vec[_] =>
        vec.map(x => AxiSlaveGenerator.translateType(x.asInstanceOf[UInt]))
      case elem: UInt =>
        Seq(AxiSlaveGenerator.translateType(elem))
      case _ =>
        throw new Exception(
          "Can not lower anything other than UInt and Vec[UInt]"
        )
    }

  }

  def makeRegs(
      name: String,
      t: Data,
      prefix: String
  ): Seq[AxiSlaveGenerator.AxiRegister] = {

    val cpp_type = lowerType(t)
    val cpp_name = cpp_type match {
      case x +: Nil  => Seq(name)
      case x +: tail => cpp_type.indices.map(i => name + s"_${i}")
    }

    cpp_name.zip(cpp_type).map { case (_n, _t) =>
      AxiSlaveGenerator.AxiRegister(_n, _t, prefix)
    }
  }

  val host_regs =
    new HostRegisters(ManticoreFullISA).elements.flatMap { case (name, t) =>
      makeRegs(name, t, "h")
    }.toSeq
  val dev_regs = new DeviceRegisters(ManticoreFullISA).elements.flatMap {
    case (name, t) =>
      makeRegs(name, t, "d")
  }.toSeq

  val pointer_regs = (new MemoryPointers).elements.flatMap { case (name, t) =>
    makeRegs(name, t, "p")
  }.toSeq

  // generate the axi slave module

  val port_map = AxiSlaveGenerator(
    pointer_regs ++ host_regs ++ dev_regs,
    out_dir, part_num
  )

  val mem_if = Seq.tabulate(4) { i =>
    KernelInfo.KernelMemoryInterface("memory_gateway", 256, "ddr_" + i)
  }

  require(pointer_regs.size == 4)

  val slave_regs_args = {
    pointer_regs.zip(mem_if).map { case (r, mem) =>
      KernelInfo.KernelSlaveRegister(
        id = 0,
        name = r.prefix + "_" + r.name,
        offset = port_map(r).sortBy(_.hw_name).head.offset,
        cpp_type = r.cppType,
        is_pointer = true,
        port_interface = mem.portName
      )
    } ++ (host_regs ++ dev_regs).map { r =>
      KernelInfo.KernelSlaveRegister(
        id = 0,
        name = r.prefix + "_" + r.name,
        offset = port_map(r).sortBy(_.hw_name).head.offset,
        cpp_type = r.cppType,
        is_pointer = false,
        port_interface = "s_axi_control"
      )
    }
  }.zipWithIndex.map { case (arg, i) => arg.withId(i) }

  println("Creating kernel xml")

  // // generate the axi master modules
  // mem_if.foreach { m =>  
  //   AxiMasterGenerator(m.name, m.bundle, out_dir, part_num)
  // }

  


  KernelXmlGenrator(
    "manticore",
    mem_if,
    slave_regs_args,
    out_dir
  )

  

}
