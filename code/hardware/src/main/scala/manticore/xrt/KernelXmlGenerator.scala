package manticore.xrt


import manticore.xrt.KernelInfo.KernelSlaveRegister
import manticore.xrt.KernelInfo.KernelMemoryInterface
import java.nio.file.Files
import java.io.PrintWriter
import java.io.File
import java.nio.file.Paths
import java.nio.file.StandardCopyOption

object KernelXmlGenrator {


  def apply(
      kernel_name: String,
      master: Seq[KernelMemoryInterface],
      args: Seq[KernelSlaveRegister],
      outdir: String
  ) = {

    def getMasterPort(m: KernelMemoryInterface) = {
      <port name={m.name} portType="addressable" mode="master" base="0x0" range="0xFFFFFFFFFFFFFFFF" dataWidth={m.width.toString}/>
    }
    
    def getSize(r: KernelSlaveRegister) = {
        if (r.is_pointer) {
            8
        } else {
            r.cpp_type match {
                case "unsigned char" => 1
                case "unsigned short" => 2
                case "unsigned int" => 4
                case "unsigned long" => 8
                case _ => throw new Exception(s"Unsupported kernel xml type ${r.cpp_type}")
            }
        }
    }
    val xml =
      <root versionMajor="1" versionMinor="0">
            <kernel name={kernel_name} language="ip_c" vlnv={"vlsc.epfl.ch:kernes:" + kernel_name + ":1.0"} attributes="" preferredWorkGroupSizeMultiple="0" workGroupSize="1" interrupt="true">
                <ports>
                    {master.map(getMasterPort)}
                    <port name="s_axi_control" portType="addressable" mode="slave" base="0x0" range="0x1000" dataWidth="32"/>
                </ports>
                <args>
                    
                    {
                        args.zipWithIndex.map { case (p, i) => 
                            <arg id={p.id.toString()} addressQualifier={if (p.is_pointer) "1" else "0"} port={p.port_interface} hostOffset="0x0" offset={f"0x${p.offset}%x"} size={f"0x${getSize(p)}%x"} type={p.cpp_type}/>    
                        }
                    }

                </args>
                <compileWorkGroupSize x="1" y="1" z="1"/>
                <maxWorkGroupSize x="1" y="1" z="1"/>
            </kernel>
        </root>
    
    
    val fp = Files.createTempFile(kernel_name, "_kernel.xml")
    
    val pw = new PrintWriter(fp.toFile())
    
    
    pw.print(new scala.xml.PrettyPrinter(300, 4).format(xml))
    pw.close()

    val out_dir = Files.createDirectories(Paths.get(outdir))
    
    Files.copy(
        fp, out_dir.resolve(kernel_name + "_kernel.xml"),
         StandardCopyOption.REPLACE_EXISTING
    )

  }

}


