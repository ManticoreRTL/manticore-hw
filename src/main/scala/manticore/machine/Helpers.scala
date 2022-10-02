package manticore.machine

import chisel3._

import chisel3.experimental.annotate
import chisel3.experimental.ChiselAnnotation
import firrtl.annotations.Annotation
import firrtl.AttributeAnnotation
import chisel3.util.Pipe

object Helpers {

  object SrlStyle {
    trait SrlStyleAnnotation { def name: String }

    object Reg       extends SrlStyleAnnotation { def name: String = "register"    }
    object Srl       extends SrlStyleAnnotation { def name: String = "srl"         }
    object SrlReg    extends SrlStyleAnnotation { def name: String = "srl_reg"     }
    object RegSrl    extends SrlStyleAnnotation { def name: String = "reg_srl"     }
    object RegSrlReg extends SrlStyleAnnotation { def name: String = "reg_srl_reg" }
    object Block     extends SrlStyleAnnotation { def name: String = "block"       }
  }

  def InlinePipeWithStyle[T <: Data](
      data: T,
      latency: Int = 1,
      style: SrlStyle.SrlStyleAnnotation = SrlStyle.Reg
  ): T = {
    require(latency >= 0, "Pipe latency must be >= 0!")

    val prefix = s"PipeWithStyle_${style.name}"

    // I use a foldLeft here as I want to name individual wires. If I use a Vec,
    // then calling `suggestName` on individual elements of the Vec does not
    // assign the name I want to the individual registers.
    Range.inclusive(1, latency).foldLeft(WireInit(data)) { case (prevWire, idx) =>
      val nextRegName = s"${prefix}_${idx}"
      val nextReg     = Reg(chiselTypeOf(data))
      nextReg.suggestName(nextRegName)
      nextReg := prevWire

      // Only the last register in an SRL chain needs to be labeled.
      if (idx == latency) {
        annotate(new ChiselAnnotation {
          def toFirrtl: Annotation = AttributeAnnotation(nextReg.toNamed, s"srl_style = \"${style.name}\"")
        })
      }

      nextReg
    }
  }

  // This just wraps the InlinePipeWithStyle above in a module so we can introduce a hierarchy.
  // It is useful for identifying cells in vivado by name.
  object WrappedPipeWithStyle {
    class WrappedPipeWithStyle[T <: Data](
        data: T,
        latency: Int,
        style: SrlStyle.SrlStyleAnnotation
    ) extends Module {
      val io = IO(new Bundle {
        val in  = Input(chiselTypeOf(data))
        val out = Output(chiselTypeOf(data))
      })

      io.out := InlinePipeWithStyle(io.in, latency, style)
    }

    def apply[T <: Data](
        data: T,
        latency: Int = 1,
        style: SrlStyle.SrlStyleAnnotation = SrlStyle.Reg
    ): T = {
      val pipe = Module(new WrappedPipeWithStyle(data, latency, style))
      pipe.io.in := data
      pipe.io.out
    }
  }

  def RegDontTouch[T <: Data](
      data: T,
      style: SrlStyle.SrlStyleAnnotation = SrlStyle.Reg
  ): T = {
    val regDontTouch = RegNext(data)

    annotate(new ChiselAnnotation {
      def toFirrtl: Annotation = AttributeAnnotation(regDontTouch.toNamed, s"DONT_TOUCH = \"yes\"")
    })
    annotate(new ChiselAnnotation {
      def toFirrtl: Annotation = AttributeAnnotation(regDontTouch.toNamed, s"srl_style = \"${style.name}\"")
    })

    regDontTouch
  }

}
