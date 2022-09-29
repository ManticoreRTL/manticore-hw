package manticore.machine

import chisel3._

import chisel3.experimental.annotate
import chisel3.experimental.ChiselAnnotation
import firrtl.annotations.Annotation
import firrtl.AttributeAnnotation

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

  def PipeWithStyle[T <: Data](
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

      // annotate(new ChiselAnnotation {
      //   def toFirrtl: Annotation = AttributeAnnotation(nextReg.toNamed, "DONT_TOUCH = \"yes\"")
      // })

      // Only the last register in an SRL chain needs to be labeled.
      if (idx == latency) {
        annotate(new ChiselAnnotation {
          def toFirrtl: Annotation = AttributeAnnotation(nextReg.toNamed, s"srl_style = \"${style.name}\"")
        })
      }

      nextReg
    }
  }

  // // (skashani): The name "manticoreSlrCrossing" is matched code that
  // // generates USER_SLL_REG constraints.
  // // If you modify the name here, do not forget to modify it in the other
  // // parts of the code!
  // def slrCrossingSuffix = "manticoreSlrCrossing"

  // // This function creates a pipeline register with the given latency and a
  // // specific suffix for certain of the registers in the chain.
  // // This code exists as we want a unique name for SLR-crossing wires so that
  // // we can emit USER_SLL_REG properties when we create pblock constraints.
  // def SlrCrossing[T <: Data](
  //     data: T,
  //     latency: Int,
  //     slrCrossingIndices: Set[Int]
  // ): T = {
  //   require(latency >= 2, "SLR crossing needs >= 2 registers!")

  //   val suffixMap = slrCrossingIndices.map(idx => idx -> slrCrossingSuffix).toMap

  //   PipeNoSRL(data, latency, suffixMap)
  // }
}
