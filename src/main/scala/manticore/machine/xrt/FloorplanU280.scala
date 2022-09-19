package manticore.machine.xrt

import collection.mutable.{Map => MMap}

trait U280Floorplan extends Floorplan {
  def getManticoreKernelInstName(): String = "level0_i/ulp/ManticoreKernel_1/inst"
}

object U280FloorplanImpl {
  import Coordinates._

  case class ArbitraryPblock(name: String, resources: String) extends Pblock

  // format: off
  val slr2Slr3Pblock = ArbitraryPblock("pblock_slr0_slr1_slr2", "{ SLR0:SLR2 }")
  // format: on

  // Cores are placed as follows:
  // - Place all cores in a single large pblock that spans SLR0:SLR2.
  //
  // Switches are placed as follows:
  // - Place all switches in a single large pblock that spans SLR0:SLR2.
  object LooseNonShell extends U250Floorplan {
    // The anchor doesn't matter as we are leaving the cores in a single large pblock
    // that spans all SLRs.
    val anchor = GridLoc(0, 0)

    def getRootClock(): ClockRegion = ClockRegion(4, 6)

    def getCoreToPblockMap(dimX: Int, dimY: Int): Map[TorusLoc, ArbitraryPblock] = {
      val gridToTorus = getGridLocToTorusLocMap(dimX, dimY, anchor)

      gridToTorus.map { case (gridLoc, torusLoc) =>
        torusLoc -> slr2Slr3Pblock
      }
    }

    def getSwitchToPblockMap(dimX: Int, dimY: Int): Map[TorusLoc, ArbitraryPblock] = {
      getCoreToPblockMap(dimX, dimY)
    }
  }
}
