package manticore.machine.xrt

import collection.mutable.{Map => MMap}

object U250Floorplan {
  import Coordinates._

  case class ArbitraryPblock(name: String, resources: String) extends Pblock

  // format: off
  val slr2Slr3Pblock = ArbitraryPblock("pblock_slr2_slr3", "{ SLR2:SLR3 }")
  // format: on

  // Cores are placed as follows:
  // - Place all cores in a single large pblock that spans SLR2 and SLR3
  //
  // Switches are placed as follows:
  // - Place all switches in a single large pblock that spans SLR2 and SLR3.
  object LooseSlr2Slr3 extends Floorplan {
    // The anchor doesn't matter as we are leaving the cores in a single large pblock
    // that spans 2 SLRs.
    val anchor = GridLoc(0, 0)

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