package manticore.machine.xrt

import collection.mutable.{Map => MMap}

trait U280Floorplan extends Floorplan {
  def getManticoreKernelInstName(): String = "level0_i/level1/level1_i/ulp/ManticoreKernel_1/inst"
}

object U280FloorplanImpl {
  import Coordinates._

  case class ArbitraryPblock(name: String, resources: String) extends Pblock

  // format: off
  val nonShellPblock = ArbitraryPblock("pblock_slr2_slr3", "{ SLR2:SLR3 }")
  // format: on

  // Cores are placed as follows:
  // - Place all cores in a single large pblock that spans SLR2 and SLR3
  //
  // Switches are placed as follows:
  // - Place all switches in a single large pblock that spans SLR2 and SLR3.
  object LooseNonShell extends U280Floorplan {
    // The anchor doesn't matter as we are leaving the cores in a single large pblock
    // that spans 2 SLRs.
    val anchor = GridLoc(0, 0)

    def getRootClock(): ClockRegion = ClockRegion(4, 11)

    def getCoreToPblockMap(dimX: Int, dimY: Int): Map[TorusLoc, ArbitraryPblock] = {
      val gridToTorus = getGridLocToTorusLocMap(dimX, dimY, anchor)

      gridToTorus.map { case (gridLoc, torusLoc) =>
        torusLoc -> nonShellPblock
      }
    }

    def getSwitchToPblockMap(dimX: Int, dimY: Int): Map[TorusLoc, ArbitraryPblock] = {
      getCoreToPblockMap(dimX, dimY)
    }

    override def toTcl(dimX: Int, dimY: Int): String = ""
  }
}
