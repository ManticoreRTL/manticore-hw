package manticore.machine.xrt

import collection.mutable.{Map => MMap}

trait U280Floorplan extends Floorplan {
  def getManticoreKernelInstName(): String = "level0_i/ulp/ManticoreKernel_1/inst"
}

object U280FloorplanImpl {
  import Coordinates._

  case class ArbitraryPblock(name: String, resources: String) extends Pblock

  // format: off
  val nonShellPblock = ArbitraryPblock("pblock_non_shell", "{ BUFCE_LEAF_X0Y0:BUFCE_LEAF_X1007Y47 BUFCE_ROW_X0Y0:BUFCE_ROW_X0Y287 BUFCE_ROW_FSR_X0Y0:BUFCE_ROW_FSR_X228Y11 BUFGCE_X0Y0:BUFGCE_X0Y287 BUFGCE_DIV_X0Y0:BUFGCE_DIV_X0Y47 BUFGCTRL_X0Y0:BUFGCTRL_X0Y95 BUFG_GT_X0Y0:BUFG_GT_X0Y287 BUFG_GT_SYNC_X0Y0:BUFG_GT_SYNC_X0Y179 DSP48E2_X0Y0:DSP48E2_X27Y281 MMCM_X0Y0:MMCM_X0Y11 PLL_X0Y0:PLL_X0Y23 PLL_SELECT_SITE_X0Y0:PLL_SELECT_SITE_X0Y95 RAMB18_X0Y0:RAMB18_X11Y287 RAMB36_X0Y0:RAMB36_X11Y143 SLICE_X0Y0:SLICE_X196Y719 URAM288_X0Y0:URAM288_X4Y191 }")
  // format: on

  // Cores are placed as follows:
  // - Place all cores in a single large pblock that spans everything except the shell.
  //
  // Switches are placed as follows:
  // - Place all switches in a single large pblock that spans everything except the shell.
  object LooseNonShell extends U280Floorplan {
    // The anchor doesn't matter as we are leaving the cores in a single large pblock
    // that spans all SLRs.
    val anchor = GridLoc(0, 0)

    def getRootClock(): ClockRegion = ClockRegion(4, 6)

    def getCoreToPblockMap(dimX: Int, dimY: Int): Map[TorusLoc, ArbitraryPblock] = {
      val gridToTorus = getGridLocToTorusLocMap(dimX, dimY, anchor)

      gridToTorus.map { case (gridLoc, torusLoc) =>
        torusLoc -> nonShellPblock
      }
    }

    def getSwitchToPblockMap(dimX: Int, dimY: Int): Map[TorusLoc, ArbitraryPblock] = {
      getCoreToPblockMap(dimX, dimY)
    }
  }
}
