import argparse
import enum
import re
from collections import defaultdict
from pathlib import Path

from pblocks_extract_helpers import *


class Side(enum.Enum):
  LEFT = enum.auto()
  RIGHT = enum.auto()

def get_pblocks(slr_crY_crX_tileCol_sites: slr_composition_t) -> pblocks_t:
  # We generate a Left and Right pblock for every clock region column.
  #
  # SLR0 / SLR2:
  # - Left pblock  = 6 BRAMs + 2 URAMs
  # - Right pblock = 6 BRAMs + 2 URAMs
  #
  # SLR1:
  # - Left pblock  = 3 BRAMs + 1 URAM
  # - Right pblock = 4 BRAMs + 1 URAM
  def isNarrowSlr(crY: int) -> bool: return 5 <= crY <= 9 # SLR1 is at X*Y[5-9]
  def wideSlrLeftNumBrams() -> int: return 6
  def wideSlrLeftNumUrams() -> int: return 2
  def wideSlrRightmostClockRegionX() -> int: return 5
  def narrowSlrLeftNumBrams() -> int: return 3
  def narrowSlrLeftNumUrams() -> int: return 1
  def narrowSlrRightmostClockRegionX() -> int: return 2 # SLR1 shell is at X[3-5]Y[5-9]

  def isBramSite(siteName: str) -> bool: return re.match(r"RAMB(18|36)_X\d+Y\d+", siteName) is not None
  def isUramSite(siteName: str) -> bool: return re.match(r"URAM288_X\d+Y\d+", siteName) is not None
  def isSliceSite(siteName: str) -> bool: return re.match(r"SLICE_X\d+Y\d+", siteName) is not None


  def extract_row_sites(
    is_narrow_row: bool,
    crXDict: dict[
      int, # crX
      dict[
        int, # tile column
        list[str] # sites
      ]
    ]
  ) -> dict[
    Side,
    list[str] # sites
  ]:
    targetBramColCount = narrowSlrLeftNumBrams() if is_narrow_row else wideSlrLeftNumBrams()
    targetUramColCount = narrowSlrLeftNumUrams() if is_narrow_row else wideSlrLeftNumUrams()
    targetCrx = narrowSlrRightmostClockRegionX() if is_narrow_row else wideSlrRightmostClockRegionX()
    numBramColsFound = 0
    numURamColsFound = 0
    current_side = Side.LEFT

    side_sites_dict = defaultdict( # side
      list[str] # sites
    )

    for (crX, tileColDict) in crXDict.items():
      if crX > targetCrx:
        # Have passed the last permissible X-valued clock region.
        break

      # We use this indexing scheme as we want to use a while loop below instead of a for loop.
      # We use a while loop because we want to increase the tile column index after seeing a BRAM to ensure the 2
      # SLICE columns that come after it are put in the same pblock as the BRAM.
      tileCols = list(tileColDict.keys())
      tileColsIdx = 0

      clockRegionEnd = False
      while not clockRegionEnd:
        tileCol = tileCols[tileColsIdx]
        siteNames = tileColDict[tileCol]
        tileColsIdx += 1
        side_sites_dict[current_side].extend(siteNames)

        isBramCol = any((isBramSite(siteName) for siteName in siteNames))
        isUramCol = any((isUramSite(siteName) for siteName in siteNames))

        if isBramCol or isUramCol:
          if isBramCol:
            numBramColsFound += 1
          else:
            numURamColsFound += 1

          # If a BRAM/URAM column is found, then we also want to add the 2 SLICE columns that come after it (there may
          # be clock columns after it, so we use a while loop and stop when we get our SLICE columns).
          numSliceColsFound = 0

          while numSliceColsFound < 2:
            tileCol = tileCols[tileColsIdx]
            siteNames = tileColDict[tileCol]
            tileColsIdx += 1
            side_sites_dict[current_side].extend(siteNames)

            isSliceCol = any((isSliceSite(siteName) for siteName in siteNames))
            if isSliceCol:
              numSliceColsFound += 1

        if (numBramColsFound >= targetBramColCount) and (numURamColsFound >= targetUramColCount):
          current_side = Side.RIGHT

        # No more tiles left in the clock region.
        if tileColsIdx >= len(tileCols):
          clockRegionEnd = True

    return side_sites_dict

  pblocks = defaultdict( # pblock name
    list[str] # sites
  )

  # We begin by determining which tile column indices should go in the left/right pblock.
  for (slrName, crYDict) in slr_crY_crX_tileCol_sites.items():
    for (crY, crXDict) in crYDict.items():
      side_sites_dict = extract_row_sites(isNarrowSlr(crY), crXDict)
      for (side, sites) in side_sites_dict.items():
        pblockName = f"pblock_Y{crY:02d}_{side.name}"
        pblocks[pblockName].extend(sites)

  return pblocks

def emit_pblocks(pblocks: pblocks_t) -> str:
  lines = list[str]()

  for (pblockName, sites) in pblocks.items():
    sites_str = " ".join(sites)
    lines.append(f"{pblockName} -> {{ {sites_str} }}")

  return "\n".join(lines)

# Main program (if executed as script)
if __name__ == "__main__":
  parser = argparse.ArgumentParser(description="Defines a grid of Pblocks for the xcu200 device.")
  parser.add_argument("device_info", type=str, help="Input device info JSON file.")
  args = parser.parse_args()

  composition = read_json(Path(args.device_info))["composition"]
  slr_crY_crX_tileCol_sites = get_slr_composition(composition)
  pblocks = get_pblocks(slr_crY_crX_tileCol_sites)
  pblocksCompacted = compact_resource_ranges(pblocks)
  pblocksStr = emit_pblocks(pblocksCompacted)

  print(pblocksStr)
