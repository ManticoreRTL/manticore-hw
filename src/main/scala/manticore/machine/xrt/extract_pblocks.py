import argparse
import json
import typing
import re
import enum

from pathlib import Path
from collections import defaultdict
from pprint import pprint

class Side(enum.Enum):
  LEFT = enum.auto()
  RIGHT = enum.auto()

def read_json(
  path: Path
) -> dict[str, typing.Any]:
  if path is not None:
    with open(path, "r") as f:
      return json.loads(f.read())

  return dict()

def isNarrowSlr(crY: int) -> bool: return 5 <= crY <= 9 # SLR1

# def extract_bels_in_row(slr_properties, cr_row: int, max_cr_col: int):
#   res = dict()
#   clock_regions = [f"X{x}Y{cr_row}" for x in range(0, max_cr_col + 1)]
#   for clock_region in clock_regions:
#     for (tile_col_str, tile_col_properties) in slr_properties["clock_regions"][clock_region]["tile_cols"].items():
#       print(tile_col_str)
#       exit(1)

def get_sites_per_slr_row(composition) -> dict[str, dict[int, dict[int, list[str]]]]:
  def xy_bounds(clock_region_names: list[str]):
    pattern = r"X(?P<x>\d+)Y(?P<y>\d+)"

    xs = list[int]()
    ys = list[int]()

    for cr_name in clock_region_names:
      match = re.match(pattern, cr_name)
      assert match is not None
      x = int(match.group("x"))
      y = int(match.group("y"))
      xs.append(x)
      ys.append(y)

    return (min(xs), max(xs), min(ys), max(ys))

  slr_crY_tileCol_sites = defaultdict( # slr
    lambda: defaultdict( # crY
      lambda: defaultdict( # tileCol
        list # sites
      )
    )
  )

  num_slrs = composition["num_slrs"]
  for slrIdx in range(0, num_slrs):
    slrName = f"SLR{slrIdx}"
    slrProperties = composition["slrs"][slrName]
    (minX, maxX, minY, maxY) = xy_bounds(list(slrProperties["clock_regions"].keys()))

    for crY in range(minY, maxY + 1):
      if isNarrowSlr(crY):
        rowCrNames = [f"X{crX}Y{crY}" for crX in range(minX, maxX//2 + 1)]
      else:
        rowCrNames = [f"X{crX}Y{crY}" for crX in range(minX, maxX + 1)]

      for rowCrName in rowCrNames:
        crProperties = slrProperties["clock_regions"][rowCrName]
        tileColStrIndices = list(crProperties["tile_cols"].keys())

        for tileColStrIdx in tileColStrIndices:
          tileColProperties = crProperties["tile_cols"][tileColStrIdx]

          for (tileName, siteNames) in tileColProperties.items():
            # Only select resources of interest.
            sitePattern = r"(?P<resource>SLICE|DSP48E2|RAMB18|RAMB36|URAM288)_X(?P<x>\d+)Y(?P<y>\d+)"

            for siteName in siteNames:
              if re.match(sitePattern, siteName):
                slr_crY_tileCol_sites[slrName][crY][int(tileColStrIdx)].append(siteName)

  return slr_crY_tileCol_sites

def get_pblocks(slr_crY_tileCol_sites: dict[str, dict[int, dict[int, list[str]]]]) -> dict[int, dict[Side, list[str]]]:
  # We generate a Left and Right pblock for every clock region column.
  #
  # SLR0 / SLR2:
  # - Left pblock  = 6 BRAMs + 2 URAMs
  # - Right pblock = 6 BRAMs + 2 URAMs
  #
  # SLR1:
  # - Left pblock  = 4 BRAMs + 1 URAM
  # - Right pblock = 3 BRAMs + 1 URAM
  def wideSlrLeftNumBrams() -> int: return 6
  def wideSlrLeftNumUrams() -> int: return 2
  def narrowSlrLeftNumBrams() -> int: return 4
  def narrowSlrLeftNumUrams() -> int: return 1

  def isBramSite(siteName: str) -> bool: return re.match(r"RAMB(18|36)_X\d+Y\d+", siteName) is not None
  def isUramSite(siteName: str) -> bool: return re.match(r"URAM288_X\d+Y\d+", siteName) is not None

  pblocks: dict[int, dict[Side, list[str]]] = defaultdict( # crY
    lambda: defaultdict( # side
      list # sites
    )
  )

  # We begin by determining which tile column indices should go in the left/right pblock.
  for (slrName, crYDict) in slr_crY_tileCol_sites.items():
    for (crY, tileColDict) in crYDict.items():
      targetBramColCount = narrowSlrLeftNumBrams() if isNarrowSlr(crY) else wideSlrLeftNumBrams()
      targetUramColCount = narrowSlrLeftNumUrams() if isNarrowSlr(crY) else wideSlrLeftNumUrams()
      tileColIndices = list(tileColDict.keys())

      numBramColsFound = 0
      numURamColsFound = 0
      side = Side.LEFT

      # We use this indexing scheme as we want to use a while loop below instead of a for loop.
      # We use a while loop because we want to increase the tile column index after seeing a BRAM to ensure the 2 SLICE
      # columns that come after it are put in the same pblock as the BRAM.
      tileColIndicesIdx = 0
      stop = False
      while not stop:
        tileColIdx = tileColIndices[tileColIndicesIdx]
        siteNames = tileColDict[tileColIdx]

        isBramCol = any((isBramSite(siteName) for siteName in siteNames))
        isUramCol = any((isUramSite(siteName) for siteName in siteNames))

        pblocks[crY][side].extend(siteNames)
        tileColIndicesIdx += 1

        if isBramCol:
          numBramColsFound += 1
          # If a BRAM column is found, then we also want to add the 2 tile columns that come after it (which are always
          # SLICE columns).
          for _ in range(2):
            tileColIdx = tileColIndices[tileColIndicesIdx]
            siteNames = tileColDict[tileColIdx]
            pblocks[crY][side].extend(siteNames)
            tileColIndicesIdx += 1

        elif isUramCol:
          numURamColsFound += 1

        if (numBramColsFound >= targetBramColCount) and (numURamColsFound >= targetUramColCount):
          side = Side.RIGHT

        if (tileColIndicesIdx >= len(tileColIndices)):
          stop = True

  return pblocks

def compact_resource_ranges(pblocks: dict[int, dict[Side, list[str]]]) -> dict[int, dict[Side, list[str]]]:
  def xy_bounds(sites: list[str]):
    pattern = r"\w+_X(?P<x>\d+)Y(?P<y>\d+)"

    xs = list[int]()
    ys = list[int]()

    for site in sites:
      match = re.match(pattern, site)
      assert match is not None
      x = int(match.group("x"))
      y = int(match.group("y"))
      xs.append(x)
      ys.append(y)

    return (min(xs), max(xs), min(ys), max(ys))

  pblocksCompacted: dict[int, dict[Side, list[str]]] = defaultdict( # crY
    lambda: defaultdict( # side
      list[str] # ranges
    )
  )

  for (crY, sideDict) in pblocks.items():
    for (side, sites) in sideDict.items():
      # All the sites are explicitly named above. For example, we see something
      # like:
      #
      #   SLICE_X0Y0, SLICE_X0Y1, ... SLICE_X0Y59
      #
      # However, Vivado expects to see a "range" instead. The above would be
      # written as a range this way:
      #
      #   SLICE_X0Y0:SLICE_X0Y59
      #
      # Ranges can span multiple X and Y values like SLICE_X0Y0:SLICE_X10Y59 as well.

      resourceType_sites: dict[str, list[str]] = defaultdict(list)

      pattern = r"(?P<resource>\w+)_X\d+Y\d+"
      for site in sites:
        match = re.match(pattern, site)
        assert match is not None
        resourceType = match.group("resource")
        resourceType_sites[resourceType].append(site)

      for (resourceType, sites) in resourceType_sites.items():
        # Get X/Y bounds of each resource so we can construct a range.
        (minX, maxX, minY, maxY) = xy_bounds(sites)
        pblocksCompacted[crY][side].append(f"{resourceType}_X{minX}Y{minY}:{resourceType}_X{maxX}Y{maxY}")

  return pblocksCompacted

def emit_pblocks(pblocks: dict[int, dict[Side, list[str]]]) -> dict[int, dict[Side, str]]:
  res: dict[int, dict[Side, str]] = defaultdict( # crY
    lambda: defaultdict( # side
      str
    )
  )

  for (crY, sideDict) in pblocks.items():
    for (side, names) in sideDict.items():
      pblockName = f"pblock_y{crY}_{side.name}"
      pblockContents = " ".join(names)
      pblockDefinition = (
        f"startgroup\n"
        f"create_pblock {pblockName}\n"
        f"resize_pblock {pblockName} -add {{ {pblockContents} }}\n"
        f"endgroup"
      )
      res[crY][side] = pblockDefinition

  return res

# Main program (if executed as script)
if __name__ == "__main__":
  parser = argparse.ArgumentParser(description="Defines a grid of Pblocks for the xcu200 device.")
  parser.add_argument("device_info", type=str, help="Input device info JSON file.")
  args = parser.parse_args()

  composition = read_json(Path(args.device_info))["composition"]
  slr_crY_tileCol_sites = get_sites_per_slr_row(composition)
  pblocks = get_pblocks(slr_crY_tileCol_sites)
  pblocksCompacted = compact_resource_ranges(pblocks)
  pblocksTcl = emit_pblocks(pblocksCompacted)

  for (crY, sideDict) in pblocksTcl.items():
    for (side, pblockTclStr) in sideDict.items():
      print(pblockTclStr)
      print()