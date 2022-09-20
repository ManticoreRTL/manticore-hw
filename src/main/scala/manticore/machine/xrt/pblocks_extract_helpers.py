import json
import re
import typing
from collections import defaultdict
from pathlib import Path


def read_json(
  path: Path
) -> dict[str, typing.Any]:
  if path is not None:
    with open(path, "r") as f:
      return json.loads(f.read())

  return dict()

# dict[
#   str, // SLR name
#   dict[
#     int, // clock region row (Y-value of clock region name)
#     dict[
#       int, // clock region col (X-value of clock region name)
#       dict[
#         int, // tile column index
#         list[str] // sites
#       ]
#     ]
#   ]
# ]
slr_composition_t = dict[str, dict[int, dict[int, dict[int, list[str]]]]]

# dict[
#   str, // pblock name
#   list[str] // sites
# ]
pblocks_t = dict[str, list[str]]

def get_slr_composition(device_info) -> slr_composition_t:
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

  slr_crY_crX_tileCol_sites = defaultdict( # slr
    lambda: defaultdict( # crY
      lambda: defaultdict( # crX
        lambda: defaultdict( # tileCol
          list[str] # sites
        )
      )
    )
  )

  num_slrs = device_info["num_slrs"]
  for slrIdx in range(0, num_slrs):
    slrName = f"SLR{slrIdx}"
    slrProperties = device_info["slrs"][slrName]
    (minX, maxX, minY, maxY) = xy_bounds(list(slrProperties["clock_regions"].keys()))

    for crY in range(minY, maxY + 1):
      for crX in range(minX, maxX + 1):
        crName = f"X{crX}Y{crY}"
        crProperties = slrProperties["clock_regions"][crName]
        tileColStrIndices = list(crProperties["tile_cols"].keys())

        for tileColStrIdx in tileColStrIndices:
          tileColProperties = crProperties["tile_cols"][tileColStrIdx]

          for (tileName, siteNames) in tileColProperties.items():
            # Only select resources of interest.
            sitePattern = r"(?P<resource>SLICE|DSP48E2|RAMB18|RAMB36|URAM288|BUF\w*|MMCM\w*|PLL\w*)_X(?P<x>\d+)Y(?P<y>\d+)"

            for siteName in siteNames:
              if re.match(sitePattern, siteName):
                slr_crY_crX_tileCol_sites[slrName][crY][crX][int(tileColStrIdx)].append(siteName)

  return slr_crY_crX_tileCol_sites


# Takes and returns:
#
# dict[
#   str, // pblock name
#   list[str] // sites
# ]
def compact_resource_ranges(pblocks: pblocks_t) -> pblocks_t:
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

  pblocksCompacted: dict[str, list[str]] = defaultdict( # pblock name
    list[str] # ranges
  )

  for (pblockName, sites) in pblocks.items():
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

    for (resourceType, sites) in sorted(resourceType_sites.items()):
      # Get X/Y bounds of each resource so we can construct a range.
      (minX, maxX, minY, maxY) = xy_bounds(sites)
      pblocksCompacted[pblockName].append(f"{resourceType}_X{minX}Y{minY}:{resourceType}_X{maxX}Y{maxY}")

  return pblocksCompacted
