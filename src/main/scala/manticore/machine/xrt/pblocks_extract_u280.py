import argparse
import re
from collections import defaultdict
from pathlib import Path

from pblocks_extract_helpers import *


def get_pblocks(slr_crY_crX_tileCol_sites: slr_composition_t) -> pblocks_t:
  # DSP_X28Y* is the last column before the shell starts. I will extract all resources until (excluding) this column
  # here as a single large pblock.
  def isLastNonShellDspSite(siteName: str) -> bool: return re.match(r"DSP48E2_X28Y\d+", siteName) is not None

  pblocks = defaultdict( # pblock name
    list[str] # sites
  )

  pblockName = "pblock_non_shell"

  for (slrName, crYDict) in slr_crY_crX_tileCol_sites.items():
    for (crY, crXDict) in crYDict.items():
      # Take all sites in the row until (excluding) the last DSP column that is not part of the shell.
      last_non_shell_col_seen = False
      for (crX, tileColDict) in crXDict.items():
        for (tileCol, siteNames) in tileColDict.items():
          if any((isLastNonShellDspSite(siteName) for siteName in siteNames)):
            last_non_shell_col_seen = True

          if not last_non_shell_col_seen:
            pblocks[pblockName].extend(siteNames)

  return pblocks

def emit_pblocks(pblocks: pblocks_t) -> str:
  lines = list[str]()

  for (pblockName, sites) in pblocks.items():
    sites_str = " ".join(sites)
    lines.append(f"{pblockName} -> {{ {sites_str} }}")

  return "\n".join(lines)

# Main program (if executed as script)
if __name__ == "__main__":
  parser = argparse.ArgumentParser(description="Defines a grid of Pblocks for the xcu280 device.")
  parser.add_argument("device_info", type=str, help="Input device info JSON file.")
  args = parser.parse_args()

  composition = read_json(Path(args.device_info))["composition"]
  slr_crY_crX_tileCol_sites = get_slr_composition(composition)
  pblocks = get_pblocks(slr_crY_crX_tileCol_sites)
  pblocksCompacted = compact_resource_ranges(pblocks)
  pblocksStr = emit_pblocks(pblocksCompacted)

  print(pblocksStr)
