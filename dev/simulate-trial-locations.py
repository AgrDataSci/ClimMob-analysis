"""Spread the simulated maize fields across the maize areas of Kenya.

The simulated project (tests/testdata/simulated-maize/data.json) first had
all its fields in one small square near Bungoma, which looked odd on the
trial map. This script draws one location per participant from a handful of
centres in the main maize producing counties, with District 1 in the west
and District 2 in the Rift Valley and the central and eastern highlands.
The centres are on farmland, away from the lakes and the national parks.

Run from the repository root

    python3 dev/simulate-trial-locations.py

It rewrites ASS*_geotrial in place and keeps the rows without a location.
"""

import json
import random
from pathlib import Path

PATH = Path("tests/testdata/simulated-maize/data.json")
SEED = 20261110

# name, latitude, longitude, spread in degrees, weight
WEST = [
    ("Trans Nzoia", 1.02, 35.00, 0.14, 3),
    ("Uasin Gishu", 0.55, 35.25, 0.16, 3),
    ("Bungoma", 0.60, 34.60, 0.12, 2),
    ("Kakamega", 0.30, 34.78, 0.12, 2),
    ("Nandi", 0.15, 35.10, 0.10, 1),
    ("Kisii", -0.70, 34.80, 0.10, 2),
]

EAST = [
    ("Nakuru", -0.25, 35.95, 0.15, 3),
    ("Bomet", -0.78, 35.35, 0.10, 1),
    ("Narok", -1.05, 35.80, 0.14, 2),
    ("Nyeri", -0.55, 36.95, 0.10, 1),
    ("Murang'a", -0.80, 37.10, 0.10, 1),
    ("Embu", -0.55, 37.50, 0.10, 2),
    ("Meru", 0.05, 37.65, 0.12, 2),
    ("Machakos", -1.50, 37.25, 0.14, 2),
]


def draw(rng, centres):
    names = [c[0] for c in centres]
    weights = [c[4] for c in centres]
    name = rng.choices(names, weights)[0]
    _, lat, lon, sd, _ = centres[names.index(name)]
    return rng.gauss(lat, sd), rng.gauss(lon, sd)


def main():
    rng = random.Random(SEED)
    d = json.loads(PATH.read_text())
    rows = d["data"]
    geo_cols = sorted(k for k in rows[0] if k.endswith("_geotrial"))
    n = 0
    for r in rows:
        centres = WEST if r.get("REG_district") == "District 1" else EAST
        for col in geo_cols:
            if not r.get(col):
                continue
            parts = r[col].split()
            lat, lon = draw(rng, centres)
            parts[0] = f"{lat:.4f}"
            parts[1] = f"{lon:.4f}"
            r[col] = " ".join(parts)
            n += 1
    PATH.write_text(json.dumps(d, ensure_ascii=False, separators=(",", ": ")))
    print(f"rewrote {n} locations in {PATH}")


if __name__ == "__main__":
    main()
