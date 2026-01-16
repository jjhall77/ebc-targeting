# Every Block Counts (EBC) Targeting Analysis

**A systematic framework for identifying and comparing candidate blocks for a place-based violence intervention RCT in New York City.**

## Overview

This repository contains the analytical pipeline for the **Every Block Counts** project, a stepped-wedge cluster randomized controlled trial of non-police interventions at violent micro-places in NYC (planned 2026-2030).

The targeting analysis addresses a fundamental question: **How do we define "high crime blocks" for intervention eligibility?** Different definitions (gun violence, all violent crime, outdoor crime only) yield different candidate pools with distinct characteristics.

## Key Features

- **Four targeting algorithms** compared systematically:
  1. Gun Violence Total (shootings + shots fired)
  2. Shootings Only (with tiebreakers)
  3. All Violent Crime
  4. Violent Street Crime (outdoor only)

- **Intersection-aware crime allocation** using pre-built LION crosswalk tables
- **Comprehensive diagnostics**: concentration curves, capture rates, overlap analysis
- **Pre-registration ready**: defensible, pre-specifiable eligibility criteria

## Repository Structure

```
ebc-targeting/
├── README.md
├── scripts/
│   ├── 00_load_data.R          # Data loading and preparation
│   └── 01_targeting_analysis.R  # Main analysis pipeline
├── data/
│   ├── physical_blocks.gpkg     # Block geometries (from LION)
│   ├── intersection_nodes.gpkg  # Intersection points
│   ├── intersection_to_blocks.rds # Node → block crosswalk
│   ├── nypp_25d/                # Precinct polygons
│   ├── nynta2020_25d/           # NTA polygons
│   ├── NYPD_Complaint_Data_*.csv
│   ├── NYPD_Shooting_Incident_Data_*.csv
│   ├── sf_since_2017.csv        # Shots fired (older)
│   ├── shots_fired_new.csv      # Shots fired (newer)
│   ├── nypd_precinct_locations.csv
│   └── Health_Facility_General_Information_*.csv
├── output/
│   ├── block_matrix.rds         # Block-level crime counts
│   ├── concentration_curves.png
│   ├── overlap_heatmap_*.png
│   └── targeting_report.txt
└── docs/
    └── targeting_framework.md   # Analytical considerations
```

## Quick Start

### Prerequisites

```r
install.packages(c("here", "tidyverse", "janitor", "sf", "lubridate", "ggplot2", "patchwork"))
```

### Usage

```r
# 1. Load all data objects
source("scripts/00_load_data.R")

# 2. Run targeting analysis
source("scripts/01_targeting_analysis.R")
output <- run_targeting_analysis()

# 3. Access results
output$cutoffs              # Threshold values at each N
output$capture_rates        # What % of crime each algorithm captures
output$overlap              # How much algorithms agree
output$block_matrix         # Full block-level data
```

## Data Sources

| Dataset | Source | Update Frequency |
|---------|--------|------------------|
| NYPD Complaint Data | [NYC Open Data](https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i) | Quarterly |
| NYPD Shooting Incidents | [NYC Open Data](https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Historic-/833y-fsy8) | Quarterly |
| Shots Fired | NYPD (via MOCJ) | Ad hoc |
| LION Street Network | [NYC Open Data](https://data.cityofnewyork.us/City-Government/LION/2v4z-66xt) | Annual |
| Health Facilities | [NY Health Data](https://health.data.ny.gov/) | Annual |

## Methodology

### Crime-to-Block Allocation

Crimes are assigned to physical blocks using a two-step process:

1. **Intersection detection**: If a crime is within 50 feet of an intersection node, it is split equally across all adjacent blocks (using pre-built crosswalk tables from LION processing).

2. **Block assignment**: Crimes not at intersections are assigned to the nearest physical block.

This approach handles the common geocoding pattern where crimes at intersections (e.g., "125th St & Lenox Ave") would otherwise be arbitrarily assigned to a single block.

### Block Exclusions

Blocks containing the following facilities are excluded from the eligibility pool:
- NYPD precinct station houses (N = 77)
- Hospitals (N = 68)

### Outdoor Crime Classification

Crimes are classified as "outdoor" based on `loc_of_occur_desc` or `prem_typ_desc` fields containing keywords like: STREET, SIDEWALK, PARK, FRONT OF, OUTSIDE, etc.

## Key Outputs

### Cutoff Thresholds

For each target sample size (N = 200, 300, 400), the analysis reports:
- Minimum, median, and maximum crime counts for blocks in the top N
- Allows pre-specification of eligibility criteria (e.g., "blocks with ≥X gun violence events")

### Capture Rates

Cross-tabulated matrix showing what percentage of each crime type each algorithm captures. Example interpretation:
- "Gun Violence algorithm at N=300 captures X% of citywide shootings"
- Helps assess whether violent crime algorithm still captures substantial gun violence

### Algorithm Overlap

Pairwise and multi-way overlap statistics:
- Blocks appearing in all 4 algorithms = most robust candidates
- Low overlap = algorithm choice substantively matters

### Geographic Distribution

- Blocks per precinct (for within-precinct randomization feasibility)
- Blocks per borough (for citywide generalizability)

## Parameters

Key parameters can be adjusted in the analysis script:

```r
# Target sample sizes
TARGET_N <- c(200, 300, 400)

# Baseline period (5 years)
START_DATE <- as.Date("2020-10-01")
END_DATE   <- as.Date("2025-09-30")

# Intersection proximity threshold (feet)
INTERSECTION_THRESHOLD <- 50
```

## Considerations for Pre-Registration

The targeting framework document (`docs/targeting_framework.md`) outlines additional filters to consider:

**Policy-relevant**:
- NYCHA proximity
- Commercial vs. residential land use
- School/transit proximity
- BID coverage

**Research-relevant**:
- Minimum baseline events (power)
- Persistence requirement (regression-to-mean)
- Geographic minimum spacing (spillover)
- Prior intervention site exclusions

## Citation

If you use this code, please cite:

```
[Author]. (2026). Every Block Counts Targeting Analysis. 
NYC Mayor's Office of Criminal Justice / University of Cambridge.
```

## License

[Specify license]

## Contact

[Contact information]

---

*This analysis is part of the Every Block Counts project, a collaboration between the NYC Mayor's Office of Criminal Justice and the University of Cambridge Institute of Criminology.*
