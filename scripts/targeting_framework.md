# EBC Targeting Algorithm Analysis: Framework & Considerations

## Overview

This document outlines the analytical framework for comparing targeting algorithms and identifies key decision points for credibility in both policy and research contexts.

**Baseline Period**: October 1, 2020 – September 30, 2025 (5 years)
**Data Cutoff**: September 30, 2025 (latest available data)

---

## The Four Algorithms Under Comparison

| Algorithm | Primary Ranking | Tiebreakers | Theoretical Justification |
|-----------|----------------|-------------|---------------------------|
| **1. Gun Violence Total** | shootings + shots_fired | shootings → shots_fired | Near-misses are signal; maximizes gun violence coverage |
| **2. Shootings Only** | shootings | shots_fired → violent_crime | Focuses on highest-severity outcome |
| **3. All Violent Crime** | violent_crime total | shootings | Broader intervention logic; more statistical power |
| **4. Violent Street Crime** | outdoor violent only | shootings | Most theoretically aligned with place-based intervention |

---

## Key Analytical Outputs

### 1. Cutoff Thresholds
- **What it tells you**: Minimum crime count needed to make the eligibility list at each N
- **Why it matters**: Defensibility of inclusion criteria; can you pre-specify "blocks with ≥X events"?
- **Watch for**: If cutoffs are very low (e.g., 1-2 events), you're including noisy blocks

### 2. Concentration Curves
- **What it tells you**: How concentrated is crime? What % of citywide crime do top N blocks capture?
- **Why it matters**: Justifies place-based approach; determines theoretical maximum effect size
- **Watch for**: If concentration is weak, intervention may not move citywide numbers much

### 3. Citywide Capture Rates
- **What it tells you**: What share of each crime type does your selected sample account for?
- **Why it matters**: 
  - For policy: Can you claim impact on citywide shootings?
  - For research: Do you have enough baseline events for power?
- **Key comparison**: Does violent crime algorithm still capture substantial gun violence?

### 4. Precinct Distribution
- **What it matters**:
  - Randomization strategy (within-precinct vs. citywide)
  - Implementation feasibility (are blocks clustered or scattered?)
  - External validity (are you studying one neighborhood or citywide phenomenon?)

### 5. Algorithm Overlap
- **What it tells you**: How much do different algorithms agree?
- **Why it matters**:
  - High overlap = robust candidates regardless of specification
  - Low overlap = algorithm choice substantively matters
- **Blocks in all 4 algorithms**: Strongest candidates for core sample

---

## Additional Filters to Consider

### Policy-Relevant Filters

| Filter | Rationale | Implementation |
|--------|-----------|----------------|
| **NYCHA proximity** | Housing authority has different governance; may need separate permission | Buffer around NYCHA developments |
| **Commercial vs. residential** | Intervention may differ by land use | Join to PLUTO land use data |
| **School proximity** | Different intervention protocols near schools | Buffer around school locations |
| **Transit proximity** | Transit crimes may need different treatment | Buffer around subway entrances |
| **Park proximity** | Parks jurisdiction issues | Overlay with parks data |
| **BID coverage** | Business Improvement Districts may have parallel interventions | BID boundary overlay |

### Research-Relevant Filters

| Filter | Rationale | Implementation |
|--------|-----------|----------------|
| **Minimum baseline events** | Power requirements; regression-to-mean concerns | Require ≥X events over baseline period |
| **Persistence requirement** | Chronic vs. spiking blocks | Require presence in top N% for ≥3 of 5 years |
| **Geographic minimum spacing** | Avoid spillover contamination between treatment/control | Enforce minimum distance between selected blocks |
| **Precinct representation** | Ensure citywide generalizability | Stratify or cap per-precinct |
| **Exclude prior intervention sites** | Avoid contamination from existing programs | Overlay with Cure Violence, SOS, etc. catchment areas |

### Data Quality Filters

| Filter | Rationale | Implementation |
|--------|-----------|----------------|
| **Geocoding confidence** | Poorly geocoded crimes add noise | Use distance-to-block threshold |
| **Exclude indoor-only blocks** | Low tractability for place-based intervention | Require ≥X% outdoor crime |
| **Block length minimum** | Very short blocks are edge cases | Require ≥100 ft length |

---

## Critical Questions for Algorithm Selection

### 1. What is the intervention theory?
- If intervention targets **gun violence specifically** (e.g., group violence intervention, focused deterrence) → weight toward gun violence algorithms
- If intervention targets **environmental conditions** (e.g., cleaning, lighting, greening) → violent street crime makes most sense
- If intervention is **community capacity building** → broader violent crime may be appropriate

### 2. What are your power requirements?
- Gun violence is rare → fewer baseline events per block → harder to detect effects
- Violent crime is common → more baseline events → more power, but may dilute gun violence effect

### 3. What is the counterfactual?
- If NYPD is already targeting high-shooting blocks with police intervention, your "treatment" is *non-police intervention ON TOP OF police intervention*
- If NYPD is NOT systematically targeting these blocks, your treatment is cleaner

### 4. What is the implementation capacity?
- 400 blocks requires ~400 sites for intervention delivery
- Are interventions place-specific (each block gets treatment) or area-based (treatment covers multiple blocks)?

---

## Recommended Analysis Sequence

1. **Run base comparison** with current 4 algorithms (this script)
2. **Examine overlap** - blocks in all/most algorithms are robust candidates
3. **Map top blocks** - visual inspection for clustering, implementation feasibility
4. **Test sensitivity** to time window (3-year vs. 5-year baseline)
5. **Apply policy filters** iteratively - see how N changes
6. **Apply research filters** - minimum baseline, persistence
7. **Final algorithm specification** for pre-registration

---

## Pre-Registration Considerations

For the stepped-wedge design, you'll want to pre-specify:

1. **Eligibility criteria**: Exact algorithm with all filters
2. **Exclusion criteria**: Precincts, hospitals, NYCHA, etc.
3. **Randomization strata**: Precinct? Borough? Crime level?
4. **Primary outcome**: Shootings? Violent crime? Gun violence composite?
5. **Secondary outcomes**: Other crime types, spatial displacement tests
6. **Time windows**: Baseline period, treatment rollout schedule, follow-up

---

## Next Steps

After running this analysis:

1. [ ] Review concentration curves - is place-based approach justified?
2. [ ] Review overlap - how sensitive is selection to algorithm choice?
3. [ ] Review precinct distribution - randomization feasibility?
4. [ ] Decide on primary outcome (determines lead algorithm)
5. [ ] Apply policy filters to narrow eligible pool
6. [ ] Apply research filters for final sample
7. [ ] Generate maps for visual inspection
8. [ ] Document decision rules for pre-registration
