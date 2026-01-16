#==============================================================================
# 03_algorithm_comparison.R
# Every Block Counts (EBC) Project
# Purpose: Compare targeting algorithms for block selection
#
# USAGE: Run section by section in RStudio
# PREREQUISITE: source("scripts/00_load_data.R") first
#==============================================================================

library(tidyverse)
library(sf)
library(ggplot2)
library(scales)
library(patchwork)
library(here)

#==============================================================================
# PARAMETERS
#==============================================================================

TARGET_N <- c(200, 300, 400)

# Baseline period (5 years)
START_DATE <- as.Date("2020-10-01")
END_DATE   <- as.Date("2025-09-30")

# Recent year (for recency metrics)
RECENT_START <- as.Date("2024-10-01")
RECENT_END   <- as.Date("2025-09-30")

# Intersection threshold (feet)
INTERSECTION_THRESHOLD <- 50

# CHI Weights
CHI_WEIGHTS <- list(
  murder = 30,
  shooting = 30,
  robbery = 19,
  felony_assault = 10,
  shots_fired = 12,
  misd_assault = 1
)

# Gun violence boost multiplier for Felony+GV algorithms
GV_BOOST <- 3

# Persistence thresholds (top N blocks per year)
PERSISTENCE_NS <- c(100, 300, 500)

#==============================================================================
# SECTION 1: VERIFY DATA LOADED
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("EBC TARGETING ALGORITHM COMPARISON\n")
cat(strrep("=", 70), "\n\n")

required_objects <- c(
  "physical_blocks", "intersection_nodes", "intersection_to_blocks",
  "nypp", "shootings_sf", "shots_fired", "hospitals_sf", 
  "nypd_precinct_locations_sf", "complaints"
)

missing <- setdiff(required_objects, ls())
if (length(missing) > 0) {
  stop("Missing: ", paste(missing, collapse = ", "), 
       "\nRun source('scripts/00_load_data.R') first!")
}
cat("All required objects present.\n\n")

#==============================================================================
# SECTION 2: CREATE CRIME TYPE SUBSETS
#==============================================================================

cat("SECTION 2: Creating crime type subsets...\n")
cat(strrep("-", 50), "\n")

# Outdoor classification patterns
outside_loc_pattern <- "FRONT OF|OPPOSITE OF|OUTSIDE|REAR OF|STREET|IN STREET|SIDEWALK"
outside_prem_pattern <- "PARK|STREET|PUBLIC PLACE|HIGHWAY|BRIDGE|SIDEWALK|VACANT LOT|PUBLIC HOUSING AREA|OUTSIDE"

# Base complaints: filter dates, add outdoor flag, convert to sf
compl_base <- complaints %>%
  filter(!is.na(x_coord_cd), !is.na(y_coord_cd)) %>%
  mutate(
    date = mdy(rpt_dt),
    loc_of_occur_desc = str_to_upper(coalesce(loc_of_occur_desc, "")),
    prem_typ_desc = str_to_upper(coalesce(prem_typ_desc, "")),
    is_outdoor = str_detect(loc_of_occur_desc, outside_loc_pattern) |
                 str_detect(prem_typ_desc, outside_prem_pattern)
  ) %>%
  filter(date >= START_DATE & date <= END_DATE) %>%
  st_as_sf(coords = c("x_coord_cd", "y_coord_cd"), crs = 2263, remove = FALSE)

# Create crime type sf objects
murder_sf <- compl_base %>% filter(ky_cd == 101)
murder_outdoor_sf <- murder_sf %>% filter(is_outdoor)

robbery_sf <- compl_base %>% filter(ky_cd == 105)
robbery_outdoor_sf <- robbery_sf %>% filter(is_outdoor)

felony_assault_sf <- compl_base %>% filter(ky_cd == 106)
felony_assault_outdoor_sf <- felony_assault_sf %>% filter(is_outdoor)

misd_assault_sf <- compl_base %>% filter(ky_cd == 344)
misd_assault_outdoor_sf <- misd_assault_sf %>% filter(is_outdoor)

shootings_baseline <- shootings_sf %>% filter(date >= START_DATE & date <= END_DATE)
shots_fired_baseline <- shots_fired %>% filter(date >= START_DATE & date <= END_DATE)

# Recent year subsets
murder_recent <- murder_sf %>% filter(date >= RECENT_START)
robbery_recent <- robbery_sf %>% filter(date >= RECENT_START)
felony_assault_recent <- felony_assault_sf %>% filter(date >= RECENT_START)
misd_assault_recent <- misd_assault_sf %>% filter(date >= RECENT_START)
shootings_recent <- shootings_baseline %>% filter(date >= RECENT_START)
shots_fired_recent <- shots_fired_baseline %>% filter(date >= RECENT_START)

# Print summary
cat("\n5-YEAR BASELINE COUNTS:\n")
cat("  Murder:         ", format(nrow(murder_sf), big.mark = ","), 
    " (outdoor: ", nrow(murder_outdoor_sf), ")\n", sep = "")
cat("  Robbery:        ", format(nrow(robbery_sf), big.mark = ","), 
    " (outdoor: ", nrow(robbery_outdoor_sf), ")\n", sep = "")
cat("  Felony Assault: ", format(nrow(felony_assault_sf), big.mark = ","), 
    " (outdoor: ", nrow(felony_assault_outdoor_sf), ")\n", sep = "")
cat("  Misd Assault:   ", format(nrow(misd_assault_sf), big.mark = ","), 
    " (outdoor: ", nrow(misd_assault_outdoor_sf), ")\n", sep = "")
cat("  Shootings:      ", format(nrow(shootings_baseline), big.mark = ","), "\n", sep = "")
cat("  Shots Fired:    ", format(nrow(shots_fired_baseline), big.mark = ","), "\n", sep = "")

cat("\nRECENT YEAR (", as.character(RECENT_START), " to ", as.character(RECENT_END), "):\n", sep = "")
cat("  Shootings:      ", nrow(shootings_recent), "\n", sep = "")
cat("  Shots Fired:    ", nrow(shots_fired_recent), "\n", sep = "")
cat("  Felony Violent: ", nrow(murder_recent) + nrow(robbery_recent) + nrow(felony_assault_recent), "\n", sep = "")

#==============================================================================
# SECTION 3: IDENTIFY EXCLUSION BLOCKS
#==============================================================================

cat("\nSECTION 3: Identifying blocks to exclude...\n")
cat(strrep("-", 50), "\n")

hospital_block_idx <- st_nearest_feature(hospitals_sf, physical_blocks)
hospital_block_ids <- physical_blocks$physical_id[hospital_block_idx]

precinct_block_idx <- st_nearest_feature(nypd_precinct_locations_sf, physical_blocks)
precinct_block_ids <- physical_blocks$physical_id[precinct_block_idx]

exclude_block_ids <- unique(c(hospital_block_ids, precinct_block_ids))

cat("  Hospital blocks:  ", length(unique(hospital_block_ids)), "\n")
cat("  Precinct blocks:  ", length(unique(precinct_block_ids)), "\n")
cat("  Total excluded:   ", length(exclude_block_ids), "\n")

#==============================================================================
# SECTION 4: CRIME-TO-BLOCK ALLOCATION FUNCTION
#==============================================================================

allocate_crimes_to_blocks <- function(crime_sf, 
                                      physical_blocks,
                                      intersection_nodes,
                                      intersection_to_blocks,
                                      threshold = INTERSECTION_THRESHOLD) {
  
  n_crimes <- nrow(crime_sf)
  if (n_crimes == 0) {
    return(tibble(block_id = integer(), crime_count = numeric()))
  }
  
  # Find nearest intersection and block for each crime

nearest_node_idx <- st_nearest_feature(crime_sf, intersection_nodes)
  nearest_node_id <- intersection_nodes$nodeid[nearest_node_idx]
  dist_to_intersection <- as.numeric(
    st_distance(crime_sf, intersection_nodes[nearest_node_idx, ], by_element = TRUE)
  )
  
  nearest_block_idx <- st_nearest_feature(crime_sf, physical_blocks)
  nearest_block_id <- physical_blocks$physical_id[nearest_block_idx]
  
  # Allocation table
  crime_allocation <- tibble(
    crime_idx = 1:n_crimes,
    nearest_node_id = nearest_node_id,
    dist_to_intersection = dist_to_intersection,
    nearest_block_id = nearest_block_id,
    is_at_intersection = dist_to_intersection <= threshold
  )
  
  # Split intersection crimes across adjacent blocks
  intersection_crimes <- crime_allocation %>% filter(is_at_intersection)
  
  if (nrow(intersection_crimes) > 0) {
    intersection_split <- intersection_crimes %>%
      left_join(
        intersection_to_blocks %>% select(node_id, adjacent_blocks),
        by = c("nearest_node_id" = "node_id")
      ) %>%
      unnest(adjacent_blocks) %>%
      rename(block_id = adjacent_blocks) %>%
      group_by(crime_idx) %>%
      mutate(weight = 1 / n()) %>%
      ungroup() %>%
      group_by(block_id) %>%
      summarise(crime_count = sum(weight), .groups = "drop")
  } else {
    intersection_split <- tibble(block_id = integer(), crime_count = numeric())
  }
  
  # Non-intersection crimes go to nearest block
  non_intersection <- crime_allocation %>%
    filter(!is_at_intersection) %>%
    count(nearest_block_id, name = "crime_count") %>%
    rename(block_id = nearest_block_id)
  
  # Combine
  bind_rows(intersection_split, non_intersection) %>%
    group_by(block_id) %>%
    summarise(crime_count = sum(crime_count), .groups = "drop")
}

#==============================================================================
# SECTION 5: BUILD BLOCK MATRIX
#==============================================================================

cat("\nSECTION 5: Building block crime matrix...\n")
cat(strrep("-", 50), "\n")

# Initialize block matrix
block_matrix <- physical_blocks %>%
  st_drop_geometry() %>%
  select(physical_id, total_length_ft, streets, boro) %>%
  rename(block_id = physical_id) %>%
  mutate(excluded = block_id %in% exclude_block_ids)

# Allocate each crime type
cat("  Allocating crimes to blocks:\n")

cat("    Murder...")
murder_counts <- allocate_crimes_to_blocks(
  murder_sf, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(murder = crime_count)
murder_outdoor_counts <- allocate_crimes_to_blocks(
  murder_outdoor_sf, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(murder_outdoor = crime_count)
cat("done\n")

cat("    Robbery...")
robbery_counts <- allocate_crimes_to_blocks(
  robbery_sf, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(robbery = crime_count)
robbery_outdoor_counts <- allocate_crimes_to_blocks(
  robbery_outdoor_sf, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(robbery_outdoor = crime_count)
cat("done\n")

cat("    Felony Assault...")
fel_assault_counts <- allocate_crimes_to_blocks(
  felony_assault_sf, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(felony_assault = crime_count)
fel_assault_outdoor_counts <- allocate_crimes_to_blocks(
  felony_assault_outdoor_sf, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(felony_assault_outdoor = crime_count)
cat("done\n")

cat("    Misd Assault...")
misd_assault_counts <- allocate_crimes_to_blocks(
  misd_assault_sf, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(misd_assault = crime_count)
misd_assault_outdoor_counts <- allocate_crimes_to_blocks(
  misd_assault_outdoor_sf, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(misd_assault_outdoor = crime_count)
cat("done\n")

cat("    Shootings...")
shooting_counts <- allocate_crimes_to_blocks(
  shootings_baseline, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(shootings = crime_count)
cat("done\n")

cat("    Shots Fired...")
shots_fired_counts <- allocate_crimes_to_blocks(
  shots_fired_baseline, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(shots_fired = crime_count)
cat("done\n")

cat("    Recent year...")
murder_recent_counts <- allocate_crimes_to_blocks(
  murder_recent, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(murder_recent = crime_count)
robbery_recent_counts <- allocate_crimes_to_blocks(
  robbery_recent, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(robbery_recent = crime_count)
fel_assault_recent_counts <- allocate_crimes_to_blocks(
  felony_assault_recent, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(felony_assault_recent = crime_count)
misd_assault_recent_counts <- allocate_crimes_to_blocks(
  misd_assault_recent, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(misd_assault_recent = crime_count)
shooting_recent_counts <- allocate_crimes_to_blocks(
  shootings_recent, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(shootings_recent = crime_count)
shots_fired_recent_counts <- allocate_crimes_to_blocks(
  shots_fired_recent, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(shots_fired_recent = crime_count)
cat("done\n")

# Join all counts
block_matrix <- block_matrix %>%
  left_join(murder_counts, by = "block_id") %>%
  left_join(murder_outdoor_counts, by = "block_id") %>%
  left_join(robbery_counts, by = "block_id") %>%
  left_join(robbery_outdoor_counts, by = "block_id") %>%
  left_join(fel_assault_counts, by = "block_id") %>%
  left_join(fel_assault_outdoor_counts, by = "block_id") %>%
  left_join(misd_assault_counts, by = "block_id") %>%
  left_join(misd_assault_outdoor_counts, by = "block_id") %>%
  left_join(shooting_counts, by = "block_id") %>%
  left_join(shots_fired_counts, by = "block_id") %>%
  left_join(murder_recent_counts, by = "block_id") %>%
  left_join(robbery_recent_counts, by = "block_id") %>%
  left_join(fel_assault_recent_counts, by = "block_id") %>%
  left_join(misd_assault_recent_counts, by = "block_id") %>%
  left_join(shooting_recent_counts, by = "block_id") %>%
  left_join(shots_fired_recent_counts, by = "block_id") %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

# Add precinct
cat("  Adding precinct...")
block_centroids <- physical_blocks %>% st_centroid() %>% select(physical_id)
block_precincts <- st_join(block_centroids, nypp, join = st_within) %>%
  st_drop_geometry() %>%
  select(physical_id, precinct)
block_matrix <- block_matrix %>%
  left_join(block_precincts, by = c("block_id" = "physical_id"))
cat("done\n")

# Add borough name
block_matrix <- block_matrix %>%
  mutate(
    borough = case_when(
      boro == 1 ~ "Manhattan",
      boro == 2 ~ "Bronx",
      boro == 3 ~ "Brooklyn",
      boro == 4 ~ "Queens",
      boro == 5 ~ "Staten Island",
      TRUE ~ NA_character_
    )
  )

cat("\n  Block matrix complete:\n")
cat("    Total blocks:    ", format(nrow(block_matrix), big.mark = ","), "\n")
cat("    Excluded:        ", sum(block_matrix$excluded), "\n")
cat("    Eligible:        ", sum(!block_matrix$excluded), "\n")

#==============================================================================
# SECTION 6: CREATE COMPOSITE SCORES
#==============================================================================

cat("\nSECTION 6: Creating composite scores...\n")
cat(strrep("-", 50), "\n")

block_matrix <- block_matrix %>%
  mutate(
    # Base composites
    gun_violence = shootings + shots_fired,
    gun_violence_recent = shootings_recent + shots_fired_recent,
    
    felony_violent = murder + robbery + felony_assault,
    felony_violent_outdoor = murder_outdoor + robbery_outdoor + felony_assault_outdoor,
    felony_violent_recent = murder_recent + robbery_recent + felony_assault_recent,
    
    # CHI Scores (with misd assault)
    chi_full = murder * CHI_WEIGHTS$murder + 
               shootings * CHI_WEIGHTS$shooting + 
               robbery * CHI_WEIGHTS$robbery + 
               felony_assault * CHI_WEIGHTS$felony_assault + 
               shots_fired * CHI_WEIGHTS$shots_fired + 
               misd_assault * CHI_WEIGHTS$misd_assault,
    
    # CHI Scores (without misd assault)
    chi_no_misd = murder * CHI_WEIGHTS$murder + 
                  shootings * CHI_WEIGHTS$shooting + 
                  robbery * CHI_WEIGHTS$robbery + 
                  felony_assault * CHI_WEIGHTS$felony_assault + 
                  shots_fired * CHI_WEIGHTS$shots_fired,
    
    # Outdoor CHI (with misd)
    chi_outdoor_full = murder_outdoor * CHI_WEIGHTS$murder + 
                       shootings * CHI_WEIGHTS$shooting +
                       robbery_outdoor * CHI_WEIGHTS$robbery + 
                       felony_assault_outdoor * CHI_WEIGHTS$felony_assault + 
                       shots_fired * CHI_WEIGHTS$shots_fired +
                       misd_assault_outdoor * CHI_WEIGHTS$misd_assault,
    
    # Outdoor CHI (without misd)
    chi_outdoor_no_misd = murder_outdoor * CHI_WEIGHTS$murder + 
                          shootings * CHI_WEIGHTS$shooting + 
                          robbery_outdoor * CHI_WEIGHTS$robbery + 
                          felony_assault_outdoor * CHI_WEIGHTS$felony_assault + 
                          shots_fired * CHI_WEIGHTS$shots_fired,
    
    # Felony + Gun Violence Boost
    felony_gv_boost = felony_violent + (gun_violence * GV_BOOST),
    felony_outdoor_gv_boost = felony_violent_outdoor + (gun_violence * GV_BOOST)
  )

# Percentile ranks for blended algorithms
block_matrix <- block_matrix %>%
  mutate(
    pct_rank_gv = percent_rank(gun_violence) * 100,
    pct_rank_felony = percent_rank(felony_violent) * 100,
    pct_rank_felony_outdoor = percent_rank(felony_violent_outdoor) * 100,
    pct_blend_felony_gv = (pct_rank_felony + pct_rank_gv) / 2,
    pct_blend_outdoor_gv = (pct_rank_felony_outdoor + pct_rank_gv) / 2
  )

cat("  Composite scores created.\n")

#==============================================================================
# SECTION 7: DEFINE ALGORITHMS
#==============================================================================

cat("\nSECTION 7: Defining algorithms...\n")
cat(strrep("-", 50), "\n")

# All algorithms incorporate violent crime
algorithms <- list(
  
  # CHI Variants
  list(
    name = "CHI_Full",
    description = "Full CHI (Murder×30, Shooting×30, Robbery×19, FA×10, SF×12, MA×1)",
    rank_col = "chi_full",
    tiebreaker1 = "shootings",
    tiebreaker2 = "gun_violence"
  ),
  list(
    name = "CHI_No_Misd",
    description = "CHI without Misd Assault",
    rank_col = "chi_no_misd",
    tiebreaker1 = "shootings",
    tiebreaker2 = "gun_violence"
  ),
  list(
    name = "CHI_Outdoor_Full",
    description = "Outdoor CHI with Misd Assault",
    rank_col = "chi_outdoor_full",
    tiebreaker1 = "shootings",
    tiebreaker2 = "gun_violence"
  ),
  list(
    name = "CHI_Outdoor_No_Misd",
    description = "Outdoor CHI without Misd Assault",
    rank_col = "chi_outdoor_no_misd",
    tiebreaker1 = "shootings",
    tiebreaker2 = "gun_violence"
  ),
  
  # Felony + GV Boost
  list(
    name = "Felony_GV_Boost",
    description = "Felony Violent + (Gun Violence × 3)",
    rank_col = "felony_gv_boost",
    tiebreaker1 = "shootings",
    tiebreaker2 = "gun_violence"
  ),
  list(
    name = "Outdoor_Felony_GV_Boost",
    description = "Outdoor Felony + (Gun Violence × 3)",
    rank_col = "felony_outdoor_gv_boost",
    tiebreaker1 = "shootings",
    tiebreaker2 = "gun_violence"
  ),
  
  # Simple benchmark (no gun violence weighting)
  list(
    name = "Felony_Violent_Only",
    description = "Felony Violent only (no GV weighting)",
    rank_col = "felony_violent",
    tiebreaker1 = "shootings",
    tiebreaker2 = "gun_violence"
  ),
  
  # Percentile blends
  list(
    name = "Pct_Felony_GV",
    description = "Percentile blend: Felony + GV",
    rank_col = "pct_blend_felony_gv",
    tiebreaker1 = "shootings",
    tiebreaker2 = "gun_violence"
  ),
  list(
    name = "Pct_Outdoor_GV",
    description = "Percentile blend: Outdoor Felony + GV",
    rank_col = "pct_blend_outdoor_gv",
    tiebreaker1 = "shootings",
    tiebreaker2 = "gun_violence"
  )
)

cat("  ", length(algorithms), " algorithms defined:\n", sep = "")
for (alg in algorithms) {
  cat("    - ", alg$name, ": ", alg$description, "\n", sep = "")
}

#==============================================================================
# SECTION 8: RANKING FUNCTION
#==============================================================================

rank_blocks <- function(block_matrix, algorithm) {
  block_matrix %>%
    filter(!excluded) %>%
    arrange(
      desc(!!sym(algorithm$rank_col)),
      desc(!!sym(algorithm$tiebreaker1)),
      desc(!!sym(algorithm$tiebreaker2))
    ) %>%
    mutate(rank = row_number())
}

#==============================================================================
# SECTION 9: CALCULATE METRICS FOR EACH ALGORITHM
#==============================================================================

cat("\nSECTION 9: Calculating algorithm metrics...\n")
cat(strrep("-", 50), "\n")

eligible <- block_matrix %>% filter(!excluded)

# Citywide totals
citywide <- list(
  gun_violence = sum(eligible$gun_violence),
  shootings = sum(eligible$shootings),
  felony_violent = sum(eligible$felony_violent),
  misd_assault = sum(eligible$misd_assault)
)

calculate_metrics <- function(block_matrix, alg, target_ns = TARGET_N) {
  
  ranked <- rank_blocks(block_matrix, alg)
  alg_name <- alg$name
  alg_rank_col <- alg$rank_col
  
  map_dfr(target_ns, function(n) {
    top_n <- ranked %>% filter(rank <= n)
    
    gv_5yr <- sum(top_n$gun_violence)
    fv_5yr <- sum(top_n$felony_violent)
    misd_5yr <- sum(top_n$misd_assault)
    gv_recent <- sum(top_n$gun_violence_recent)
    fv_recent <- sum(top_n$felony_violent_recent)
    
    tibble(
      algorithm = alg_name,
      N = n,
      
      # 5-Year Totals
      gun_violence_5yr = gv_5yr,
      shootings_5yr = sum(top_n$shootings),
      shots_fired_5yr = sum(top_n$shots_fired),
      felony_violent_5yr = fv_5yr,
      misd_assault_5yr = misd_5yr,
      
      # Outdoor percentages
      pct_felony_outdoor = round(sum(top_n$felony_violent_outdoor) / max(fv_5yr, 1) * 100, 1),
      
      # Recent year
      gun_violence_1yr = gv_recent,
      felony_violent_1yr = fv_recent,
      
      # Recency ratio
      recency_ratio_gv = round(gv_recent / max(gv_5yr / 5, 0.1), 2),
      recency_ratio_fv = round(fv_recent / max(fv_5yr / 5, 0.1), 2),
      
      # Capture rates
      capture_gv = round(gv_5yr / citywide$gun_violence * 100, 1),
      capture_shootings = round(sum(top_n$shootings) / citywide$shootings * 100, 1),
      capture_felony = round(fv_5yr / citywide$felony_violent * 100, 1),
      
      # Geography
      n_precincts = n_distinct(top_n$precinct, na.rm = TRUE),
      max_per_precinct = max(table(top_n$precinct)),
      
      # Cutoffs
      score_min = min(top_n[[alg_rank_col]]),
      score_median = median(top_n[[alg_rank_col]]),
      score_max = max(top_n[[alg_rank_col]])
    )
  })
}

all_metrics <- map_dfr(algorithms, function(alg) {
  cat("  ", alg$name, "...\n", sep = "")
  calculate_metrics(block_matrix, alg)
})

cat("  Metrics calculated for all algorithms.\n")

#==============================================================================
# SECTION 10: BOROUGH DISTRIBUTION
#==============================================================================

cat("\nSECTION 10: Calculating borough distributions...\n")
cat(strrep("-", 50), "\n")

borough_distributions <- map_dfr(algorithms, function(alg) {
  ranked <- rank_blocks(block_matrix, alg)
  map_dfr(TARGET_N, function(n) {
    ranked %>%
      filter(rank <= n) %>%
      count(borough, name = "n_blocks") %>%
      mutate(algorithm = alg$name, N = n)
  })
})

cat("  Borough distributions calculated.\n")

#==============================================================================
# SECTION 11: OVERLAP ANALYSIS
#==============================================================================

cat("\nSECTION 11: Calculating algorithm overlaps...\n")
cat(strrep("-", 50), "\n")

calculate_overlap <- function(block_matrix, algorithms, n) {
  
  top_n_ids <- map(algorithms, function(alg) {
    ranked <- rank_blocks(block_matrix, alg)
    ranked$block_id[1:n]
  })
  names(top_n_ids) <- map_chr(algorithms, ~.x$name)
  
  # Pairwise overlap matrix
  alg_names <- names(top_n_ids)
  overlap_matrix <- matrix(0, nrow = length(alg_names), ncol = length(alg_names),
                           dimnames = list(alg_names, alg_names))
  
  for (i in seq_along(alg_names)) {
    for (j in seq_along(alg_names)) {
      overlap_matrix[i, j] <- length(intersect(top_n_ids[[i]], top_n_ids[[j]]))
    }
  }
  
  # How many algorithms each block appears in
  all_ids <- unlist(top_n_ids)
  id_counts <- table(all_ids)
  
  list(
    overlap_matrix = overlap_matrix,
    in_all = sum(id_counts == length(algorithms)),
    in_most = sum(id_counts >= length(algorithms) - 2),
    in_half = sum(id_counts >= ceiling(length(algorithms) / 2)),
    union_size = length(unique(all_ids))
  )
}

overlap_200 <- calculate_overlap(block_matrix, algorithms, 200)
overlap_300 <- calculate_overlap(block_matrix, algorithms, 300)
overlap_400 <- calculate_overlap(block_matrix, algorithms, 400)

cat("  Overlaps calculated.\n")

#==============================================================================
# SECTION 12: PERSISTENCE ANALYSIS (YEAR-BY-YEAR)
#==============================================================================

cat("\nSECTION 12: Calculating persistence (year-by-year analysis)...\n")
cat(strrep("-", 50), "\n")
cat("  This takes a few minutes - calculating yearly crime for all blocks.\n\n")

# Get all eligible block IDs
eligible_ids <- block_matrix %>% filter(!excluded) %>% pull(block_id)

# Calculate year-by-year metrics for all eligible blocks
calculate_yearly_metrics <- function() {
  
  years <- 2021:2025
  
  yearly_data <- map_dfr(years, function(yr) {
    cat("    Processing FY", yr, "...")
    yr_start <- as.Date(paste0(yr - 1, "-10-01"))
    yr_end <- as.Date(paste0(yr, "-09-30"))
    
    # Filter each crime type
    murder_yr <- murder_sf %>% filter(date >= yr_start & date <= yr_end)
    robbery_yr <- robbery_sf %>% filter(date >= yr_start & date <= yr_end)
    fel_assault_yr <- felony_assault_sf %>% filter(date >= yr_start & date <= yr_end)
    misd_assault_yr <- misd_assault_sf %>% filter(date >= yr_start & date <= yr_end)
    shootings_yr <- shootings_baseline %>% filter(date >= yr_start & date <= yr_end)
    shots_fired_yr <- shots_fired_baseline %>% filter(date >= yr_start & date <= yr_end)
    
    # Allocate to blocks
    murder_ct <- allocate_crimes_to_blocks(murder_yr, physical_blocks, intersection_nodes, intersection_to_blocks) %>% rename(murder = crime_count)
    robbery_ct <- allocate_crimes_to_blocks(robbery_yr, physical_blocks, intersection_nodes, intersection_to_blocks) %>% rename(robbery = crime_count)
    fel_ct <- allocate_crimes_to_blocks(fel_assault_yr, physical_blocks, intersection_nodes, intersection_to_blocks) %>% rename(felony_assault = crime_count)
    misd_ct <- allocate_crimes_to_blocks(misd_assault_yr, physical_blocks, intersection_nodes, intersection_to_blocks) %>% rename(misd_assault = crime_count)
    shoot_ct <- allocate_crimes_to_blocks(shootings_yr, physical_blocks, intersection_nodes, intersection_to_blocks) %>% rename(shootings = crime_count)
    sf_ct <- allocate_crimes_to_blocks(shots_fired_yr, physical_blocks, intersection_nodes, intersection_to_blocks) %>% rename(shots_fired = crime_count)
    
    # Start with all eligible blocks
    result <- tibble(block_id = eligible_ids) %>%
      left_join(murder_ct, by = "block_id") %>%
      left_join(robbery_ct, by = "block_id") %>%
      left_join(fel_ct, by = "block_id") %>%
      left_join(misd_ct, by = "block_id") %>%
      left_join(shoot_ct, by = "block_id") %>%
      left_join(sf_ct, by = "block_id") %>%
      replace_na(list(murder = 0, robbery = 0, felony_assault = 0, 
                      misd_assault = 0, shootings = 0, shots_fired = 0)) %>%
      mutate(
        gun_violence = shootings + shots_fired,
        felony_violent = murder + robbery + felony_assault,
        chi_full = murder * CHI_WEIGHTS$murder + shootings * CHI_WEIGHTS$shooting + 
                   robbery * CHI_WEIGHTS$robbery + felony_assault * CHI_WEIGHTS$felony_assault + 
                   shots_fired * CHI_WEIGHTS$shots_fired + misd_assault * CHI_WEIGHTS$misd_assault,
        chi_no_misd = murder * CHI_WEIGHTS$murder + shootings * CHI_WEIGHTS$shooting + 
                      robbery * CHI_WEIGHTS$robbery + felony_assault * CHI_WEIGHTS$felony_assault + 
                      shots_fired * CHI_WEIGHTS$shots_fired,
        year = yr
      )
    
    cat("done\n")
    result
  })
  
  yearly_data
}

yearly_data <- calculate_yearly_metrics()

# Calculate persistence: years in top N for each metric
calculate_persistence <- function(yearly_data, metrics, top_ns = PERSISTENCE_NS) {
  
  results <- tibble(block_id = unique(yearly_data$block_id))
  
  for (metric in metrics) {
    cat("    Ranking by ", metric, "...\n", sep = "")
    
    yearly_ranked <- yearly_data %>%
      group_by(year) %>%
      mutate(rank = rank(-!!sym(metric), ties.method = "min")) %>%
      ungroup()
    
    for (n in top_ns) {
      col_name <- paste0("years_top", n, "_", metric)
      
      counts <- yearly_ranked %>%
        group_by(block_id) %>%
        summarise(!!col_name := sum(rank <= n), .groups = "drop")
      
      results <- results %>% left_join(counts, by = "block_id")
    }
  }
  
  results
}

cat("\n  Calculating persistence ranks...\n")
persistence_metrics <- c("gun_violence", "chi_full", "chi_no_misd", "felony_violent")
persistence_data <- calculate_persistence(yearly_data, persistence_metrics)

# Add to block matrix
block_matrix <- block_matrix %>%
  select(-any_of(names(persistence_data)[-1])) %>%
  left_join(persistence_data, by = "block_id") %>%
  mutate(across(starts_with("years_top"), ~replace_na(., 0)))

cat("\n  Persistence analysis complete.\n")

#==============================================================================
# SECTION 13: PERSISTENCE SUMMARY
#==============================================================================

cat("\nSECTION 13: Persistence summary...\n")
cat(strrep("-", 50), "\n")

eligible <- block_matrix %>% filter(!excluded)

cat("\nGUN VIOLENCE - Years in Top N:\n")
cat("  Top 100: 5/5 yrs = ", sum(eligible$years_top100_gun_violence == 5),
    ", 4/5 = ", sum(eligible$years_top100_gun_violence >= 4),
    ", 3/5 = ", sum(eligible$years_top100_gun_violence >= 3), "\n", sep = "")
cat("  Top 300: 5/5 yrs = ", sum(eligible$years_top300_gun_violence == 5),
    ", 4/5 = ", sum(eligible$years_top300_gun_violence >= 4),
    ", 3/5 = ", sum(eligible$years_top300_gun_violence >= 3), "\n", sep = "")
cat("  Top 500: 5/5 yrs = ", sum(eligible$years_top500_gun_violence == 5),
    ", 4/5 = ", sum(eligible$years_top500_gun_violence >= 4),
    ", 3/5 = ", sum(eligible$years_top500_gun_violence >= 3), "\n", sep = "")

cat("\nCHI (no misd) - Years in Top N:\n")
cat("  Top 100: 5/5 yrs = ", sum(eligible$years_top100_chi_no_misd == 5),
    ", 4/5 = ", sum(eligible$years_top100_chi_no_misd >= 4),
    ", 3/5 = ", sum(eligible$years_top100_chi_no_misd >= 3), "\n", sep = "")
cat("  Top 300: 5/5 yrs = ", sum(eligible$years_top300_chi_no_misd == 5),
    ", 4/5 = ", sum(eligible$years_top300_chi_no_misd >= 4),
    ", 3/5 = ", sum(eligible$years_top300_chi_no_misd >= 3), "\n", sep = "")
cat("  Top 500: 5/5 yrs = ", sum(eligible$years_top500_chi_no_misd == 5),
    ", 4/5 = ", sum(eligible$years_top500_chi_no_misd >= 4),
    ", 3/5 = ", sum(eligible$years_top500_chi_no_misd >= 3), "\n", sep = "")

cat("\nCHI (full) - Years in Top N:\n")
cat("  Top 100: 5/5 yrs = ", sum(eligible$years_top100_chi_full == 5),
    ", 4/5 = ", sum(eligible$years_top100_chi_full >= 4),
    ", 3/5 = ", sum(eligible$years_top100_chi_full >= 3), "\n", sep = "")
cat("  Top 300: 5/5 yrs = ", sum(eligible$years_top300_chi_full == 5),
    ", 4/5 = ", sum(eligible$years_top300_chi_full >= 4),
    ", 3/5 = ", sum(eligible$years_top300_chi_full >= 3), "\n", sep = "")
cat("  Top 500: 5/5 yrs = ", sum(eligible$years_top500_chi_full == 5),
    ", 4/5 = ", sum(eligible$years_top500_chi_full >= 4),
    ", 3/5 = ", sum(eligible$years_top500_chi_full >= 3), "\n", sep = "")

cat("\nFELONY VIOLENT - Years in Top N:\n")
cat("  Top 100: 5/5 yrs = ", sum(eligible$years_top100_felony_violent == 5),
    ", 4/5 = ", sum(eligible$years_top100_felony_violent >= 4),
    ", 3/5 = ", sum(eligible$years_top100_felony_violent >= 3), "\n", sep = "")
cat("  Top 300: 5/5 yrs = ", sum(eligible$years_top300_felony_violent == 5),
    ", 4/5 = ", sum(eligible$years_top300_felony_violent >= 4),
    ", 3/5 = ", sum(eligible$years_top300_felony_violent >= 3), "\n", sep = "")
cat("  Top 500: 5/5 yrs = ", sum(eligible$years_top500_felony_violent == 5),
    ", 4/5 = ", sum(eligible$years_top500_felony_violent >= 4),
    ", 3/5 = ", sum(eligible$years_top500_felony_violent >= 3), "\n", sep = "")

#==============================================================================
# SECTION 14: PERSISTENCE BY ALGORITHM
#==============================================================================

cat("\nSECTION 14: Persistence by algorithm (N=300)...\n")
cat(strrep("-", 50), "\n")

persistence_by_alg <- map_dfr(algorithms, function(alg) {
  ranked <- rank_blocks(block_matrix, alg)
  top_300 <- ranked %>% filter(rank <= 300)
  
  tibble(
    algorithm = alg$name,
    # Gun violence persistence
    gv_top100_3yr = sum(top_300$years_top100_gun_violence >= 3),
    gv_top300_3yr = sum(top_300$years_top300_gun_violence >= 3),
    gv_top300_4yr = sum(top_300$years_top300_gun_violence >= 4),
    gv_top300_5yr = sum(top_300$years_top300_gun_violence == 5),
    # CHI persistence
    chi_top100_3yr = sum(top_300$years_top100_chi_no_misd >= 3),
    chi_top300_3yr = sum(top_300$years_top300_chi_no_misd >= 3),
    chi_top300_4yr = sum(top_300$years_top300_chi_no_misd >= 4),
    chi_top300_5yr = sum(top_300$years_top300_chi_no_misd == 5),
    # Felony violent persistence
    fv_top300_3yr = sum(top_300$years_top300_felony_violent >= 3),
    fv_top300_4yr = sum(top_300$years_top300_felony_violent >= 4)
  )
})

cat("\nOf each algorithm's top 300 blocks, how many were persistently in top N?\n\n")
print(persistence_by_alg, width = Inf)

#==============================================================================
# SECTION 15: PRINT RESULTS SUMMARY
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("RESULTS SUMMARY\n")
cat(strrep("=", 70), "\n")

# Main metrics at N=300
cat("\nALGORITHM METRICS (N = 300)\n")
cat(strrep("-", 50), "\n\n")

metrics_300 <- all_metrics %>%
  filter(N == 300) %>%
  select(algorithm, gun_violence_5yr, shootings_5yr, felony_violent_5yr,
         capture_gv, capture_shootings, capture_felony, n_precincts, max_per_precinct)

print(metrics_300, width = Inf)

# Borough distribution
cat("\n\nBOROUGH DISTRIBUTION (N = 300)\n")
cat(strrep("-", 50), "\n")

borough_wide <- borough_distributions %>%
  filter(N == 300) %>%
  pivot_wider(names_from = borough, values_from = n_blocks, values_fill = 0) %>%
  select(algorithm, Bronx, Brooklyn, Manhattan, Queens, `Staten Island`)

print(borough_wide)

# Overlap
cat("\n\nOVERLAP (N = 300)\n")
cat(strrep("-", 50), "\n")
cat("  Blocks in all ", length(algorithms), " algorithms: ", overlap_300$in_all, "\n", sep = "")
cat("  Blocks in ", length(algorithms) - 2, "+ algorithms:  ", overlap_300$in_most, "\n", sep = "")
cat("  Blocks in ", ceiling(length(algorithms)/2), "+ algorithms:   ", overlap_300$in_half, "\n", sep = "")
cat("  Union (any algorithm):       ", overlap_300$union_size, "\n", sep = "")

# Recency
cat("\n\nRECENCY RATIOS (N = 300)\n")
cat(strrep("-", 50), "\n")
cat("Values > 1.0 = blocks heating up; < 1.0 = cooling down\n\n")

recency <- all_metrics %>%
  filter(N == 300) %>%
  select(algorithm, recency_ratio_gv, recency_ratio_fv)

print(recency)

#==============================================================================
# SECTION 16: SAVE OUTPUTS
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SAVING OUTPUTS\n")
cat(strrep("=", 70), "\n\n")

if (!dir.exists("output")) dir.create("output")

saveRDS(block_matrix, "output/block_matrix_full.rds")
cat("  Saved: output/block_matrix_full.rds\n")

write_csv(all_metrics, "output/algorithm_metrics.csv")
cat("  Saved: output/algorithm_metrics.csv\n")

write_csv(borough_distributions, "output/borough_distributions.csv")
cat("  Saved: output/borough_distributions.csv\n")

write_csv(persistence_by_alg, "output/persistence_by_algorithm.csv")
cat("  Saved: output/persistence_by_algorithm.csv\n")

saveRDS(list(n200 = overlap_200, n300 = overlap_300, n400 = overlap_400),
        "output/overlap_matrices.rds")
cat("  Saved: output/overlap_matrices.rds\n")

saveRDS(yearly_data, "output/yearly_data.rds")
cat("  Saved: output/yearly_data.rds\n")

cat("\n")
cat(strrep("=", 70), "\n")
cat("ANALYSIS COMPLETE\n")
cat(strrep("=", 70), "\n\n")

cat("Key objects in environment:\n")
cat("  - block_matrix: Full block data with all scores and persistence\n")
cat("  - all_metrics: Algorithm comparison metrics\n")
cat("  - borough_distributions: Borough breakdown\n")
cat("  - persistence_by_alg: Persistence by algorithm\n")
cat("  - yearly_data: Year-by-year metrics for all blocks\n")
cat("  - overlap_200/300/400: Overlap matrices\n\n")

