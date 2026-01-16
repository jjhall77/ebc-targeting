#==============================================================================
# 03_algorithm_comparison.R
# Every Block Counts (EBC) Project
# Purpose: Compare targeting algorithms for block selection
#
# USAGE: Run section by section in RStudio
# PREREQUISITE: source("scripts/00_load_data.R") first
#
# OUTPUTS: All results saved to output/ directory as CSVs and RDS files
#==============================================================================

library(tidyverse)
library(sf)
library(ggplot2)
library(scales)
library(here)

#==============================================================================
# PARAMETERS
#==============================================================================

TARGET_NS <- c(200, 400)  # Removed 300 per decision

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

# Gun violence boost multiplier
GV_BOOST <- 3

# Persistence thresholds
PERSISTENCE_NS <- c(100, 300, 500)

# Output directory
OUTPUT_DIR <- "output"

#==============================================================================
# HELPER FUNCTIONS
#==============================================================================

# Safe print for tibbles
safe_print <- function(x, ...) {
  print(as.data.frame(x), row.names = FALSE, ...)
}

# Create output directory
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

#==============================================================================
# SECTION 1: VERIFY DATA LOADED
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("EBC TARGETING ALGORITHM COMPARISON\n")
cat(strrep("=", 70), "\n\n")

# NOTE: Uses _sf objects from 00_load_data.R
required_objects <- c(
  "physical_blocks", "intersection_nodes", "intersection_to_blocks",
  "nypp", "shootings_sf", "shots_fired_sf", "hospitals_sf", 
  "nypd_precinct_locations_sf", "violent_crime", "violent_street_crime"
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

# violent_crime and violent_street_crime are already filtered and sf objects
# Extract component crime types from violent_crime
murder_sf <- violent_crime %>% filter(ky_cd == 101)
robbery_sf <- violent_crime %>% filter(ky_cd == 105)
felony_assault_sf <- violent_crime %>% filter(ky_cd == 106)
misd_assault_sf <- violent_crime %>% filter(ky_cd == 344)

# Outdoor versions
murder_outdoor_sf <- murder_sf %>% filter(is_outdoor)
robbery_outdoor_sf <- robbery_sf %>% filter(is_outdoor)
felony_assault_outdoor_sf <- felony_assault_sf %>% filter(is_outdoor)
misd_assault_outdoor_sf <- misd_assault_sf %>% filter(is_outdoor)

# Shootings and shots fired already filtered to baseline in 00_load_data.R
shootings_baseline <- shootings_sf
shots_fired_baseline <- shots_fired_sf  # FIXED: Use sf object

# Recent year subsets
murder_recent <- murder_sf %>% filter(date >= RECENT_START)
robbery_recent <- robbery_sf %>% filter(date >= RECENT_START)
felony_assault_recent <- felony_assault_sf %>% filter(date >= RECENT_START)
misd_assault_recent <- misd_assault_sf %>% filter(date >= RECENT_START)
shootings_recent <- shootings_baseline %>% filter(date >= RECENT_START)
shots_fired_recent <- shots_fired_baseline %>% filter(date >= RECENT_START)

# Build crime counts summary
crime_counts_summary <- tibble(
  crime_type = c("Murder", "Robbery", "Felony Assault", "Misd Assault", 
                 "Shootings", "Shots Fired"),
  total_5yr = c(nrow(murder_sf), nrow(robbery_sf), nrow(felony_assault_sf),
                nrow(misd_assault_sf), nrow(shootings_baseline), nrow(shots_fired_baseline)),
  outdoor_5yr = c(nrow(murder_outdoor_sf), nrow(robbery_outdoor_sf), 
                  nrow(felony_assault_outdoor_sf), nrow(misd_assault_outdoor_sf),
                  NA, NA),
  pct_outdoor = round(outdoor_5yr / total_5yr * 100, 1),
  recent_1yr = c(nrow(murder_recent), nrow(robbery_recent), nrow(felony_assault_recent),
                 nrow(misd_assault_recent), nrow(shootings_recent), nrow(shots_fired_recent))
)

cat("\nCRIME COUNTS SUMMARY:\n")
safe_print(crime_counts_summary)

write_csv(crime_counts_summary, file.path(OUTPUT_DIR, "crime_counts_summary.csv"))
cat("\nSaved: output/crime_counts_summary.csv\n")

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
                                      threshold = INTERSECTION_THRESHOLD,
                                      verbose = FALSE) {
  
  n_crimes <- nrow(crime_sf)
  if (n_crimes == 0) {
    return(tibble(block_id = integer(), crime_count = numeric()))
  }
  
  # Find nearest intersection and block
  nearest_node_idx <- st_nearest_feature(crime_sf, intersection_nodes)
  nearest_node_id <- intersection_nodes$nodeid[nearest_node_idx]
  dist_to_intersection <- as.numeric(
    st_distance(crime_sf, intersection_nodes[nearest_node_idx, ], by_element = TRUE)
  )
  
  nearest_block_idx <- st_nearest_feature(crime_sf, physical_blocks)
  nearest_block_id <- physical_blocks$physical_id[nearest_block_idx]
  
  crime_allocation <- tibble(
    crime_idx = 1:n_crimes,
    nearest_node_id = nearest_node_id,
    dist_to_intersection = dist_to_intersection,
    nearest_block_id = nearest_block_id,
    is_at_intersection = dist_to_intersection <= threshold
  )
  
  # Split intersection crimes
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
  
  non_intersection <- crime_allocation %>%
    filter(!is_at_intersection) %>%
    count(nearest_block_id, name = "crime_count") %>%
    rename(block_id = nearest_block_id)
  
  result <- bind_rows(intersection_split, non_intersection) %>%
    group_by(block_id) %>%
    summarise(crime_count = sum(crime_count), .groups = "drop")
  
  if (verbose) {
    n_int <- sum(crime_allocation$is_at_intersection)
    cat("    ", n_int, "/", n_crimes, " at intersections (", 
        round(n_int/n_crimes*100, 1), "%)\n", sep = "")
  }
  
  result
}

#==============================================================================
# SECTION 5: BUILD BLOCK MATRIX
#==============================================================================

cat("\nSECTION 5: Building block crime matrix...\n")
cat(strrep("-", 50), "\n")

block_matrix <- physical_blocks %>%
  st_drop_geometry() %>%
  select(physical_id, total_length_ft, streets, boro) %>%
  rename(block_id = physical_id) %>%
  mutate(excluded = block_id %in% exclude_block_ids)

# Allocate each crime type
cat("  Allocating crimes to blocks:\n")

crime_types <- list(
  list(name = "murder", sf = murder_sf),
  list(name = "murder_outdoor", sf = murder_outdoor_sf),
  list(name = "robbery", sf = robbery_sf),
  list(name = "robbery_outdoor", sf = robbery_outdoor_sf),
  list(name = "felony_assault", sf = felony_assault_sf),
  list(name = "felony_assault_outdoor", sf = felony_assault_outdoor_sf),
  list(name = "misd_assault", sf = misd_assault_sf),
  list(name = "misd_assault_outdoor", sf = misd_assault_outdoor_sf),
  list(name = "shootings", sf = shootings_baseline),
  list(name = "shots_fired", sf = shots_fired_baseline),
  list(name = "murder_recent", sf = murder_recent),
  list(name = "robbery_recent", sf = robbery_recent),
  list(name = "felony_assault_recent", sf = felony_assault_recent),
  list(name = "misd_assault_recent", sf = misd_assault_recent),
  list(name = "shootings_recent", sf = shootings_recent),
  list(name = "shots_fired_recent", sf = shots_fired_recent)
)

for (ct in crime_types) {
  cat("    ", ct$name, "...", sep = "")
  counts <- allocate_crimes_to_blocks(
    ct$sf, physical_blocks, intersection_nodes, intersection_to_blocks
  ) %>% rename(!!ct$name := crime_count)
  block_matrix <- block_matrix %>% left_join(counts, by = "block_id")
  cat("done\n")
}

# Replace NAs
block_matrix <- block_matrix %>%
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
    
    # CHI Scores
    chi_full = murder * CHI_WEIGHTS$murder + 
      shootings * CHI_WEIGHTS$shooting + 
      robbery * CHI_WEIGHTS$robbery + 
      felony_assault * CHI_WEIGHTS$felony_assault + 
      shots_fired * CHI_WEIGHTS$shots_fired + 
      misd_assault * CHI_WEIGHTS$misd_assault,
    
    chi_no_misd = murder * CHI_WEIGHTS$murder + 
      shootings * CHI_WEIGHTS$shooting + 
      robbery * CHI_WEIGHTS$robbery + 
      felony_assault * CHI_WEIGHTS$felony_assault + 
      shots_fired * CHI_WEIGHTS$shots_fired,
    
    chi_outdoor_full = murder_outdoor * CHI_WEIGHTS$murder + 
      shootings * CHI_WEIGHTS$shooting +
      robbery_outdoor * CHI_WEIGHTS$robbery + 
      felony_assault_outdoor * CHI_WEIGHTS$felony_assault + 
      shots_fired * CHI_WEIGHTS$shots_fired +
      misd_assault_outdoor * CHI_WEIGHTS$misd_assault,
    
    chi_outdoor_no_misd = murder_outdoor * CHI_WEIGHTS$murder + 
      shootings * CHI_WEIGHTS$shooting + 
      robbery_outdoor * CHI_WEIGHTS$robbery + 
      felony_assault_outdoor * CHI_WEIGHTS$felony_assault + 
      shots_fired * CHI_WEIGHTS$shots_fired,
    
    # Felony + GV Boost
    felony_gv_boost = felony_violent + (gun_violence * GV_BOOST),
    felony_outdoor_gv_boost = felony_violent_outdoor + (gun_violence * GV_BOOST)
  )

cat("  Composite scores created.\n")

#==============================================================================
# SECTION 7: DEFINE ALGORITHMS
#==============================================================================

cat("\nSECTION 7: Defining algorithms...\n")
cat(strrep("-", 50), "\n")

algorithms <- list(
  list(name = "CHI_Full", rank_col = "chi_full",
       desc = "Full CHI (Murder×30, Shooting×30, Robbery×19, FA×10, SF×12, MA×1)"),
  list(name = "CHI_No_Misd", rank_col = "chi_no_misd",
       desc = "CHI without Misd Assault"),
  list(name = "CHI_Outdoor_Full", rank_col = "chi_outdoor_full",
       desc = "Outdoor CHI with Misd Assault"),
  list(name = "CHI_Outdoor_No_Misd", rank_col = "chi_outdoor_no_misd",
       desc = "Outdoor CHI without Misd Assault"),
  list(name = "Felony_GV_Boost", rank_col = "felony_gv_boost",
       desc = "Felony Violent + (Gun Violence × 3)"),
  list(name = "Outdoor_Felony_GV_Boost", rank_col = "felony_outdoor_gv_boost",
       desc = "Outdoor Felony + (Gun Violence × 3)"),
  list(name = "Gun_Violence", rank_col = "gun_violence",
       desc = "Gun Violence only (shootings + shots fired)"),
  list(name = "Felony_Violent_Only", rank_col = "felony_violent",
       desc = "Felony Violent only (no GV weighting)")
)

# Add common tiebreakers
algorithms <- map(algorithms, ~c(.x, list(tiebreaker1 = "shootings", tiebreaker2 = "gun_violence")))

algorithm_list <- tibble(
  algorithm = map_chr(algorithms, ~.x$name),
  rank_col = map_chr(algorithms, ~.x$rank_col),
  description = map_chr(algorithms, ~.x$desc)
)

cat("  ", length(algorithms), " algorithms defined:\n", sep = "")
safe_print(algorithm_list %>% select(algorithm, description))

write_csv(algorithm_list, file.path(OUTPUT_DIR, "algorithm_definitions.csv"))

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

citywide <- tibble(
  metric = c("gun_violence", "shootings", "shots_fired", "felony_violent", "misd_assault"),
  total = c(sum(eligible$gun_violence), sum(eligible$shootings), sum(eligible$shots_fired),
            sum(eligible$felony_violent), sum(eligible$misd_assault))
)

all_metrics <- map_dfr(algorithms, function(alg) {
  cat("  ", alg$name, "...\n", sep = "")
  ranked <- rank_blocks(block_matrix, alg)
  
  map_dfr(TARGET_NS, function(n) {
    top_n <- ranked %>% filter(rank <= n)
    
    tibble(
      algorithm = alg$name,
      N = n,
      gun_violence = sum(top_n$gun_violence),
      shootings = sum(top_n$shootings),
      shots_fired = sum(top_n$shots_fired),
      felony_violent = sum(top_n$felony_violent),
      misd_assault = sum(top_n$misd_assault),
      felony_outdoor = sum(top_n$felony_violent_outdoor),
      pct_felony_outdoor = round(felony_outdoor / max(felony_violent, 1) * 100, 1),
      gun_violence_recent = sum(top_n$gun_violence_recent),
      felony_recent = sum(top_n$felony_violent_recent),
      capture_gv = round(gun_violence / citywide$total[1] * 100, 1),
      capture_shootings = round(shootings / citywide$total[2] * 100, 1),
      capture_felony = round(felony_violent / citywide$total[4] * 100, 1),
      n_precincts = n_distinct(top_n$precinct, na.rm = TRUE),
      max_per_precinct = max(table(top_n$precinct)),
      score_min = min(top_n[[alg$rank_col]]),
      score_median = median(top_n[[alg$rank_col]]),
      score_max = max(top_n[[alg$rank_col]])
    )
  })
})

cat("\nALGORITHM METRICS:\n")
safe_print(all_metrics %>% select(algorithm, N, gun_violence, shootings, felony_violent,
                                  capture_gv, capture_shootings, capture_felony))

write_csv(all_metrics, file.path(OUTPUT_DIR, "algorithm_metrics.csv"))
cat("\nSaved: output/algorithm_metrics.csv\n")

#==============================================================================
# SECTION 10: BLOCK-LEVEL STATISTICS
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 10: Block-level statistics\n")
cat(strrep("=", 70), "\n\n")

crime_vars <- c("felony_violent", "misd_assault", "chi_no_misd", "shootings", "shots_fired", "gun_violence")

block_stats <- map_dfr(algorithms, function(alg) {
  ranked <- rank_blocks(block_matrix, alg)
  
  map_dfr(TARGET_NS, function(n) {
    top_n <- ranked %>% filter(rank <= n)
    
    map_dfr(crime_vars, function(var) {
      vals <- top_n[[var]]
      tibble(
        algorithm = alg$name,
        N = n,
        variable = var,
        min = min(vals),
        max = max(vals),
        mean = round(mean(vals), 2),
        median = median(vals),
        sd = round(sd(vals), 2),
        var = round(var(vals), 2),
        sum = sum(vals)
      )
    })
  })
})

# Wide format: algorithms as columns
block_stats_wide <- block_stats %>%
  select(algorithm, N, variable, mean) %>%
  pivot_wider(names_from = algorithm, values_from = mean)

cat("BLOCK-LEVEL STATISTICS (mean by algorithm):\n\n")
for (n_val in TARGET_NS) {
  cat("N =", n_val, ":\n")
  safe_print(block_stats_wide %>% filter(N == n_val) %>% select(-N))
  cat("\n")
}

write_csv(block_stats, file.path(OUTPUT_DIR, "block_level_stats.csv"))
write_csv(block_stats_wide, file.path(OUTPUT_DIR, "block_stats_wide.csv"))
cat("Saved: output/block_level_stats.csv, output/block_stats_wide.csv\n")

#==============================================================================
# SECTION 11: INDOOR/OUTDOOR ANALYSIS
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 11: Indoor/Outdoor Analysis\n")
cat(strrep("=", 70), "\n\n")

indoor_outdoor <- map_dfr(algorithms, function(alg) {
  ranked <- rank_blocks(block_matrix, alg)
  
  map_dfr(TARGET_NS, function(n) {
    top_n <- ranked %>% filter(rank <= n)
    
    tibble(
      algorithm = alg$name,
      N = n,
      felony_total = sum(top_n$felony_violent),
      felony_outdoor = sum(top_n$felony_violent_outdoor),
      felony_indoor = felony_total - felony_outdoor,
      pct_outdoor = round(felony_outdoor / max(felony_total, 1) * 100, 1),
      pct_indoor = round(felony_indoor / max(felony_total, 1) * 100, 1),
      misd_total = sum(top_n$misd_assault),
      misd_outdoor = sum(top_n$misd_assault_outdoor),
      misd_indoor = misd_total - misd_outdoor,
      misd_pct_outdoor = round(misd_outdoor / max(misd_total, 1) * 100, 1)
    )
  })
})

# Transposed: metrics as rows, algorithms as columns
indoor_outdoor_transposed <- indoor_outdoor %>%
  filter(N == 400) %>%
  select(algorithm, pct_outdoor, pct_indoor, misd_pct_outdoor) %>%
  pivot_longer(-algorithm, names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = algorithm, values_from = value)

cat("INDOOR/OUTDOOR PERCENTAGES (N=400):\n")
safe_print(indoor_outdoor_transposed)

write_csv(indoor_outdoor, file.path(OUTPUT_DIR, "indoor_outdoor_analysis.csv"))
write_csv(indoor_outdoor_transposed, file.path(OUTPUT_DIR, "indoor_outdoor_transposed.csv"))
cat("\nSaved: output/indoor_outdoor_analysis.csv, output/indoor_outdoor_transposed.csv\n")

#==============================================================================
# SECTION 12: BOROUGH DISTRIBUTION
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 12: Borough Distribution\n")
cat(strrep("=", 70), "\n\n")

borough_dist <- map_dfr(algorithms, function(alg) {
  ranked <- rank_blocks(block_matrix, alg)
  map_dfr(TARGET_NS, function(n) {
    ranked %>%
      filter(rank <= n) %>%
      count(borough, name = "n_blocks") %>%
      mutate(algorithm = alg$name, N = n, pct = round(n_blocks / n * 100, 1))
  })
})

borough_wide <- borough_dist %>%
  filter(N == 400) %>%
  select(algorithm, borough, n_blocks) %>%
  pivot_wider(names_from = borough, values_from = n_blocks, values_fill = 0)

cat("BOROUGH DISTRIBUTION (N=400):\n")
safe_print(borough_wide)

write_csv(borough_dist, file.path(OUTPUT_DIR, "borough_distribution.csv"))
cat("\nSaved: output/borough_distribution.csv\n")

#==============================================================================
# SECTION 13: PRECINCT DISTRIBUTION
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 13: Precinct Distribution\n")
cat(strrep("=", 70), "\n\n")

precinct_dist <- map_dfr(algorithms, function(alg) {
  ranked <- rank_blocks(block_matrix, alg)
  map_dfr(TARGET_NS, function(n) {
    ranked %>%
      filter(rank <= n) %>%
      count(precinct, borough, name = "n_blocks") %>%
      mutate(algorithm = alg$name, N = n) %>%
      filter(!is.na(precinct))
  })
})

# Top 15 precincts per algorithm
top_precincts <- precinct_dist %>%
  filter(N == 400) %>%
  group_by(algorithm) %>%
  slice_max(n_blocks, n = 15) %>%
  ungroup()

cat("TOP 15 PRECINCTS (N=400, CHI_No_Misd):\n")
safe_print(top_precincts %>% filter(algorithm == "CHI_No_Misd") %>% select(-algorithm))

write_csv(precinct_dist, file.path(OUTPUT_DIR, "precinct_distribution.csv"))
write_csv(top_precincts, file.path(OUTPUT_DIR, "top_precincts.csv"))
cat("\nSaved: output/precinct_distribution.csv, output/top_precincts.csv\n")

#==============================================================================
# SECTION 14: ALGORITHM OVERLAP
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 14: Algorithm Overlap\n")
cat(strrep("=", 70), "\n\n")

calculate_overlap <- function(block_matrix, algorithms, n) {
  top_n_ids <- map(algorithms, function(alg) {
    ranked <- rank_blocks(block_matrix, alg)
    ranked$block_id[1:n]
  })
  names(top_n_ids) <- map_chr(algorithms, ~.x$name)
  
  # Pairwise overlap
  alg_names <- names(top_n_ids)
  overlap_matrix <- matrix(0, nrow = length(alg_names), ncol = length(alg_names),
                           dimnames = list(alg_names, alg_names))
  for (i in seq_along(alg_names)) {
    for (j in seq_along(alg_names)) {
      overlap_matrix[i, j] <- length(intersect(top_n_ids[[i]], top_n_ids[[j]]))
    }
  }
  
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

overlap_results <- map(TARGET_NS, function(n) {
  result <- calculate_overlap(block_matrix, algorithms, n)
  result$N <- n
  result
})
names(overlap_results) <- paste0("N", TARGET_NS)

overlap_summary <- tibble(
  N = TARGET_NS,
  in_all = map_int(overlap_results, ~.x$in_all),
  in_most = map_int(overlap_results, ~.x$in_most),
  in_half = map_int(overlap_results, ~.x$in_half),
  union_size = map_int(overlap_results, ~.x$union_size)
)

cat("OVERLAP SUMMARY:\n")
safe_print(overlap_summary)

write_csv(overlap_summary, file.path(OUTPUT_DIR, "overlap_summary.csv"))
saveRDS(overlap_results, file.path(OUTPUT_DIR, "overlap_matrices.rds"))
cat("\nSaved: output/overlap_summary.csv, output/overlap_matrices.rds\n")

#==============================================================================
# SECTION 15: PERSISTENCE ANALYSIS
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 15: Persistence Analysis (Year-by-Year)\n")
cat(strrep("=", 70), "\n\n")

cat("  This takes a few minutes - calculating yearly crime for all blocks.\n\n")

eligible_ids <- block_matrix %>% filter(!excluded) %>% pull(block_id)

# Calculate year-by-year
yearly_data <- map_dfr(2021:2025, function(yr) {
  cat("    Processing FY", yr, "...")
  yr_start <- as.Date(paste0(yr - 1, "-10-01"))
  yr_end <- as.Date(paste0(yr, "-09-30"))
  
  # Filter each crime type to this year
  murder_yr <- murder_sf %>% filter(date >= yr_start & date <= yr_end)
  robbery_yr <- robbery_sf %>% filter(date >= yr_start & date <= yr_end)
  fel_yr <- felony_assault_sf %>% filter(date >= yr_start & date <= yr_end)
  misd_yr <- misd_assault_sf %>% filter(date >= yr_start & date <= yr_end)
  shoot_yr <- shootings_baseline %>% filter(date >= yr_start & date <= yr_end)
  sf_yr <- shots_fired_baseline %>% filter(date >= yr_start & date <= yr_end)
  
  # Allocate to blocks
  murder_ct <- allocate_crimes_to_blocks(murder_yr, physical_blocks, intersection_nodes, intersection_to_blocks) %>% rename(murder = crime_count)
  robbery_ct <- allocate_crimes_to_blocks(robbery_yr, physical_blocks, intersection_nodes, intersection_to_blocks) %>% rename(robbery = crime_count)
  fel_ct <- allocate_crimes_to_blocks(fel_yr, physical_blocks, intersection_nodes, intersection_to_blocks) %>% rename(felony_assault = crime_count)
  misd_ct <- allocate_crimes_to_blocks(misd_yr, physical_blocks, intersection_nodes, intersection_to_blocks) %>% rename(misd_assault = crime_count)
  shoot_ct <- allocate_crimes_to_blocks(shoot_yr, physical_blocks, intersection_nodes, intersection_to_blocks) %>% rename(shootings = crime_count)
  sf_ct <- allocate_crimes_to_blocks(sf_yr, physical_blocks, intersection_nodes, intersection_to_blocks) %>% rename(shots_fired = crime_count)
  
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
      chi_no_misd = murder * CHI_WEIGHTS$murder + shootings * CHI_WEIGHTS$shooting + 
        robbery * CHI_WEIGHTS$robbery + felony_assault * CHI_WEIGHTS$felony_assault + 
        shots_fired * CHI_WEIGHTS$shots_fired,
      year = yr
    )
  
  cat("done\n")
  result
})

# Calculate persistence
persistence_metrics <- c("gun_violence", "chi_no_misd", "felony_violent")

persistence_data <- tibble(block_id = eligible_ids)

for (metric in persistence_metrics) {
  cat("  Ranking by ", metric, "...\n", sep = "")
  
  yearly_ranked <- yearly_data %>%
    group_by(year) %>%
    mutate(rank = rank(-!!sym(metric), ties.method = "min")) %>%
    ungroup()
  
  for (n in PERSISTENCE_NS) {
    col_name <- paste0("years_top", n, "_", metric)
    counts <- yearly_ranked %>%
      group_by(block_id) %>%
      summarise(!!col_name := sum(rank <= n), .groups = "drop")
    persistence_data <- persistence_data %>% left_join(counts, by = "block_id")
  }
}

# Add to block_matrix
block_matrix <- block_matrix %>%
  select(-any_of(names(persistence_data)[-1])) %>%
  left_join(persistence_data, by = "block_id") %>%
  mutate(across(starts_with("years_top"), ~replace_na(., 0)))

# Persistence summary
persistence_summary <- map_dfr(persistence_metrics, function(metric) {
  map_dfr(PERSISTENCE_NS, function(n) {
    col <- paste0("years_top", n, "_", metric)
    vals <- eligible[[col]]
    if (is.null(vals)) vals <- block_matrix %>% filter(!excluded) %>% pull(!!sym(col))
    tibble(
      metric = metric,
      top_n = n,
      blocks_5yr = sum(vals == 5),
      blocks_4yr = sum(vals >= 4),
      blocks_3yr = sum(vals >= 3)
    )
  })
})

cat("\nPERSISTENCE SUMMARY:\n")
safe_print(persistence_summary)

write_csv(persistence_summary, file.path(OUTPUT_DIR, "persistence_summary.csv"))
saveRDS(yearly_data, file.path(OUTPUT_DIR, "yearly_data.rds"))
cat("\nSaved: output/persistence_summary.csv, output/yearly_data.rds\n")

#==============================================================================
# SECTION 16: PERSISTENCE BY ALGORITHM
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 16: Persistence by Algorithm\n")
cat(strrep("=", 70), "\n\n")

# Refresh eligible with persistence columns
eligible <- block_matrix %>% filter(!excluded)

persistence_by_alg <- map_dfr(algorithms, function(alg) {
  map_dfr(TARGET_NS, function(n) {
    ranked <- rank_blocks(block_matrix, alg)
    top_n <- ranked %>% filter(rank <= n)
    
    tibble(
      algorithm = alg$name,
      N = n,
      gv_top100_3yr = sum(top_n$years_top100_gun_violence >= 3, na.rm = TRUE),
      gv_top300_3yr = sum(top_n$years_top300_gun_violence >= 3, na.rm = TRUE),
      gv_top300_5yr = sum(top_n$years_top300_gun_violence == 5, na.rm = TRUE),
      chi_top300_3yr = sum(top_n$years_top300_chi_no_misd >= 3, na.rm = TRUE),
      chi_top300_5yr = sum(top_n$years_top300_chi_no_misd == 5, na.rm = TRUE),
      fv_top300_3yr = sum(top_n$years_top300_felony_violent >= 3, na.rm = TRUE)
    )
  })
})

cat("PERSISTENCE BY ALGORITHM:\n")
safe_print(persistence_by_alg)

write_csv(persistence_by_alg, file.path(OUTPUT_DIR, "persistence_by_algorithm.csv"))
cat("\nSaved: output/persistence_by_algorithm.csv\n")

#==============================================================================
# SECTION 17: TOP BLOCKS DETAIL
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 17: Top Blocks Detail\n")
cat(strrep("=", 70), "\n\n")

# Top 20 blocks by gun violence with all algorithm ranks
top_gv_blocks <- rank_blocks(block_matrix, algorithms[[7]]) %>%  # Gun_Violence
  filter(rank <= 20) %>%
  select(block_id, streets, borough, precinct, gun_violence, shootings, 
         shots_fired, felony_violent, chi_no_misd)

# Add ranks from each algorithm
for (alg in algorithms) {
  ranked <- rank_blocks(block_matrix, alg)
  rank_col <- paste0("rank_", alg$name)
  top_gv_blocks <- top_gv_blocks %>%
    left_join(ranked %>% select(block_id, rank) %>% rename(!!rank_col := rank),
              by = "block_id")
}

cat("TOP 20 GUN VIOLENCE BLOCKS WITH ALL ALGORITHM RANKS:\n")
safe_print(top_gv_blocks %>% select(block_id, borough, precinct, gun_violence, 
                                    starts_with("rank_")))

write_csv(top_gv_blocks, file.path(OUTPUT_DIR, "top20_gv_blocks_detailed.csv"))
cat("\nSaved: output/top20_gv_blocks_detailed.csv\n")

#==============================================================================
# SECTION 18: SAVE BLOCK MATRIX
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SAVING FINAL OUTPUTS\n")
cat(strrep("=", 70), "\n\n")

saveRDS(block_matrix, file.path(OUTPUT_DIR, "block_matrix_full.rds"))
cat("Saved: output/block_matrix_full.rds\n")

# Also save eligible blocks only as CSV for easy access
eligible_export <- block_matrix %>%
  filter(!excluded) %>%
  select(block_id, streets, borough, precinct, boro,
         shootings, shots_fired, gun_violence,
         murder, robbery, felony_assault, misd_assault,
         felony_violent, felony_violent_outdoor,
         chi_full, chi_no_misd, chi_outdoor_no_misd,
         felony_gv_boost, 
         starts_with("years_top"))

write_csv(eligible_export, file.path(OUTPUT_DIR, "eligible_blocks.csv"))
cat("Saved: output/eligible_blocks.csv\n")

#==============================================================================
# COMPLETE
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("ANALYSIS COMPLETE\n")
cat(strrep("=", 70), "\n\n")

cat("Output files saved to:", OUTPUT_DIR, "\n\n")

output_files <- list.files(OUTPUT_DIR, pattern = "\\.(csv|rds)$")
cat("Files created:\n")
for (f in output_files) {
  cat("  -", f, "\n")
}

cat("\nKey objects in environment:\n")
cat("  - block_matrix: Full block data with all scores and persistence\n")
cat("  - all_metrics: Algorithm comparison metrics\n")
cat("  - block_stats: Block-level statistics by algorithm\n")
cat("  - indoor_outdoor: Indoor/outdoor analysis\n")
cat("  - borough_dist: Borough distribution\n")
cat("  - precinct_dist: Precinct distribution\n")
cat("  - persistence_by_alg: Persistence by algorithm\n")
cat("  - yearly_data: Year-by-year metrics for all blocks\n")
cat("  - overlap_results: Overlap matrices\n\n")