#==============================================================================
# 04_decision_support_v3.R
# Every Block Counts (EBC) Project
# Purpose: Generate decision-support metrics for algorithm selection
#
# KEY OUTPUTS (ALL SAVED AS DATA FRAMES):
#   1. crime_counts: Total crime by algorithm and N
#   2. block_level_stats: Min/max/mean/var per block (CRITICAL)
#   3. indoor_outdoor_pct: % inside/outside with algorithms as columns
#   4. precinct_top6_all: Top 6 blocks for EVERY precinct
#   5. top20_gv_coverage: Which models capture top 20 GV blocks
#
# PREREQUISITE: Run 03_algorithm_comparison.R first (needs block_matrix)
#==============================================================================

library(tidyverse)
library(pwr)

#==============================================================================
# PARAMETERS
#==============================================================================

TARGET_NS <- c(200, 400)  # Focus on N = 200 and N = 400

# Note: Full algorithm list defined in SETUP section below
# This is just for reference - actual list has 8 models

# CHI weights for standardized harm score
HARM_WEIGHTS <- list(
  murder = 30,
  shooting = 30,
  robbery = 19,
  felony_assault = 10,
  shots_fired = 12,
  misd_assault = 1
)

#==============================================================================
# VERIFY DATA
#==============================================================================

if (!exists("block_matrix")) {
  stop("block_matrix not found. Run 03_algorithm_comparison.R first!")
}

cat("\n")
cat(strrep("=", 70), "\n")
cat("DECISION SUPPORT ANALYSIS (V3) - WITH FULL DATA FRAMES\n")
cat(strrep("=", 70), "\n\n")

#==============================================================================
# SETUP: Define algorithms and ranking function
#==============================================================================

# EXPANDED MODEL SET (8 models)
# Covers: CHI variants (indoor/outdoor × with/without misd)
#         GV weighting spectrum (none → boost → pure)
#         Outdoor variants for street-level intervention focus

algorithms <- list(
  # CHI Variants (2×2 matrix)
  list(name = "CHI_No_Misd", rank_col = "chi_no_misd", 
       tiebreaker1 = "shootings", tiebreaker2 = "gun_violence",
       description = "CHI without misd assault"),
  list(name = "CHI_Full", rank_col = "chi_full", 
       tiebreaker1 = "shootings", tiebreaker2 = "gun_violence",
       description = "Full CHI with misd assault"),
  list(name = "CHI_Outdoor_No_Misd", rank_col = "chi_outdoor_no_misd", 
       tiebreaker1 = "shootings", tiebreaker2 = "gun_violence",
       description = "Outdoor CHI without misd"),
  list(name = "CHI_Outdoor_Full", rank_col = "chi_outdoor_full", 
       tiebreaker1 = "shootings", tiebreaker2 = "gun_violence",
       description = "Outdoor CHI with misd"),
  
  # Felony + GV Boost variants
  list(name = "Felony_GV_Boost", rank_col = "felony_gv_boost", 
       tiebreaker1 = "shootings", tiebreaker2 = "gun_violence",
       description = "Felony violent + 3× gun violence"),
  list(name = "Outdoor_Felony_GV_Boost", rank_col = "felony_outdoor_gv_boost", 
       tiebreaker1 = "shootings", tiebreaker2 = "gun_violence",
       description = "Outdoor felony + 3× gun violence"),
  
  # Benchmarks
  list(name = "Felony_Violent_Only", rank_col = "felony_violent", 
       tiebreaker1 = "shootings", tiebreaker2 = "gun_violence",
       description = "Felony violent only (no GV weight)"),
  list(name = "Gun_Violence_Only", rank_col = "gun_violence", 
       tiebreaker1 = "shootings", tiebreaker2 = "felony_violent",
       description = "Pure gun violence ranking")
)

# List of algorithm names for iteration
ALL_ALGORITHMS <- sapply(algorithms, function(a) a$name)

# Print algorithm summary
cat("Analyzing", length(algorithms), "targeting algorithms:\n")
for (alg in algorithms) {
  cat("  - ", alg$name, ": ", alg$description, "\n", sep = "")
}
cat("\nTarget sample sizes: N =", paste(TARGET_NS, collapse = ", "), "\n\n")

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

# Ensure harm_score exists
if (!"harm_score" %in% names(block_matrix)) {
  block_matrix <- block_matrix %>%
    mutate(
      harm_score = murder * HARM_WEIGHTS$murder +
        shootings * HARM_WEIGHTS$shooting +
        robbery * HARM_WEIGHTS$robbery +
        felony_assault * HARM_WEIGHTS$felony_assault +
        shots_fired * HARM_WEIGHTS$shots_fired +
        misd_assault * HARM_WEIGHTS$misd_assault
    )
}

#==============================================================================
# SECTION 1: CRIME COUNTS BY ALGORITHM AND N (CRITICAL - SAVED AS DF)
#==============================================================================

cat(strrep("=", 70), "\n")
cat("SECTION 1: CRIME COUNTS BY ALGORITHM AND N\n")
cat(strrep("=", 70), "\n\n")

crime_counts <- map_dfr(algorithms, function(alg) {
  ranked <- rank_blocks(block_matrix, alg)
  
  map_dfr(TARGET_NS, function(n) {
    top_n <- ranked %>% filter(rank <= n)
    
    tibble(
      algorithm = alg$name,
      N = n,
      felony_violent = sum(top_n$felony_violent),
      misd_assault = sum(top_n$misd_assault),
      chi_score = sum(top_n$chi_no_misd),
      shootings = sum(top_n$shootings),
      shots_fired = sum(top_n$shots_fired),
      gun_violence = sum(top_n$gun_violence),
      harm_score = sum(top_n$harm_score)
    )
  })
})

# Print
cat("CRIME TOTALS (SUM across all selected blocks):\n\n")
for (n in TARGET_NS) {
  cat("--- N =", n, "---\n\n")
  print(as.data.frame(crime_counts %>% filter(N == n) %>% select(-N)), row.names = FALSE)
  cat("\n")
}

#==============================================================================
# SECTION 2: BLOCK-LEVEL STATISTICS (MIN/MAX/MEAN/VAR) - CRITICAL
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 2: BLOCK-LEVEL STATISTICS (MIN/MAX/MEAN/VAR) - CRITICAL\n")
cat(strrep("=", 70), "\n")

# Variables to calculate stats for
stat_variables <- c(
  "felony_violent",     # Violent felonies
  "misd_assault",       # Misdemeanor assault
  "chi_no_misd",        # CHI score (without misd)
  "shootings",          # Shootings
  "shots_fired",        # Shots fired
  "gun_violence"        # GV total (shootings + shots_fired)
)

# Function to calculate stats for a given variable
calc_block_stats <- function(top_n, var_name) {
  vals <- top_n[[var_name]]
  tibble(
    variable = var_name,
    min = min(vals, na.rm = TRUE),
    max = max(vals, na.rm = TRUE),
    mean = round(mean(vals, na.rm = TRUE), 2),
    variance = round(var(vals, na.rm = TRUE), 2),
    sd = round(sd(vals, na.rm = TRUE), 2),
    sum = sum(vals, na.rm = TRUE)
  )
}

# Calculate for all algorithms and Ns
block_level_stats <- map_dfr(algorithms, function(alg) {
  ranked <- rank_blocks(block_matrix, alg)
  
  map_dfr(TARGET_NS, function(n) {
    top_n <- ranked %>% filter(rank <= n)
    
    # Calculate stats for each variable
    stats_list <- map(stat_variables, ~calc_block_stats(top_n, .x))
    
    bind_rows(stats_list) %>%
      mutate(
        algorithm = alg$name,
        N = n
      ) %>%
      select(algorithm, N, everything())
  })
})

# Print by N with clear formatting
for (n in TARGET_NS) {
  cat("\n")
  cat(strrep("-", 60), "\n")
  cat("BLOCK-LEVEL STATS: N =", n, "\n")
  cat(strrep("-", 60), "\n")
  
  for (alg_name in ALL_ALGORITHMS) {
    cat("\n  ", alg_name, ":\n")
    stats_subset <- block_level_stats %>%
      filter(N == n, algorithm == alg_name) %>%
      select(-algorithm, -N)
    print(as.data.frame(stats_subset), row.names = FALSE)
  }
}

# Create WIDE format: One row per variable/N, algorithms as columns
# This is the CRITICAL view for comparison
block_stats_wide <- block_level_stats %>%
  select(algorithm, N, variable, min, max, mean, variance) %>%
  pivot_wider(
    id_cols = c(N, variable),
    names_from = algorithm,
    values_from = c(min, max, mean, variance),
    names_glue = "{algorithm}_{.value}"
  ) %>%
  arrange(N, variable)

cat("\n\n")
cat(strrep("-", 60), "\n")
cat("BLOCK-LEVEL STATS - WIDE FORMAT (algorithms as columns):\n")
cat(strrep("-", 60), "\n")

for (n in TARGET_NS) {
  cat("\n--- N =", n, "---\n\n")
  wide_subset <- block_stats_wide %>% 
    filter(N == n) %>% 
    select(-N)
  print(as.data.frame(wide_subset), row.names = FALSE)
  cat("\n")
}

#==============================================================================
# SECTION 3: INDOOR VS OUTDOOR BREAKDOWN - ALGORITHMS AS COLUMNS
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 3: INDOOR VS OUTDOOR CRIME PERCENTAGES\n")
cat(strrep("=", 70), "\n")

indoor_outdoor_raw <- map_dfr(algorithms, function(alg) {
  ranked <- rank_blocks(block_matrix, alg)
  
  map_dfr(TARGET_NS, function(n) {
    top_n <- ranked %>% filter(rank <= n)
    
    fv_total <- sum(top_n$felony_violent)
    fv_outdoor <- sum(top_n$felony_violent_outdoor)
    fv_indoor <- fv_total - fv_outdoor
    
    misd_total <- sum(top_n$misd_assault)
    misd_outdoor <- sum(top_n$misd_assault_outdoor)
    misd_indoor <- misd_total - misd_outdoor
    
    tibble(
      algorithm = alg$name,
      N = n,
      fv_total = round(fv_total, 1),
      fv_outdoor = round(fv_outdoor, 1),
      fv_indoor = round(fv_indoor, 1),
      pct_fv_outdoor = round(fv_outdoor / fv_total * 100, 1),
      pct_fv_indoor = round(fv_indoor / fv_total * 100, 1),
      misd_total = round(misd_total, 1),
      misd_outdoor = round(misd_outdoor, 1),
      misd_indoor = round(misd_indoor, 1),
      pct_misd_outdoor = round(misd_outdoor / misd_total * 100, 1),
      pct_misd_indoor = round(misd_indoor / misd_total * 100, 1)
    )
  })
})

# Create TRANSPOSED version: Metrics as rows, Algorithms as columns
# This is the format you requested
indoor_outdoor_pct <- indoor_outdoor_raw %>%
  select(algorithm, N, pct_fv_outdoor, pct_fv_indoor, pct_misd_outdoor, pct_misd_indoor) %>%
  pivot_longer(
    cols = c(pct_fv_outdoor, pct_fv_indoor, pct_misd_outdoor, pct_misd_indoor),
    names_to = "metric",
    values_to = "pct"
  ) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = pct
  ) %>%
  mutate(
    metric = case_when(
      metric == "pct_fv_outdoor" ~ "% Felony Violent OUTDOOR",
      metric == "pct_fv_indoor" ~ "% Felony Violent INDOOR",
      metric == "pct_misd_outdoor" ~ "% Misd Assault OUTDOOR",
      metric == "pct_misd_indoor" ~ "% Misd Assault INDOOR",
      TRUE ~ metric
    )
  )

cat("\nINDOOR/OUTDOOR PERCENTAGES (Metrics as rows, Algorithms as columns):\n")
for (n in TARGET_NS) {
  cat("\n--- N =", n, "---\n\n")
  print(as.data.frame(
    indoor_outdoor_pct %>% filter(N == n) %>% select(-N)
  ), row.names = FALSE)
  cat("\n")
}

#==============================================================================
# SECTION 4: TOP 6 BLOCKS PER PRECINCT - ALL PRECINCTS (CRITICAL)
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 4: TOP 6 BLOCKS PER PRECINCT - ALL PRECINCTS\n")
cat(strrep("=", 70), "\n\n")

# Get ALL precincts with their top 6 blocks for each algorithm
precinct_top6_all <- map_dfr(algorithms, function(alg) {
  ranked <- rank_blocks(block_matrix, alg)
  
  ranked %>%
    filter(!is.na(precinct)) %>%
    group_by(precinct) %>%
    slice_head(n = 6) %>%
    ungroup() %>%
    mutate(
      algorithm = alg$name,
      within_pct_rank = row_number()
    ) %>%
    group_by(precinct) %>%
    mutate(within_pct_rank = row_number()) %>%
    ungroup() %>%
    select(
      algorithm, precinct, borough, within_pct_rank, block_id,
      gun_violence, shootings, shots_fired, felony_violent, 
      misd_assault, chi_no_misd, harm_score
    )
})

# Summary: count per precinct by algorithm
precinct_summary <- precinct_top6_all %>%
  group_by(algorithm, precinct, borough) %>%
  summarise(
    n_blocks = n(),
    total_gv = sum(gun_violence),
    total_shootings = sum(shootings),
    total_fv = sum(felony_violent),
    total_chi = sum(chi_no_misd),
    .groups = "drop"
  ) %>%
  arrange(algorithm, desc(total_gv))

cat("Precinct summary (top 6 blocks each):\n\n")
for (alg_name in ALL_ALGORITHMS) {
  cat("\n--- ", alg_name, " ---\n\n", sep = "")
  alg_summary <- precinct_summary %>% 
    filter(algorithm == alg_name) %>%
    select(-algorithm) %>%
    arrange(desc(total_gv))
  print(as.data.frame(alg_summary), row.names = FALSE)
}

# Also create a WIDE version showing how many blocks each precinct gets per algorithm
precinct_counts_wide <- precinct_summary %>%
  select(algorithm, precinct, borough, n_blocks) %>%
  pivot_wider(
    id_cols = c(precinct, borough),
    names_from = algorithm,
    values_from = n_blocks,
    values_fill = 0
  ) %>%
  arrange(precinct)

cat("\n\nBlocks per precinct by algorithm (should all be 6):\n\n")
print(as.data.frame(precinct_counts_wide), row.names = FALSE)

#==============================================================================
# SECTION 5: TOP 20 GUN VIOLENCE BLOCKS - COVERAGE CHECK (CRITICAL)
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 5: TOP 20 GUN VIOLENCE BLOCKS - COVERAGE CHECK\n")
cat(strrep("=", 70), "\n\n")

# Get top 20 blocks by gun violence
eligible <- block_matrix %>% filter(!excluded)

top_20_gv <- eligible %>%
  arrange(desc(gun_violence), desc(shootings)) %>%
  head(20) %>%
  mutate(gv_rank = row_number()) %>%
  select(gv_rank, block_id, precinct, borough, gun_violence, shootings, 
         shots_fired, felony_violent, chi_no_misd, harm_score)

cat("TOP 20 BLOCKS BY GUN VIOLENCE:\n\n")
print(as.data.frame(top_20_gv), row.names = FALSE)

# Store the IDs
top_20_ids <- top_20_gv$block_id

# Check where these blocks rank in each algorithm at each N
top20_gv_coverage <- map_dfr(algorithms, function(alg) {
  ranked <- rank_blocks(block_matrix, alg)
  
  top_20_ranks <- ranked %>%
    filter(block_id %in% top_20_ids) %>%
    select(block_id, rank)
  
  map_dfr(TARGET_NS, function(n) {
    tibble(
      algorithm = alg$name,
      N = n,
      covered = sum(top_20_ranks$rank <= n),
      pct_covered = round(sum(top_20_ranks$rank <= n) / 20 * 100, 0),
      missed = 20 - sum(top_20_ranks$rank <= n),
      worst_rank = max(top_20_ranks$rank),
      median_rank = median(top_20_ranks$rank),
      mean_rank = round(mean(top_20_ranks$rank), 1)
    )
  })
})

cat("\n\nCOVERAGE OF TOP 20 GV BLOCKS BY ALGORITHM AND N:\n\n")
print(as.data.frame(top20_gv_coverage), row.names = FALSE)

# Create WIDE version: N as rows, algorithms showing coverage
coverage_wide <- top20_gv_coverage %>%
  select(algorithm, N, covered, pct_covered) %>%
  pivot_wider(
    id_cols = N,
    names_from = algorithm,
    values_from = c(covered, pct_covered),
    names_glue = "{algorithm}_{.value}"
  )

cat("\n\nCOVERAGE SUMMARY (algorithms as columns):\n\n")
print(as.data.frame(coverage_wide), row.names = FALSE)

# Detailed: which specific blocks are covered/missed by each algorithm at each N
detailed_coverage <- map_dfr(algorithms, function(alg) {
  ranked <- rank_blocks(block_matrix, alg)
  
  top_20_with_ranks <- ranked %>%
    filter(block_id %in% top_20_ids) %>%
    select(block_id, rank) %>%
    rename(alg_rank = rank)
  
  top_20_gv %>%
    left_join(top_20_with_ranks, by = "block_id") %>%
    mutate(
      algorithm = alg$name,
      in_top_200 = alg_rank <= 200,
      in_top_400 = alg_rank <= 400
    ) %>%
    select(algorithm, gv_rank, block_id, precinct, gun_violence, alg_rank, 
           in_top_200, in_top_400)
})

cat("\n\nDETAILED RANKS FOR EACH TOP-20 GV BLOCK:\n\n")
for (alg_name in ALL_ALGORITHMS) {
  cat("\n--- ", alg_name, " ---\n", sep = "")
  alg_detail <- detailed_coverage %>%
    filter(algorithm == alg_name) %>%
    select(-algorithm) %>%
    arrange(gv_rank)
  print(as.data.frame(alg_detail), row.names = FALSE)
}

# Create a matrix view: rows = top 20 blocks, columns = algorithm ranks
rank_matrix <- detailed_coverage %>%
  select(gv_rank, block_id, gun_violence, precinct, algorithm, alg_rank) %>%
  pivot_wider(
    id_cols = c(gv_rank, block_id, gun_violence, precinct),
    names_from = algorithm,
    values_from = alg_rank,
    names_prefix = "rank_"
  )

cat("\n\nRANK MATRIX (Top 20 GV blocks × Algorithm ranks):\n\n")
print(as.data.frame(rank_matrix), row.names = FALSE)

#==============================================================================
# SECTION 6: PRECINCT DISTRIBUTION BY ALGORITHM AND N
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 6: PRECINCT DISTRIBUTION BY ALGORITHM AND N\n")
cat(strrep("=", 70), "\n\n")

precinct_distribution <- map_dfr(algorithms, function(alg) {
  ranked <- rank_blocks(block_matrix, alg)
  
  map_dfr(TARGET_NS, function(n) {
    pct_stats <- ranked %>%
      filter(rank <= n) %>%
      group_by(precinct, borough) %>%
      summarise(
        n_blocks = n(),
        gun_violence = sum(gun_violence),
        shootings = sum(shootings),
        felony_violent = sum(felony_violent),
        chi_score = sum(chi_no_misd),
        .groups = "drop"
      ) %>%
      arrange(desc(n_blocks)) %>%
      mutate(
        pct_rank = row_number(),
        algorithm = alg$name,
        N = n
      ) %>%
      select(algorithm, N, pct_rank, everything())
    
    pct_stats
  })
})

# Print top 15 precincts for each combo
cat("TOP 15 PRECINCTS by blocks selected:\n")
for (alg_name in ALL_ALGORITHMS) {
  cat("\n")
  cat(strrep("-", 50), "\n")
  cat("ALGORITHM:", alg_name, "\n")
  cat(strrep("-", 50), "\n")
  
  for (n in TARGET_NS) {
    cat("\n  N =", n, "- Top 15 Precincts:\n")
    print(as.data.frame(
      precinct_distribution %>%
        filter(algorithm == alg_name, N == n, pct_rank <= 15) %>%
        select(-algorithm, -N)
    ), row.names = FALSE)
  }
}

#==============================================================================
# SECTION 7: POWER CALCULATIONS
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 7: POWER CALCULATIONS\n")
cat(strrep("=", 70), "\n\n")

cat("Assumptions:\n")
cat("  - Within-precinct randomization (EARLY vs LATE)\n")
cat("  - 2-year comparison period\n")
cat("  - Clustered at precinct level\n")
cat("  - Alpha = 0.05, Power = 0.80\n")
cat("  - Design effect for clustering = 2.0 (conservative)\n\n")

# Calculate block-level means and SDs for each outcome
calculate_power_stats <- function(alg, n) {
  ranked <- rank_blocks(block_matrix, alg)
  top_n <- ranked %>% filter(rank <= n)
  
  # Annual averages (5-year totals / 5)
  top_n <- top_n %>%
    mutate(
      annual_gv = gun_violence / 5,
      annual_fv = felony_violent / 5,
      annual_harm = harm_score / 5
    )
  
  list(
    gv_mean = mean(top_n$annual_gv),
    gv_sd = sd(top_n$annual_gv),
    fv_mean = mean(top_n$annual_fv),
    fv_sd = sd(top_n$annual_fv),
    harm_mean = mean(top_n$annual_harm),
    harm_sd = sd(top_n$annual_harm),
    n_blocks = n,
    n_precincts = n_distinct(top_n$precinct)
  )
}

# MDE calculations
calc_mde <- function(mean_val, sd_val, n_per_group, design_effect = 2.0) {
  effective_n <- n_per_group / design_effect
  
  pwr_result <- tryCatch(
    pwr.t.test(n = effective_n, sig.level = 0.05, power = 0.80, type = "two.sample"),
    error = function(e) NULL
  )
  
  if (!is.null(pwr_result)) {
    mde_absolute <- pwr_result$d * sd_val
    mde_pct <- (mde_absolute / mean_val) * 100
    return(round(mde_pct, 1))
  }
  return(NA)
}

power_results <- map_dfr(algorithms, function(alg) {
  map_dfr(TARGET_NS, function(n) {
    stats <- calculate_power_stats(alg, n)
    
    tibble(
      algorithm = alg$name,
      N = n,
      n_precincts = stats$n_precincts,
      gv_annual_mean = round(stats$gv_mean, 2),
      gv_annual_sd = round(stats$gv_sd, 2),
      fv_annual_mean = round(stats$fv_mean, 2),
      fv_annual_sd = round(stats$fv_sd, 2),
      mde_gv = calc_mde(stats$gv_mean, stats$gv_sd, n/2),
      mde_fv = calc_mde(stats$fv_mean, stats$fv_sd, n/2)
    )
  })
})

cat("Power and MDE by algorithm and N:\n\n")
print(as.data.frame(power_results), row.names = FALSE)

#==============================================================================
# SECTION 8: COMPREHENSIVE SUMMARY TABLE
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 8: COMPREHENSIVE SUMMARY TABLE\n")
cat(strrep("=", 70), "\n\n")

summary_table <- map_dfr(algorithms, function(alg) {
  map_dfr(TARGET_NS, function(n) {
    ranked <- rank_blocks(block_matrix, alg)
    top_n <- ranked %>% filter(rank <= n)
    
    # Check top 20 GV coverage
    top_20_covered <- sum(top_20_ids %in% top_n$block_id)
    
    tibble(
      algorithm = alg$name,
      N = n,
      
      # Crime counts (sums)
      gun_violence = round(sum(top_n$gun_violence), 1),
      shootings = round(sum(top_n$shootings), 1),
      shots_fired = round(sum(top_n$shots_fired), 1),
      felony_violent = round(sum(top_n$felony_violent), 1),
      misd_assault = round(sum(top_n$misd_assault), 1),
      chi_score = round(sum(top_n$chi_no_misd), 1),
      
      # Coverage
      top20_gv_covered = top_20_covered,
      
      # Geography
      n_precincts = n_distinct(top_n$precinct),
      max_per_pct = max(table(top_n$precinct)),
      
      # Indoor/Outdoor
      pct_fv_outdoor = round(sum(top_n$felony_violent_outdoor) / 
                               sum(top_n$felony_violent) * 100, 1),
      pct_misd_outdoor = round(sum(top_n$misd_assault_outdoor) / 
                                 sum(top_n$misd_assault) * 100, 1)
    )
  })
})

cat("COMPREHENSIVE SUMMARY:\n\n")
for (n in TARGET_NS) {
  cat("--- N =", n, "---\n\n")
  print(as.data.frame(summary_table %>% filter(N == n) %>% select(-N)), row.names = FALSE)
  cat("\n")
}

#==============================================================================
# SECTION 9: SAVE ALL OUTPUTS
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SAVING ALL OUTPUTS\n")
cat(strrep("=", 70), "\n\n")

if (!dir.exists("output")) dir.create("output")

# 1. Crime counts
write_csv(crime_counts, "output/decision_crime_counts.csv")
cat("  Saved: output/decision_crime_counts.csv\n")

# 2. Block-level statistics (CRITICAL)
write_csv(block_level_stats, "output/decision_block_level_stats.csv")
cat("  Saved: output/decision_block_level_stats.csv\n")

write_csv(block_stats_wide, "output/decision_block_stats_wide.csv")
cat("  Saved: output/decision_block_stats_wide.csv\n")

# 3. Indoor/outdoor percentages
write_csv(indoor_outdoor_raw, "output/decision_indoor_outdoor_raw.csv")
cat("  Saved: output/decision_indoor_outdoor_raw.csv\n")

write_csv(indoor_outdoor_pct, "output/decision_indoor_outdoor_pct.csv")
cat("  Saved: output/decision_indoor_outdoor_pct.csv\n")

# 4. Top 6 blocks per precinct - ALL PRECINCTS (CRITICAL)
write_csv(precinct_top6_all, "output/decision_precinct_top6_all.csv")
cat("  Saved: output/decision_precinct_top6_all.csv\n")

write_csv(precinct_summary, "output/decision_precinct_summary.csv")
cat("  Saved: output/decision_precinct_summary.csv\n")

# 5. Top 20 GV coverage (CRITICAL)
write_csv(as.data.frame(top_20_gv), "output/decision_top20_gv_blocks.csv")
cat("  Saved: output/decision_top20_gv_blocks.csv\n")

write_csv(top20_gv_coverage, "output/decision_top20_gv_coverage.csv")
cat("  Saved: output/decision_top20_gv_coverage.csv\n")

write_csv(detailed_coverage, "output/decision_top20_detailed_coverage.csv")
cat("  Saved: output/decision_top20_detailed_coverage.csv\n")

write_csv(as.data.frame(rank_matrix), "output/decision_top20_rank_matrix.csv")
cat("  Saved: output/decision_top20_rank_matrix.csv\n")

# 6. Precinct distribution
write_csv(precinct_distribution, "output/decision_precinct_distribution.csv")
cat("  Saved: output/decision_precinct_distribution.csv\n")

# 7. Power results
write_csv(power_results, "output/decision_power_results.csv")
cat("  Saved: output/decision_power_results.csv\n")

# 8. Summary table
write_csv(summary_table, "output/decision_summary.csv")
cat("  Saved: output/decision_summary.csv\n")

#==============================================================================
# SECTION 10: LIST OF DATA FRAMES IN ENVIRONMENT
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("DATA FRAMES AVAILABLE IN ENVIRONMENT\n")
cat(strrep("=", 70), "\n\n")

cat("CRITICAL DATA FRAMES:\n")
cat("  - crime_counts: Crime totals by algorithm and N (200, 400)\n")
cat("  - block_level_stats: Min/max/mean/variance per block (CRITICAL)\n")
cat("  - block_stats_wide: Block stats with algorithms as columns\n")
cat("  - indoor_outdoor_pct: % inside/outside with algorithms as columns\n")
cat("  - precinct_top6_all: Top 6 blocks for EVERY precinct (CRITICAL)\n")
cat("  - precinct_summary: Summary by precinct\n")
cat("  - top_20_gv: The top 20 gun violence blocks\n")
cat("  - top20_gv_coverage: Coverage of top 20 GV by algorithm/N (CRITICAL)\n")
cat("  - detailed_coverage: Detailed coverage for each top-20 block\n")
cat("  - rank_matrix: Ranks of top 20 GV blocks across all algorithms\n")
cat("  - precinct_distribution: Full precinct distribution by algorithm/N\n")
cat("  - power_results: Power and MDE calculations\n")
cat("  - summary_table: Comprehensive summary\n\n")

cat(strrep("=", 70), "\n")
cat("DECISION SUPPORT ANALYSIS V3 COMPLETE\n")
cat(strrep("=", 70), "\n")