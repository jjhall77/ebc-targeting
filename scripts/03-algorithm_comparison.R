#==============================================================================
# 04_decision_support.R
# Every Block Counts (EBC) Project
# Purpose: Generate decision-support metrics for algorithm selection
#
# PREREQUISITE: Run 03_algorithm_comparison.R first (needs block_matrix)
#==============================================================================

library(tidyverse)
library(pwr)

#==============================================================================
# PARAMETERS
#==============================================================================

TARGET_NS <- c(200, 300, 400)
TOP_4_ALGORITHMS <- c("CHI_No_Misd", "CHI_Full", "Felony_GV_Boost", "CHI_Outdoor_No_Misd")

# CHI weights for standardized harm score (applied to all models)
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
cat("DECISION SUPPORT ANALYSIS\n")
cat(strrep("=", 70), "\n\n")

#==============================================================================
# SETUP: Define algorithms and ranking function
#==============================================================================

algorithms <- list(
  list(name = "CHI_No_Misd", rank_col = "chi_no_misd", 
       tiebreaker1 = "shootings", tiebreaker2 = "gun_violence"),
  list(name = "CHI_Full", rank_col = "chi_full", 
       tiebreaker1 = "shootings", tiebreaker2 = "gun_violence"),
  list(name = "Felony_GV_Boost", rank_col = "felony_gv_boost", 
       tiebreaker1 = "shootings", tiebreaker2 = "gun_violence"),
  list(name = "CHI_Outdoor_No_Misd", rank_col = "chi_outdoor_no_misd", 
       tiebreaker1 = "shootings", tiebreaker2 = "gun_violence")
)

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

# Add standardized harm score to block_matrix (same formula for all)
block_matrix <- block_matrix %>%
  mutate(
    harm_score = murder * HARM_WEIGHTS$murder +
      shootings * HARM_WEIGHTS$shooting +
      robbery * HARM_WEIGHTS$robbery +
      felony_assault * HARM_WEIGHTS$felony_assault +
      shots_fired * HARM_WEIGHTS$shots_fired +
      misd_assault * HARM_WEIGHTS$misd_assault
  )

#==============================================================================
# SECTION 1: CRIME COUNTS BY ALGORITHM AND N
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
      violent_plus_misd = sum(top_n$felony_violent) + sum(top_n$misd_assault),
      shootings = sum(top_n$shootings),
      shots_fired = sum(top_n$shots_fired),
      gun_violence = sum(top_n$gun_violence),
      harm_score = sum(top_n$harm_score),
      misd_assault = sum(top_n$misd_assault)
    )
  })
})

# Print by N
for (n in TARGET_NS) {
  cat("\n--- N =", n, "---\n\n")
  crime_counts %>%
    filter(N == n) %>%
    select(-N) %>%
    print(width = Inf)
}

#==============================================================================
# SECTION 2: INDOOR VS OUTDOOR BREAKDOWN
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 2: INDOOR VS OUTDOOR CRIME\n")
cat(strrep("=", 70), "\n")

indoor_outdoor <- map_dfr(algorithms, function(alg) {
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
      fv_total = fv_total,
      fv_outdoor = fv_outdoor,
      fv_indoor = fv_indoor,
      pct_fv_outdoor = round(fv_outdoor / fv_total * 100, 1),
      pct_fv_indoor = round(fv_indoor / fv_total * 100, 1),
      misd_total = misd_total,
      pct_misd_outdoor = round(misd_outdoor / misd_total * 100, 1),
      pct_misd_indoor = round(misd_indoor / misd_total * 100, 1)
    )
  })
})

for (n in TARGET_NS) {
  cat("\n--- N =", n, "---\n\n")
  indoor_outdoor %>%
    filter(N == n) %>%
    select(algorithm, fv_total, pct_fv_outdoor, pct_fv_indoor, 
           misd_total, pct_misd_outdoor, pct_misd_indoor) %>%
    print(width = Inf)
}

#==============================================================================
# SECTION 3: TOP 15 PRECINCTS BY ALGORITHM AND N
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 3: TOP 15 PRECINCTS BY ALGORITHM AND N\n")
cat(strrep("=", 70), "\n")

for (alg in algorithms) {
  cat("\n")
  cat(strrep("-", 50), "\n")
  cat("ALGORITHM:", alg$name, "\n")
  cat(strrep("-", 50), "\n")
  
  ranked <- rank_blocks(block_matrix, alg)
  
  for (n in c(200, 400)) {
    cat("\n  N =", n, "- Top 15 Precincts:\n")
    
    top_pcts <- ranked %>%
      filter(rank <= n) %>%
      count(precinct, borough, sort = TRUE, name = "n_blocks") %>%
      head(15)
    
    # Add crime totals for these precincts
    pct_stats <- ranked %>%
      filter(rank <= n, precinct %in% top_pcts$precinct) %>%
      group_by(precinct, borough) %>%
      summarise(
        n_blocks = n(),
        gun_violence = sum(gun_violence),
        felony_violent = sum(felony_violent),
        harm_score = sum(harm_score),
        .groups = "drop"
      ) %>%
      arrange(desc(n_blocks))
    
    print(pct_stats, n = 15)
  }
}

#==============================================================================
# SECTION 4: POWER CALCULATIONS
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 4: POWER CALCULATIONS\n")
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
    # Gun Violence
    gv_mean = mean(top_n$annual_gv),
    gv_sd = sd(top_n$annual_gv),
    
    # Felony Violent
    fv_mean = mean(top_n$annual_fv),
    fv_sd = sd(top_n$annual_fv),
    
    # Harm Score
    harm_mean = mean(top_n$annual_harm),
    harm_sd = sd(top_n$annual_harm),
    
    # Block counts
    n_blocks = n,
    n_precincts = n_distinct(top_n$precinct)
  )
}

# Function to calculate required sample size for given effect size
calc_required_n <- function(mean_val, sd_val, effect_pct, design_effect = 2.0) {
  # Effect size in SD units
  effect_size <- (mean_val * effect_pct / 100) / sd_val
  
  # Required n per group (unadjusted)
  if (effect_size > 0) {
    pwr_result <- tryCatch(
      pwr.t.test(d = effect_size, sig.level = 0.05, power = 0.80, type = "two.sample"),
      error = function(e) NULL
    )
    if (!is.null(pwr_result)) {
      n_per_group <- ceiling(pwr_result$n * design_effect)
      return(n_per_group * 2)  # Total N
    }
  }
  return(NA)
}

# Calculate for each algorithm and N
power_results <- map_dfr(algorithms, function(alg) {
  map_dfr(TARGET_NS, function(n) {
    stats <- calculate_power_stats(alg, n)
    
    tibble(
      algorithm = alg$name,
      N = n,
      n_precincts = stats$n_precincts,
      
      # Means
      gv_annual_mean = round(stats$gv_mean, 2),
      fv_annual_mean = round(stats$fv_mean, 2),
      harm_annual_mean = round(stats$harm_mean, 0),
      
      # Required N for different effect sizes (gun violence)
      gv_10pct = calc_required_n(stats$gv_mean, stats$gv_sd, 10),
      gv_15pct = calc_required_n(stats$gv_mean, stats$gv_sd, 15),
      gv_20pct = calc_required_n(stats$gv_mean, stats$gv_sd, 20),
      gv_25pct = calc_required_n(stats$gv_mean, stats$gv_sd, 25),
      
      # Required N for felony violent
      fv_10pct = calc_required_n(stats$fv_mean, stats$fv_sd, 10),
      fv_15pct = calc_required_n(stats$fv_mean, stats$fv_sd, 15),
      fv_20pct = calc_required_n(stats$fv_mean, stats$fv_sd, 20),
      
      # Required N for harm score
      harm_10pct = calc_required_n(stats$harm_mean, stats$harm_sd, 10),
      harm_15pct = calc_required_n(stats$harm_mean, stats$harm_sd, 15),
      harm_20pct = calc_required_n(stats$harm_mean, stats$harm_sd, 20)
    )
  })
})

cat("REQUIRED SAMPLE SIZE (Total blocks, both arms)\n")
cat("For within-precinct randomization with design effect = 2.0\n\n")

cat("--- Gun Violence (Blocks needed to detect X% reduction) ---\n\n")
power_results %>%
  select(algorithm, N, gv_annual_mean, gv_10pct, gv_15pct, gv_20pct, gv_25pct) %>%
  print(width = Inf, n = 20)

cat("\n--- Felony Violent Crime (Blocks needed to detect X% reduction) ---\n\n")
power_results %>%
  select(algorithm, N, fv_annual_mean, fv_10pct, fv_15pct, fv_20pct) %>%
  print(width = Inf, n = 20)

cat("\n--- Harm Score (Blocks needed to detect X% reduction) ---\n\n")
power_results %>%
  select(algorithm, N, harm_annual_mean, harm_10pct, harm_15pct, harm_20pct) %>%
  print(width = Inf, n = 20)

#==============================================================================
# SECTION 5: DETECTABLE EFFECT SIZE AT FIXED N
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 5: MINIMUM DETECTABLE EFFECT (MDE) AT FIXED N\n")
cat(strrep("=", 70), "\n\n")

# Given a fixed sample size, what effect can we detect?
calc_mde <- function(mean_val, sd_val, n_total, design_effect = 2.0) {
  n_per_group <- n_total / 2 / design_effect  # Effective n per group
  
  if (n_per_group < 2) return(NA)
  
  pwr_result <- tryCatch(
    pwr.t.test(n = n_per_group, sig.level = 0.05, power = 0.80, type = "two.sample"),
    error = function(e) NULL
  )
  
  if (!is.null(pwr_result)) {
    # Convert Cohen's d back to percent
    mde_absolute <- pwr_result$d * sd_val
    mde_pct <- (mde_absolute / mean_val) * 100
    return(round(mde_pct, 1))
  }
  return(NA)
}

mde_results <- map_dfr(algorithms, function(alg) {
  map_dfr(TARGET_NS, function(n) {
    stats <- calculate_power_stats(alg, n)
    
    tibble(
      algorithm = alg$name,
      N = n,
      
      # MDE for gun violence
      mde_gv = calc_mde(stats$gv_mean, stats$gv_sd, n),
      
      # MDE for felony violent
      mde_fv = calc_mde(stats$fv_mean, stats$fv_sd, n),
      
      # MDE for harm score
      mde_harm = calc_mde(stats$harm_mean, stats$harm_sd, n)
    )
  })
})

cat("Minimum Detectable Effect (% reduction) at each sample size:\n")
cat("(Alpha = 0.05, Power = 0.80, Design effect = 2.0)\n\n")

print(mde_results, width = Inf, n = 20)

#==============================================================================
# SECTION 6: TOP 6 BLOCKS PER PRECINCT (PRECINCT-CAPPED)
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 6: PRECINCT-CAPPED SELECTION (Top 6 per precinct)\n")
cat(strrep("=", 70), "\n\n")

# For each algorithm, select top 6 blocks per precinct
precinct_capped <- map_dfr(algorithms, function(alg) {
  ranked <- rank_blocks(block_matrix, alg)
  
  # Top 6 per precinct
  capped <- ranked %>%
    group_by(precinct) %>%
    slice_head(n = 6) %>%
    ungroup()
  
  tibble(
    algorithm = alg$name,
    total_blocks = nrow(capped),
    n_precincts = n_distinct(capped$precinct),
    gun_violence = sum(capped$gun_violence),
    felony_violent = sum(capped$felony_violent),
    harm_score = sum(capped$harm_score),
    shootings = sum(capped$shootings),
    avg_blocks_per_pct = round(nrow(capped) / n_distinct(capped$precinct), 1)
  )
})

cat("Summary of precinct-capped selection (max 6 blocks per precinct):\n\n")
print(precinct_capped, width = Inf)

# Distribution of blocks per precinct
cat("\n\nBlocks per precinct distribution (CHI_No_Misd, capped at 6):\n")

chi_no_misd_alg <- algorithms[[1]]
ranked_chi <- rank_blocks(block_matrix, chi_no_misd_alg)

capped_chi <- ranked_chi %>%
  group_by(precinct) %>%
  slice_head(n = 6) %>%
  ungroup()

capped_chi %>%
  count(precinct, borough, name = "n_blocks") %>%
  count(n_blocks, name = "n_precincts") %>%
  arrange(desc(n_blocks)) %>%
  print()

cat("\nTop 15 precincts (CHI_No_Misd, capped at 6):\n")
capped_chi %>%
  group_by(precinct, borough) %>%
  summarise(
    n_blocks = n(),
    gun_violence = sum(gun_violence),
    felony_violent = sum(felony_violent),
    harm_score = sum(harm_score),
    .groups = "drop"
  ) %>%
  arrange(desc(harm_score)) %>%
  head(15) %>%
  print()

#==============================================================================
# SECTION 7: WHERE DO TOP 20 GUN VIOLENCE BLOCKS FALL?
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 7: TOP 20 GUN VIOLENCE BLOCKS - COVERAGE CHECK\n")
cat(strrep("=", 70), "\n\n")

# Get top 20 blocks by gun violence (our "must-have" blocks)
eligible <- block_matrix %>% filter(!excluded)
top_20_gv <- eligible %>%
  arrange(desc(gun_violence), desc(shootings)) %>%
  head(20) %>%
  select(block_id, precinct, borough, gun_violence, shootings, 
         felony_violent, harm_score)

cat("Top 20 blocks by Gun Violence:\n\n")
print(top_20_gv, n = 20)

# Check where these blocks rank in each algorithm
cat("\n\nWhere do these top 20 GV blocks rank in each algorithm?\n\n")

top_20_ids <- top_20_gv$block_id

coverage_check <- map_dfr(algorithms, function(alg) {
  ranked <- rank_blocks(block_matrix, alg)
  
  top_20_ranks <- ranked %>%
    filter(block_id %in% top_20_ids) %>%
    select(block_id, rank)
  
  tibble(
    algorithm = alg$name,
    in_top_100 = sum(top_20_ranks$rank <= 100),
    in_top_200 = sum(top_20_ranks$rank <= 200),
    in_top_300 = sum(top_20_ranks$rank <= 300),
    in_top_400 = sum(top_20_ranks$rank <= 400),
    worst_rank = max(top_20_ranks$rank),
    median_rank = median(top_20_ranks$rank)
  )
})

print(coverage_check, width = Inf)

# Detailed ranking for each block
cat("\n\nDetailed ranks for each top-20 GV block:\n\n")

detailed_ranks <- map_dfc(algorithms, function(alg) {
  ranked <- rank_blocks(block_matrix, alg)
  
  ranks <- ranked %>%
    filter(block_id %in% top_20_ids) %>%
    arrange(match(block_id, top_20_ids)) %>%
    pull(rank)
  
  tibble(!!alg$name := ranks)
})

detailed_ranks <- bind_cols(
  top_20_gv %>% select(block_id, precinct, gun_violence),
  detailed_ranks
)

print(detailed_ranks, n = 20, width = Inf)

#==============================================================================
# SECTION 8: SUMMARY COMPARISON TABLE
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SECTION 8: SUMMARY COMPARISON TABLE\n")
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
      
      # Crime counts
      gun_violence = sum(top_n$gun_violence),
      shootings = sum(top_n$shootings),
      felony_violent = sum(top_n$felony_violent),
      harm_score = sum(top_n$harm_score),
      
      # Coverage
      top20_gv_covered = top_20_covered,
      
      # Geography
      n_precincts = n_distinct(top_n$precinct),
      max_per_pct = max(table(top_n$precinct)),
      
      # Indoor/Outdoor
      pct_fv_outdoor = round(sum(top_n$felony_violent_outdoor) / 
                               sum(top_n$felony_violent) * 100, 1),
      
      # Persistence (if available)
      persist_chi_3yr = if ("years_top300_chi_no_misd" %in% names(top_n)) 
        sum(top_n$years_top300_chi_no_misd >= 3) else NA,
      persist_fv_3yr = if ("years_top300_felony_violent" %in% names(top_n))
        sum(top_n$years_top300_felony_violent >= 3) else NA
    )
  })
})

cat("COMPREHENSIVE SUMMARY\n\n")

for (n in TARGET_NS) {
  cat("--- N =", n, "---\n\n")
  summary_table %>%
    filter(N == n) %>%
    select(-N) %>%
    print(width = Inf)
  cat("\n")
}

#==============================================================================
# SECTION 9: SAVE OUTPUTS
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SAVING OUTPUTS\n")
cat(strrep("=", 70), "\n\n")

if (!dir.exists("output")) dir.create("output")

write_csv(crime_counts, "output/decision_crime_counts.csv")
cat("  Saved: output/decision_crime_counts.csv\n")

write_csv(indoor_outdoor, "output/decision_indoor_outdoor.csv")
cat("  Saved: output/decision_indoor_outdoor.csv\n")

write_csv(power_results, "output/decision_power_calcs.csv")
cat("  Saved: output/decision_power_calcs.csv\n")

write_csv(mde_results, "output/decision_mde.csv")
cat("  Saved: output/decision_mde.csv\n")

write_csv(precinct_capped, "output/decision_precinct_capped.csv")
cat("  Saved: output/decision_precinct_capped.csv\n")

write_csv(coverage_check, "output/decision_top20_coverage.csv")
cat("  Saved: output/decision_top20_coverage.csv\n")

write_csv(summary_table, "output/decision_summary.csv")
cat("  Saved: output/decision_summary.csv\n")

# Save the capped selection block list
write_csv(capped_chi, "output/blocks_chi_no_misd_capped6.csv")
cat("  Saved: output/blocks_chi_no_misd_capped6.csv\n")

cat("\n")
cat(strrep("=", 70), "\n")
cat("DECISION SUPPORT ANALYSIS COMPLETE\n")
cat(strrep("=", 70), "\n")