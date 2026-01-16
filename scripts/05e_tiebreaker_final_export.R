#==============================================================================
# 05e_tiebreaker_final_export.R
# Every Block Counts (EBC) Project
# Purpose: Final tiebreaker comparison CSV with annualized values + persistence
#
# PREREQUISITE: Run 03c_add_annual_columns.R first (needs persistence_results)
#               OR run after block_matrix has annual columns
#==============================================================================

library(tidyverse)

#==============================================================================
# VERIFY DATA
#==============================================================================

if (!exists("block_matrix")) {
  stop("block_matrix not found. Run earlier scripts first!")
}

if (!exists("persistence_results")) {
  cat("persistence_results not found. Running persistence calculation...\n")
  # Will calculate below
  calc_persistence <- TRUE
} else {
  calc_persistence <- FALSE
}

cat("\n")
cat(strrep("=", 70), "\n")
cat("FINAL TIEBREAKER EXPORT - ANNUALIZED + PERSISTENCE\n")
cat(strrep("=", 70), "\n\n")

#==============================================================================
# SETUP
#==============================================================================

TARGET_NS <- c(200, 400)

algorithms <- list(
  list(name = "Gun_Violence_Only", rank_col = "gun_violence", 
       tiebreaker1 = "shootings", tiebreaker2 = "felony_violent"),
  list(name = "Felony_Violent_Only", rank_col = "felony_violent", 
       tiebreaker1 = "shootings", tiebreaker2 = "gun_violence"),
  list(name = "CHI_Outdoor_No_Misd", rank_col = "chi_outdoor_no_misd", 
       tiebreaker1 = "shootings", tiebreaker2 = "gun_violence"),
  list(name = "CHI_Outdoor_Full", rank_col = "chi_outdoor_full", 
       tiebreaker1 = "shootings", tiebreaker2 = "gun_violence"),
  list(name = "Outdoor_Felony_GV_Boost", rank_col = "felony_outdoor_gv_boost", 
       tiebreaker1 = "shootings", tiebreaker2 = "gun_violence"),
  list(name = "Felony_GV_Boost", rank_col = "felony_gv_boost", 
       tiebreaker1 = "shootings", tiebreaker2 = "gun_violence"),
  list(name = "CHI_No_Misd", rank_col = "chi_no_misd", 
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

eligible <- block_matrix %>% filter(!excluded)

# Top 20 GV blocks
top_20_gv_ids <- eligible %>%
  arrange(desc(gun_violence), desc(shootings)) %>%
  head(20) %>%
  pull(block_id)

# Ensure all_violence columns exist
if (!"all_violence" %in% names(block_matrix)) {
  block_matrix <- block_matrix %>%
    mutate(
      all_violence = felony_violent + misd_assault,
      all_violence_outdoor = felony_violent_outdoor + misd_assault_outdoor
    )
}

#==============================================================================
# CALCULATE PERSISTENCE (if not already done)
#==============================================================================

if (calc_persistence) {
  cat("Calculating persistence from annual columns...\n")
  
  # Check for annual GV and FV columns
  gv_annual_cols <- names(block_matrix)[grepl("^gun_violence_(20[0-9]{2})$", names(block_matrix))]
  fv_annual_cols <- names(block_matrix)[grepl("^felony_violent_(20[0-9]{2})$", names(block_matrix))]
  
  if (length(gv_annual_cols) == 0) {
    cat("  No annual gun_violence columns found. GV persistence will be NA.\n")
    cat("  Run 03c_add_annual_columns.R first for persistence metrics.\n\n")
  } else {
    gv_years <- as.integer(gsub("gun_violence_", "", gv_annual_cols))
    cat("  Found GV annual columns for years:", paste(gv_years, collapse = ", "), "\n")
  }
  
  if (length(fv_annual_cols) == 0) {
    cat("  No annual felony_violent columns found. FV persistence will be NA.\n")
  } else {
    fv_years <- as.integer(gsub("felony_violent_", "", fv_annual_cols))
    cat("  Found FV annual columns for years:", paste(fv_years, collapse = ", "), "\n")
  }
  
  # Calculate persistence for each algorithm
  persistence_results <- map_dfr(algorithms, function(alg) {
    
    if (!alg$rank_col %in% names(block_matrix)) {
      return(tibble(algorithm = alg$name, 
                    persist_gv_5of5 = NA, persist_gv_3of5 = NA,
                    persist_fv_5of5 = NA, persist_fv_3of5 = NA))
    }
    
    # Get top 400 by 5-year (using algorithm's ranking)
    ranked_5yr <- rank_blocks(block_matrix, alg)
    top_400_ids <- ranked_5yr %>% filter(rank <= 400) %>% pull(block_id)
    
    # --- GV PERSISTENCE ---
    if (length(gv_annual_cols) > 0) {
      gv_year_in_top <- map_dfc(gv_years, function(yr) {
        col <- paste0("gun_violence_", yr)
        annual_top_400 <- eligible %>%
          arrange(desc(!!sym(col))) %>%
          head(400) %>%
          pull(block_id)
        tibble(!!as.character(yr) := top_400_ids %in% annual_top_400)
      })
      
      gv_years_in_top <- rowSums(as.matrix(gv_year_in_top), na.rm = TRUE)
      gv_5of5 <- sum(gv_years_in_top == length(gv_years))
      gv_3of5 <- sum(gv_years_in_top >= 3)
    } else {
      gv_5of5 <- NA
      gv_3of5 <- NA
    }
    
    # --- FV PERSISTENCE ---
    if (length(fv_annual_cols) > 0) {
      fv_year_in_top <- map_dfc(fv_years, function(yr) {
        col <- paste0("felony_violent_", yr)
        annual_top_400 <- eligible %>%
          arrange(desc(!!sym(col))) %>%
          head(400) %>%
          pull(block_id)
        tibble(!!as.character(yr) := top_400_ids %in% annual_top_400)
      })
      
      fv_years_in_top <- rowSums(as.matrix(fv_year_in_top), na.rm = TRUE)
      fv_5of5 <- sum(fv_years_in_top == length(fv_years))
      fv_3of5 <- sum(fv_years_in_top >= 3)
    } else {
      fv_5of5 <- NA
      fv_3of5 <- NA
    }
    
    tibble(
      algorithm = alg$name,
      persist_gv_5of5 = gv_5of5,
      persist_gv_3of5 = gv_3of5,
      persist_fv_5of5 = fv_5of5,
      persist_fv_3of5 = fv_3of5
    )
  })
}

#==============================================================================
# BUILD FINAL TABLE - ALL VALUES ANNUALIZED
#==============================================================================

final_table <- map_dfr(algorithms, function(alg) {
  
  if (!alg$rank_col %in% names(block_matrix)) {
    return(NULL)
  }
  
  ranked <- rank_blocks(block_matrix, alg)
  
  map_dfr(TARGET_NS, function(n) {
    top_n <- ranked %>% filter(rank <= n)
    
    # Top 20 GV coverage
    top20_covered <- sum(top_20_gv_ids %in% top_n$block_id)
    
    # 5-year totals
    total_shootings <- sum(top_n$shootings)
    total_gv <- sum(top_n$gun_violence)
    total_fv <- sum(top_n$felony_violent)
    total_fv_outdoor <- sum(top_n$felony_violent_outdoor)
    total_all_violence <- sum(top_n$all_violence)
    total_all_violence_outdoor <- sum(top_n$all_violence_outdoor)
    
    # Get persistence for this algorithm (only for N=400)
    if (n == 400) {
      persist_row <- persistence_results %>% filter(algorithm == alg$name)
      p_gv_5of5 <- if (nrow(persist_row) > 0) persist_row$persist_gv_5of5 else NA
      p_gv_3of5 <- if (nrow(persist_row) > 0) persist_row$persist_gv_3of5 else NA
      p_fv_5of5 <- if (nrow(persist_row) > 0) persist_row$persist_fv_5of5 else NA
      p_fv_3of5 <- if (nrow(persist_row) > 0) persist_row$persist_fv_3of5 else NA
    } else {
      p_gv_5of5 <- NA
      p_gv_3of5 <- NA
      p_fv_5of5 <- NA
      p_fv_3of5 <- NA
    }
    
    tibble(
      algorithm = alg$name,
      N = n,
      
      # Top 20 GV coverage
      top20_gv_covered = top20_covered,
      
      # ANNUALIZED averages (5-year / 5 / N = per block per year)
      avg_shootings_1yr = round(total_shootings / 5 / n, 2),
      avg_gv_1yr = round(total_gv / 5 / n, 2),
      
      # % outdoor
      pct_fv_outdoor = round(total_fv_outdoor / total_fv * 100, 1),
      
      # ANNUALIZED FV and all violence
      avg_fv_1yr = round(total_fv / 5 / n, 2),
      avg_all_violence_1yr = round(total_all_violence / 5 / n, 2),
      
      # ANNUALIZED outdoor
      avg_fv_outdoor_1yr = round(total_fv_outdoor / 5 / n, 2),
      avg_all_violence_outdoor_1yr = round(total_all_violence_outdoor / 5 / n, 2),
      
      # Persistence (N=400 only)
      persist_gv_5of5 = p_gv_5of5,
      persist_gv_3of5 = p_gv_3of5,
      persist_fv_5of5 = p_fv_5of5,
      persist_fv_3of5 = p_fv_3of5
    )
  })
})

#==============================================================================
# PRINT RESULTS
#==============================================================================

cat("\nFINAL TIEBREAKER TABLE:\n")
for (n in TARGET_NS) {
  cat("\n--- N =", n, "---\n\n")
  print(as.data.frame(final_table %>% filter(N == n) %>% select(-N)), row.names = FALSE)
}

#==============================================================================
# EXPORT TO SINGLE CSV
#==============================================================================

export_table <- final_table %>%
  pivot_longer(
    cols = -c(algorithm, N),
    names_to = "metric",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = algorithm,
    values_from = value
  ) %>%
  mutate(
    metric = case_when(
      metric == "top20_gv_covered" ~ "Top 20 GV blocks captured",
      metric == "avg_shootings_1yr" ~ "Avg shootings/block/year",
      metric == "avg_gv_1yr" ~ "Avg GV/block/year",
      metric == "pct_fv_outdoor" ~ "% FV outdoor",
      metric == "avg_fv_1yr" ~ "Avg FV/block/year",
      metric == "avg_all_violence_1yr" ~ "Avg all violence/block/year",
      metric == "avg_fv_outdoor_1yr" ~ "Avg OUTDOOR FV/block/year",
      metric == "avg_all_violence_outdoor_1yr" ~ "Avg OUTDOOR all violence/block/year",
      metric == "persist_gv_5of5" ~ "Persist GV: in top 400 all 5 years",
      metric == "persist_gv_3of5" ~ "Persist GV: in top 400 any 3 of 5 years",
      metric == "persist_fv_5of5" ~ "Persist FV: in top 400 all 5 years",
      metric == "persist_fv_3of5" ~ "Persist FV: in top 400 any 3 of 5 years",
      TRUE ~ metric
    )
  ) %>%
  # Reorder columns
  select(
    N, metric,
    any_of(c("Gun_Violence_Only", "Felony_Violent_Only",
             "CHI_Outdoor_No_Misd", "CHI_Outdoor_Full", "Outdoor_Felony_GV_Boost",
             "Felony_GV_Boost", "CHI_No_Misd"))
  ) %>%
  arrange(N, factor(metric, levels = c(
    "Top 20 GV blocks captured",
    "Avg shootings/block/year",
    "Avg GV/block/year",
    "% FV outdoor",
    "Avg FV/block/year",
    "Avg all violence/block/year",
    "Avg OUTDOOR FV/block/year",
    "Avg OUTDOOR all violence/block/year",
    "Persist GV: in top 400 all 5 years",
    "Persist GV: in top 400 any 3 of 5 years",
    "Persist FV: in top 400 all 5 years",
    "Persist FV: in top 400 any 3 of 5 years"
  )))

#==============================================================================
# SAVE
#==============================================================================

if (!dir.exists("output")) dir.create("output")

write_csv(export_table, "output/algorithm_tiebreaker_final.csv")

cat("\n")
cat(strrep("=", 70), "\n")
cat("Saved: output/algorithm_tiebreaker_final.csv\n")
cat(strrep("=", 70), "\n")

#==============================================================================
# PREVIEW EXPORT TABLE
#==============================================================================

cat("\nEXPORT TABLE PREVIEW:\n\n")
print(as.data.frame(export_table), row.names = FALSE)
