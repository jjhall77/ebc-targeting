#==============================================================================
# 03c_add_annual_columns.R
# Every Block Counts (EBC) Project
# Purpose: Add CALENDAR YEAR breakdown columns to block_matrix for persistence
#
# PREREQUISITE: Run 01_targeting_analysis.R first (needs crime_data sf objects)
#               OR have shootings_sf, shots_fired_sf, violent_crime_sf available
#
# This script re-aggregates crime data by calendar year to add columns:
#   - shootings_2021, shootings_2022, ..., shootings_2025
#   - gun_violence_2021, ..., gun_violence_2025
#   - felony_violent_2021, ..., felony_violent_2025
#
# Then calculates persistence metrics for the tiebreaker analysis
#==============================================================================

library(tidyverse)
library(lubridate)
library(sf)

#==============================================================================
# PARAMETERS
#==============================================================================

CALENDAR_YEARS <- c(2021, 2022, 2023, 2024, 2025)
TARGET_N <- 400

cat("\n")
cat(strrep("=", 70), "\n")
cat("ADDING CALENDAR YEAR COLUMNS TO BLOCK_MATRIX\n")
cat(strrep("=", 70), "\n\n")

#==============================================================================
# CHECK REQUIRED OBJECTS
#==============================================================================

required_objects <- c("block_matrix", "physical_blocks", "intersection_nodes", 
                      "intersection_to_blocks")

missing <- required_objects[!sapply(required_objects, exists)]
if (length(missing) > 0) {
  stop("Missing required objects: ", paste(missing, collapse = ", "), 
       "\nRun 01_targeting_analysis.R or 03_algorithm_comparison.R first!")
}

# Check for crime sf objects (need at least shootings and shots_fired)
crime_objects <- c("shootings_sf", "shots_fired_sf")
missing_crime <- crime_objects[!sapply(crime_objects, exists)]

if (length(missing_crime) > 0) {
  cat("Crime sf objects not found. Attempting to load from crime_data list...\n")
  if (exists("crime_data") && is.list(crime_data)) {
    if ("shootings_sf" %in% names(crime_data)) shootings_sf <- crime_data$shootings_sf
    if ("shots_fired_sf" %in% names(crime_data)) shots_fired_sf <- crime_data$shots_fired_sf
    if ("violent_crime_sf" %in% names(crime_data)) violent_crime_sf <- crime_data$violent_crime_sf
  }
}

# Final check
if (!exists("shootings_sf") || !exists("shots_fired_sf")) {
  stop("Cannot find shootings_sf or shots_fired_sf. Need crime data with dates!")
}

cat("Found required crime data objects.\n\n")

#==============================================================================
# INTERSECTION THRESHOLD (same as main analysis)
#==============================================================================

INTERSECTION_THRESHOLD <- 50  # feet

#==============================================================================
# ALLOCATION FUNCTION (same as 01_targeting_analysis.R)
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
  
  # Find nearest intersection node for each crime
  nearest_node_idx <- st_nearest_feature(crime_sf, intersection_nodes)
  nearest_node_id <- intersection_nodes$nodeid[nearest_node_idx]
  
  # Calculate distance to nearest intersection
  dist_to_intersection <- as.numeric(
    st_distance(crime_sf, intersection_nodes[nearest_node_idx, ], by_element = TRUE)
  )
  
  # Find nearest block for each crime
  nearest_block_idx <- st_nearest_feature(crime_sf, physical_blocks)
  nearest_block_id <- physical_blocks$physical_id[nearest_block_idx]
  
  # Build allocation table
  crime_allocation <- tibble(
    crime_idx = 1:n_crimes,
    nearest_node_id = nearest_node_id,
    dist_to_intersection = dist_to_intersection,
    nearest_block_id = nearest_block_id,
    is_at_intersection = dist_to_intersection <= threshold
  )
  
  # Process intersection crimes (split across adjacent blocks)
  intersection_crimes <- crime_allocation %>%
    filter(is_at_intersection)
  
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
  
  # Process non-intersection crimes
  non_intersection_crimes <- crime_allocation %>%
    filter(!is_at_intersection) %>%
    count(nearest_block_id, name = "crime_count") %>%
    rename(block_id = nearest_block_id)
  
  # Combine
  bind_rows(intersection_split, non_intersection_crimes) %>%
    group_by(block_id) %>%
    summarise(crime_count = sum(crime_count), .groups = "drop")
}

#==============================================================================
# CLEAN UP ANY EXISTING ANNUAL COLUMNS (for idempotency)
#==============================================================================

existing_annual <- names(block_matrix)[grepl("_(2021|2022|2023|2024|2025)$", names(block_matrix))]
if (length(existing_annual) > 0) {
  cat("Removing", length(existing_annual), "existing annual columns for clean recalculation...\n")
  block_matrix <- block_matrix %>%
    select(-all_of(existing_annual))
}

#==============================================================================
# ADD CALENDAR YEAR COLUMNS
#==============================================================================

cat("Processing crime data by calendar year...\n\n")

# Ensure date column exists in shootings_sf
if (!"date" %in% names(shootings_sf)) {
  if ("occur_date" %in% names(shootings_sf)) {
    shootings_sf <- shootings_sf %>% mutate(date = mdy(occur_date))
  }
}

# Ensure date column exists in shots_fired_sf
if (!"date" %in% names(shots_fired_sf)) {
  if ("rpt_dt" %in% names(shots_fired_sf)) {
    shots_fired_sf <- shots_fired_sf %>% mutate(date = mdy(rpt_dt))
  } else if ("rec_create_dt" %in% names(shots_fired_sf)) {
    shots_fired_sf <- shots_fired_sf %>% mutate(date = mdy(rec_create_dt))
  }
}

# Process each calendar year using match() for robust column assignment
for (yr in CALENDAR_YEARS) {
  
  cat("Processing year", yr, "...\n")
  
  start_date <- as.Date(paste0(yr, "-01-01"))
  end_date <- as.Date(paste0(yr, "-12-31"))
  
  shootings_yr <- shootings_sf %>% filter(date >= start_date & date <= end_date)
  shots_fired_yr <- shots_fired_sf %>% filter(date >= start_date & date <= end_date)
  
  cat("  Shootings:", nrow(shootings_yr), "| Shots fired:", nrow(shots_fired_yr), "\n")
  
  # Column names
  shootings_col <- paste0("shootings_", yr)
  shots_col <- paste0("shots_fired_", yr)
  gv_col <- paste0("gun_violence_", yr)
  
  # Initialize with zeros
  block_matrix[[shootings_col]] <- 0
  block_matrix[[shots_col]] <- 0
  
  # Allocate shootings via match()
  if (nrow(shootings_yr) > 0) {
    shooting_counts <- allocate_crimes_to_blocks(
      shootings_yr, physical_blocks, intersection_nodes, intersection_to_blocks
    )
    idx <- match(shooting_counts$block_id, block_matrix$block_id)
    valid <- !is.na(idx)
    block_matrix[[shootings_col]][idx[valid]] <- shooting_counts$crime_count[valid]
  }
  
  # Allocate shots fired via match()
  if (nrow(shots_fired_yr) > 0) {
    shots_counts <- allocate_crimes_to_blocks(
      shots_fired_yr, physical_blocks, intersection_nodes, intersection_to_blocks
    )
    idx <- match(shots_counts$block_id, block_matrix$block_id)
    valid <- !is.na(idx)
    block_matrix[[shots_col]][idx[valid]] <- shots_counts$crime_count[valid]
  }
  
  # Create gun_violence
  block_matrix[[gv_col]] <- block_matrix[[shootings_col]] + block_matrix[[shots_col]]
}

# Also process felony violent if available
if (exists("violent_crime_sf")) {
  cat("\nProcessing felony violent by calendar year...\n")
  
  # Ensure date column exists
  if (!"date" %in% names(violent_crime_sf)) {
    if ("rpt_dt" %in% names(violent_crime_sf)) {
      violent_crime_sf <- violent_crime_sf %>% mutate(date = mdy(rpt_dt))
    }
  }
  
  # Define felony violent KY codes
  felony_violent_ky <- c(101, 105, 106)  # Murder, Robbery, Felony Assault (excluding misd 344)
  
  for (yr in CALENDAR_YEARS) {
    start_date <- as.Date(paste0(yr, "-01-01"))
    end_date <- as.Date(paste0(yr, "-12-31"))
    
    # Filter for felony violent only
    fv_yr <- violent_crime_sf %>%
      filter(date >= start_date & date <= end_date) %>%
      filter(ky_cd %in% felony_violent_ky)
    
    cat("  Year", yr, "- Felony violent:", nrow(fv_yr), "\n")
    
    fv_col <- paste0("felony_violent_", yr)
    block_matrix[[fv_col]] <- 0
    
    if (nrow(fv_yr) > 0) {
      fv_counts <- allocate_crimes_to_blocks(
        fv_yr, physical_blocks, intersection_nodes, intersection_to_blocks
      )
      idx <- match(fv_counts$block_id, block_matrix$block_id)
      valid <- !is.na(idx)
      block_matrix[[fv_col]][idx[valid]] <- fv_counts$crime_count[valid]
    }
  }
}

cat("\nCalendar year columns added to block_matrix.\n")

# Verify columns exist and have data
cat("\nColumn sums:\n")
for (yr in CALENDAR_YEARS) {
  gv_sum <- sum(block_matrix[[paste0("gun_violence_", yr)]], na.rm = TRUE)
  cat("  ", yr, "- GV:", round(gv_sum), "\n")
}

#==============================================================================
# CALCULATE PERSISTENCE
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("CALCULATING PERSISTENCE METRICS (N=400)\n")
cat(strrep("=", 70), "\n\n")

# Ranking function
rank_blocks <- function(df, rank_col, tb1, tb2) {
  df %>%
    filter(!excluded) %>%
    arrange(desc(!!sym(rank_col)), desc(!!sym(tb1)), desc(!!sym(tb2))) %>%
    mutate(rank = row_number())
}

eligible <- block_matrix %>% filter(!excluded)

# Algorithms to compare
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

# Get annual column lists
gv_annual_cols <- names(block_matrix)[grepl("^gun_violence_(20[0-9]{2})$", names(block_matrix))]
fv_annual_cols <- names(block_matrix)[grepl("^felony_violent_(20[0-9]{2})$", names(block_matrix))]

gv_years <- as.integer(gsub("gun_violence_", "", gv_annual_cols))
fv_years <- as.integer(gsub("felony_violent_", "", fv_annual_cols))

cat("GV annual columns found for years:", paste(gv_years, collapse = ", "), "\n")
cat("FV annual columns found for years:", paste(fv_years, collapse = ", "), "\n\n")

# Calculate persistence for each algorithm
persistence_results <- map_dfr(algorithms, function(alg) {
  
  # Skip if rank column doesn't exist
  if (!alg$rank_col %in% names(block_matrix)) {
    cat("  Skipping", alg$name, "- column not found\n")
    return(NULL)
  }
  
  # Get top 400 by 5-year total
  ranked_5yr <- rank_blocks(block_matrix, alg$rank_col, alg$tiebreaker1, alg$tiebreaker2)
  top_400_ids <- ranked_5yr %>% filter(rank <= TARGET_N) %>% pull(block_id)
  
  # --- GV PERSISTENCE ---
  if (length(gv_years) > 0) {
    gv_year_in_top <- map_dfc(gv_years, function(yr) {
      gv_col <- paste0("gun_violence_", yr)
      
      # Rank by this year's GV
      top_400_annual <- eligible %>%
        arrange(desc(!!sym(gv_col))) %>%
        head(TARGET_N) %>%
        pull(block_id)
      
      tibble(!!as.character(yr) := top_400_ids %in% top_400_annual)
    })
    
    gv_years_in_top <- rowSums(as.matrix(gv_year_in_top), na.rm = TRUE)
  } else {
    gv_years_in_top <- rep(NA, length(top_400_ids))
  }
  
  # --- FV PERSISTENCE ---
  if (length(fv_years) > 0) {
    fv_year_in_top <- map_dfc(fv_years, function(yr) {
      fv_col <- paste0("felony_violent_", yr)
      
      # Rank by this year's FV
      top_400_annual <- eligible %>%
        arrange(desc(!!sym(fv_col))) %>%
        head(TARGET_N) %>%
        pull(block_id)
      
      tibble(!!paste0("fv_", yr) := top_400_ids %in% top_400_annual)
    })
    
    fv_years_in_top <- rowSums(as.matrix(fv_year_in_top), na.rm = TRUE)
  } else {
    fv_years_in_top <- rep(NA, length(top_400_ids))
  }
  
  tibble(
    algorithm = alg$name,
    top_n = TARGET_N,
    # GV persistence
    persist_gv_5of5 = sum(gv_years_in_top == length(gv_years), na.rm = TRUE),
    persist_gv_4of5 = sum(gv_years_in_top >= 4, na.rm = TRUE),
    persist_gv_3of5 = sum(gv_years_in_top >= 3, na.rm = TRUE),
    # FV persistence
    persist_fv_5of5 = sum(fv_years_in_top == length(fv_years), na.rm = TRUE),
    persist_fv_4of5 = sum(fv_years_in_top >= 4, na.rm = TRUE),
    persist_fv_3of5 = sum(fv_years_in_top >= 3, na.rm = TRUE)
  )
})

#==============================================================================
# PRINT RESULTS
#==============================================================================

cat("PERSISTENCE RESULTS (N=400):\n")
cat("Of the top 400 blocks by 5-year total, how many were in top 400 annually?\n\n")

print(as.data.frame(persistence_results), row.names = FALSE)

cat("\n")
cat("INTERPRETATION:\n")
cat("  persist_gv_5of5: In top 400 by annual GV for ALL 5 years\n")
cat("  persist_gv_3of5: In top 400 by annual GV for at least 3 of 5 years\n")
cat("  persist_fv_5of5: In top 400 by annual FV for ALL 5 years\n")
cat("  persist_fv_3of5: In top 400 by annual FV for at least 3 of 5 years\n\n")

#==============================================================================
# SAVE RESULTS
#==============================================================================

if (!dir.exists("output")) dir.create("output")

write_csv(persistence_results, "output/persistence_by_calendar_year.csv")
cat("Saved: output/persistence_by_calendar_year.csv\n")

cat("\n")
cat(strrep("=", 70), "\n")
cat("ANNUAL COLUMNS AND PERSISTENCE ANALYSIS COMPLETE\n")
cat(strrep("=", 70), "\n")