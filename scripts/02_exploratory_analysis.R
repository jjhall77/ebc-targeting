#==============================================================================
# 02_exploratory_analysis.R
# Every Block Counts (EBC) Project
# Purpose: Step-by-step exploratory analysis for targeting algorithm comparison
#
# USAGE: Run line-by-line or section-by-section in RStudio
# PREREQUISITE: source("scripts/00_load_data.R") first
#==============================================================================

library(tidyverse)
library(sf)
library(ggplot2)
library(scales)
library(patchwork)

#==============================================================================
# PARAMETERS
#==============================================================================

# Target sample sizes
TARGET_N <- c(200, 300, 400)

# Baseline period (5 years ending 9/30/2025)
START_DATE <- as.Date("2020-10-01")
END_DATE   <- as.Date("2025-09-30")

# Intersection threshold (feet)
INTERSECTION_THRESHOLD <- 50

#==============================================================================
# SECTION 1: VERIFY DATA IS LOADED
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("VERIFYING DATA OBJECTS\n")
cat(strrep("=", 70), "\n\n")

required_objects <- c(
  "physical_blocks", "intersection_nodes", "intersection_to_blocks",
  "nypp", "shootings_sf", "shots_fired", "violent_crime", 
  "violent_street_crime", "hospitals_sf", "nypd_precinct_locations_sf"
)

missing <- setdiff(required_objects, ls())
if (length(missing) > 0) {
  stop("Missing objects: ", paste(missing, collapse = ", "), 
       "\nRun source('scripts/00_load_data.R') first!")
} else {
  cat("All required objects present. Proceeding.\n\n
")
}

#==============================================================================
# SECTION 2: IDENTIFY EXCLUSION BLOCKS
#==============================================================================

cat(strrep("=", 70), "\n")
cat("IDENTIFYING BLOCKS TO EXCLUDE\n")
cat(strrep("=", 70), "\n\n")

# Blocks with hospitals
hospital_block_idx <- st_nearest_feature(hospitals_sf, physical_blocks)
hospital_block_ids <- physical_blocks$physical_id[hospital_block_idx]

cat("Blocks with hospitals:", length(unique(hospital_block_ids)), "\n")

# Blocks with precinct station houses
precinct_block_idx <- st_nearest_feature(nypd_precinct_locations_sf, physical_blocks)
precinct_block_ids <- physical_blocks$physical_id[precinct_block_idx]

cat("Blocks with precincts:", length(unique(precinct_block_ids)), "\n")

# Combined exclusion list
exclude_block_ids <- unique(c(hospital_block_ids, precinct_block_ids))
cat("Total unique blocks excluded:", length(exclude_block_ids), "\n\n")

#==============================================================================
# SECTION 3: FILTER CRIME DATA TO BASELINE PERIOD
#==============================================================================

cat(strrep("=", 70), "\n")
cat("FILTERING TO BASELINE PERIOD\n")
cat(strrep("=", 70), "\n\n")

cat("Baseline period:", as.character(START_DATE), "to", as.character(END_DATE), "\n\n")

# Filter each crime dataset
shootings_baseline <- shootings_sf %>%
  filter(date >= START_DATE & date <= END_DATE)
cat("Shootings in baseline:", format(nrow(shootings_baseline), big.mark = ","), "\n")

shots_fired_baseline <- shots_fired %>%
  filter(date >= START_DATE & date <= END_DATE)
cat("Shots fired in baseline:", format(nrow(shots_fired_baseline), big.mark = ","), "\n")

violent_crime_baseline <- violent_crime %>%
  filter(date >= START_DATE & date <= END_DATE)
cat("Violent crime in baseline:", format(nrow(violent_crime_baseline), big.mark = ","), "\n")

violent_street_baseline <- violent_street_crime %>%
  filter(date >= START_DATE & date <= END_DATE)
cat("Violent street crime in baseline:", format(nrow(violent_street_baseline), big.mark = ","), "\n\n")

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
  
  # Find nearest intersection for each crime
  nearest_node_idx <- st_nearest_feature(crime_sf, intersection_nodes)
  nearest_node_id <- intersection_nodes$nodeid[nearest_node_idx]
  
  # Distance to nearest intersection
  dist_to_intersection <- as.numeric(
    st_distance(crime_sf, intersection_nodes[nearest_node_idx, ], by_element = TRUE)
  )
  
  # Find nearest block (fallback)
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
  
  # Process intersection crimes
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
# SECTION 5: ALLOCATE ALL CRIME TYPES TO BLOCKS
#==============================================================================

cat(strrep("=", 70), "\n")
cat("ALLOCATING CRIMES TO BLOCKS\n")
cat(strrep("=", 70), "\n\n")

cat("Processing shootings...\n")
shooting_counts <- allocate_crimes_to_blocks(
  shootings_baseline, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(shootings = crime_count)

cat("Processing shots fired...\n")
shots_counts <- allocate_crimes_to_blocks(
  shots_fired_baseline, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(shots_fired = crime_count)

cat("Processing violent crime...\n")
violent_counts <- allocate_crimes_to_blocks(
  violent_crime_baseline, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(violent_crime = crime_count)

cat("Processing violent street crime...\n")
street_counts <- allocate_crimes_to_blocks(
  violent_street_baseline, physical_blocks, intersection_nodes, intersection_to_blocks
) %>% rename(violent_street_crime = crime_count)

cat("\nAllocation complete.\n\n")

#==============================================================================
# SECTION 6: BUILD BLOCK MATRIX
#==============================================================================

cat(strrep("=", 70), "\n")
cat("BUILDING BLOCK MATRIX\n")
cat(strrep("=", 70), "\n\n")

# Start with physical blocks
block_matrix <- physical_blocks %>%
  st_drop_geometry() %>%
  select(physical_id, total_length_ft, streets, boro) %>%
  rename(block_id = physical_id)

# Add exclusion flag
block_matrix <- block_matrix %>%
  mutate(excluded = block_id %in% exclude_block_ids)

# Join crime counts
block_matrix <- block_matrix %>%
  left_join(shooting_counts, by = "block_id") %>%
  left_join(shots_counts, by = "block_id") %>%
  left_join(violent_counts, by = "block_id") %>%
  left_join(street_counts, by = "block_id")

# Replace NAs with 0
block_matrix <- block_matrix %>%
  mutate(across(c(shootings, shots_fired, violent_crime, violent_street_crime),
                ~replace_na(., 0)))

# Create composite
block_matrix <- block_matrix %>%
  mutate(gun_violence_total = shootings + shots_fired)

# Add precinct via spatial join
block_centroids <- physical_blocks %>%
  st_centroid() %>%
  select(physical_id)

block_precincts <- st_join(block_centroids, nypp, join = st_within) %>%
  st_drop_geometry() %>%
  select(physical_id, precinct)

block_matrix <- block_matrix %>%
  left_join(block_precincts, by = c("block_id" = "physical_id"))

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

cat("Block matrix built.\n")
cat("Total blocks:", format(nrow(block_matrix), big.mark = ","), "\n")
cat("Excluded blocks:", sum(block_matrix$excluded), "\n")
cat("Eligible blocks:", sum(!block_matrix$excluded), "\n\n")

#==============================================================================
# SECTION 7: SUMMARY STATISTICS
#==============================================================================

cat(strrep("=", 70), "\n")
cat("SUMMARY STATISTICS\n")
cat(strrep("=", 70), "\n\n")

eligible <- block_matrix %>% filter(!excluded)

cat("CITYWIDE TOTALS (5-year baseline, eligible blocks only):\n")
cat(strrep("-", 50), "\n")
cat("  Shootings:            ", format(sum(eligible$shootings), big.mark = ","), "\n")
cat("  Shots fired:          ", format(sum(eligible$shots_fired), big.mark = ","), "\n")
cat("  Gun violence total:   ", format(sum(eligible$gun_violence_total), big.mark = ","), "\n")
cat("  Violent crime:        ", format(sum(eligible$violent_crime), big.mark = ","), "\n")
cat("  Violent street crime: ", format(sum(eligible$violent_street_crime), big.mark = ","), "\n\n")

cat("BLOCK DISTRIBUTION:\n")
cat(strrep("-", 50), "\n")
cat("  Blocks with >= 1 shooting:      ", format(sum(eligible$shootings >= 1), big.mark = ","),
    " (", round(sum(eligible$shootings >= 1)/nrow(eligible)*100, 2), "%)\n", sep = "")
cat("  Blocks with >= 2 shootings:     ", format(sum(eligible$shootings >= 2), big.mark = ","),
    " (", round(sum(eligible$shootings >= 2)/nrow(eligible)*100, 2), "%)\n", sep = "")
cat("  Blocks with >= 3 shootings:     ", format(sum(eligible$shootings >= 3), big.mark = ","),
    " (", round(sum(eligible$shootings >= 3)/nrow(eligible)*100, 2), "%)\n", sep = "")
cat("  Blocks with >= 5 gun violence:  ", format(sum(eligible$gun_violence_total >= 5), big.mark = ","),
    " (", round(sum(eligible$gun_violence_total >= 5)/nrow(eligible)*100, 2), "%)\n", sep = "")
cat("  Blocks with >= 10 violent crime:", format(sum(eligible$violent_crime >= 10), big.mark = ","),
    " (", round(sum(eligible$violent_crime >= 10)/nrow(eligible)*100, 2), "%)\n\n", sep = "")

#==============================================================================
# SECTION 8: CREATE RANKED LISTS FOR EACH ALGORITHM
#==============================================================================

cat(strrep("=", 70), "\n")
cat("RANKING BLOCKS BY ALGORITHM\n")
cat(strrep("=", 70), "\n\n")

# Algorithm 1: Gun Violence Total
ranked_gun_violence <- eligible %>%
  arrange(desc(gun_violence_total), desc(shootings), desc(shots_fired)) %>%
  mutate(rank = row_number())

# Algorithm 2: Shootings Only
ranked_shootings <- eligible %>%
  arrange(desc(shootings), desc(shots_fired), desc(violent_crime)) %>%
  mutate(rank = row_number())

# Algorithm 3: Violent Crime
ranked_violent_crime <- eligible %>%
  arrange(desc(violent_crime), desc(shootings), desc(shots_fired)) %>%
  mutate(rank = row_number())

# Algorithm 4: Violent Street Crime
ranked_violent_street <- eligible %>%
  arrange(desc(violent_street_crime), desc(shootings), desc(shots_fired)) %>%
  mutate(rank = row_number())

cat("Rankings complete for all 4 algorithms.\n\n")

#==============================================================================
# SECTION 9: CUTOFF THRESHOLDS TABLE
#==============================================================================

cat(strrep("=", 70), "\n")
cat("CUTOFF THRESHOLDS BY SAMPLE SIZE\n")
cat(strrep("=", 70), "\n\n")

cutoff_table <- map_dfr(TARGET_N, function(n) {
  tibble(
    N = n,
    
    # Gun Violence
    GV_min = min(ranked_gun_violence$gun_violence_total[1:n]),
    GV_median = median(ranked_gun_violence$gun_violence_total[1:n]),
    GV_max = max(ranked_gun_violence$gun_violence_total[1:n]),
    
    # Shootings
    SH_min = min(ranked_shootings$shootings[1:n]),
    SH_median = median(ranked_shootings$shootings[1:n]),
    SH_max = max(ranked_shootings$shootings[1:n]),
    
    # Violent Crime
    VC_min = min(ranked_violent_crime$violent_crime[1:n]),
    VC_median = median(ranked_violent_crime$violent_crime[1:n]),
    VC_max = max(ranked_violent_crime$violent_crime[1:n]),
    
    # Violent Street
    VS_min = min(ranked_violent_street$violent_street_crime[1:n]),
    VS_median = median(ranked_violent_street$violent_street_crime[1:n]),
    VS_max = max(ranked_violent_street$violent_street_crime[1:n])
  )
})

print(cutoff_table, width = Inf)
cat("\n")

#==============================================================================
# SECTION 10: CITYWIDE CAPTURE RATES
#==============================================================================

cat(strrep("=", 70), "\n")
cat("CITYWIDE CAPTURE RATES\n")
cat(strrep("=", 70), "\n\n")

totals <- list(
  gun_violence = sum(eligible$gun_violence_total),
  shootings = sum(eligible$shootings),
  violent_crime = sum(eligible$violent_crime),
  violent_street = sum(eligible$violent_street_crime)
)

capture_table <- map_dfr(TARGET_N, function(n) {
  gv <- ranked_gun_violence[1:n, ]
  sh <- ranked_shootings[1:n, ]
  vc <- ranked_violent_crime[1:n, ]
  vs <- ranked_violent_street[1:n, ]
  
  tibble(
    N = n,
    
    # Gun Violence algorithm captures:
    `GV_alg→GV` = round(sum(gv$gun_violence_total) / totals$gun_violence * 100, 1),
    `GV_alg→SH` = round(sum(gv$shootings) / totals$shootings * 100, 1),
    `GV_alg→VC` = round(sum(gv$violent_crime) / totals$violent_crime * 100, 1),
    
    # Shootings algorithm captures:
    `SH_alg→GV` = round(sum(sh$gun_violence_total) / totals$gun_violence * 100, 1),
    `SH_alg→SH` = round(sum(sh$shootings) / totals$shootings * 100, 1),
    `SH_alg→VC` = round(sum(sh$violent_crime) / totals$violent_crime * 100, 1),
    
    # Violent Crime algorithm captures:
    `VC_alg→GV` = round(sum(vc$gun_violence_total) / totals$gun_violence * 100, 1),
    `VC_alg→SH` = round(sum(vc$shootings) / totals$shootings * 100, 1),
    `VC_alg→VC` = round(sum(vc$violent_crime) / totals$violent_crime * 100, 1),
    
    # Violent Street algorithm captures:
    `VS_alg→GV` = round(sum(vs$gun_violence_total) / totals$gun_violence * 100, 1),
    `VS_alg→SH` = round(sum(vs$shootings) / totals$shootings * 100, 1),
    `VS_alg→VS` = round(sum(vs$violent_street_crime) / totals$violent_street * 100, 1)
  )
})

print(capture_table, width = Inf)
cat("\n")

#==============================================================================
# SECTION 11: ALGORITHM OVERLAP
#==============================================================================

cat(strrep("=", 70), "\n")
cat("ALGORITHM OVERLAP\n")
cat(strrep("=", 70), "\n\n")

overlap_table <- map_dfr(TARGET_N, function(n) {
  gv_ids <- ranked_gun_violence$block_id[1:n]
  sh_ids <- ranked_shootings$block_id[1:n]
  vc_ids <- ranked_violent_crime$block_id[1:n]
  vs_ids <- ranked_violent_street$block_id[1:n]
  
  tibble(
    N = n,
    `GV∩SH` = length(intersect(gv_ids, sh_ids)),
    `GV∩VC` = length(intersect(gv_ids, vc_ids)),
    `GV∩VS` = length(intersect(gv_ids, vs_ids)),
    `SH∩VC` = length(intersect(sh_ids, vc_ids)),
    `SH∩VS` = length(intersect(sh_ids, vs_ids)),
    `VC∩VS` = length(intersect(vc_ids, vs_ids)),
    `All 4` = length(Reduce(intersect, list(gv_ids, sh_ids, vc_ids, vs_ids))),
    `In ≥3` = sum(table(c(gv_ids, sh_ids, vc_ids, vs_ids)) >= 3),
    `Union` = length(unique(c(gv_ids, sh_ids, vc_ids, vs_ids)))
  )
})

print(overlap_table)
cat("\n")

#==============================================================================
# SECTION 12: BLOCKS BY BOROUGH
#==============================================================================

cat(strrep("=", 70), "\n")
cat("PROSPECTIVE BLOCKS BY BOROUGH\n")
cat(strrep("=", 70), "\n\n")

borough_table <- map_dfr(TARGET_N, function(n) {
  gv <- ranked_gun_violence[1:n, ] %>% count(borough, name = "GV")
  sh <- ranked_shootings[1:n, ] %>% count(borough, name = "SH")
  vc <- ranked_violent_crime[1:n, ] %>% count(borough, name = "VC")
  vs <- ranked_violent_street[1:n, ] %>% count(borough, name = "VS")
  
  gv %>%
    full_join(sh, by = "borough") %>%
    full_join(vc, by = "borough") %>%
    full_join(vs, by = "borough") %>%
    mutate(N = n) %>%
    replace_na(list(GV = 0, SH = 0, VC = 0, VS = 0)) %>%
    select(N, borough, GV, SH, VC, VS)
})

# Print by N
for (n in TARGET_N) {
  cat("N =", n, "\n")
  cat(strrep("-", 40), "\n")
  borough_table %>%
    filter(N == n) %>%
    select(-N) %>%
    print()
  cat("\n")
}

#==============================================================================
# SECTION 13: TOP 15 PRECINCTS BY ALGORITHM
#==============================================================================

cat(strrep("=", 70), "\n")
cat("TOP 15 PRECINCTS BY ALGORITHM\n")
cat(strrep("=", 70), "\n\n")

# Function to get top precincts
get_top_precincts <- function(ranked_data, algorithm_name) {
  map_dfr(TARGET_N, function(n) {
    ranked_data[1:n, ] %>%
      count(precinct, name = "n_blocks") %>%
      arrange(desc(n_blocks)) %>%
      mutate(
        algorithm = algorithm_name,
        N = n,
        rank = row_number()
      )
  })
}

top_precincts_gv <- get_top_precincts(ranked_gun_violence, "Gun Violence")
top_precincts_sh <- get_top_precincts(ranked_shootings, "Shootings")
top_precincts_vc <- get_top_precincts(ranked_violent_crime, "Violent Crime")
top_precincts_vs <- get_top_precincts(ranked_violent_street, "Violent Street")

# Combine all
top_precincts_all <- bind_rows(
  top_precincts_gv, top_precincts_sh, top_precincts_vc, top_precincts_vs
)

# Print top 15 for each algorithm and N
for (alg in c("Gun Violence", "Shootings", "Violent Crime", "Violent Street")) {
  cat("\n", alg, " Algorithm\n", sep = "")
  cat(strrep("-", 50), "\n")
  
  top_precincts_all %>%
    filter(algorithm == alg, rank <= 15) %>%
    select(-algorithm) %>%
    pivot_wider(names_from = N, values_from = n_blocks, names_prefix = "N=") %>%
    select(rank, precinct, `N=200`, `N=300`, `N=400`) %>%
    print(n = 150)
}

#==============================================================================
# SECTION 14: PRECINCT-LEVEL CONCENTRATIONS
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("PRECINCT-LEVEL CRIME CONCENTRATIONS\n")
cat(strrep("=", 70), "\n\n")

# Aggregate crime by precinct
precinct_totals <- eligible %>%
  group_by(precinct) %>%
  summarise(
    n_blocks = n(),
    shootings = sum(shootings),
    shots_fired = sum(shots_fired),
    gun_violence = sum(gun_violence_total),
    violent_crime = sum(violent_crime),
    violent_street = sum(violent_street_crime),
    .groups = "drop"
  ) %>%
  filter(!is.na(precinct)) %>%
  arrange(desc(gun_violence))

cat("Top 15 Precincts by Gun Violence (raw counts):\n")
cat(strrep("-", 50), "\n")
precinct_totals %>%
  slice_head(n = 15) %>%
  select(precinct, n_blocks, shootings, gun_violence, violent_crime) %>%
  print()

cat("\n\nPrecinct concentration (what % of citywide crime in top X precincts):\n")
cat(strrep("-", 50), "\n")

for (top_n_pct in c(5, 10, 15, 20)) {
  top_pcts <- precinct_totals %>% slice_head(n = top_n_pct)
  
  cat("Top", top_n_pct, "precincts contain:\n")
  cat("  ", round(sum(top_pcts$shootings) / sum(precinct_totals$shootings) * 100, 1), 
      "% of shootings\n", sep = "")
  cat("  ", round(sum(top_pcts$gun_violence) / sum(precinct_totals$gun_violence) * 100, 1), 
      "% of gun violence\n", sep = "")
  cat("  ", round(sum(top_pcts$violent_crime) / sum(precinct_totals$violent_crime) * 100, 1), 
      "% of violent crime\n", sep = "")
  cat("\n")
}

#==============================================================================
# SECTION 15: PLOTS
#==============================================================================

cat(strrep("=", 70), "\n")
cat("GENERATING PLOTS\n")
cat(strrep("=", 70), "\n\n")

# Set theme
theme_set(theme_minimal(base_size = 12))

#------------------------------------------------------------------------------
# PLOT 1: Concentration Curves
#------------------------------------------------------------------------------

cat("Creating concentration curves...\n")

# Build concentration data
build_concentration <- function(data, crime_col, crime_name) {
  data %>%
    arrange(desc(!!sym(crime_col))) %>%
    mutate(
      cum_blocks = row_number(),
      cum_crime = cumsum(!!sym(crime_col)),
      cum_crime_pct = cum_crime / sum(!!sym(crime_col)) * 100
    ) %>%
    select(cum_blocks, cum_crime_pct) %>%
    mutate(crime_type = crime_name)
}

concentration_data <- bind_rows(
  build_concentration(eligible, "gun_violence_total", "Gun Violence"),
  build_concentration(eligible, "shootings", "Shootings"),
  build_concentration(eligible, "violent_crime", "Violent Crime"),
  build_concentration(eligible, "violent_street_crime", "Violent Street")
)

p_concentration <- ggplot(concentration_data, 
                          aes(x = cum_blocks, y = cum_crime_pct, color = crime_type)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = TARGET_N, linetype = "dashed", alpha = 0.5, color = "gray40") +
  geom_hline(yintercept = c(25, 50, 75), linetype = "dotted", alpha = 0.3) +
  scale_x_continuous(
    breaks = c(TARGET_N, 500, 1000, 2000, 5000),
    labels = comma,
    limits = c(0, 3000)
  ) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Crime Concentration by Block Ranking",
    subtitle = paste0("Baseline: ", START_DATE, " to ", END_DATE,
                      " | Dashed lines = N = 200, 300, 400"),
    x = "Number of Blocks (ranked by crime count)",
    y = "Cumulative % of Citywide Crime",
    color = "Crime Type"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p_concentration)

#------------------------------------------------------------------------------
# PLOT 2: Borough Distribution (Faceted Bar Chart)
#------------------------------------------------------------------------------

cat("Creating borough distribution plot...\n")

borough_plot_data <- borough_table %>%
  pivot_longer(cols = c(GV, SH, VC, VS), names_to = "Algorithm", values_to = "n_blocks") %>%
  mutate(
    Algorithm = case_when(
      Algorithm == "GV" ~ "Gun Violence",
      Algorithm == "SH" ~ "Shootings",
      Algorithm == "VC" ~ "Violent Crime",
      Algorithm == "VS" ~ "Violent Street"
    ),
    N = factor(paste0("N = ", N), levels = c("N = 200", "N = 300", "N = 400"))
  ) %>%
  filter(!is.na(borough))

p_borough <- ggplot(borough_plot_data, 
                    aes(x = borough, y = n_blocks, fill = Algorithm)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~N, ncol = 3) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Prospective Blocks by Borough and Algorithm",
    subtitle = "Comparison across target sample sizes",
    x = "",
    y = "Number of Blocks",
    fill = "Algorithm"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

print(p_borough)

#------------------------------------------------------------------------------
# PLOT 3: Top 15 Precincts Heatmap
#------------------------------------------------------------------------------

cat("Creating precinct heatmap...\n")

# Get precincts that appear in top 15 for any algorithm at N=300
top_pcts_300 <- top_precincts_all %>%
  filter(N == 300, rank <= 15) %>%
  pull(precinct) %>%
  unique()

precinct_heatmap_data <- top_precincts_all %>%
  filter(N == 300, precinct %in% top_pcts_300) %>%
  mutate(
    precinct = factor(precinct),
    algorithm = factor(algorithm, levels = c("Gun Violence", "Shootings", 
                                             "Violent Crime", "Violent Street"))
  )

p_precinct_heatmap <- ggplot(precinct_heatmap_data, 
                             aes(x = algorithm, y = reorder(precinct, n_blocks), 
                                 fill = n_blocks)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = n_blocks), color = "white", size = 3) +
  scale_fill_gradient(low = "steelblue", high = "darkred") +
  labs(
    title = "Block Count by Precinct and Algorithm (N = 300)",
    subtitle = "Precincts appearing in top 15 for any algorithm",
    x = "",
    y = "Precinct",
    fill = "# Blocks"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

print(p_precinct_heatmap)

#------------------------------------------------------------------------------
# PLOT 4: Overlap Visualization
#------------------------------------------------------------------------------

cat("Creating overlap visualization...\n")

overlap_plot_data <- overlap_table %>%
  select(N, `GV∩SH`, `GV∩VC`, `GV∩VS`, `SH∩VC`, `SH∩VS`, `VC∩VS`) %>%
  pivot_longer(-N, names_to = "Pair", values_to = "Overlap") %>%
  mutate(
    N = factor(paste0("N = ", N)),
    Overlap_pct = Overlap / as.numeric(gsub("N = ", "", N)) * 100
  )

p_overlap <- ggplot(overlap_plot_data, 
                    aes(x = Pair, y = Overlap, fill = N)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = Overlap), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Pairwise Algorithm Overlap",
    subtitle = "Number of blocks appearing in both algorithms' top N",
    x = "Algorithm Pair",
    y = "Number of Overlapping Blocks",
    fill = "Sample Size"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  coord_cartesian(ylim = c(0, max(overlap_plot_data$Overlap) * 1.15))

print(p_overlap)

#------------------------------------------------------------------------------
# PLOT 5: Algorithm Comparison - Stacked Bar
#------------------------------------------------------------------------------

cat("Creating algorithm comparison plot...\n")

# For each N, show how much each algorithm's blocks overlap with others
comparison_data <- map_dfr(TARGET_N, function(n) {
  gv_ids <- ranked_gun_violence$block_id[1:n]
  sh_ids <- ranked_shootings$block_id[1:n]
  vc_ids <- ranked_violent_crime$block_id[1:n]
  vs_ids <- ranked_violent_street$block_id[1:n]
  
  all_ids <- c(gv_ids, sh_ids, vc_ids, vs_ids)
  id_counts <- table(all_ids)
  
  tibble(
    N = n,
    `In all 4` = sum(id_counts == 4),
    `In 3` = sum(id_counts == 3),
    `In 2` = sum(id_counts == 2),
    `In 1 only` = sum(id_counts == 1)
  )
}) %>%
  pivot_longer(-N, names_to = "Category", values_to = "Count") %>%
  mutate(
    N = factor(paste0("N = ", N)),
    Category = factor(Category, levels = c("In 1 only", "In 2", "In 3", "In all 4"))
  )

p_comparison <- ggplot(comparison_data, 
                       aes(x = N, y = Count, fill = Category)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5), 
            color = "white", size = 3.5) +
  scale_fill_manual(values = c("In 1 only" = "#fee0d2", "In 2" = "#fc9272", 
                               "In 3" = "#de2d26", "In all 4" = "#67000d")) +
  labs(
    title = "Block Overlap Across Algorithms",
    subtitle = "How many algorithms select each block?",
    x = "Target Sample Size",
    y = "Number of Unique Blocks",
    fill = ""
  ) +
  theme(legend.position = "bottom")

print(p_comparison)

#------------------------------------------------------------------------------
# PLOT 6: Precinct Distribution by N (Top 20 Precincts)
#------------------------------------------------------------------------------

cat("Creating precinct distribution by N plot...\n")

# Get top 20 precincts by gun violence count
top_20_pcts <- precinct_totals %>%
  slice_head(n = 20) %>%
  pull(precinct)

precinct_by_n_data <- top_precincts_all %>%
  filter(algorithm == "Gun Violence", precinct %in% top_20_pcts) %>%
  mutate(N = factor(paste0("N = ", N), levels = c("N = 200", "N = 300", "N = 400")))

p_precinct_by_n <- ggplot(precinct_by_n_data, 
                          aes(x = reorder(factor(precinct), -n_blocks), 
                              y = n_blocks, fill = N)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Blocks per Precinct by Sample Size (Gun Violence Algorithm)",
    subtitle = "Top 20 precincts by total gun violence",
    x = "Precinct",
    y = "Number of Blocks",
    fill = "Sample Size"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p_precinct_by_n)

#==============================================================================
# SECTION 16: SAVE PLOTS
#==============================================================================

cat("\nSaving plots...\n")

ggsave("output/01_concentration_curves.png", p_concentration, 
       width = 10, height = 7, dpi = 150)
ggsave("output/02_borough_distribution.png", p_borough, 
       width = 12, height = 6, dpi = 150)
ggsave("output/03_precinct_heatmap.png", p_precinct_heatmap, 
       width = 10, height = 10, dpi = 150)
ggsave("output/04_algorithm_overlap.png", p_overlap, 
       width = 10, height = 6, dpi = 150)
ggsave("output/05_block_overlap_stacked.png", p_comparison, 
       width = 8, height = 6, dpi = 150)
ggsave("output/06_precinct_by_n.png", p_precinct_by_n, 
       width = 12, height = 6, dpi = 150)

cat("Plots saved to output/ folder.\n\n")

#==============================================================================
# SECTION 17: COMBINED PLOT PANEL
#==============================================================================

cat("Creating combined summary panel...\n")

# Combine key plots
combined_panel <- (p_concentration | p_borough) / 
  (p_overlap | p_comparison) +
  plot_annotation(
    title = "EBC Targeting Algorithm Comparison Summary",
    subtitle = paste0("Baseline: ", START_DATE, " to ", END_DATE),
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

ggsave("output/00_summary_panel.png", combined_panel, 
       width = 16, height = 12, dpi = 150)

cat("Combined panel saved.\n\n")

#==============================================================================
# SECTION 18: EXPORT KEY TABLES
#==============================================================================

cat(strrep("=", 70), "\n")
cat("EXPORTING KEY TABLES\n")
cat(strrep("=", 70), "\n\n")

# Create output directory if needed
if (!dir.exists("output")) dir.create("output")

# Save block matrix
saveRDS(block_matrix, "output/block_matrix.rds")
cat("Saved: output/block_matrix.rds\n")

# Save tables
write_csv(cutoff_table, "output/cutoff_thresholds.csv")
cat("Saved: output/cutoff_thresholds.csv\n")

write_csv(capture_table, "output/capture_rates.csv")
cat("Saved: output/capture_rates.csv\n")

write_csv(overlap_table, "output/algorithm_overlap.csv")
cat("Saved: output/algorithm_overlap.csv\n")

write_csv(borough_table, "output/borough_distribution.csv")
cat("Saved: output/borough_distribution.csv\n")

write_csv(precinct_totals, "output/precinct_crime_totals.csv")
cat("Saved: output/precinct_crime_totals.csv\n")

# Save top precincts table
top_15_wide <- top_precincts_all %>%
  filter(rank <= 15) %>%
  pivot_wider(
    id_cols = c(algorithm, rank, precinct),
    names_from = N,
    values_from = n_blocks,
    names_prefix = "N_"
  )
write_csv(top_15_wide, "output/top_15_precincts_by_algorithm.csv")
cat("Saved: output/top_15_precincts_by_algorithm.csv\n")

cat("\n")
cat(strrep("=", 70), "\n")
cat("EXPLORATORY ANALYSIS COMPLETE\n")
cat(strrep("=", 70), "\n\n")

cat("Key objects in environment:\n")
cat("  - block_matrix: Full block-level data\n")
cat("  - ranked_gun_violence, ranked_shootings, etc.: Ranked block lists\n")
cat("  - cutoff_table, capture_table, overlap_table: Summary statistics\n")
cat("  - precinct_totals: Precinct-level crime totals\n")
cat("  - p_concentration, p_borough, etc.: ggplot objects\n\n")

cat("Output files saved to output/ folder.\n")

