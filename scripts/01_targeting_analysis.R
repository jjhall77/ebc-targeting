#==============================================================================
# EBC TARGETING ALGORITHM COMPARISON (v2)
# Purpose: Compare different targeting schemes for Every Block Counts RCT
# Uses pre-built intersection crosswalk tables from LION processing
#==============================================================================

library(here)
library(tidyverse)
library(janitor)
library(sf)
library(lubridate)
library(ggplot2)
library(patchwork)

#==============================================================================
# PARAMETERS
#==============================================================================

# Target sample sizes (total blocks, assuming 1:1 randomization)
TARGET_N <- c(200, 300, 400)

# Time window: 5 years ending 9/30/2025
END_DATE   <- as.Date("2025-09-30")
START_DATE <- as.Date("2020-10-01")

# Intersection proximity threshold (feet) - crimes within this distance of an
# intersection node get split across adjacent blocks
INTERSECTION_THRESHOLD <- 50  # feet (CRS 2263 is in US survey feet)

#==============================================================================
# SECTION 1: LOAD DATA
#==============================================================================

load_ebc_data <- function() {
  
  cat("Loading spatial infrastructure...\n")
  
  # Physical blocks
  physical_blocks <- st_read(here("data", "physical_blocks.gpkg"), quiet = TRUE) %>%
    st_transform(2263)
  cat("  Physical blocks:", nrow(physical_blocks), "\n")
  
  # Intersection nodes (point geometries)
  intersection_nodes <- st_read(here("data", "intersection_nodes.gpkg"), quiet = TRUE) %>%
    st_transform(2263)
  cat("  Intersection nodes:", nrow(intersection_nodes), "\n")
  
  # Intersection to blocks crosswalk (the key allocation table)
  intersection_to_blocks <- readRDS(here("data", "intersection_to_blocks.rds"))
  cat("  Intersection-block pairs:", nrow(intersection_to_blocks), "\n")
  
  # Precinct polygons
  nypp <- st_read(here("data", "nypp_25d"), quiet = TRUE) %>%
    st_transform(2263) %>%
    clean_names()
  cat("  Precincts:", nrow(nypp), "\n")
  
  cat("\nLoading crime data...\n")
  
  # Complaints
  complaints_current <- read_csv(
    here("data", "NYPD_Complaint_Data_Current_(Year_To_Date)_20251214.csv"),
    show_col_types = FALSE
  ) %>%
    clean_names() %>%
    mutate(housing_psa = as.character(housing_psa))
  
  complaints_historic <- read_csv(
    here("data", "NYPD_Complaint_Data_Historic_20251214.csv"),
    show_col_types = FALSE
  ) %>%
    clean_names() %>%
    mutate(housing_psa = as.character(housing_psa))
  
  complaints <- bind_rows(complaints_historic, complaints_current) %>%
    filter(!pd_cd %in% c(111, 113, 186)) %>%  # Remove menacing
    filter(!ofns_desc == "RAPE")
  cat("  Complaints:", nrow(complaints), "\n")
  
  # Shootings
  shootings_historic <- read_csv(
    here("data", "NYPD_Shooting_Incident_Data_(Historic)_20251215.csv"),
    show_col_types = FALSE
  ) %>%
    clean_names() %>%
    mutate(statistical_murder_flag = as.character(statistical_murder_flag))
  
  shootings_current <- read_csv(
    here("data", "NYPD_Shooting_Incident_Data_(Year_To_Date)_20251215.csv"),
    show_col_types = FALSE
  ) %>%
    clean_names()
  
  shootings <- bind_rows(shootings_historic, shootings_current)
  cat("  Shootings:", nrow(shootings), "\n")
  
  # Shots fired
  shots_fired_since_2017 <- read_csv(
    here("data", "sf_since_2017.csv"),
    show_col_types = FALSE
  ) %>%
    clean_names()
  
  shots_fired_new <- read_csv(
    here("data", "shots_fired_new.csv"),
    show_col_types = FALSE
  ) %>%
    clean_names()
  
  # Combine shots fired with temporal handoff
  cutoff_date <- min(mdy(shots_fired_new$rec_create_dt), na.rm = TRUE)
  
  # Keep cmplnt_key through standardization, then distinct() on it
  
  sf_2017_std <- shots_fired_since_2017 %>%
    mutate(date = mdy(rpt_dt)) %>%
    filter(date < cutoff_date) %>%
    transmute(
      cmplnt_key = cmplnt_key,
      source = "shots_fired_since_2017",
      date = as.Date(date),
      pct = as.integer(pct),
      x = as.numeric(x_coord_cd),
      y = as.numeric(y_coord_cd)
    ) %>%
    distinct(cmplnt_key, .keep_all = TRUE)
  
  sf_new_std <- shots_fired_new %>%
    mutate(date = mdy(rec_create_dt)) %>%
    filter(date >= cutoff_date) %>%
    transmute(
      cmplnt_key = cmplnt_key,
      source = "shots_fired_new",
      date = as.Date(date),
      pct = as.integer(cmplnt_pct_cd),
      x = as.numeric(x_coordinate_code),
      y = as.numeric(y_coordinate_code)
    ) %>%
    distinct(cmplnt_key, .keep_all = TRUE)
  
  shots_fired <- bind_rows(sf_2017_std, sf_new_std) %>%
    filter(!is.na(x), !is.na(y)) %>%
    distinct(cmplnt_key, .keep_all = TRUE) %>%  # Final dedup across both sources
    select(-cmplnt_key)  # Drop key after deduplication
  
  # Hospitals
  hospitals <- read_csv(
    here("data", "Health_Facility_General_Information_20260115.csv"),
    show_col_types = FALSE
  ) %>%
    clean_names() %>%
    filter(facility_county %in% c("Bronx", "Queens", "New York", "Richmond", "Kings")) %>%
    filter(description == "Hospital")
  cat("  Hospitals:", nrow(hospitals), "\n")
  
  # Precinct locations
  nypd_precinct_locations <- read_csv(
    here("data", "nypd_precinct_locations.csv"),
    show_col_types = FALSE
  ) %>%
    clean_names()
  cat("  Precinct locations:", nrow(nypd_precinct_locations), "\n")
  
  # Return all data
  list(
    physical_blocks = physical_blocks,
    intersection_nodes = intersection_nodes,
    intersection_to_blocks = intersection_to_blocks,
    nypp = nypp,
    complaints = complaints,
    shootings = shootings,
    shots_fired = shots_fired,
    hospitals = hospitals,
    nypd_precinct_locations = nypd_precinct_locations
  )
}

#==============================================================================
# SECTION 2: PREPARE CRIME SPATIAL DATA
#==============================================================================

prepare_crime_data <- function(data) {
  
  cat("\nPreparing crime spatial objects...\n")
  
  # Define crime categories
  violent_ky <- c(101, 104, 105, 106, 344)
  
  outside_loc_keywords <- c(
    "FRONT OF", "OPPOSITE OF", "OUTSIDE", "REAR OF",
    "STREET", "IN STREET", "SIDEWALK"
  )
  outside_prem_keywords <- c(
    "PARK", "STREET", "PUBLIC PLACE", "HIGHWAY",
    "BRIDGE", "SIDEWALK", "VACANT LOT",
    "PUBLIC HOUSING AREA", "OUTSIDE"
  )
  outside_loc_pattern <- str_c(outside_loc_keywords, collapse = "|")
  outside_prem_pattern <- str_c(outside_prem_keywords, collapse = "|")
  
  # Process complaints
  compl_base <- data$complaints %>%
    filter(!is.na(x_coord_cd), !is.na(y_coord_cd)) %>%
    mutate(
      date = mdy(rpt_dt),
      loc_of_occur_desc = str_to_upper(coalesce(loc_of_occur_desc, "")),
      prem_typ_desc = str_to_upper(coalesce(prem_typ_desc, "")),
      is_outdoor = str_detect(loc_of_occur_desc, outside_loc_pattern) |
        str_detect(prem_typ_desc, outside_prem_pattern)
    ) %>%
    filter(date >= START_DATE & date <= END_DATE)
  
  # Create sf objects
  violent_crime_sf <- compl_base %>%
    filter(ky_cd %in% violent_ky) %>%
    st_as_sf(coords = c("x_coord_cd", "y_coord_cd"), crs = 2263, remove = FALSE)
  cat("  Violent crime (in date range):", nrow(violent_crime_sf), "\n")
  
  violent_street_crime_sf <- compl_base %>%
    filter(ky_cd %in% violent_ky, is_outdoor) %>%
    st_as_sf(coords = c("x_coord_cd", "y_coord_cd"), crs = 2263, remove = FALSE)
  cat("  Violent street crime (in date range):", nrow(violent_street_crime_sf), "\n")
  
  # Shootings
  shootings_sf <- data$shootings %>%
    filter(!is.na(x_coord_cd), !is.na(y_coord_cd)) %>%
    mutate(date = mdy(occur_date)) %>%
    filter(date >= START_DATE & date <= END_DATE) %>%
    st_as_sf(coords = c("x_coord_cd", "y_coord_cd"), crs = 2263, remove = FALSE)
  cat("  Shootings (in date range):", nrow(shootings_sf), "\n")
  
  # Shots fired
  shots_fired_sf <- data$shots_fired %>%
    filter(date >= START_DATE & date <= END_DATE) %>%
    st_as_sf(coords = c("x", "y"), crs = 2263, remove = FALSE)
  cat("  Shots fired (in date range):", nrow(shots_fired_sf), "\n")
  
  # Hospitals sf
  hospitals_sf <- data$hospitals %>%
    filter(!is.na(facility_latitude), !is.na(facility_longitude)) %>%
    st_as_sf(
      coords = c("facility_longitude", "facility_latitude"),
      crs = 4326, remove = FALSE
    ) %>%
    st_transform(2263)
  
  # Precinct locations sf
  precinct_locations_sf <- data$nypd_precinct_locations %>%
    filter(!is.na(longitude), !is.na(latitude)) %>%
    st_as_sf(
      coords = c("longitude", "latitude"),
      crs = 4326, remove = FALSE
    ) %>%
    st_transform(2263)
  
  list(
    violent_crime_sf = violent_crime_sf,
    violent_street_crime_sf = violent_street_crime_sf,
    shootings_sf = shootings_sf,
    shots_fired_sf = shots_fired_sf,
    hospitals_sf = hospitals_sf,
    precinct_locations_sf = precinct_locations_sf
  )
}

#==============================================================================
# SECTION 3: IDENTIFY BLOCKS TO EXCLUDE
#==============================================================================

identify_exclusion_blocks <- function(physical_blocks, hospitals_sf, precinct_locations_sf) {
  
  cat("\nIdentifying blocks to exclude...\n")
  
  # Find nearest block to each hospital
  hospital_block_idx <- st_nearest_feature(hospitals_sf, physical_blocks)
  hospital_blocks <- physical_blocks$physical_id[hospital_block_idx]
  
  # Find nearest block to each precinct
  precinct_block_idx <- st_nearest_feature(precinct_locations_sf, physical_blocks)
  precinct_blocks <- physical_blocks$physical_id[precinct_block_idx]
  
  # Combine unique
  exclude_ids <- unique(c(hospital_blocks, precinct_blocks))
  
  cat("  Blocks with hospitals:", length(unique(hospital_blocks)), "\n")
  cat("  Blocks with precincts:", length(unique(precinct_blocks)), "\n")
  cat("  Total unique excluded:", length(exclude_ids), "\n")
  
  exclude_ids
}

#==============================================================================
# SECTION 4: CRIME TO BLOCK ALLOCATION (Using Intersection Crosswalk)
#==============================================================================

allocate_crimes_to_blocks <- function(crime_sf, 
                                      physical_blocks,
                                      intersection_nodes,
                                      intersection_to_blocks,
                                      threshold = INTERSECTION_THRESHOLD) {
  #
  # This function implements the EBC intersection allocation logic:
  # 1. For each crime, find distance to nearest intersection node
  
  # 2. If within threshold: split crime equally across all adjacent blocks
  # 3. If beyond threshold: assign entirely to nearest block
  #
  
  n_crimes <- nrow(crime_sf)
  if (n_crimes == 0) {
    return(tibble(block_id = integer(), crime_count = numeric()))
  }
  
  # Step 1: Find nearest intersection node for each crime
  nearest_node_idx <- st_nearest_feature(crime_sf, intersection_nodes)
  nearest_node_id <- intersection_nodes$nodeid[nearest_node_idx]
  
  # Step 2: Calculate distance to nearest intersection
  dist_to_intersection <- as.numeric(
    st_distance(crime_sf, intersection_nodes[nearest_node_idx, ], by_element = TRUE)
  )
  
  # Step 3: Find nearest block for each crime (fallback for non-intersection crimes)
  nearest_block_idx <- st_nearest_feature(crime_sf, physical_blocks)
  nearest_block_id <- physical_blocks$physical_id[nearest_block_idx]
  
  # Step 4: Build allocation table
  crime_allocation <- tibble(
    crime_idx = 1:n_crimes,
    nearest_node_id = nearest_node_id,
    dist_to_intersection = dist_to_intersection,
    nearest_block_id = nearest_block_id,
    is_at_intersection = dist_to_intersection <= threshold
  )
  
  # Step 5: Process intersection crimes (split across adjacent blocks)
  intersection_crimes <- crime_allocation %>%
    filter(is_at_intersection)
  
  if (nrow(intersection_crimes) > 0) {
    # Join to get adjacent blocks for each intersection crime
    intersection_split <- intersection_crimes %>%
      left_join(
        intersection_to_blocks %>% select(node_id, adjacent_blocks),
        by = c("nearest_node_id" = "node_id")
      ) %>%
      # Unnest the list column to get one row per block
      unnest(adjacent_blocks) %>%
      rename(block_id = adjacent_blocks) %>%
      # Count how many blocks each crime splits across
      group_by(crime_idx) %>%
      mutate(weight = 1 / n()) %>%
      ungroup() %>%
      # Aggregate by block
      group_by(block_id) %>%
      summarise(crime_count = sum(weight), .groups = "drop")
  } else {
    intersection_split <- tibble(block_id = integer(), crime_count = numeric())
  }
  
  # Step 6: Process non-intersection crimes (assign to nearest block)
  non_intersection_crimes <- crime_allocation %>%
    filter(!is_at_intersection) %>%
    count(nearest_block_id, name = "crime_count") %>%
    rename(block_id = nearest_block_id)
  
  # Step 7: Combine and aggregate
  total_counts <- bind_rows(intersection_split, non_intersection_crimes) %>%
    group_by(block_id) %>%
    summarise(crime_count = sum(crime_count), .groups = "drop")
  
  # Report
  n_at_intersection <- sum(crime_allocation$is_at_intersection)
  cat("    Crimes at intersections:", n_at_intersection, 
      "(", round(n_at_intersection/n_crimes*100, 1), "%)\n")
  cat("    Crimes on blocks:", n_crimes - n_at_intersection,
      "(", round((n_crimes - n_at_intersection)/n_crimes*100, 1), "%)\n")
  
  total_counts
}

#==============================================================================
# SECTION 5: BUILD BLOCK CRIME MATRIX
#==============================================================================

build_block_crime_matrix <- function(physical_blocks,
                                     crime_data,
                                     intersection_nodes,
                                     intersection_to_blocks,
                                     exclude_block_ids,
                                     nypp) {
  
  cat("\n" , strrep("=", 60), "\n")
  cat("BUILDING BLOCK-LEVEL CRIME MATRIX\n")
  cat(strrep("=", 60), "\n\n")
  
  # Initialize matrix with all blocks
  block_matrix <- physical_blocks %>%
    st_drop_geometry() %>%
    select(physical_id, total_length_ft, streets, boro, cd,
           endpoint_node_1, endpoint_node_2) %>%
    rename(block_id = physical_id) %>%
    mutate(excluded = block_id %in% exclude_block_ids)
  
  cat("Total blocks:", nrow(block_matrix), "\n")
  cat("Excluded blocks:", sum(block_matrix$excluded), "\n")
  cat("Eligible blocks:", sum(!block_matrix$excluded), "\n\n")
  
  # --- SHOOTINGS ---
  cat("Processing SHOOTINGS...\n")
  shooting_counts <- allocate_crimes_to_blocks(
    crime_data$shootings_sf,
    physical_blocks,
    intersection_nodes,
    intersection_to_blocks
  ) %>%
    rename(shootings = crime_count)
  
  block_matrix <- block_matrix %>%
    left_join(shooting_counts, by = "block_id")
  
  # --- SHOTS FIRED ---
  cat("\nProcessing SHOTS FIRED...\n")
  shots_counts <- allocate_crimes_to_blocks(
    crime_data$shots_fired_sf,
    physical_blocks,
    intersection_nodes,
    intersection_to_blocks
  ) %>%
    rename(shots_fired = crime_count)
  
  block_matrix <- block_matrix %>%
    left_join(shots_counts, by = "block_id")
  
  # --- VIOLENT CRIME ---
  cat("\nProcessing VIOLENT CRIME...\n")
  violent_counts <- allocate_crimes_to_blocks(
    crime_data$violent_crime_sf,
    physical_blocks,
    intersection_nodes,
    intersection_to_blocks
  ) %>%
    rename(violent_crime = crime_count)
  
  block_matrix <- block_matrix %>%
    left_join(violent_counts, by = "block_id")
  
  # --- VIOLENT STREET CRIME ---
  cat("\nProcessing VIOLENT STREET CRIME...\n")
  street_counts <- allocate_crimes_to_blocks(
    crime_data$violent_street_crime_sf,
    physical_blocks,
    intersection_nodes,
    intersection_to_blocks
  ) %>%
    rename(violent_street_crime = crime_count)
  
  block_matrix <- block_matrix %>%
    left_join(street_counts, by = "block_id")
  
  # Replace NAs with 0
  block_matrix <- block_matrix %>%
    mutate(across(c(shootings, shots_fired, violent_crime, violent_street_crime),
                  ~replace_na(., 0)))
  
  # Create composite: Gun Violence Total
  block_matrix <- block_matrix %>%
    mutate(gun_violence_total = shootings + shots_fired)
  
  # Add precinct via spatial join
  cat("\nAdding precinct information...\n")
  block_centroids <- physical_blocks %>%
    st_centroid() %>%
    select(physical_id)
  
  block_precincts <- st_join(block_centroids, nypp, join = st_within) %>%
    st_drop_geometry() %>%
    select(physical_id, precinct)
  
  block_matrix <- block_matrix %>%
    left_join(block_precincts, by = c("block_id" = "physical_id"))
  
  # Add borough name from precinct
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
  
  cat("\nBlock matrix complete.\n")
  
  block_matrix
}

#==============================================================================
# SECTION 6: RANKING FUNCTIONS
#==============================================================================

# Algorithm 1: Gun Violence Total (shootings + shots_fired)
rank_gun_violence <- function(block_matrix) {
  block_matrix %>%
    filter(!excluded) %>%
    arrange(desc(gun_violence_total), desc(shootings), desc(shots_fired)) %>%
    mutate(rank = row_number())
}

# Algorithm 2: Shootings Only (with tiebreakers)
rank_shootings <- function(block_matrix) {
  block_matrix %>%
    filter(!excluded) %>%
    arrange(desc(shootings), desc(shots_fired), desc(violent_crime)) %>%
    mutate(rank = row_number())
}

# Algorithm 3: All Violent Crime
rank_violent_crime <- function(block_matrix) {
  block_matrix %>%
    filter(!excluded) %>%
    arrange(desc(violent_crime), desc(shootings), desc(shots_fired)) %>%
    mutate(rank = row_number())
}

# Algorithm 4: Violent Street Crime
rank_violent_street <- function(block_matrix) {
  block_matrix %>%
    filter(!excluded) %>%
    arrange(desc(violent_street_crime), desc(shootings), desc(shots_fired)) %>%
    mutate(rank = row_number())
}

#==============================================================================
# SECTION 7: ANALYSIS FUNCTIONS
#==============================================================================

# Get top N blocks for each algorithm
get_top_n_blocks <- function(block_matrix, n) {
  list(
    gun_violence = rank_gun_violence(block_matrix) %>% 
      filter(rank <= n) %>%
      mutate(algorithm = "Gun Violence"),
    
    shootings = rank_shootings(block_matrix) %>% 
      filter(rank <= n) %>%
      mutate(algorithm = "Shootings"),
    
    violent_crime = rank_violent_crime(block_matrix) %>% 
      filter(rank <= n) %>%
      mutate(algorithm = "Violent Crime"),
    
    violent_street = rank_violent_street(block_matrix) %>% 
      filter(rank <= n) %>%
      mutate(algorithm = "Violent Street")
  )
}

# Calculate cutoffs at each sample size
calculate_cutoffs <- function(block_matrix, target_ns = TARGET_N) {
  
  results <- map_dfr(target_ns, function(n) {
    top_blocks <- get_top_n_blocks(block_matrix, n)
    
    tibble(
      N = n,
      
      # Gun Violence
      GV_min = min(top_blocks$gun_violence$gun_violence_total),
      GV_median = median(top_blocks$gun_violence$gun_violence_total),
      GV_max = max(top_blocks$gun_violence$gun_violence_total),
      
      # Shootings
      SH_min = min(top_blocks$shootings$shootings),
      SH_median = median(top_blocks$shootings$shootings),
      SH_max = max(top_blocks$shootings$shootings),
      
      # Violent Crime
      VC_min = min(top_blocks$violent_crime$violent_crime),
      VC_median = median(top_blocks$violent_crime$violent_crime),
      VC_max = max(top_blocks$violent_crime$violent_crime),
      
      # Violent Street
      VS_min = min(top_blocks$violent_street$violent_street_crime),
      VS_median = median(top_blocks$violent_street$violent_street_crime),
      VS_max = max(top_blocks$violent_street$violent_street_crime)
    )
  })
  
  results
}

# Calculate concentration statistics
calculate_concentration <- function(block_matrix) {
  
  eligible <- block_matrix %>% filter(!excluded)
  
  # Function to build concentration curve for one crime type
  build_curve <- function(data, crime_col, crime_name) {
    data %>%
      arrange(desc(!!sym(crime_col))) %>%
      mutate(
        cum_blocks = row_number(),
        cum_blocks_pct = cum_blocks / n() * 100,
        cum_crime = cumsum(!!sym(crime_col)),
        cum_crime_pct = cum_crime / sum(!!sym(crime_col)) * 100
      ) %>%
      select(cum_blocks, cum_blocks_pct, cum_crime, cum_crime_pct) %>%
      mutate(crime_type = crime_name)
  }
  
  bind_rows(
    build_curve(eligible, "gun_violence_total", "Gun Violence"),
    build_curve(eligible, "shootings", "Shootings"),
    build_curve(eligible, "violent_crime", "Violent Crime"),
    build_curve(eligible, "violent_street_crime", "Violent Street")
  )
}

# Calculate citywide capture rates
calculate_capture_rates <- function(block_matrix, target_ns = TARGET_N) {
  
  eligible <- block_matrix %>% filter(!excluded)
  
  # Citywide totals
  totals <- list(
    gun_violence = sum(eligible$gun_violence_total),
    shootings = sum(eligible$shootings),
    shots_fired = sum(eligible$shots_fired),
    violent_crime = sum(eligible$violent_crime),
    violent_street = sum(eligible$violent_street_crime)
  )
  
  results <- map_dfr(target_ns, function(n) {
    top <- get_top_n_blocks(block_matrix, n)
    
    tibble(
      N = n,
      
      # What % of each crime type does each algorithm capture?
      
      # Gun Violence algorithm captures:
      GV_alg_captures_GV = round(sum(top$gun_violence$gun_violence_total) / totals$gun_violence * 100, 1),
      GV_alg_captures_SH = round(sum(top$gun_violence$shootings) / totals$shootings * 100, 1),
      GV_alg_captures_VC = round(sum(top$gun_violence$violent_crime) / totals$violent_crime * 100, 1),
      
      # Shootings algorithm captures:
      SH_alg_captures_GV = round(sum(top$shootings$gun_violence_total) / totals$gun_violence * 100, 1),
      SH_alg_captures_SH = round(sum(top$shootings$shootings) / totals$shootings * 100, 1),
      SH_alg_captures_VC = round(sum(top$shootings$violent_crime) / totals$violent_crime * 100, 1),
      
      # Violent Crime algorithm captures:
      VC_alg_captures_GV = round(sum(top$violent_crime$gun_violence_total) / totals$gun_violence * 100, 1),
      VC_alg_captures_SH = round(sum(top$violent_crime$shootings) / totals$shootings * 100, 1),
      VC_alg_captures_VC = round(sum(top$violent_crime$violent_crime) / totals$violent_crime * 100, 1),
      
      # Violent Street algorithm captures:
      VS_alg_captures_GV = round(sum(top$violent_street$gun_violence_total) / totals$gun_violence * 100, 1),
      VS_alg_captures_SH = round(sum(top$violent_street$shootings) / totals$shootings * 100, 1),
      VS_alg_captures_VS = round(sum(top$violent_street$violent_street_crime) / totals$violent_street * 100, 1)
    )
  })
  
  results
}

# Calculate precinct distribution
calculate_precinct_distribution <- function(block_matrix, target_ns = TARGET_N) {
  
  results <- map_dfr(target_ns, function(n) {
    top <- get_top_n_blocks(block_matrix, n)
    
    # Count per precinct for each algorithm
    gv <- top$gun_violence %>% count(precinct, name = "n_GV")
    sh <- top$shootings %>% count(precinct, name = "n_SH")
    vc <- top$violent_crime %>% count(precinct, name = "n_VC")
    vs <- top$violent_street %>% count(precinct, name = "n_VS")
    
    # Combine
    gv %>%
      full_join(sh, by = "precinct") %>%
      full_join(vc, by = "precinct") %>%
      full_join(vs, by = "precinct") %>%
      mutate(target_N = n) %>%
      replace_na(list(n_GV = 0, n_SH = 0, n_VC = 0, n_VS = 0))
  })
  
  results
}

# Calculate overlap across algorithms
calculate_overlap <- function(block_matrix, target_ns = TARGET_N) {
  
  results <- map_dfr(target_ns, function(n) {
    top <- get_top_n_blocks(block_matrix, n)
    
    gv_ids <- top$gun_violence$block_id
    sh_ids <- top$shootings$block_id
    vc_ids <- top$violent_crime$block_id
    vs_ids <- top$violent_street$block_id
    
    # All pairwise overlaps
    tibble(
      N = n,
      
      GV_SH = length(intersect(gv_ids, sh_ids)),
      GV_VC = length(intersect(gv_ids, vc_ids)),
      GV_VS = length(intersect(gv_ids, vs_ids)),
      SH_VC = length(intersect(sh_ids, vc_ids)),
      SH_VS = length(intersect(sh_ids, vs_ids)),
      VC_VS = length(intersect(vc_ids, vs_ids)),
      
      in_all_4 = length(Reduce(intersect, list(gv_ids, sh_ids, vc_ids, vs_ids))),
      in_at_least_3 = length(which(table(c(gv_ids, sh_ids, vc_ids, vs_ids)) >= 3)),
      union_all = length(unique(c(gv_ids, sh_ids, vc_ids, vs_ids)))
    )
  })
  
  results
}

# Calculate borough distribution
calculate_borough_distribution <- function(block_matrix, target_ns = TARGET_N) {
  
  results <- map_dfr(target_ns, function(n) {
    top <- get_top_n_blocks(block_matrix, n)
    
    bind_rows(
      top$gun_violence %>% count(borough) %>% mutate(algorithm = "Gun Violence"),
      top$shootings %>% count(borough) %>% mutate(algorithm = "Shootings"),
      top$violent_crime %>% count(borough) %>% mutate(algorithm = "Violent Crime"),
      top$violent_street %>% count(borough) %>% mutate(algorithm = "Violent Street")
    ) %>%
      mutate(target_N = n)
  })
  
  results
}

#==============================================================================
# SECTION 8: VISUALIZATION FUNCTIONS
#==============================================================================

plot_concentration_curves <- function(concentration_data, target_ns = TARGET_N) {
  
  # Limit x-axis for readability
  max_blocks <- min(5000, max(concentration_data$cum_blocks))
  
  p <- ggplot(concentration_data, 
              aes(x = cum_blocks, y = cum_crime_pct, color = crime_type)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = target_ns, linetype = "dashed", alpha = 0.5) +
    geom_abline(slope = 100/max(concentration_data$cum_blocks), 
                intercept = 0, linetype = "dotted", color = "gray50") +
    scale_x_continuous(
      breaks = c(target_ns, 500, 1000, 2000, 5000),
      labels = scales::comma,
      limits = c(0, max_blocks)
    ) +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "Crime Concentration by Block Ranking",
      subtitle = paste0("Baseline: ", START_DATE, " to ", END_DATE, 
                        " | Dashed lines = target Ns (", paste(target_ns, collapse = ", "), ")"),
      x = "Number of Blocks (ranked by crime count)",
      y = "Cumulative % of Citywide Crime",
      color = "Crime Type"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  p
}

plot_overlap_heatmap <- function(overlap_data, n = 300) {
  
  row <- overlap_data %>% filter(N == n)
  
  # Build matrix
  algorithms <- c("Gun Violence", "Shootings", "Violent Crime", "Violent Street")
  overlap_matrix <- matrix(n, nrow = 4, ncol = 4, 
                           dimnames = list(algorithms, algorithms))
  
  overlap_matrix[1, 2] <- overlap_matrix[2, 1] <- row$GV_SH
  overlap_matrix[1, 3] <- overlap_matrix[3, 1] <- row$GV_VC
  overlap_matrix[1, 4] <- overlap_matrix[4, 1] <- row$GV_VS
  overlap_matrix[2, 3] <- overlap_matrix[3, 2] <- row$SH_VC
  overlap_matrix[2, 4] <- overlap_matrix[4, 2] <- row$SH_VS
  overlap_matrix[3, 4] <- overlap_matrix[4, 3] <- row$VC_VS
  
  # Convert to long format
  overlap_long <- as_tibble(overlap_matrix, rownames = "Algorithm1") %>%
    pivot_longer(-Algorithm1, names_to = "Algorithm2", values_to = "overlap") %>%
    mutate(
      Algorithm1 = factor(Algorithm1, levels = algorithms),
      Algorithm2 = factor(Algorithm2, levels = algorithms),
      pct_overlap = round(overlap / n * 100, 0)
    )
  
  ggplot(overlap_long, aes(x = Algorithm1, y = Algorithm2, fill = overlap)) +
    geom_tile() +
    geom_text(aes(label = paste0(overlap, "\n(", pct_overlap, "%)")), 
              color = "white", size = 3.5) +
    scale_fill_gradient(low = "steelblue", high = "darkred") +
    labs(
      title = paste0("Algorithm Overlap (N = ", n, ")"),
      subtitle = "Number of blocks (and % of N) shared between algorithms",
      x = "", y = "", fill = "Overlap"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
}

plot_precinct_distribution <- function(precinct_data, n = 300) {
  
  pct_data <- precinct_data %>%
    filter(target_N == n) %>%
    pivot_longer(
      cols = starts_with("n_"),
      names_to = "algorithm",
      values_to = "n_blocks"
    ) %>%
    mutate(
      algorithm = case_when(
        algorithm == "n_GV" ~ "Gun Violence",
        algorithm == "n_SH" ~ "Shootings",
        algorithm == "n_VC" ~ "Violent Crime",
        algorithm == "n_VS" ~ "Violent Street"
      )
    ) %>%
    filter(!is.na(precinct))
  
  ggplot(pct_data, aes(x = factor(precinct), y = n_blocks, fill = algorithm)) +
    geom_col(position = "dodge") +
    scale_fill_brewer(palette = "Set1") +
    labs(
      title = paste0("Blocks per Precinct by Algorithm (N = ", n, ")"),
      x = "Precinct",
      y = "Number of Blocks",
      fill = "Algorithm"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
      legend.position = "bottom"
    )
}

plot_borough_distribution <- function(borough_data, n = 300) {
  
  boro_data <- borough_data %>%
    filter(target_N == n) %>%
    filter(!is.na(borough))
  
  ggplot(boro_data, aes(x = borough, y = n, fill = algorithm)) +
    geom_col(position = "dodge") +
    scale_fill_brewer(palette = "Set1") +
    labs(
      title = paste0("Blocks per Borough by Algorithm (N = ", n, ")"),
      x = "Borough",
      y = "Number of Blocks",
      fill = "Algorithm"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

#==============================================================================
# SECTION 9: SUMMARY REPORT
#==============================================================================

generate_summary_report <- function(block_matrix, target_ns = TARGET_N) {
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("EBC TARGETING ALGORITHM COMPARISON REPORT\n")
  cat(strrep("=", 70), "\n\n")
  
  eligible <- block_matrix %>% filter(!excluded)
  
  cat("BASELINE PARAMETERS\n")
  cat(strrep("-", 40), "\n")
  cat("Date range:", as.character(START_DATE), "to", as.character(END_DATE), "\n")
  cat("Intersection threshold:", INTERSECTION_THRESHOLD, "feet\n\n")
  
  cat("BLOCK COUNTS\n")
  cat(strrep("-", 40), "\n")
  cat("Total physical blocks:", nrow(block_matrix), "\n")
  cat("Excluded (precincts/hospitals):", sum(block_matrix$excluded), "\n")
  cat("Eligible for selection:", nrow(eligible), "\n\n")
  
  cat("CITYWIDE CRIME TOTALS (eligible blocks, 5-year baseline)\n")
  cat(strrep("-", 40), "\n")
  cat("Shootings:", sum(eligible$shootings), "\n")
  cat("Shots fired:", sum(eligible$shots_fired), "\n")
  cat("Gun violence total:", sum(eligible$gun_violence_total), "\n")
  cat("Violent crime:", sum(eligible$violent_crime), "\n")
  cat("Violent street crime:", sum(eligible$violent_street_crime), "\n\n")
  
  cat("CRIME DISTRIBUTION ACROSS BLOCKS\n")
  cat(strrep("-", 40), "\n")
  cat("Blocks with ≥1 shooting:", sum(eligible$shootings >= 1),
      "(", round(sum(eligible$shootings >= 1)/nrow(eligible)*100, 2), "%)\n")
  cat("Blocks with ≥2 shootings:", sum(eligible$shootings >= 2),
      "(", round(sum(eligible$shootings >= 2)/nrow(eligible)*100, 2), "%)\n")
  cat("Blocks with ≥1 gun violence event:", sum(eligible$gun_violence_total >= 1),
      "(", round(sum(eligible$gun_violence_total >= 1)/nrow(eligible)*100, 2), "%)\n")
  cat("Blocks with ≥5 gun violence events:", sum(eligible$gun_violence_total >= 5),
      "(", round(sum(eligible$gun_violence_total >= 5)/nrow(eligible)*100, 2), "%)\n")
  cat("Blocks with ≥1 violent crime:", sum(eligible$violent_crime >= 1),
      "(", round(sum(eligible$violent_crime >= 1)/nrow(eligible)*100, 2), "%)\n")
  cat("Blocks with ≥10 violent crimes:", sum(eligible$violent_crime >= 10),
      "(", round(sum(eligible$violent_crime >= 10)/nrow(eligible)*100, 2), "%)\n\n")
  
  # Calculate all analysis outputs
  cutoffs <- calculate_cutoffs(block_matrix, target_ns)
  capture <- calculate_capture_rates(block_matrix, target_ns)
  overlap <- calculate_overlap(block_matrix, target_ns)
  concentration <- calculate_concentration(block_matrix)
  precinct_dist <- calculate_precinct_distribution(block_matrix, target_ns)
  borough_dist <- calculate_borough_distribution(block_matrix, target_ns)
  
  cat("\nCUTOFF THRESHOLDS BY SAMPLE SIZE\n")
  cat(strrep("-", 40), "\n")
  cat("(min/median/max crime count for blocks in top N)\n\n")
  print(cutoffs, n = Inf, width = Inf)
  
  cat("\n\nCITYWIDE CAPTURE RATES (%)\n")
  cat(strrep("-", 40), "\n")
  cat("(What % of each crime type does each algorithm's top N capture?)\n\n")
  print(capture, n = Inf, width = Inf)
  
  cat("\n\nALGORITHM OVERLAP\n")
  cat(strrep("-", 40), "\n")
  cat("(Number of blocks appearing in both algorithms' top N)\n\n")
  print(overlap, n = Inf, width = Inf)
  
  cat("\n")
  
  # Return results for further use
  list(
    block_matrix = block_matrix,
    cutoffs = cutoffs,
    capture_rates = capture,
    overlap = overlap,
    concentration = concentration,
    precinct_distribution = precinct_dist,
    borough_distribution = borough_dist
  )
}

#==============================================================================
# SECTION 10: MAIN EXECUTION
#==============================================================================

run_targeting_analysis <- function() {
  
  # Step 1: Load all data
  cat("STEP 1: Loading data...\n")
  data <- load_ebc_data()
  
  # Step 2: Prepare crime spatial objects
  cat("\nSTEP 2: Preparing crime data...\n")
  crime_data <- prepare_crime_data(data)
  
  # Step 3: Identify exclusion blocks
  exclude_ids <- identify_exclusion_blocks(
    data$physical_blocks,
    crime_data$hospitals_sf,
    crime_data$precinct_locations_sf
  )
  
  # Step 4: Build block crime matrix
  block_matrix <- build_block_crime_matrix(
    data$physical_blocks,
    crime_data,
    data$intersection_nodes,
    data$intersection_to_blocks,
    exclude_ids,
    data$nypp
  )
  
  # Step 5: Generate report and analysis
  cat("\nSTEP 5: Generating analysis...\n")
  results <- generate_summary_report(block_matrix, TARGET_N)
  
  # Step 6: Create visualizations
  cat("\nSTEP 6: Creating visualizations...\n")
  
  plots <- list(
    concentration = plot_concentration_curves(results$concentration),
    overlap_200 = plot_overlap_heatmap(results$overlap, 200),
    overlap_300 = plot_overlap_heatmap(results$overlap, 300),
    overlap_400 = plot_overlap_heatmap(results$overlap, 400),
    precinct_300 = plot_precinct_distribution(results$precinct_distribution, 300),
    borough_300 = plot_borough_distribution(results$borough_distribution, 300)
  )
  
  # Save plots
  ggsave("concentration_curves.png", plots$concentration, 
         width = 10, height = 7, dpi = 150)
  ggsave("overlap_heatmap_300.png", plots$overlap_300, 
         width = 8, height = 6, dpi = 150)
  ggsave("borough_distribution_300.png", plots$borough_300,
         width = 10, height = 6, dpi = 150)
  
  cat("\nAnalysis complete. Plots saved.\n")
  
  #============================================================================
  # EXPORT KEY OBJECTS TO GLOBAL ENVIRONMENT FOR DOWNSTREAM SCRIPTS
  #============================================================================
  cat("\nExporting objects to global environment for downstream scripts...\n")
  
  assign("crime_data", crime_data, envir = .GlobalEnv)
  assign("physical_blocks", data$physical_blocks, envir = .GlobalEnv)
  assign("intersection_nodes", data$intersection_nodes, envir = .GlobalEnv)
  assign("intersection_to_blocks", data$intersection_to_blocks, envir = .GlobalEnv)
  assign("nypp", data$nypp, envir = .GlobalEnv)
  
  # Also export individual sf objects for convenience
  assign("shootings_sf", crime_data$shootings_sf, envir = .GlobalEnv)
  assign("shots_fired_sf", crime_data$shots_fired_sf, envir = .GlobalEnv)
  assign("violent_crime_sf", crime_data$violent_crime_sf, envir = .GlobalEnv)
  
  cat("  Exported: crime_data, physical_blocks, intersection_nodes,\n")
  cat("            intersection_to_blocks, nypp, shootings_sf,\n")
  cat("            shots_fired_sf, violent_crime_sf\n")
  
  # Return everything
  c(results, list(plots = plots))
}

#==============================================================================
# QUICK DIAGNOSTIC (run before full analysis)
#==============================================================================

quick_diagnostic <- function() {
  # Check that required files exist
  required_files <- c(
    "physical_blocks.gpkg",
    "intersection_nodes.gpkg",
    "intersection_to_blocks.rds",
    "nypp_25d",
    "NYPD_Complaint_Data_Current_(Year_To_Date)_20251214.csv",
    "NYPD_Complaint_Data_Historic_20251214.csv",
    "NYPD_Shooting_Incident_Data_(Historic)_20251215.csv",
    "NYPD_Shooting_Incident_Data_(Year_To_Date)_20251215.csv",
    "sf_since_2017.csv",
    "shots_fired_new.csv",
    "Health_Facility_General_Information_20260115.csv",
    "nypd_precinct_locations.csv"
  )
  
  cat("Checking required files in data folder...\n\n")
  
  for (f in required_files) {
    path <- here("data", f)
    exists <- file.exists(path) | dir.exists(path)
    status <- if (exists) "✓" else "✗ MISSING"
    cat(status, "-", f, "\n")
  }
  
  cat("\nIf all files present, run: output <- run_targeting_analysis()\n")
}

# Run diagnostic first:
# quick_diagnostic()

output <- run_targeting_analysis()