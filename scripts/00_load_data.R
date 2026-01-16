#==============================================================================
# 00_load_data.R
# Every Block Counts (EBC) Project
# Purpose: Load and prepare all data objects for targeting analysis
#==============================================================================
#
# USAGE:
#   source("scripts/00_load_data.R")
#
# OUTPUTS:
#   This script creates the following objects in your environment:
#
#   SPATIAL INFRASTRUCTURE:
#     - physical_blocks      : Physical block geometries (sf, MULTILINESTRING)
#     - intersection_nodes   : Intersection point geometries (sf, POINT)
#     - intersection_to_blocks : Node → adjacent blocks crosswalk (tibble w/ list col)
#     - nypp                 : NYPD precinct polygons (sf, MULTIPOLYGON)
#     - nynta                : NYC Neighborhood Tabulation Areas (sf, MULTIPOLYGON)
#
#   CRIME DATA (sf objects, filtered to valid coordinates):
#     - shootings_sf         : NYPD shooting incidents
#     - shots_fired          : Shots fired (no victim hit), combined from 2 sources
#     - violent_crime        : All violent crime complaints
#     - violent_street_crime : Outdoor violent crime only
#     - property_crime       : Property crimes
#     - property_street_crime: Outdoor property crimes
#
#   FACILITY DATA (sf objects):
#     - hospitals_sf              : NYC hospital locations
#     - nypd_precinct_locations_sf: NYPD precinct station house locations
#
#   RAW DATA (tibbles, for reference):
#     - complaints           : Full complaints dataset (filtered, not sf)
#     - shootings            : Full shootings dataset (not sf)
#
#   REFERENCE VECTORS:
#     - violent_ky           : KY codes for violent crimes
#     - property_ky          : KY codes for property crimes
#
#==============================================================================

# Load required packages
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(lubridate)

cat("\n")
cat(strrep("=", 70), "\n")
cat("EBC DATA LOADING SCRIPT\n")
cat(strrep("=", 70), "\n\n")

#==============================================================================
# SECTION 1: SPATIAL INFRASTRUCTURE
#==============================================================================

cat("LOADING SPATIAL INFRASTRUCTURE...\n")
cat(strrep("-", 40), "\n")

# Physical blocks (street segments between intersections)
physical_blocks <- st_read(
  here("data", "physical_blocks.gpkg"),
  quiet = TRUE
) %>%
  st_transform(2263)
cat("  physical_blocks:", format(nrow(physical_blocks), big.mark = ","), "blocks\n")

# Intersection nodes (point geometries where streets meet)
intersection_nodes <- st_read(
  here("data", "intersection_nodes.gpkg"),
  quiet = TRUE
) %>%
  st_transform(2263)
cat("  intersection_nodes:", format(nrow(intersection_nodes), big.mark = ","), "nodes\n")

# Intersection to blocks crosswalk (key allocation table)
intersection_to_blocks <- readRDS(here("data", "intersection_to_blocks.rds"))
cat("  intersection_to_blocks:", format(nrow(intersection_to_blocks), big.mark = ","), "intersection-block mappings\n")

# NYPD precinct polygons
nypp <- st_read(
  here("data", "nypp_25d"),
  quiet = TRUE
) %>%
  st_transform(2263) %>%
  clean_names()
cat("  nypp:", nrow(nypp), "precincts\n")

# NYC Neighborhood Tabulation Areas
nynta <- st_read(
  here("data", "nynta2020_25d"),
  quiet = TRUE
) %>%
  st_transform(2263) %>%
  clean_names()
cat("  nynta:", nrow(nynta), "NTAs\n")

#==============================================================================
# SECTION 2: CRIME CLASSIFICATION DEFINITIONS
#==============================================================================

cat("\nSETTING UP CRIME CLASSIFICATIONS...\n")
cat(strrep("-", 40), "\n")

# KY codes for crime types
# Source: NYPD complaint data codebook
violent_ky  <- c(101, 104, 105, 106, 344)
#   101 = Murder & Non-Negligent Manslaughter
#   104 = Rape (excluded due to geocoding issues)
#   105 = Robbery
#   106 = Felony Assault
#   344 = Misdemeanor Assault (Assault 3)

property_ky <- c(107, 109, 110)
#   107 = Burglary
#   109 = Grand Larceny
#   110 = Grand Larceny of Motor Vehicle

cat("  violent_ky:", paste(violent_ky, collapse = ", "), "\n")
cat("  property_ky:", paste(property_ky, collapse = ", "), "\n")

# Location keywords for outdoor/street classification
outside_loc_keywords <- c(

"FRONT OF", "OPPOSITE OF", "OUTSIDE", "REAR OF",
  "STREET", "IN STREET", "SIDEWALK"
)

outside_prem_keywords <- c(
  "PARK", "STREET", "PUBLIC PLACE", "HIGHWAY",
  "BRIDGE", "SIDEWALK", "VACANT LOT",
  "PUBLIC HOUSING AREA", "OUTSIDE"
)

outside_loc_pattern  <- str_c(outside_loc_keywords, collapse = "|")
outside_prem_pattern <- str_c(outside_prem_keywords, collapse = "|")

#==============================================================================
# SECTION 3: NYPD COMPLAINT DATA
#==============================================================================

cat("\nLOADING NYPD COMPLAINT DATA...\n")
cat(strrep("-", 40), "\n")

# Load current year complaints
complaints_current <- read_csv(
  here("data", "NYPD_Complaint_Data_Current_(Year_To_Date)_20251214.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(housing_psa = as.character(housing_psa))
cat("  complaints_current:", format(nrow(complaints_current), big.mark = ","), "records\n")

# Load historic complaints
complaints_historic <- read_csv(
  here("data", "NYPD_Complaint_Data_Historic_20251214.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(housing_psa = as.character(housing_psa))
cat("  complaints_historic:", format(nrow(complaints_historic), big.mark = ","), "records\n")

# Combine and filter
complaints <- bind_rows(complaints_historic, complaints_current) %>%
  # Remove menacing (not relevant for place-based intervention)
  filter(!pd_cd %in% c(111, 113, 186)) %>%
  # Remove rape (geocoding quality issues)
  filter(!ofns_desc == "RAPE") %>%
  filter(jurisdiction_code != 1) #no SUBWAY CRIME
cat("  complaints (combined, filtered):", format(nrow(complaints), big.mark = ","), "records\n")

# Create base sf object with outdoor flag
compl_base <- complaints %>%
  filter(!is.na(x_coord_cd), !is.na(y_coord_cd)) %>%
  st_as_sf(
    coords = c("x_coord_cd", "y_coord_cd"),
    crs    = 2263,
    remove = FALSE
  ) %>%
  mutate(
    date = mdy(rpt_dt),
    loc_of_occur_desc = str_to_upper(coalesce(loc_of_occur_desc, "")),
    prem_typ_desc     = str_to_upper(coalesce(prem_typ_desc, "")),
    is_outdoor = str_detect(loc_of_occur_desc, outside_loc_pattern) |
                 str_detect(prem_typ_desc, outside_prem_pattern)
  )

# Create crime type subsets
violent_crime <- compl_base %>%
  filter(ky_cd %in% violent_ky)
cat("  violent_crime:", format(nrow(violent_crime), big.mark = ","), "incidents\n")

violent_street_crime <- compl_base %>%
  filter(ky_cd %in% violent_ky, is_outdoor)
cat("  violent_street_crime:", format(nrow(violent_street_crime), big.mark = ","), "incidents\n")

property_crime <- compl_base %>%
  filter(ky_cd %in% property_ky)
cat("  property_crime:", format(nrow(property_crime), big.mark = ","), "incidents\n")

property_street_crime <- compl_base %>%
  filter(ky_cd %in% property_ky, is_outdoor)
cat("  property_street_crime:", format(nrow(property_street_crime), big.mark = ","), "incidents\n")

# Clean up intermediate object
rm(compl_base, complaints_current, complaints_historic)

#==============================================================================
# SECTION 4: NYPD SHOOTING DATA
#==============================================================================

cat("\nLOADING NYPD SHOOTING DATA...\n")
cat(strrep("-", 40), "\n")

# Load historic shootings
shootings_historic <- read_csv(
  here("data", "NYPD_Shooting_Incident_Data_(Historic)_20251215.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(statistical_murder_flag = as.character(statistical_murder_flag))

# Load current year shootings
shootings_current <- read_csv(
  here("data", "NYPD_Shooting_Incident_Data_(Year_To_Date)_20251215.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

# Combine
shootings <- bind_rows(shootings_historic, shootings_current)
cat("  shootings (raw):", format(nrow(shootings), big.mark = ","), "records\n")

# Create sf object
shootings_sf <- shootings %>%
  filter(!is.na(x_coord_cd), !is.na(y_coord_cd)) %>%
  mutate(date = mdy(occur_date)) %>%
  st_as_sf(
    coords = c("x_coord_cd", "y_coord_cd"),
    crs = 2263,
    remove = FALSE
  )
cat("  shootings_sf:", format(nrow(shootings_sf), big.mark = ","), "geocoded incidents\n")
cat("    Date range:", as.character(min(shootings_sf$date)), "to", 
    as.character(max(shootings_sf$date)), "\n")

# Clean up
rm(shootings_historic, shootings_current)

#==============================================================================
# SECTION 5: SHOTS FIRED DATA
#==============================================================================

cat("\nLOADING SHOTS FIRED DATA...\n")
cat(strrep("-", 40), "\n")

# Load older shots fired data (2017-2022)
shots_fired_since_2017 <- read_csv(
  here("data", "sf_since_2017.csv"),
  show_col_types = FALSE
) %>%
  clean_names()
cat("  shots_fired_since_2017:", format(nrow(shots_fired_since_2017), big.mark = ","), "records\n")

# Load newer shots fired data (2022+)
shots_fired_new <- read_csv(
  here("data", "shots_fired_new.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(date = mdy(rec_create_dt))
cat("  shots_fired_new:", format(nrow(shots_fired_new), big.mark = ","), "records\n")

# Define temporal handoff point
# The two datasets overlap; we use shots_fired_new starting from its earliest date
cutoff_date <- min(shots_fired_new$date, na.rm = TRUE)
cat("  Temporal cutoff:", as.character(cutoff_date), "\n")
cat("    sf_since_2017: records BEFORE", as.character(cutoff_date), "\n")
cat("    shots_fired_new: records FROM", as.character(cutoff_date), "onward\n")

# Standardize older dataset
sf_2017_std <- shots_fired_since_2017 %>%
  mutate(date = mdy(rpt_dt)) %>%
  filter(date < cutoff_date) %>%
  transmute(
    source = "shots_fired_since_2017",
    cmplnt_key = cmplnt_key,
    date   = as.Date(date),
    pct    = as.integer(pct),
    rpt_classfctn_desc = rpt_classfctn_desc,
    pd_desc   = pd_desc,
    ofns_desc = ofns_desc,
    x = as.numeric(x_coord_cd),
    y = as.numeric(y_coord_cd)
  )

# Standardize newer dataset
sf_new_std <- shots_fired_new %>%
  filter(date >= cutoff_date) %>%
  transmute(
    source = "shots_fired_new",
    date   = as.Date(date),
    cmplnt_key = cmplnt_key,
    pct    = as.integer(cmplnt_pct_cd),
    rpt_classfctn_desc = rpt_classfctn_desc,
    pd_desc   = NA_character_,
    ofns_desc = NA_character_,
    x = as.numeric(x_coordinate_code),
    y = as.numeric(y_coordinate_code)
  )

# Convert to sf before binding
sf_2017_sf <- sf_2017_std %>%
  filter(!is.na(x), !is.na(y)) %>%
  st_as_sf(coords = c("x", "y"), crs = 2263, remove = FALSE)

sf_new_sf <- sf_new_std %>%
  filter(!is.na(x), !is.na(y)) %>%
  st_as_sf(coords = c("x", "y"), crs = 2263, remove = FALSE)

# Combine into single dataset (no temporal overlap)
shots_fired <- bind_rows(sf_2017_sf, sf_new_sf) %>%
  distinct(cmplnt_key, .keep_all = T)
cat("  shots_fired (combined sf):", format(nrow(shots_fired), big.mark = ","), "geocoded incidents\n")
cat("    Date range:", as.character(min(shots_fired$date)), "to", 
    as.character(max(shots_fired$date)), "\n")

# Clean up
rm(shots_fired_since_2017, shots_fired_new, sf_2017_std, sf_new_std, 
   sf_2017_sf, sf_new_sf, cutoff_date)


shots_fired_sf <- shots_fired %>%
  st_as_sf(coords = c("x","y")) %>%
  st_set_crs(2263)

#==============================================================================
# SECTION 6: FACILITY DATA
#==============================================================================

cat("\nLOADING FACILITY DATA...\n")
cat(strrep("-", 40), "\n")

# NYPD precinct station house locations
nypd_precinct_locations <- read_csv(
  here("data", "nypd_precinct_locations.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

nypd_precinct_locations_sf <- nypd_precinct_locations %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(2263)
cat("  nypd_precinct_locations_sf:", nrow(nypd_precinct_locations_sf), "locations\n")

# Hospital locations
hospitals <- read_csv(
  here("data", "Health_Facility_General_Information_20260115.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  filter(facility_county %in% c("Bronx", "Queens", "New York", "Richmond", "Kings")) %>%
  filter(description == "Hospital")

hospitals_sf <- hospitals %>%
  filter(!is.na(facility_latitude), !is.na(facility_longitude)) %>%
  st_as_sf(
    coords = c("facility_longitude", "facility_latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(2263)
cat("  hospitals_sf:", nrow(hospitals_sf), "hospitals\n")

# Clean up
rm(nypd_precinct_locations, hospitals)

#==============================================================================
# SECTION 7: SUMMARY
#==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("DATA LOADING COMPLETE\n")
cat(strrep("=", 70), "\n\n")

cat("SPATIAL INFRASTRUCTURE:\n")
cat("  physical_blocks        :", format(nrow(physical_blocks), big.mark = ","), "blocks\n")
cat("  intersection_nodes     :", format(nrow(intersection_nodes), big.mark = ","), "nodes\n")
cat("  intersection_to_blocks :", format(nrow(intersection_to_blocks), big.mark = ","), "mappings\n")
cat("  nypp                   :", nrow(nypp), "precincts\n")
cat("  nynta                  :", nrow(nynta), "NTAs\n")

cat("\nCRIME DATA (sf objects):\n")
cat("  shootings_sf           :", format(nrow(shootings_sf), big.mark = ","), "incidents\n")
cat("  shots_fired            :", format(nrow(shots_fired), big.mark = ","), "incidents\n")
cat("  violent_crime          :", format(nrow(violent_crime), big.mark = ","), "incidents\n")
cat("  violent_street_crime   :", format(nrow(violent_street_crime), big.mark = ","), "incidents\n")
cat("  property_crime         :", format(nrow(property_crime), big.mark = ","), "incidents\n")
cat("  property_street_crime  :", format(nrow(property_street_crime), big.mark = ","), "incidents\n")

cat("\nFACILITY DATA (sf objects):\n")
cat("  hospitals_sf           :", nrow(hospitals_sf), "hospitals\n")
cat("  nypd_precinct_locations_sf:", nrow(nypd_precinct_locations_sf), "precinct locations\n")

cat("\nAll objects loaded. Ready for analysis.\n\n")

