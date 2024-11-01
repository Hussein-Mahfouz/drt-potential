### the purpose of this script is to work with travel demand data from cpc  ###
### the data is only available at MSOA level. It is cleaned and joined onto ###
### the travel time results

library(tidyverse)
library(sf)


source("R/study_area_geographies.R")
source("R/filter_od_matrix.R")

# --------------- 1. READ IN THE DEMAND DATA

# ------- Zoning layer (2011 MSOAs)

zones <- read_csv("data/external/cpc/ZoningTemplateFilled_lad_scot_wales.csv")
# study area zones
zones_internal = zones %>%
  filter(zone_type == 1)

# ------- CPC data

# Define the path to the ZIP file
zip_file_path <- "data/external/cpc/cpc_west_yorkshire_weekdays_march_may.zip"
# Create a temporary directory to extract the files
temp_dir <- tempdir()
# Extract the ZIP file to the temporary directory
unzip(zip_file_path, exdir = temp_dir)
# Get a list of all CSV files in the temporary directory
csv_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)

# Read each CSV file into a list of data frames
cpc_matrices <- purrr::map(csv_files, read.csv)

# Extract the part of the filename after the underscore and before the .csv extension
file_names <- basename(csv_files)
short_names <- stringr::str_extract(file_names, "(?<=_).*(?=\\.csv)")

# Name each data frame in the list by its original file name
names(cpc_matrices) <- short_names

# --- Combine the dfs

# Add a new column to each data frame with the name of the data frame
cpc_matrices <- purrr::map2(cpc_matrices, short_names, ~mutate(.x, source = .y))

# Combine into one df
cpc_matrices_all <- bind_rows(cpc_matrices)

# replace NA values with 0
cols <- c("hbw_outbound", "hbw_inbound", "hbo_outbound", "hbo_inbound", "nhb")

cpc_matrices_all <- cpc_matrices_all %>%
  mutate(across(all_of(cols), ~replace_na(., 0)))

# Add column to sum all flow types
cpc_matrices_all <- cpc_matrices_all %>%
  mutate(total_flow = rowSums(across(hbw_outbound:nhb), na.rm = TRUE))

# ------- Geo boundaries

# England
msoa_shp_2011 <- st_read("data/external/msoa_england_2011/infuse_msoa_lyr_2011_clipped.shp")

# West Yorkshire (All zones with zone_type == 1)

study_area_large <- msoa_shp_2011 %>%
  filter(geo_code %in% zones_internal$msoa) %>%
  rename(MSOA11CD = geo_code)

# Leeds

# --- administrative boundaries
study_area <- st_read("data/interim/study_area_boundary.geojson")
# convert to desired resolution
geography = "MSOA"
study_area = study_area_geographies(study_area = study_area,
                                    geography = geography)

# ADD 2011 MSOA codes to study area
msoa_2011_2021_lookup <- read_csv("data/external/cpc/MSOA_2011_2021.csv")

study_area <- study_area %>%
  left_join(msoa_2011_2021_lookup %>%
              select(c("MSOA11CD", "MSOA21CD")),
            by = "MSOA21CD")


# ----------------------- 2. ADD MSOA CODES AND FILTER TO INTERNAL FLOWS ----------------------- #

#  ----- filter zones to study area

# filter matrices to keep study area zones only (i.e intrazonal flows only)
cpc_matrices_all_internal <- cpc_matrices_all %>%
  filter(from_zone %in% zones_internal$zone_id & to_zone %in% zones_internal$zone_id)

# prepare data for od::od_to_sf()

# ----- add msoa 2011 codes
cpc_matrices_all_internal <- cpc_matrices_all_internal %>%
  # join origin data
  left_join(zones_internal %>%
              select(msoa, zone_id, msoa_name) %>%
              rename(from_msoa = msoa, from_msoa_name = msoa_name),
            by = c("from_zone" = "zone_id")) %>%
  # join destination data
  left_join(zones_internal %>%
              select(msoa, zone_id, msoa_name) %>%
              rename(to_msoa = msoa, to_msoa_name = msoa_name),
            by = c("to_zone" = "zone_id")) %>%
  # move msoa codes to first two columns (for od::od_to_sf())
  relocate("from_msoa", .before = everything()) %>%
  relocate("to_msoa", .after = "from_msoa")


# Filter to Leeds only
cpc_matrices_all_internal <- cpc_matrices_all_internal %>%
  filter(from_msoa %in% study_area$MSOA11CD & to_msoa %in% study_area$MSOA11CD)

# -------------- 3.  JOIN TRAVEL TIME DATA ---------- #

# ----------  MSOA level

# read in the data
tt_matrix_msoa <- arrow::read_parquet("data/processed/travel_times/MSOA/travel_time_matrix_expanded.parquet")

# some OD pairs don't have travel time data (for some combinations). Expand grid so that we explitly mention these OD pairs
tt_matrix_msoa_exp <- tidyr::crossing(from_id = tt_matrix_msoa$from_id, to_id = tt_matrix_msoa$to_id, combination = tt_matrix_msoa$combination)

tt_matrix_msoa <- tt_matrix_msoa_exp %>%
  left_join(tt_matrix_msoa, by = c("from_id", "to_id", "combination"))



# add metadata (MSOA for each home and workplace zone)
tt_matrix_msoa <- tt_matrix_msoa %>%
  left_join(study_area %>%
              select(MSOA21CD, MSOA11CD, OBJECTID) %>%
              st_drop_geometry() %>%
              rename_with(~paste0(., "_home")) %>%
              mutate(across(everything(), ~as.character(.))),
            by = c("from_id" = "OBJECTID_home")) %>%
  left_join(study_area %>%
              select(MSOA21CD, MSOA11CD, OBJECTID) %>%
              st_drop_geometry() %>%
              rename_with(~paste0(., "_work")) %>%
              mutate(across(everything(), ~as.character(.))),
            by = c("to_id" = "OBJECTID_work"))


# ----- Prepare time_of_day column in tt_matrix to match demand matrix

# keep only pt travel times
tt_matrix_msoa <- tt_matrix_msoa %>%
  filter(str_detect(combination, "pt_"))

# add combination column to cpc data
cpc_matrices_all_internal <- cpc_matrices_all_internal %>%
  mutate(combination = case_when(
    source %in% c("05-06", "06-07", "07-08", "08-09", "09-10", "10-11", "11-12") ~ "pt_wkday_morning",
    source %in% c("12-13", "13-14", "14-15", "15-16", "16-17") ~ "pt_wkday_afternoon",
    source %in% c("17-18", "18-19", "19-20") ~ "pt_wkday_evening")
    )

# group by combination column
cols_to_sum = c("hbw_outbound", "hbw_inbound", "hbo_outbound", "hbo_inbound", "nhb", "total_flow")

cpc_matrices_all_internal_grouped <- cpc_matrices_all_internal %>%
  group_by(from_msoa, to_msoa, combination) %>%
  summarise(across(cols_to_sum, sum, na.rm = TRUE)) %>%
  ungroup()


# join travel time data and census commute data
#TODO: join on time of day also!!!

cpc_matrices_all_internal_tt <- cpc_matrices_all_internal_grouped %>%
  left_join(tt_matrix_msoa, by = c("from_msoa" = "MSOA11CD_home", "to_msoa" = "MSOA11CD_work", "combination"))


# # ---------- 4. ADD DESIRE LINES ---------- #
#
# # # Filter matrix by distance also adds desire lines
# cpc_matrices_all_internal_tt_sf <- filter_matrix_by_distance(zones = study_area_large,
#                                                           od_matrix = cpc_matrices_all_internal_tt,
#                                                           dist_threshold = 500)



# save
#write_csv(cpc_matrices_all_internal_tt, "data/raw/travel_demand/cpc_matrices_2019/demand_study_area_msoa.csv")
arrow::write_parquet(cpc_matrices_all_internal_tt, "data/raw/travel_demand/cpc_matrices_2019/demand_study_area_msoa.parquet")



