### the purpose of this script is to work with travel demand data from census 2021 ###
### the data is cleaned at different geo levels (OA, LSOA, MSOA) and joined onto   ###
### the travel time results

library(tidyverse)
library(sf)


# --------------- 1. read in the data

# ----- census od data

# Replace 'your_archive.zip' with the actual name of your zip file.
zip_file <- "data/raw/travel_demand/od_census_2021/odwp01ew.zip"

# Replace 'file_inside_zip.csv' with the name of the file you want to extract and read.
file_to_extract <- "ODWP01EW_OA.csv"

# Unzip the file to a temporary directory.
temp_dir <- tempdir()
unzip(zip_file, exdir = temp_dir)

# Read the file from the temporary directory.
file_path <- file.path(temp_dir, file_to_extract)
census_work_oa <- read_csv(file_path)  # Use the appropriate read function (e.g., read_csv, read.table, readRDS, etc.) for your file format.


# ----- study area boundary
study_area <- st_read("data/interim/study_area_boundary.geojson")


# ------------- 2. filter to study area

census_work_oa_clipped <- census_work_oa %>%
  filter(`Output Areas code` %in% study_area$OA21CD |
         `OA of workplace code` %in% study_area$OA21CD)

# ------------- 3. Remove unnecessary columns and rename others

census_work_oa_clipped <- census_work_oa_clipped %>%
  select(-c(`Output Areas label`, `OA of workplace label`)) %>%
  rename(OA21CD_home = `Output Areas code`,
         OA21CD_work = `OA of workplace code`)


# save
write_csv(census_work_oa_clipped, "data/raw/travel_demand/od_census_2021/demand_study_area_oa.csv")


# -------------- 4. Convert to LSOA and MSOA level

# ----- prepare MSOA and LSOA columns for joining
study_area_home <- study_area %>%
  st_drop_geometry() %>%
  select(OBJECTID, OA21CD, LSOA21CD, MSOA21CD) %>%
  rename_with(~paste0(., "_home")) %>%
  mutate(across(everything(), ~as.character(.)))

study_area_work <- study_area %>%
  st_drop_geometry() %>%
  select(OBJECTID, OA21CD, LSOA21CD, MSOA21CD) %>%
  rename_with(~paste0(., "_work")) %>%
  mutate(across(everything(), ~as.character(.)))


# join LSOA and MSOA columns
census_work_oa <- census_work_oa_clipped %>%
  left_join(study_area_home, by = "OA21CD_home") %>%
  left_join(study_area_work, by = "OA21CD_work")

# Remove rows where any of the specified columns have NA
specified_columns <- c("LSOA21CD_home", "LSOA21CD_work", "MSOA21CD_home", "MSOA21CD_work")

census_work_oa <- census_work_oa[complete.cases(census_work_oa[, specified_columns]), ]

# ----- LSOA data
census_work_lsoa <- census_work_oa %>%
  group_by(LSOA21CD_home, LSOA21CD_work, `Place of work indicator (4 categories) code`, `Place of work indicator (4 categories) label`) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  ungroup()

# ----- MSOA data
census_work_msoa <- census_work_oa %>%
  group_by(MSOA21CD_home, MSOA21CD_work, `Place of work indicator (4 categories) code`, `Place of work indicator (4 categories) label`) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  ungroup()


# -------------- 5.  Join travel time data

# ---------- OA level

# read in the data
tt_matrix_oa <- arrow::read_parquet("data/processed/travel_times/OA/travel_time_matrix_expanded.parquet")

# add metadata (OA, LSOA, and MSOA for each home and workplace zone)
tt_matrix_oa <- tt_matrix_oa %>%
  left_join(study_area_home, by = c("from_id" = "OBJECTID_home")) %>%
  left_join(study_area_work, by = c("to_id" = "OBJECTID_work"))

# get multiple columns for commute types
census_work_oa <- census_work_oa %>%
  pivot_wider(id_cols = c(OA21CD_home, OA21CD_work),
              names_from = `Place of work indicator (4 categories) label`,
              values_from = Count) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)),
         commute_all = rowSums(across(where(is.numeric))))

# join travel time data and census commute data
census_work_oa_tt <- census_work_oa %>%
  left_join(tt_matrix_oa, by = c("OA21CD_home", "OA21CD_work"))

# save
#write_csv(census_work_oa_tt, "data/raw/travel_demand/od_census_2021/demand_study_area_oa.csv")
arrow::write_parquet(census_work_oa_tt, "data/raw/travel_demand/od_census_2021/demand_study_area_oa.parquet")



# ---------- LSOA level

# read in the data
tt_matrix_lsoa <- arrow::read_parquet("data/processed/travel_times/LSOA/travel_time_matrix_expanded.parquet")

# add metadata (OA, LSOA, and MSOA for each home and workplace zone)
tt_matrix_lsoa <- tt_matrix_lsoa %>%
  left_join(study_area_home, by = c("from_id" = "OBJECTID_home")) %>%
  left_join(study_area_work, by = c("to_id" = "OBJECTID_work"))

# get multiple columns for commute types
census_work_lsoa <- census_work_lsoa %>%
  pivot_wider(id_cols = c(LSOA21CD_home, LSOA21CD_work),
              names_from = `Place of work indicator (4 categories) label`,
              values_from = Count) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)),
         commute_all = rowSums(across(where(is.numeric))))

# join travel time data and census commute data
census_work_lsoa_tt <- census_work_lsoa %>%
  left_join(tt_matrix_lsoa, by = c("LSOA21CD_home", "LSOA21CD_work"))

# save
#write_csv(census_work_lsoa_tt, "data/raw/travel_demand/od_census_2021/demand_study_area_lsoa.csv")
arrow::write_parquet(census_work_lsoa_tt, "data/raw/travel_demand/od_census_2021/demand_study_area_lsoa.parquet")


# ----------  MSOA level

# read in the data
tt_matrix_msoa <- arrow::read_parquet("data/processed/travel_times/MSOA/travel_time_matrix_expanded.parquet")

# add metadata (OA, LSOA, and MSOA for each home and workplace zone)
tt_matrix_msoa <- tt_matrix_msoa %>%
  left_join(study_area_home, by = c("from_id" = "OBJECTID_home")) %>%
  left_join(study_area_work, by = c("to_id" = "OBJECTID_work"))

# get multiple columns for commute types
census_work_msoa <- census_work_msoa %>%
  pivot_wider(id_cols = c(MSOA21CD_home, MSOA21CD_work),
              names_from = `Place of work indicator (4 categories) label`,
              values_from = Count) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)),
         commute_all = rowSums(across(where(is.numeric))))

# join travel time data and census commute data
census_work_msoa_tt <- census_work_msoa %>%
  left_join(tt_matrix_msoa, by = c("MSOA21CD_home", "MSOA21CD_work"))

# save
#write_csv(census_work_msoa_tt, "data/raw/travel_demand/od_census_2021/demand_study_area_msoa.csv")
arrow::write_parquet(census_work_msoa_tt, "data/raw/travel_demand/od_census_2021/demand_study_area_msoa.parquet")


