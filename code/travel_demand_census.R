### the purpose of this script is to work with travel demand data from census 2021 ###

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



write_csv(census_work_oa_clipped, "data/raw/travel_demand/od_census_2021/demand_study_area_oa.csv")

# join travel time data
tt_matrix <- arrow::read_parquet("data/processed/travel_times/MSOA/travel_time_matrix_expanded.parquet")
