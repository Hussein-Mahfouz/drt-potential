### the purpose of this script is to work with travel demand data from census 2021 ###
### IT IS THE SAME AS travel_demand_census.R, but uses MSOA DATA BY MODE           ###

library(tidyverse)
library(sf)


source("R/study_area_geographies.R")
# --------------- 1. read in the data

# ----- census od data

# Replace 'your_archive.zip' with the actual name of your zip file.
zip_file <- "data/raw/travel_demand/od_census_2021/ODWP15EW_MSOA_v1.zip"

# Replace 'file_inside_zip.csv' with the name of the file you want to extract and read.
file_to_extract <- "ODWP15EW_MSOA.csv"

# Unzip the file to a temporary directory.
temp_dir <- tempdir()
unzip(zip_file, exdir = temp_dir)

# Read the file from the temporary directory.
file_path <- file.path(temp_dir, file_to_extract)
census_work_msoa <- read_csv(file_path)  # Use the appropriate read function (e.g., read_csv, read.table, readRDS, etc.) for your file format.


# ----- study area boundary
study_area <- st_read("data/interim/study_area_boundary.geojson")

# convert to desired resolution
geography = "MSOA"
study_area = study_area_geographies(study_area = study_area,
                                    geography = geography)

study_area <- study_area %>%
  st_cast("MULTIPOLYGON")


# ------------- 2. filter to study area

census_work_msoa_clipped <- census_work_msoa %>%
  filter(`Middle layer Super Output Areas code` %in% study_area$MSOA21CD |
           `MSOA of workplace code` %in% study_area$MSOA21CD)

# ------------- 3. Clean the data



# ----- create new column with more friendly commute mode
census_work_msoa_clipped <- census_work_msoa_clipped %>%
  mutate(commute_mode = case_when(`Method used to travel to workplace (12 categories) label` == "Bus, minibus or coach" ~ "bus",
                                  `Method used to travel to workplace (12 categories) label` == "Driving a car or van" ~ "car_driver",
                                  `Method used to travel to workplace (12 categories) label` == "Train" ~ "rail_heavy",
                                  `Method used to travel to workplace (12 categories) label` == "Underground, metro, light rail, tram" ~ "rail_light",
                                  `Method used to travel to workplace (12 categories) label` == "On foot" ~ "walk",
                                  `Method used to travel to workplace (12 categories) label` == "Taxi" ~ "car_taxi",
                                  `Method used to travel to workplace (12 categories) label` == "Other method of travel to work" ~ "other",
                                  `Method used to travel to workplace (12 categories) label` == "Bicycle" ~ "bicycle",
                                  `Method used to travel to workplace (12 categories) label` == "Passenger in a car or van" ~ "car_passenger",
                                  `Method used to travel to workplace (12 categories) label` == "Motorcycle, scooter or moped" ~ "motorcycle",
                                  `Method used to travel to workplace (12 categories) label` == "Work mainly at or from home" ~ "home"))

# ----- rename some columns and remove others

census_work_msoa_clipped <- census_work_msoa_clipped %>%
  # keep only people who are commuting
  # code = 3; "Working in the UK but not working at or from home"
  # code = -8; "Does not apply" (students and retired people) : REMOVED IT AS THEY AREN'T ASSIGNED A MODE OF TRAVEL
  filter(`Place of work indicator (4 categories) code` == 3) %>%
  select(-c(`Middle layer Super Output Areas label`, `MSOA of workplace label`,
            `Method used to travel to workplace (12 categories) label`, `Method used to travel to workplace (12 categories) code`,
            `Place of work indicator (4 categories) code`, `Place of work indicator (4 categories) label`)) %>%
  # rename columns for easier indexing
  rename(MSOA21CD_home = `Middle layer Super Output Areas code`,
         MSOA21CD_work = `MSOA of workplace code`)




# ----- Aggregate the Count figures. We don't care about disaggregation by SEX or AGE
census_work_msoa_clipped <- census_work_msoa_clipped %>%
  group_by(MSOA21CD_home, MSOA21CD_work, commute_mode) %>%
  summarise(Count = sum(Count)) %>%
  ungroup()

# ----- Get one column per mode

census_work_msoa_clipped <- census_work_msoa_clipped %>%
  pivot_wider(names_from = commute_mode,
              values_from = Count,
              values_fill = 0,
              names_prefix = "commute_")

# ---- summarise some columns
census_work_msoa_clipped <- census_work_msoa_clipped %>%
  rowwise() %>%
  # sum of all modes
  mutate(commute_all = sum(across(where(is.numeric)))) %>%
           ungroup() %>%
  # one column for "car" and one for "rail"
  mutate(commute_car = commute_car_driver + commute_car_passenger + commute_car_taxi,
         commute_rail = commute_rail_heavy + commute_rail_light)





# save
write_csv(census_work_msoa_clipped, "data/raw/travel_demand/od_census_2021/demand_study_area_msoa_mode.csv")


# -------------- 4. Convert to LSOA and MSOA level

# ----- prepare MSOA and LSOA columns for joining
study_area_home <- study_area %>%
  st_drop_geometry() %>%
  select(OBJECTID, MSOA21CD) %>%
  rename_with(~paste0(., "_home")) %>%
  mutate(across(everything(), ~as.character(.)))

study_area_work <- study_area %>%
  st_drop_geometry() %>%
  select(OBJECTID, MSOA21CD) %>%
  rename_with(~paste0(., "_work")) %>%
  mutate(across(everything(), ~as.character(.)))


# join LSOA and MSOA columns
census_work_msoa <- census_work_msoa_clipped %>%
  left_join(study_area_home, by = "MSOA21CD_home") %>%
  left_join(study_area_work, by = "MSOA21CD_work")

# Remove rows where any of the specified columns have NA
specified_columns <- c("MSOA21CD_home", "MSOA21CD_work")

census_work_msoa <- census_work_msoa[complete.cases(census_work_msoa[, specified_columns]), ]





# -------------- 5.  Join travel time data



# ----------  MSOA level

# read in the data
tt_matrix_msoa <- arrow::read_parquet("data/processed/travel_times/MSOA/travel_time_matrix_expanded.parquet")

# some OD pairs don't have travel time data (for some combinaitons). Expand grid so that we explitly mention these OD pairs
tt_matrix_msoa_exp <- tidyr::crossing(from_id = tt_matrix_msoa$from_id, to_id = tt_matrix_msoa$to_id, combination = tt_matrix_msoa$combination)

tt_matrix_msoa <- tt_matrix_msoa_exp %>%
  left_join(tt_matrix_msoa, by = c("from_id", "to_id", "combination"))



# add metadata (OA, LSOA, and MSOA for each home and workplace zone)
tt_matrix_msoa <- tt_matrix_msoa %>%
  left_join(study_area_home, by = c("from_id" = "OBJECTID_home")) %>%
  left_join(study_area_work, by = c("to_id" = "OBJECTID_work"))


# join travel time data and census commute data
census_work_msoa_tt <- census_work_msoa %>%
  left_join(tt_matrix_msoa, by = c("MSOA21CD_home", "MSOA21CD_work"))

# save
#write_csv(census_work_msoa_tt, "data/raw/travel_demand/od_census_2021/demand_study_area_msoa.csv")
arrow::write_parquet(census_work_msoa_tt, "data/raw/travel_demand/od_census_2021/demand_study_area_msoa_mode.parquet")


