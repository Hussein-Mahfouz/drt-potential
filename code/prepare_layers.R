###################################################################################################
###    The purpose of this script is to prepare the base layers used in the analysis. This      ###
###    includes creating a layer of Output Areas, and joining columns on the other              ###
###    corresponding statistical areas that match with each OA                                  ###
###################################################################################################

source("R/utils.R")


library(sf)
library(tidyverse)

# ----- download data from github release

# gtfs data
gh_release_download(tag = "initial-inputs",
                    pattern = "gtfs_england_06_23.zip",
                    dest = "data/raw")


# OA boundaries
gh_release_download(tag = "initial-inputs",
                    pattern = "OA_Dec_2021_Boundaries_Generalised_Clipped_EW_BGC_2022.geojson",
                    dest = "data/external")

# LOOKUP tables
gh_release_download(tag = "initial-inputs",
                    pattern = "LOOKUP_OA_LSOA_MSOA_LEP_LAD_Dec_2022.csv",
                    dest = "data/external")

gh_release_download(tag = "initial-inputs",
                    pattern = "LOOKUP_OA21_RGN22.csv",
                    dest = "data/external")

gh_release_download(tag = "initial-inputs",
                    pattern = "LOOKUP_OA21_CTRY22.csv",
                    dest = "data/external")



# -----  load in the datasets

# oa spatial layer
layer_oa <- st_read("data/raw/OA_Dec_2021_Boundaries_Generalised_Clipped_EW_BGC_2022.geojson")

# lookup tables
lookup_hierarchy <- read_csv("data/external/LOOKUP_OA_LSOA_MSOA_LEP_LAD_Dec_2022.csv") %>%
  select(-c("ObjectId", "LEP22CD2", "LEP22NM2"))

lookup_oa_region <- read_csv("data/external/LOOKUP_OA21_RGN22.csv") %>%
  select(-c("rgn22nmw"))

lookup_oa_country <- read_csv("data/external/LOOKUP_OA21_CTRY22.csv") %>%
  select(-c("ctry22nmw"))

# -----  Join metadata

# join the lookup data onto the OA spatial layer
layer_oa_heirarchy <- layer_oa %>%
  left_join(lookup_hierarchy, by = "OA21CD") %>%
  left_join(lookup_oa_region, by = c("OA21CD" = "oa21cd")) %>%
  left_join(lookup_oa_country, by = c("OA21CD" = "oa21cd"))

# check join
sum(is.na(layer_oa_heirarchy$LSOA21NM))

# Remove Wales OAs
layer_oa_heirarchy <- layer_oa_heirarchy %>%
  filter(ctry22nm == "England")


# ----- save

st_write(layer_oa_heirarchy, "data/interim/oa_england.geojson", delete_dsn =TRUE)
