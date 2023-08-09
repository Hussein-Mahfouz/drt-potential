### the purpose of this script is to create and save a study area polygon which ###
### can be used throughout the analysis pipeline                                ###

library(tidyverse)
library(sf)

# ----- load in spatial layer for all of england
england_sf <- st_read("data/interim/oa_england.geojson") %>%
  st_transform(4326) %>%
  st_make_valid()

# ----- filter to the spatial polygon of our study area
study_area <- england_sf %>%
  filter(stringr::str_detect(MSOA21NM, "Leeds"))

# # other options:
# study_area <- england_sf %>%
#   filter(rgn22nm == "Yorkshire and The Humber")
# study_area <- england_sf %>%
#   filter(LEP22NM1 == "Leeds City Region")   # (West Yorkshire)
# study_area <- england_sf %>%
#   filter(stringr::str_detect(MSOA21NM, "Leeds"))

# ----- save the layer
st_write(study_area, "data/interim/study_area_boundary.geojson", delete_dsn = TRUE)
