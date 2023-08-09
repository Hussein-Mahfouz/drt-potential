###################################################################################################
###    The purpose of this script is to download osm pbf files for a chosen bounding box        ###
###    (through the osmextract r package)                                                       ###
###################################################################################################


library(tidyverse)
library(sf)
library(osmextract)

sf_use_s2(FALSE)

# ----- where do we want to save the data
download_directory <- "data/external/"

# ----------------------------------------- PREPARE BOUNDING BOX ----------------------------------------- #

study_area <- st_read("data/interim/study_area_boundary.geojson")

# ----------------------------------------- DOWNLOAD THE DATA ----------------------------------------- #

# ----- use osmextract to download a pbf file

# check what layer the study area is matching to
oe_match(study_area)
#oe_match(study_area, level = 3) # does not work
#oe_match(study_area, provider = "bbbike") # does not work


# if it is matching to an incorrect area (or the whole of England), you can download manually from here https://download.geofabrik.de/europe/great-britain/england/

osmextract::oe_get(place = study_area,
                   layer = "lines",
                   provider = "geofabrik",
                   download_directory = download_directory,
                   # do not translate to gpkg - keep osm.pbf format
                   skip_vectortranslate = TRUE)
