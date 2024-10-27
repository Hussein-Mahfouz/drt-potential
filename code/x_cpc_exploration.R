library(tidyverse)
library(sf)
library(od)
library(tmap)

source("R/study_area_geographies.R")
source("R/filter_od_matrix.R")

plots_path <- "data/processed/plots/eda/cpc/"

# ----------------------- READ IN THE DATA ----------------------- #

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



# ----------------------- ADD GEOGRAPHIC COORDINATES TO ZONES ----------------------- #

#  ----- filter zones to study area

# filter matrices to keep study area zones only
cpc_matrices_all_internal <- cpc_matrices_all %>%
  filter(from_zone %in% zones_internal$zone_id & to_zone %in% zones_internal$zone_id)


# ----- Add desire lines

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

# od::od_to_sf

# cpc_matrices_all_internal_sf <- od::od_to_sf(x = cpc_matrices_all_internal,
#                                              z = study_area_cpc_internal,
#                                              silent = FALSE)

# remove short ods
cpc_matrices_all_internal <- cpc_matrices_all_internal %>%
  rename(Origin = from_msoa, Destination = to_msoa)

# Filter matrix by distance also adds desire lines
cpc_matrices_all_internal_sf <- filter_matrix_by_distance(zones = study_area_large, od_matrix = cpc_matrices_all_internal, dist_threshold = 1000)









map_cpc_flows = function(od_matrix,
                         area,
                         column_to_map,
                         cols,
                         min_flows){

  # --- study_area
  # all of WY
  if (area == "large"){
    study_area = study_area_large
  # Leeds only
  } else if (area == "small") {
    study_area = study_area
  }

  # ----- We filter the od_matrix to only include flows that start and end in the study_Area
  od_matrix = od_matrix %>%
    filter(Origin %in% study_area$MSOA11CD &
           Destination %in% study_area$MSOA11CD)
  # arrange so that highest flows are plotting on top
  od_matrix = od_matrix %>% arrange(!!sym(column_to_map))

  # ----- Plot

  # Use the dynamic column name in the filter and tm_lines functions
  tm_shape(study_area) +
    tm_borders(col = "grey80", alpha = 0.7) +
    tm_shape(od_matrix %>%
               filter((!!sym(column_to_map)) > min_flows)) +
    tm_lines(col = column_to_map,
             title.col = "Number of people",
             lwd = column_to_map,
             legend.lwd.show = FALSE,
             palette = "YlGnBu", # YlOrRd # PuBuGn
             scale = 7) +
    tm_facets(by = "source",
              free.coords = FALSE,
              ncol = cols) +
    # START AND ENDPOINTS
    tm_shape(od_matrix %>%
               filter((!!sym(column_to_map)) > min_flows) %>%
               mutate(geometry = lwgeom::st_startpoint(.))) +
    tm_dots(col = "darkgreen",
            alpha = 0.4,
            #jitter = 0.1,
            size = column_to_map,
            scale = 0.5,
            legend.size.show = FALSE) +
    tm_facets(by = "source",
              free.coords = FALSE,
              ncol = cols,
              showNA = FALSE) +
    tm_shape(od_matrix %>%
               filter((!!sym(column_to_map)) > min_flows) %>%
               mutate(geometry = lwgeom::st_endpoint(.))) +
    tm_dots(col = "red",
            alpha = 0.4,
            #jitter = 0.1,
            size = column_to_map,
            scale = 0.5,
            legend.size.show = FALSE) +
    tm_facets(by = "source",
              free.coords = FALSE,
              ncol = cols,
              showNA = FALSE) +
    tm_layout(fontfamily = 'Georgia',
              main.title = paste0("Flows: Purpose = ", column_to_map),
              main.title.size = 1.1,
              main.title.color = "azure4",
              main.title.position = "left",
              panel.label.size = 1,
              panel.label.bg.color = NA,
              # legend.outside = TRUE,
              # legend.outside.position = "bottom",
              # legend.stack = "horizontal",
              frame = FALSE) +
    tm_add_legend(type = "symbol", labels = 'OD Start', col = 'darkgreen') +
    tm_add_legend(type = "symbol", labels = 'OD End', col = 'red') -> res

  res

  tmap_save(tm = res, filename = paste0(plots_path, "cpc_facet_", column_to_map, "_scope_", area, ".png"), width = 12, dpi = 1080, asp = 0)







  }

# # Apply the function for a specific column
# map_cpc_flows(od_matrix = cpc_matrices_all_internal_sf,
#               area = "small", # c(small, large)
#               column_to_map = "hbo_outbound",
#               cols = 5,
#               min_flows = 20)


# Apply to all columns
columns = c("hbw_outbound", "hbw_inbound", "hbo_outbound", "hbo_inbound", "nhb", "total_flow")

# SMALL (Leeds Boundary)
purrr::map(.x = columns, ~map_cpc_flows(od_matrix = cpc_matrices_all_internal_sf,
                                        area = "small",
                                        column_to_map = .x,
                                        cols = 5,
                                        min_flow = 10))

# LARGE (West Yorskshire)
purrr::map(.x = columns, ~map_cpc_flows(od_matrix = cpc_matrices_all_internal_sf,
                                        area = "large",
                                        column_to_map = .x,
                                        cols = 5,
                                        min_flow = 10))
