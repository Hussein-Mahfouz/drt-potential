### the purpose of this script is to analyse the performance of OD pairs ###
### performance is measure by: speed of travl ...

library(tidyverse)
library(sf)
library(tmap)


source("R/study_area_geographies.R")
source("R/filter_od_matrix.R")


# ----------- 1. Load in the data


# ----- Study area

# --- administrative boundaries
study_area <- st_read("data/interim/study_area_boundary.geojson")

# convert to desired resolution
geography = "MSOA"
study_area = study_area_geographies(study_area = study_area,
                                    geography = geography)

study_area <- study_area %>%
  st_cast("MULTIPOLYGON")



# ----- Travel time and demand matrix

od_demand <- arrow::read_parquet(paste0("data/raw/travel_demand/od_census_2021/demand_study_area_", tolower(geography), ".parquet"))



# ----------- 2. Add desire lines and filter OD pairs

# we don't want OD pairs below a certain distance (say 1km)


from_id_col = paste0(geography, "21CD_home")
to_id_col = paste0(geography, "21CD_work")

# rename columns for function below
od_demand <- od_demand %>%
  rename(Origin = all_of(from_id_col),
         Destination = all_of(to_id_col))



# filter od pairs by euclidian distance
od_demand_sf = filter_matrix_by_distance(zones = study_area,
                                         od_matrix = od_demand,
                                         dist_threshold = 1000)


# ----------- 3. Get travel speeds

od_demand_sf <- od_demand_sf %>%
  mutate(speed_mps = distance_m / (total_time*60),
         speed_kph = speed_mps * 3.6)



# ----------- 4. Rank OD pairs

# group by combination as pt schedules differ throughout the day

od_demand_sf_rank <- od_demand_sf %>%
  group_by(combination) %>%
  mutate(speed_percentile = percent_rank(speed_mps),
         speed_percentile_fct = cut(speed_percentile,
                                    breaks = seq(0, 1, by = 0.25),
                                    include.lowest = TRUE))

# turn columns to character
od_demand_sf_rank <- od_demand_sf_rank %>%
  mutate(n_rides = round(n_rides))


# ---------- 5. Create communitiews based on demand (or travel time?)
   # diffeent community for PT and PVT

# ----------- 5. Plots

# ----------- prep the layer
# TODO: remove this when we have multiple start times (currently the od_supply df only has 7:30 - WHY? )
od_demand_sf_rank <- od_demand_sf_rank %>%
  filter(combination == "pt_wkday_morning")


# ----------- 6. Maps


tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(od_demand_sf_rank) +
  tm_lines(col = "commute_all",
           lwd = "commute_all",
           legend.lwd.show = FALSE,
           alpha = 0.8,
           title.col = "Demand between OD pair",
           legend.col.is.portrait = FALSE,
           #palette = "Blues",
           style = "quantile",
           scale = 5) +
  tm_facets(by = "speed_percentile_fct",
            free.coords = FALSE,
            nrow = 2) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Demand between OD pairs\nfaceted by speed (percentile) between OD pairs",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE)


# slowest OD pairs

tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
tm_shape(study_area) +
  tm_fill(col = "grey80",
             alpha = 0.5) +
  tm_shape(od_demand_sf_rank %>%
             filter(speed_percentile <= 0.25)) +
  tm_lines(col = "commute_all",
           lwd = "commute_all",
           scale = 10,
           legend.lwd.show = FALSE,
           alpha = 1,
           title.col = "Demand between OD pair",
           legend.col.is.portrait = FALSE) +
           #palette = "Blues",
           #style = "quantile",
  tm_layout(fontfamily = 'Georgia',
            main.title = "Demand between OD pairs\n(slowest 25% of OD pairs)",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE)


tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey80",
          alpha = 0.5) +
  tm_shape(od_demand_sf_rank %>%
             filter(speed_percentile <= 0.25)) +
  tm_lines(col = "commute_all",
           lwd = "commute_all",
           scale = 10,
           #palette = "Blues",
           #style = "quantile",
           legend.lwd.show = FALSE,
           alpha = 1,
           title.col = "Demand between OD pair",
           legend.col.is.portrait = FALSE) +
  tm_facets(by = "n_rides",
            free.coords = FALSE,
            nrow = 2) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Demand between OD pairs\n(slowest 25% of OD pairs)",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE)
