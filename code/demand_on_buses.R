
library(tidyverse)
library(sf)
library(gtfstools)
library(tmap)

source("R/study_area_geographies.R")
source("R/trips_to_zone_pairs.R")


########## ----------------------- Read in the data ----------------------- ##########

# ----------- 1. Study area

# --- administrative boundaries
study_area <- st_read("data/interim/study_area_boundary.geojson")

# convert to desired resolution
res = "OA"
study_area = study_area_geographies(study_area = study_area,
                                    geography = res)

# ----------- 2. GTFS feeds

# --- Identify GTFS file paths

# parent directory where feeds are saved
gtfs_dir <- "data/interim/gtfs_freq/"
# get name of gtfs feeds
gtfs_names = list.files(gtfs_dir, pattern = ".zip")
# get path for each feed
gtfs_paths = paste0(gtfs_dir, gtfs_names)


# --- read in the feeds
gtfs_bus <- gtfstools::read_gtfs(gtfs_paths[grepl("bus", gtfs_paths)])
#gtfs_rail <- gtfstools::read_gtfs(gtfs_paths[grepl("rail", gtfs_paths)])

# --- filter the feeds to a specific point in time
gtfs_trip_ids <- gtfs_bus$frequencies %>%
  filter(start_time == "09:00:00")

gtfs_bus <- gtfs_bus %>%
  gtfstools::filter_by_trip_id(gtfs_trip_ids$trip_id)

# ----------- 3.  Census OD data

# TODO: change to demand + travel time (i.e. demand + supply) - do this in demand_on_buses.R

od_demand <- read_csv("data/raw/travel_demand/od_census_2021/demand_study_area_oa.csv")


########## ----------------------- Identify which OD demand pairs are served by each bus ----------------------- ##########


# ----------- 1. Identify which od pairs are served directly by each trip

od_supply <- gtfs_trips_od_coverage(gtfs = gtfs_bus, zones = study_area, zone_column = "OA21CD")


# ----------- 2. Join OD demand onto the supply data ########## ----- (sd = supply_demand) ----- ##########

od_sd <- od_supply %>%
  #left_join(od_demand,
  inner_join(od_demand,
            by = c("Origin" = "Output Areas code", "Destination" = "OA of workplace code"))


# ----------- 3. Get the total potential ridership on each unique trip (sd = supply_demand)
trips_sd <- od_sd %>%
  group_by(trip_id, start_time) %>%
  summarise(potential_demand = sum(Count, na.rm = TRUE)) %>%
  ungroup()


# ----------- 4. Add geometry to plot results

# get route by converting shapes.txt from point to linestring geometry
gtfs_shapes <- gtfstools::convert_shapes_to_sf(gtfs_bus)

# add the trip id to the shape geometry
gtfs_shapes <- gtfs_bus$trips %>%
  select(route_id, trip_id, trip_headsign, shape_id) %>%
  left_join(gtfs_shapes, by = "shape_id") %>%
  st_as_sf()

# add the geometry to the supply <> demand df
trips_sd_sf <- trips_sd %>%
  left_join(gtfs_shapes, by = "trip_id") %>%
  st_as_sf()


# ----------- 5. Save the output
st_write(trips_sd_sf, "data/processed/travel_demand/trips_potential_demand_census.geojson", delete_dsn = TRUE)



# If we want to get the total demand per day let's try and group by shape_id (same trip at different time has a different trip_id)
trips_sd_sf_shape_sum <- trips_sd_sf %>%
  group_by(shape_id) %>%
  summarise(potential_demand = sum(potential_demand, na.rm = TRUE)) %>%
  ungroup()



# ----------- 5. Plots

# ----- a)  potential ridership on all different trips

# to minimise overlaps, we keep only one time of day
tmap_mode("plot")

tm_shape(study_area) +
  tm_borders(alpha = 0.1) +
tm_shape(trips_sd_sf %>%
           filter(start_time == "09:00:00")) +
  tm_lines(col = "potential_demand",
           legend.is.portrait = FALSE,
           alpha = 0.4) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Potential Ridership on Different Trips", # this works if you need it
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
           # legend.outside = TRUE,
            #legend.outside.position = "bottom",
            frame = FALSE)


# ----- b)  potential ridership on all different trips - facet by time of day

# NOTE: this is just a placeholder. The data needs to come from an activity based model.
# Currently the census data does not represent a specific time of day

tm_shape(trips_sd_sf ) +
  tm_lines(col = "potential_demand",
           alpha = 0.6,
           legend.col.is.portrait = FALSE) +
  tm_facets(by="start_time",
            nrow = 2,
            free.coords=FALSE)+
  tm_layout(fontfamily = 'Georgia',
            main.title = "Potential Ridership on Different Trips", # this works if you need it
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.text.size = 1,
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            frame = FALSE)


# layer 1 (fill): destinations reachable | accessibility | population

# layer 2 (lines): od matrix

# layer 3 (lines): potential ridership on buses

od matrix



