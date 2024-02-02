library(tidyverse)
library(sf)
library(lwgeom)
library(spatstat)
library(dbscan)


source("R/study_area_geographies.R")
source("R/filter_od_matrix.R")


########## ----------------------- Read in the data ----------------------- ##########

# ----------- 1. Study area

# --- administrative boundaries
study_area <- st_read("data/interim/study_area_boundary.geojson")

# convert to desired resolution
geography = "MSOA"
study_area = study_area_geographies(study_area = study_area,
                                    geography = geography)

study_area <- study_area %>%
  st_cast("MULTIPOLYGON")

# ----------- 2.  Census OD data

# Demand (census) + supply (travel time) data

od_demand <- arrow::read_parquet(paste0("data/raw/travel_demand/od_census_2021/demand_study_area_", tolower(geography), ".parquet"))

# filter to specific combination
# TODO: get seperate flows for car and pt, and keep two combinations
od_demand <- od_demand %>%
  filter(combination == "pt_wkday_morning")

# rename columns as most functions are applied on generic column names
from_id_col = paste0(geography, "21CD_home")
to_id_col = paste0(geography, "21CD_work")

od_demand = od_demand %>%
  rename("Origin" = all_of(from_id_col),
         "Destination" = all_of(to_id_col))
########## ----------------------- Convert df to sf desire lines ----------------------- ##########


# --- create desire lines and remove od pairs with very short distance

# TODO: edit this to avoid clusters of very short flows
  # "Density-based clustering for bivariate-flow data" (section 5.2): preprocessing step to avoid
  # clusters of very short flows. this involves splitting the data into 3 chunks
  # based on length (
od_demand_filtered = filter_matrix_by_distance(zones = study_area,
                                               od_matrix = od_demand,
                                               dist_threshold = 500)



########## ------------ "Spatial Cluster Detection in Spatial Flow Data" ---------- ##########

# ----- STEP 1: Get distance between flows

# metric crs
od_demand_filtered <- od_demand_filtered %>%
  st_transform(3857)

# Add columns for coordinates of startpoint (X, Y) and endpoint (U, V)

x <- od_demand_filtered %>%
  mutate(x = as_tibble(st_coordinates(st_startpoint(od_demand_filtered)))$X,
         y = as_tibble(st_coordinates(st_startpoint(od_demand_filtered)))$Y,
         u = as_tibble(st_coordinates(st_endpoint(od_demand_filtered)))$X,
         v = as_tibble(st_coordinates(st_endpoint(od_demand_filtered)))$Y) %>%
  st_drop_geometry()

# create flow ID column
x <- x %>%
  mutate(flow_ID = paste(Origin, Destination, sep = "-"))


# filter to specific origin
x_origin_i <- x %>% filter(Origin == study_area$MSOA21CD[1])

# create grid
x_origin_i_grid <- expand_grid(flow_ID_a = x_origin_i$flow_ID,
                               flow_ID_b = x_origin_i$flow_ID)

# add the coordinate data
x_origin_i_grid2 <- x_origin_i_grid %>%
  # --- add flow_a coordinates
  inner_join(x %>%
              select(flow_ID, x, y, u, v, distance_m),
            by = c("flow_ID_a" = "flow_ID")) %>%
  # rename coordinate columns by adding a suffix
  rename_with(.fn = ~ paste0(.x, "_i"), .cols = c("x", "y", "u", "v", "distance_m")) %>%
  # --- add flow_b coordinates
  inner_join(x %>%
               select(flow_ID, x, y, u, v, distance_m),
             by = c("flow_ID_b" = "flow_ID")) %>%
  # rename coordinate columns by adding a suffix
  rename_with(.fn = ~ paste0(.x, "_j"), .cols = c("x", "y", "u", "v", "distance_m"))

# ----- get "Flow Distance" and "Flow Dissimilarity"

# Flow Distance

x_origin_i_grid2 <- x_origin_i_grid2 %>%
  mutate(fd = sqrt(0.5 * ((x_i - x_j)^2 + (y_i - y_j)^2) +
              0.5 * ((u_i - u_j)^2 + (v_i - v_j)^2)),
         fds = sqrt((0.5 * ((x_i - x_j)^2 + (y_i - y_j)^2) +
                      0.5 * ((u_i - u_j)^2 + (v_i - v_j)^2)) / (distance_m_i * distance_m_j)))



flow_distance = function(study_area, flows){
  # empty list to store results for each origin
  results <- vector(mode = "list", length = nrow(study_area))

  for (i in 1:nrow(study_area)){

    #status updates
    print(paste0("Getting distances for Origin: ", i, " ....."))

    # filter to specific origin
    flows_origin_i <- flows %>% filter(Origin == study_area$MSOA21CD[i])

    # create grid
    flows_origin_i_grid <- expand_grid(flow_ID_a = flows_origin_i$flow_ID,
                                   flow_ID_b = flows_origin_i$flow_ID)

    # add the coordinate data
    flows_origin_i_grid <- flows_origin_i_grid %>%
      # --- add flow_a coordinates
      inner_join(flows %>%
                   select(flow_ID, x, y, u, v, distance_m),
                 by = c("flow_ID_a" = "flow_ID")) %>%
      # rename coordinate columns by adding a suffix
      rename_with(.fn = ~ paste0(.x, "_i"), .cols = c("x", "y", "u", "v", "distance_m")) %>%
      # --- add flow_b coordinates
      inner_join(flows %>%
                   select(flow_ID, x, y, u, v, distance_m),
                 by = c("flow_ID_b" = "flow_ID")) %>%
      # rename coordinate columns by adding a suffix
      rename_with(.fn = ~ paste0(.x, "_j"), .cols = c("x", "y", "u", "v", "distance_m"))

    # ----- get "Flow Distance" and "Flow Dissimilarity"

    # Flow Distance
    flows_origin_i_grid <- flows_origin_i_grid %>%
      mutate(fd = sqrt(0.5 * ((x_i - x_j)^2 + (y_i - y_j)^2) +
                         0.5 * ((u_i - u_j)^2 + (v_i - v_j)^2)),
             fds = sqrt((0.5 * ((x_i - x_j)^2 + (y_i - y_j)^2) +
                           0.5 * ((u_i - u_j)^2 + (v_i - v_j)^2)) / (distance_m_i * distance_m_j)))

    # add results to list
    results[[i]] <- flows_origin_i_grid

  }
  return(results)
}


test <- flow_distance(study_area = study_area,
                      flows = x)

test2 <- bind_rows(test)


# convert from long to wide
test2 %>%
  select(flow_ID_a, flow_ID_b, fd) %>%
  pivot_wider(names_from = flow_ID_b, values_from = fd) %>%
  column_to_rownames(var = "flow_ID_a") -> test3


# this is a sparse matrix (lot's of OD pairs without flow, so most pairs of OD pairs don't exist)
# replace NA with a very high number?
max(test3, na.rm = TRUE)

test3[is.na(test3)] <- max(test3, na.rm = TRUE) * 2

# hdbscan

res = dbscan::hdbscan(test3, minPts = 50)




hi <- spatstat.geom::psp(x0 = x$x, y0 = x$y, x1 = x$u, y1 = x$v,
                         window <- spatstat.geom::as.owin(study_area %>%
                                                            st_transform(3857)))


hi2 <- hi %>% Lest(.)

# Option 2: Flow Dissimilarity


# ----- STEP 2: Hotspot detection

# may have to compute Ripley's K from scratch using the equation (as we have the distance, not the actual points)
# see wikipedia for eqn

# alternatively, spatstat::kest() is a function for Ripley's K (unclear if useful)
# https://andrewmaclachlan.github.io/CASA0005repo/detecting-spatial-patterns.html

