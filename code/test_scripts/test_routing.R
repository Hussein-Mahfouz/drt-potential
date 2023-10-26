### the purpose of this script is to test the routing functionality in r5r ###
### I discovered a few NAs for travel times between OD pairs that are near ###
### each other. This was only for PT routing. I want to explore the effect ###
### of different parameters on results                                     ###

# Parameters we will check

    # max walk time
    # max trip duration
    # time window     # UPDATE; TIME WINDOW NEEDS A FREQUENCY BASED GTFS FILE!
    # percentiles?

library(tidyverse)
library(sf)
library(r5r)

# increase the memory available to Java. Needs to be done at the beginning of the script
options(java.parameters = "-Xmx30G")  # 3 gegabytes

# define path where graph will be built (path with osm and pbf data)
graph_path <- paste0("data/interim/routing_graph/")

# ------------------------------------- PREPARE (BASE) OD MATRIX LAYER ------------------------------------- #

study_area <- st_read("data/interim/study_area_boundary.geojson")
# r5r requires a POINT sf object with WGS84 CRS
study_area <- study_area %>%
  st_transform(4326) %>%
  st_centroid()



# Function takes a base layer, gets the geometry centroid, and renames the id column that we pass to it into a standard name
prep_base_layer = function(layer, id_col){
  # rename existing ID column with "id"
  id_col = sym(id_col)
  layer = layer %>%
    rename(id = !! id_col)
  # transform crs
  layer = layer %>% st_transform(4326)
  # get centroid
  layer = layer %>% #select(id) %>%
    st_centroid()
}

# apply the function
study_area_r5 <- prep_base_layer(layer = study_area, id_col = "OBJECTID")



# ------------------------ define parameters that we will test, and their ranges -------------------- #

# get all combinations using tidyr::expand_grid()
parameters <- expand_grid(time_w = c(10, 30, 50),      # time_window
                          max_walk = c(10, 15, 20))    # max_walk_time (minutes)


# add unique id column

# Create a new column with unique identifiers
# identifier uses column name combined with row value: col1=x_col2=y...
parameters$unique_id <- apply(parameters, 1, function(row) {
  paste(names(parameters), row, sep = "=", collapse = "_")
})

# parameters <- parameters %>%
#   relocate(unique_id, .before = everything())


# ------------------------------------- CALCULATE TRAVEL TIME MATRIX ------------------------------------- #


# --- setup r5
r5r_core <- setup_r5(data_path = graph_path,
                     verbose = TRUE,
                     overwrite = TRUE) # turn to true once we have elevation


# --- define variables that will not change

# random subset of origins and destinations
study_area_r5_sample <- study_area_r5[sample(nrow(study_area_r5), 500), ]


origins = study_area_r5_sample #study_area_r5[100:300,]
destinations = study_area_r5_sample #study_area_r5[100:300,]
# we are testing transit routing
mode = c("WALK", "TRANSIT")
# weekday morning peak
departure_datetime = as.POSIXct("14-08-2023 07:30:00",
                                format = "%d-%m-%Y %H:%M:%S")
# this is one of the parameters we are testing, but we can pass it as a list to r5.
# Otherwise we will have different dfs with different column names (travel_time_p25, travel_time_p50...)
percentiles = c(25, 50, 75)
max_trip_duration = 90

# empty list to store results for each combination
results <- vector(mode = "list", length = nrow(parameters))
for(i in 1:nrow(parameters)){
  #status updates
  print(paste0("CALCULATING TRAVEL TIME FOR combination ",i, " of ", nrow(parameters), ": ", parameters$unique_id[i], " ..."))
  # calculate a travel time matrix
  ttm <- r5r::travel_time_matrix(r5r_core = r5r_core,
                                  origins = origins,
                                  destinations = destinations,
                                  mode = mode,
                                  departure_datetime = departure_datetime,
                                  max_trip_duration = max_trip_duration,
              # ----------------------- START: parameters that we are testing
                                 # time_window = parameters$time_w[i],
                                  percentiles = percentiles,
                                  max_walk_time = parameters$max_walk[i],
                                  #max_trip_duration = parameters$max_dur[i],
              # ----------------------- END: parameters that we are testing
                                  # to suppress r5 output. Change to true if debugging
                                  verbose = FALSE,
                                  # slow down function by ~20%
                                  progress = TRUE)

  # add column with combination name / number
  ttm$parameters_id <- parameters$unique_id[i]

  # add ttm to results list
  results[[i]] <- ttm
  #status updates
  print(paste0("COMPLETED combination: ", parameters$unique_id[i], " ..."))

}

# stop r5
r5r::stop_r5(r5r_core)
# java garbage collector to free up memory
rJava::.jgc(R.gc = TRUE)


# combine list into 1 dataframe
results <- bind_rows(results)

# ------------------------------ PLOT RESULTS ------------------------------  #

# ----- Compare changes in time_window and max_walking_time in one facet plot

# pivot longer to get one row for each percentile result
results_long <- results %>%
  pivot_longer(cols =  c(travel_time_p25, travel_time_p50, travel_time_p75),
               values_to = "travel_time",
               names_to = "percentile") %>%
  # edit percentiles column so that name only has the percentile figure
  mutate(percentile = sub(".*_", "", percentile))

# facet density plot
ggplot(results_long, aes(x = travel_time, fill = percentile)) +
  geom_density(alpha = 0.3) +
  labs(x = "travel time (minutes)", y = "Density", title = "Travel time distribution")  +
  facet_wrap(vars(parameters_id), ncol = 3)


ggsave("data/processed/plots/routing_parameter_sensitivity/density_tt_distribution_facet.png", width = 14, height = 10)

# ----- get number of NA values per parameter combination

results_na <- results_long %>%
  group_by(parameters_id, percentile) %>%
  summarise(na_count = sum(is.na(travel_time))) %>%
  ungroup()

# create combined id for ggplotgeom_bar()
results_na <- results_na %>%
  mutate(combined_id = paste(parameters_id, percentile, sep = "_"))

# add parameters for ggplot options
results_na <- results_na %>%
  left_join(parameters, by = c("parameters_id" = "unique_id"))



# bar plot
ggplot(results_na, aes(x = parameters_id, y = na_count, fill = as.factor(time_w), col = as.factor(max_walk))) +
  geom_bar(stat = "identity", alpha = 0.5, linewidth = 1.2) +
  scale_fill_brewer(palette = "PRGn") +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "", y = "number of OD pairs with travel time = NA",
       title = "Effect of parameter combinations on number of unsuccessful routes returned",
       fill = "Time Window (min)", color = "Max Walk Time (min)")  +
  # remove grey fill from color legend
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_wrap(vars(percentile), nrow = 3)


ggsave("data/processed/plots/routing_parameter_sensitivity/bar_na_count_facet.png", width = 14, height = 10)

