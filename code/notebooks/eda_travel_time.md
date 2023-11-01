Exploring travel time results
================
11/1/23

The purpose of this script is to explore outputs from the routing. I
look at

- Car vs pt travel time

- Access, egress, and waiting times

- Transfers

- Which OD pairs have no PT route at all?

``` r
source("R/study_area_geographies.R")


library(tidyverse)
library(sf)
library(tmap)
```

# Input data

## Decide on geographic resolution

``` r
geography <- "LSOA"
```

## Read in the travel time outputs

``` r
# -------------------- read in the outputs
# read in geography
study_area <- st_read("data/interim/study_area_boundary.geojson")
```

    Reading layer `study_area_boundary' from data source 
      `/home/hussein/Documents/GitHub/drt-potential/data/interim/study_area_boundary.geojson' 
      using driver `GeoJSON'
    Simple feature collection with 2607 features and 15 fields
    Geometry type: MULTIPOLYGON
    Dimension:     XY
    Bounding box:  xmin: -1.800359 ymin: 53.69898 xmax: -1.290394 ymax: 53.94587
    Geodetic CRS:  WGS 84

``` r
# convert to necessary resolution
study_area <- study_area_geographies(study_area = study_area,
                                     geography = geography)

# --- routing results
tt_matrix <- arrow::read_parquet(paste0("data/processed/travel_times/", geography, "/travel_time_matrix_expanded.parquet"))
```

# Analysis

## Summarise values by origin and destination

``` r
# Get median values per origin
tt_matrix_o <- tt_matrix %>%
  group_by(from_id, combination, departure_time) %>%
  summarise(across(c(contains("_time"), n_rides), median, na.rm = TRUE),
            # number of destinations that can be reached
            reachable_destinations = n()) %>%
  ungroup() %>%
  mutate(n_rides = round(n_rides))

# Get median values per destination
tt_matrix_d <- tt_matrix %>%
  group_by(to_id, combination, departure_time) %>%
  summarise(across(c(contains("_time"), n_rides), median, na.rm = TRUE),
            # number of origins that can reach the destination
            reachable_origins = n()) %>%
  ungroup() %>%
  mutate(n_rides = round(n_rides))
```

## Bar plots

### Waiting time

``` r
# PLOT: BAR - wait time distribution per combination
tt_matrix_o %>%
  filter(grepl("pt_", combination)) %>%
  filter(wait_time > 0) %>%
  ggplot(aes(x = wait_time, fill = combination)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
  facet_wrap(~combination, scales = "fixed") +
  labs(title = "Wait time distribution at different times of day", subtitle = "Average per origin", x = "Time (min)", y = "No. of origins") +
  theme_minimal() +
  theme(legend.position = "top")
```

![](eda_travel_time_files/figure-commonmark/unnamed-chunk-5-1.png)

``` r
tt_matrix_d %>%
  filter(grepl("pt_", combination)) %>%
  filter(wait_time > 0) %>%
  ggplot(aes(x = wait_time, fill = combination)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
  facet_wrap(~combination, scales = "fixed") +
  labs(title = "Wait time distribution at different times of day", subtitle = "Average per destination", x = "Time (min)", y = "No. of origins") +
  theme_minimal() +
  theme(legend.position = "top")
```

![](eda_travel_time_files/figure-commonmark/unnamed-chunk-5-2.png)

### Number of reachable destinations

``` r
   # expanded dataset - any row with total_time == NA means you can't reach
   # group_by origin, combination, departure_time -> count number of rows where total_time !- NA

tm_shape(tt_matrix_o %>%
           filter(combination != "car") %>%
           left_join(study_area %>%
                       select(OBJECTID) %>%
                       mutate(OBJECTID = as.character(OBJECTID)),
                     by = c("from_id" = "OBJECTID")) %>%
           st_as_sf()) +
  tm_fill(col = 'reachable_destinations',
          title = "Destinations Reachable (120 minutes)",
          legend.is.portrait = FALSE) +
  tm_facets(by="combination",
            nrow = 2,
            free.coords=FALSE)  +  # so that the maps aren't different sizes
  tm_layout(fontfamily = 'Georgia',
            main.title = "Number of reachable destinations from each origin", # this works if you need it
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            frame = FALSE)
```

![](eda_travel_time_files/figure-commonmark/unnamed-chunk-6-1.png)

#### Which origins have the fewest reachable destinations?

Visualise geographic regions with:
`reachable_destinations < max(reachable_destinations) / 2`

``` r
# which origins have the fewest reachable destinations?

# visualise geographic regions with: reachable_destinations < max(reachable_destinations) / 2

tm_shape(study_area)+
  tm_borders() +
tm_shape(tt_matrix_o %>%
           filter(combination != "car") %>%
           left_join(study_area %>%
                       select(OBJECTID) %>%
                       mutate(OBJECTID = as.character(OBJECTID)),
                     by = c("from_id" = "OBJECTID")) %>%
           st_as_sf() %>%
           filter(reachable_destinations < max(reachable_destinations) / 2)) +
  tm_fill(col = 'reachable_destinations',
          title = "Destinations reachable",
          legend.is.portrait = FALSE) +
  tm_facets(by="combination",
            nrow = 2,
            free.coords=FALSE)  +  # so that the maps aren't different sizes
  tm_layout(fontfamily = 'Georgia',
            main.title = "Worst Performing Origins\nOrigins with least destinations reachable", # this works if you need it
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            frame = FALSE)
```

![](eda_travel_time_files/figure-commonmark/unnamed-chunk-7-1.png)

Same analysis but for the weekday morning only

``` r
tm_shape(study_area) +
  tm_borders() +
tm_shape(tt_matrix_o %>%
             filter(combination == "pt_wkday_morning") %>%
             left_join(study_area %>%
                         select(OBJECTID) %>%
                         mutate(OBJECTID = as.character(OBJECTID)),
                       by = c("from_id" = "OBJECTID")) %>%
             st_as_sf() %>%
           filter(reachable_destinations < max(reachable_destinations) / 2)) +
  tm_fill(col = 'reachable_destinations',
          title = "Destinations reachable",
          legend.is.portrait = FALSE) +
  # tm_facets(by="ntile",
  #           nrow = 2,
  #           free.coords=FALSE)  +  # so that the maps aren't different sizes
  tm_layout(fontfamily = 'Georgia',
            main.title = "Destinations Reachable - Weekday morning", # this works if you need it
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            frame = FALSE)
```

![](eda_travel_time_files/figure-commonmark/unnamed-chunk-8-1.png)

### Average number of transfers

``` r
# average number of transfers from origin (origin anchored)

tm_shape(tt_matrix_o %>%
           filter(combination != "car") %>%
           left_join(study_area %>%
                       select(OBJECTID) %>%
                       mutate(OBJECTID = as.character(OBJECTID)),
                     by = c("from_id" = "OBJECTID")) %>%
           mutate(n_rides = as.character(n_rides)) %>%
           st_as_sf()) +
  tm_fill(col = 'n_rides',
          breaks = c(0, 1, 2, 3),
          title = "Number of transfers",
          legend.is.portrait = FALSE) +
  tm_facets(by="combination",
            nrow = 2,
            free.coords=FALSE)  +  # so that the maps aren't different sizes
  tm_layout(fontfamily = 'Georgia',
            main.title = "Average number of transfers from origin", # this works if you need it
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            frame = FALSE)
```

![](eda_travel_time_files/figure-commonmark/unnamed-chunk-9-1.png)

``` r
# average number of transfers to destination (destination anchored)

tm_shape(tt_matrix_d %>%
           filter(combination != "car") %>%
           left_join(study_area %>%
                       select(OBJECTID) %>%
                       mutate(OBJECTID = as.character(OBJECTID)),
                     by = c("to_id" = "OBJECTID")) %>%
           mutate(n_rides = as.character(n_rides)) %>%
           st_as_sf()) +
  tm_fill(col = 'n_rides',
          title = "Number of transfers",
          legend.is.portrait = FALSE) +
  tm_facets(by="combination",
            nrow = 2,
            free.coords=FALSE)  +  # so that the maps aren't different sizes
  tm_layout(fontfamily = 'Georgia',
            main.title = "Average number of transfers to destination", # this works if you need it
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            frame = FALSE)
```

![](eda_travel_time_files/figure-commonmark/unnamed-chunk-10-1.png)
