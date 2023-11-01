---
title: "Exploring travel time results"
subtitle: "Exploring the Potential for Integrating Demand Responsive Transport with Public Transport"
date: "01 11 23"
format:
  gfm: default
  html:
    toc: true
execute:
  keep-md: true
# project:
#   execute-dir: project
knitr:
  opts_knit:
    root.dir: "/home/hussein/Documents/GitHub/drt-potential/"
---



The purpose of this script is to explore outputs from the routing. I look at

-   Car vs pt travel time

-   Access, egress, and waiting times

-   Transfers

-   Which OD pairs have no PT route at all?


::: {.cell}

```{.r .cell-code}
source("R/study_area_geographies.R")


library(tidyverse)
```

::: {.cell-output .cell-output-stderr}
```
── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
✔ ggplot2 3.4.2     ✔ purrr   1.0.2
✔ tibble  3.2.1     ✔ dplyr   1.1.3
✔ tidyr   1.3.0     ✔ stringr 1.5.0
✔ readr   2.1.4     ✔ forcats 0.5.1
── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
```
:::

```{.r .cell-code}
library(sf)
```

::: {.cell-output .cell-output-stderr}
```
Linking to GEOS 3.10.2, GDAL 3.4.1, PROJ 8.2.1; sf_use_s2() is TRUE
```
:::

```{.r .cell-code}
library(tmap)
```

::: {.cell-output .cell-output-stderr}
```
The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
which was just loaded, were retired in October 2023.
Please refer to R-spatial evolution reports for details, especially
https://r-spatial.org/r/2023/05/15/evolution4.html.
It may be desirable to make the sf package available;
package maintainers should consider adding sf to Suggests:.
Breaking News: tmap 3.x is retiring. Please test v4, e.g. with
remotes::install_github('r-tmap/tmap')
```
:::
:::


You can add options to executable code like this


::: {.cell}

```{.r .cell-code}
# -------------------- read in the outputs

# --- decide on geographic resolution

geography <- "MSOA"

# read in geography
study_area <- st_read("data/interim/study_area_boundary.geojson")
```

::: {.cell-output .cell-output-stdout}
```
Reading layer `study_area_boundary' from data source 
  `/home/hussein/Documents/GitHub/drt-potential/data/interim/study_area_boundary.geojson' 
  using driver `GeoJSON'
Simple feature collection with 2607 features and 15 fields
Geometry type: MULTIPOLYGON
Dimension:     XY
Bounding box:  xmin: -1.800359 ymin: 53.69898 xmax: -1.290394 ymax: 53.94587
Geodetic CRS:  WGS 84
```
:::

```{.r .cell-code}
# convert to necessary resolution
study_area <- study_area_geographies(study_area = study_area,
                                     geography = geography)
```

::: {.cell-output .cell-output-stderr}
```
converting from OA to MSOA
```
:::

```{.r .cell-code}
# --- routing results
tt_matrix <- arrow::read_parquet(paste0("data/processed/travel_times/", geography, "/travel_time_matrix_expanded.parquet"))
```
:::

::: {.cell}

```{.r .cell-code}
# -------------------- Analysis

# Get median values per origin
tt_matrix_o <- tt_matrix %>%
  group_by(from_id, combination, departure_time) %>%
  summarise(across(c(contains("_time"), n_rides), median, na.rm = TRUE),
            # number of destinations that can be reached
            reachable_destinations = n()) %>%
  ungroup() %>%
  mutate(n_rides = round(n_rides))
```

::: {.cell-output .cell-output-stderr}
```
Warning: There was 1 warning in `summarise()`.
ℹ In argument: `across(c(contains("_time"), n_rides), median, na.rm = TRUE)`.
ℹ In group 1: `from_id = "160254"`, `combination = "car"`, `departure_time =
  "07:30:00"`.
Caused by warning:
! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
Supply arguments directly to `.fns` through an anonymous function instead.

  # Previously
  across(a:b, mean, na.rm = TRUE)

  # Now
  across(a:b, \(x) mean(x, na.rm = TRUE))
```
:::

::: {.cell-output .cell-output-stderr}
```
`summarise()` has grouped output by 'from_id', 'combination'. You can override
using the `.groups` argument.
```
:::

```{.r .cell-code}
# Get median values per destination
tt_matrix_d <- tt_matrix %>%
  group_by(to_id, combination, departure_time) %>%
  summarise(across(c(contains("_time"), n_rides), median, na.rm = TRUE),
            # number of origins that can reach the destination
            reachable_origins = n()) %>%
  ungroup() %>%
  mutate(n_rides = round(n_rides))
```

::: {.cell-output .cell-output-stderr}
```
`summarise()` has grouped output by 'to_id', 'combination'. You can override
using the `.groups` argument.
```
:::
:::

::: {.cell}

```{.r .cell-code}
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

::: {.cell-output-display}
![](eda_travel_time_files/figure-html/unnamed-chunk-4-1.png){width=672}
:::

```{.r .cell-code}
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

::: {.cell-output-display}
![](eda_travel_time_files/figure-html/unnamed-chunk-4-2.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
# number of reachable destinations
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

::: {.cell-output-display}
![](eda_travel_time_files/figure-html/unnamed-chunk-5-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
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

::: {.cell-output-display}
![](eda_travel_time_files/figure-html/unnamed-chunk-6-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
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

::: {.cell-output-display}
![](eda_travel_time_files/figure-html/unnamed-chunk-7-1.png){width=672}
:::
:::
