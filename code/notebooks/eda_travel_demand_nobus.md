OD demand that is not satisfied by a direct bus
================
1/19/24

## Introduction

The purpose of this notebook is to look at travel demand that is not met
by a direct bus. This is done as follows:

1.  Get an itinerary by public transport for each od pair, and only keep
    the OD pairs that need at least one transfer (od pairs that cannot
    be reached by PT are also kept)

    - *Method*: Use the `expanded_travel_time_matrix()` function in r5r
      to route between all OD pairs. The function return a value n_rides
      for each query which represents the number of pt trips required to
      get from A to B (n_rides = 2 -\> 1 transfer, n_rides = 3 -\> 2
      transfers etc). We call the output df: `tt_matrix`

2.  Get the travel demand between od pairs and join it onto the travel
    time matrix. Travel demand refers to the number of people who travel
    between each OD pair

    - *Method*: For this version, we use the census 2021 data, and the
      process is in
      [code/travel_demand_census.R](https://github.com/Hussein-Mahfouz/drt-potential/blob/main/code/travel_demand_census.R).
      The merged df is called `od_demand_no_direct`

    - For a future version, we want to use demand data that is
      disaggregated by time of day

3.  For all OD pairs in `od_demand_no_direct` we get the geometry of the
    shortest paths connecting them. The assumption is that this is the
    path that cars would take. We add this geometry onto `od_demand`

    - *Method*: route using
      `r5r::detailed_itineraries(keep_geometry = TRUE)`

4.  Merge the demand onto the network, combining parallel lines in order
    to get total demand on each road segment. We call this
    `od_demand_overline`

    - Method: `stplanr::overline()`. Code is in
      code/demand_no_direct_bus.R

    - This can be improved as overline only combines lines that directly
      overlap. Parallel edges are not merged - see discussion in [this
      github
      issue](https://github.com/Hussein-Mahfouz/drt-potential/issues/26#issue-2078687863)

5.  Identify overlap between shortest path gemetries and high frequency
    bus routes. Even though all these od pairs are not served by a
    direct bus, they overlap with high frequency routes for part of
    their journeys. We calculate the % of intersection between each
    shortest path and the bus route it intersects with most.

    - *Method*: `get_most_overlapping_route()` and
      `get_overlap_inverse()` functions in
      `code/eda_travel_demand_nobus.R`

    This allows us to see which OD pairs could benefit from a first/last
    mile service to a high frequency bus route

The code is available in code/eda_travel_demand_nobus.R

## Merging Travel Demand

![](images/map_aggregate_flows_overline_vs_base.png)

As we can see here, the unmet demand on a weekday night is higher than a
weekday morning. To create this plot we use compare bus provision in the
morning and night periods. The demand is from the census and we use the
same input for both scenarios (this is a limitation). However, we have a
gtfs feed with bus provision that varies across the day, and the fewer
services at night is the reason we have more unmet demand at night.

![](images/map_aggregate_flows_combination_wkday_ampm.png)

The difference in bus level of service across the day can be seen here:

![](images/map_headways_combinations_all_bus.png)

## Unmet demand (aggregate-level)

In the map below, we use the aggregated demand to see how the spatial
distribution of demand overlaps with the existing bus network

![](images/map_headways_demand_combinations_all_bus.png)

Let’s remove the bus routes as well as all demand that overlaps with
them. This could give us a better picture of the spatial distribution of
unmet demand

![](images/map_demand_disjoint_all_buses_combinations_nobus.png)

If we do the same exercise but only use high frequency buses (routes
with at least 2 buses per hour), the picture is quite different. There
is a lot more unmet demand

![](images/map_demand_disjoint_high_freq_buses_combinations_nobus.png)

## Unmet demand (individual-level)

The maps in the previous section looked at the aggregate demand
(i.e. demand that has been merged onto the road segments:
`od_demand_overline`).

Here we look demand from individual od pairs that are not served by a
direct bus (`od_demand_no_direct`). We try to the level of overlap each
individual od itinerary (in `od_demand_no_direct` overlaps with the bus
network. An od pair may not be traversible by a bus route, but it may
overlap 70% with a high frequency bus - such an OD pair may benefit from
either:

- a first/last mile service to the route

- an extension to the bus route

For each geometry in `od_pair_no_direct`, we:

1.  get it’s intersection with each high frequency bus in the gtfs feed.

2.  identify the bus route that intersects with it most and calculate
    the length of intersection as a fraction of the od pair geometry
    (i.e. how much of the journey between A and B is covered by a high
    frequency bus route).

The maps below show the results for different times of day. We visualize
the part of the `od_demand_no_direct` that DOES NOT intersect with the
bus route to show where there is a need for better services. How to read
the map:

- Lines: For each shortest path geometry in `od_demand_no_direct` this
  is the part that does not intersect with a high frequency bus route

- Line color: For each geometry, we have identified the bus route that
  intersects with it most. The color shows the frequency of the
  identified bus route

- Line width: This shows the demand along each line. The demand is from
  individual OD pairs and is not aggregated

- Facet: This shows the % intersection of each shortest path with the
  bus route that intersects with it most. For example (0.75, 1\] focuses
  on demand that intersects with high frequency bus routes for over 75%
  of its geometry

### Weekday Morning

![](images/map_demand_overlap_gtfs_st_diff_morning_wkday-02.png)

### Weekday Evening

![](images/map_demand_overlap_gtfs_st_diff_morning_wkday-03.png)

### Weekend Evening

![](images/map_demand_overlap_gtfs_st_diff_evening_wkend.png)

### All times of day (Overlap \> 50%)

Let’s focus on geometries that have a high overlap with high frequency
bus routes. These are a good indication of potential first/ last mile
interventions such as a DRT service area

![](images/map_demand_overlap_gtfs_st_diff_combinations.png)
