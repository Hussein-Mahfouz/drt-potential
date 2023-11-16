#' Filter an OD matrix using distance between Origin and Destination
#'
#' @param zones: an sf layer with all the zones you want to create an OD matrix from
#' @param od_matrix an existing OD matrix between the same zones - There should be columns called `Origin` and `Destination`
#' @param dist_threshold the minimum distance in m below which zones are filtered out
#'
#' @return an sf object with the filtered rows from od_matrix
#'
#' @examples
#'
#' @export
filter_matrix_by_distance = function(zones, od_matrix, dist_threshold){
  # create an od matrix from the zones df
  zones_matrix = od::points_to_od(zones)
  # get distance between each od pair
  # create desire lines
  zones_matrix_sf = od::od_to_sf(x = zones_matrix, z = zones)
  # calculate distance
  zones_matrix_sf = sf::st_transform(zones_matrix_sf, 3857)
  zones_matrix_sf = zones_matrix_sf %>% dplyr::mutate(distance_m = units::drop_units(sf::st_length(.)))
  zones_matrix_sf = sf::st_transform(zones_matrix_sf, st_crs(zones))
  # add distances onto original od_matrix
  zones_matrix_sf = dplyr::select(zones_matrix_sf, O, D, distance_m)
  od_matrix_dist = dplyr::inner_join(od_matrix, zones_matrix_sf,
                                     by = c("Origin" = "O", "Destination" = "D"))
  # remove OD pairs below the theshold distance
  od_matrix_dist = dplyr::filter(od_matrix_dist, distance_m >= dist_threshold)

  return(od_matrix_dist)
}
