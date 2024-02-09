#' Get an hexbins polygon feature class for country ABC
#'
#' @param spdf input spatial data frame
#' @param size size of each hex bin in meters, default set to 15K meters (15KM)
#' @return country hex polygon as feature class
#' @export
#' @examples
#' \dontrun{
#'  library(gisr)
#'
#'  shp <- get_admin0(countries = "Nigeria")
#'
#'  get_hexbins(shp, 10000)
#' }
#'
get_hexbins <- function(spdf, size = 15000) {

  # CRS

  from_crs <- sf::st_crs(spdf) # Source CRS
  to_crs <- sf::st_crs(3857)   # Projected CRS

  # Dissolve if boundaries

  if ( nrow(spdf) == 0 )
    usethis::ui_stop("Input spatial data is empty.")

  # Merge all features into one
  spdf <- spdf %>%
    dplyr::mutate(
      id = dplyr::row_number(),
      area = sf::st_area(.)
    ) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(area = sum(area))

  # Make any corrections

  spdf <- spdf %>%
    sf::st_make_valid() %>%
    sf::st_as_sf()

  # Create hexbins

  spdf_hex <- spdf %>%
    sf::st_transform(crs = to_crs) %>%
    sf::st_make_grid(what = "polygones", square = FALSE, cellsize = size) %>%
    sf::st_intersection(spdf) %>%
    sf::st_make_valid() %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = from_crs)

  return(spdf_hex)
}