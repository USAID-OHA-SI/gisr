#' Generate an hexbins polygon sfc from another sfc
#'
#' @param geodata r spatial data
#' @param size size of each hex bin in meters
#' @return country hex polygon as feature class
#' @export
#' @examples
#' \dontrun{
#'  library(gisr)
#'
#'  cntry <- "Nigeria"
#'  data <- get_admin0(cntry)
#'
#'  generate_hexbins(data)
#'  generate_hexbins(geodata = data, size = 10000)
#' }
#'
generate_hexbins <- function(geodata, size=NULL) {

    # Make sure geodata is sf and in web mercator
    geodata <- geodata %>%
        sf::st_as_sf() %>%
        sf::st_transform(crs = sf::st_crs(3857))

    # Generate hexbins
    if (!is.null(size) & is.numeric(size)) {

        cntry_hex <- geodata %>%
            sf::st_make_grid(what = "polygones", square = FALSE, cellsize = size) %>%
            sf::st_intersection(geodata)

    } else {
        cntry_hex <- geodata %>%
            sf::st_make_grid(what = "polygones", square = FALSE) %>%
            sf::st_intersection(geodata)
    }

    return(cntry_hex)
}