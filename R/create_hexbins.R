#' Get an hexbins polygon feature class for country ABC
#'
#' @param country_code iso3 code
#' @param level country administrative unit level
#' @param geo_path path to geodata
#' @param size size of each hex bin in meters
#' @return country hex polygon as feature class
#' @export
#' @example
#' get_hexbins('LSO', 1)
#' get_hexbins('UGA', 2)
#'
get_hexbins <- function(country_code, adm_level=0, geo_path=NULL, size=NULL) {

    # Set up path for geodata
    path <- ""

    if (!is.null(geo_path)) {
        path <- geo_path
    }

    # Get admin boundaries
    cntry_adm <- get_adm_boundaries(country_code, adm_level=adm_level, geo_path = path) %>%
        sf::st_transform(crs = sf::st_crs(3857))

    cntry_adm0 <- cntry_adm

    # Dissolve if boundaries
    if ( adm_level != 0 ) {
        cntry_adm0 <- cntry_adm %>%
            dplyr::mutate(
                id = dplyr::row_number(),
                area = sf::st_area(.)
            ) %>%
            dplyr::group_by(id) %>%
            dplyr::summarise(area = sum(area))
    }

    # Generate hexbins
    if (!is.null(size) & is.numeric(size)) {
        cntry_hex <- cntry_adm0 %>%
            sf::st_make_grid(what = "polygones", square = FALSE, cellsize = size) %>%
            sf::st_intersection(cntry_adm0)

    } else {
        cntry_hex <- cntry_adm0 %>%
            sf::st_make_grid(what = "polygones", square = FALSE) %>%
            sf::st_intersection(cntry_adm0)
    }

    return(cntry_hex)
}