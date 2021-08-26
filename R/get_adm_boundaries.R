#' Get geospatial data from GADM
#'
#' This function extracts / reads geospatial data
#' for specified country and administrative boundary levels
#'
#' @param country_code ISO3 code
#' @param adm_level    country administrative boundaries level
#' @param download     Save a copy
#' @param path         where to save / read data from
#'
#' @return geodata as sf with crs = 4326
#' @export
#'
#' @examples
#' \dontrun{
#' library(gisr)
#' get_adm_boundaries("UGA")
#' get_adm_boundaries("UGA", adm_level = 2, path = "GIS")
#' }
#'
get_adm_boundaries <- function(country_code,
                               adm_level = 0,
                               download = TRUE,
                               path = NULL) {

    # Use default path
        if (base::is.null(path)) {
            path <- glamr::si_path("path_vector")
        }

    # Check for valid path
        if (base::is.null(path) | !base::dir.exists(path)) {
            stop(base::paste("Path is invalid or does not exist:", path))
        }

    # Extract data from GADM
        geo_data <- raster::getData("GADM",
                                    country = country_code,
                                    level = adm_level,
                                    download = download,
                                    path = path)

    # Convert data into sf format
        geo_data <- sf::st_as_sf(geo_data) %>%
            dplyr::rename_at(.vars = dplyr::vars(dplyr::everything()), .funs = tolower)

    # Return formatted data
    return(geo_data)
}