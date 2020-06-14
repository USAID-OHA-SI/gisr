#' Get geospatial data from GADM
#'
#' This function extracts / reads geospatial data
#' for specified country and administractive boundary levels
#'
#' @param country_name ISO3 code
#' @param adm_level country administrative boundaries level
#' @param local save a copy?
#' @param geo_path where to save / read data from
#' @return geodata as sf with crs = 4326
#' @export
#' @example
#' \dontrun{
#' get_adm_boundaries("UGA")
#' get_adm_boundaries("UGA", adm_level=2, local=FALSE)
#' }
#'
get_adm_boundaries <- function(country_name, adm_level=0, local=TRUE, geo_path=NULL) {

    # Read data form local dir first
        downlaod <- local != TRUE

    # geodata location
        if (!is.null(geo_path) ) {
            geo_path <- here::here()
        }

    # Extract data from GADM
        geo_data <- raster::getData("GADM",
                                    country = country_name,
                                    level = adm_level,
                                    download = downlaod,
                                    path = geo_path)

    # Force a download
        if (is.null(geo_data)) {
            geo_data <- get_adm_boundaries(country_name, adm_level, local = FALSE)
        }

    # Convert data into sf format
        geo_data <- sf::st_as_sf(geo_data) %>%
            dplyr::rename_at(dplyr::vars(everything()), tolower)

    # Return formatted data
    return(geo_data)
}