#' Get geospatial data from GADM
#'
#' This function extracts / reads geospatial data
#' for specified country and administractive boundary levels
#'
#' @param country_code ISO3 code
#' @param adm_level country administrative boundaries level
#' @param dowload Save a copy
#' @param geo_path where to save / read data from
#' @return geodata as sf with crs = 4326
#' @export
#' @examples
#' \dontrun{
#' get_adm_boundaries("UGA")
#' get_adm_boundaries("UGA", adm_level=2, geo_path="GIS")
#' }
#'
get_adm_boundaries <- function(country_code,
                               adm_level=0,
                               download=TRUE,
                               geo_path="") {


    # Extract data from GADM
        geo_data <- raster::getData("GADM",
                                    country = country_code,
                                    level = adm_level,
                                    download = download,
                                    path = geo_path)

    # Convert data into sf format
        geo_data <- sf::st_as_sf(geo_data) %>%
            dplyr::rename_at(.vars = dplyr::vars(dplyr::everything()), .funs = tolower)

    # Return formatted data
    return(geo_data)
}