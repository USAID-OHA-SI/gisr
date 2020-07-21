#' Plot polygon features
#'
#' @param geodata r spatial data
#' @export
#' @examples
#' \dontrun{
#' geo_viz(sf_data)
#' }
#'
geo_viz <- function(geodata) {

    viz <- geodata %>%
        ggplot2::ggplot() +
        ggplot2::geom_sf(fill=NA, lwd=.3, color="#BCBEC0") +
        ggplot2::coord_sf() +
        ggplot2::theme_void()

    print(viz)

    return(viz)
}


#' Get neighbors of a given contry
#'
#' @param countries a list of country names
#' @param scale spatial resolution of the geodata
#' @param crs coordinates reference system
#' @return simple feature class
#' @export
#' @examples
#' \dontrun{
#' geo_neighors(countries = list("Zambia"))
#' geo_neighors(countries = list("Zambia", "Malawi"))
#' }
#'
geo_neighbors <- function(countries, scale = "large", crs = 4326) {

    # Get the world boundaries
    world <- rnaturalearth::ne_countries(scale = {{scale}}, returnclass = "sf") %>%
        sf::st_transform(., crs = sf::st_crs({{crs}}))

    #stopifnot({{country}} %in% world$sovereignt)
    # if(!{{countries}} %in% world$sovereignt) {
    #   stop('Country name is not the Natural Earth reference dataset. Please enter a name from dataset.')
    # }

    # Get focus country(ies)
    focus_country <-  world %>%
        dplyr::filter(sovereignt %in% {{countries}})

    # Filter based on neighbors touched by polygons of interest
    neighbors <- world %>%
        dplyr::filter(lengths(sf::st_touches(., focus_country)) > 0)

    return(neighbors)
}


#' Get country boundary sf data for a given country
#'
#' @param countries list of country names
#' @param scale spatial resolution of the geodata
#' @param crs coordinates reference system
#' @return simple feature class
#' @export
#' @examples
#' \dontrun{
#' get_admin0(counties = list("Zambia"))
#' }
#'
get_admin0 <- function(countries, scale = "medium", crs = 4326)  {

    admin0 <- rnaturalearth::ne_countries(country = {{countries}}, scale = {{scale}}, returnclass = "sf") %>%
        sf::st_transform(., crs = sf::st_crs({{crs}}))

    # admin0 <- rnaturalearth::ne_countries(scale = {{scale}}, returnclass = "sf") %>%
    #     dplyr::filter(sovereignt %in% {{countries}}) %>%
    #     sf::st_transform(., crs = sf::st_crs({{crs}}))

    return(admin0)
}


#' Get admin level 1 boundaries sf data for a given country
#'
#' @param countries list of country names
#' @param scale spatial resolution of the geodata
#' @param crs coordinates reference system
#' @return simple feature class
#' @export
#' @examples
#' \dontrun{
#' get_admin1(countries = list("Zambia"))
#' }
#'
get_admin1 <- function(countries, scale = "medium", crs = 4326) {

    admin1 <- rnaturalearth::ne_states(country = {{countries}}, returnclass = "sf") %>%
        sf::st_transform(., crs = sf::st_crs({{crs}}))

    return(admin1)
}


#' Get terrain data for an AOI (Countries)
#'
#' @param countries list of the country names
#' @param mask should the extracted data match the exact boundary limits?
#' @param buffer extend AOI extent by x
#' @param terr_path path to terrain raster file
#' @return spdf spatial dataframe
#' @export
#' @examples
#' \dontrun{
#' get_terrain(countries = list("Zambia"))
#' get_terrain(countries = list("Zambia"), mask = TRUE)
#' get_terrain(countries = list("Zambia"), buffer = .5, terr_path = "../../HDX_Data")
#' }
get_terrain <- function(countries = list("Zambia"),
                        mask = FALSE,
                        buffer = .2,
                        terr_path = NULL) {

    # DEM File location
    dem_url <- "https://drive.google.com/drive/u/0/folders/1M02ToX9AnkozOHtooxU7s4tCnOZBTvm_"

    # Locate and retrieve terrain file
    if ( is.null(terr_path) )
        terr_path = "./GIS/"

    terr_file <- list.files(terr_path, pattern = "SR_LR.tif$", recursive = TRUE, full.names = TRUE)

    if ( length(terr_file) < 1 )
        stop(paste0("Could not find a TIFF file in: ", terr_path, "\nDownload file from: ", dem_url))

    #TODO: Facilitate a download from google drive
    #drive_download(dem_url)

    # Get country boundaries
    cntry <- get_admin0(countries = {{countries}})

    # Read raster file and crop it to boundaries extent
    terr <- raster::raster(terr_file) %>%
        raster::crop(raster::extend(raster::extent(cntry), {{buffer}}))

    # Crop to the exact limits if applicable
    if ( isTRUE(mask) )
        terr <- terr %>% raster::mask(cntry)

    # Convert raster data into a spatial data frame
    spdf <- terr %>%
        as("SpatialPixelsDataFrame") %>%
        as.data.frame() %>%
        dplyr::rename(value = SR_LR)

    return(spdf)
}
