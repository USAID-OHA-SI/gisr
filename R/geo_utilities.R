#' Plot polygon features
#'
#' @param geodata r spatial data
#' @export
#' @examples
#' \dontrun{
#' gview(sf_data)
#' }
#'
gview <- function(geodata) {

    viz <- geodata %>%
        ggplot2::ggplot() +
        ggplot2::geom_sf(fill = NA, lwd = .3, color = "#6c6463") +
        ggplot2::coord_sf() +
        ggplot2::theme_void()

    base::print(viz)

    return(viz)
}


#' @title Get neighbors of a given contry
#'
#' @param countries  Country or list of countries names
#' @param scale      Spatial resolution of the geodata
#' @param crs        Coordinates reference system, default is WGS84 (EPGS:4326)
#' @param crop       Crop sfc to focus countries extent?
#'
#' @return           simple feature class
#' @export
#'
#' @examples
#' \dontrun{
#' geo_neighors(countries = list("Zambia"))
#' geo_neighors(countries = list("Zambia", "Malawi"))
#' }
#'
geo_neighbors <- function(countries,
                          scale = "large",
                          crs = 4326,
                          crop = FALSE) {

    # Get the world boundaries
    world <- rnaturalearth::ne_countries(scale = {{scale}},
                                         returnclass = "sf") %>%
        sf::st_transform(., crs = sf::st_crs({{crs}}))

    # Get focus country(ies)
    focus_country <-  world %>%
        dplyr::filter(sovereignt %in% {{countries}})

    # Filter based on neighbors touched by polygons of interest
    neighbors <- world %>%
        dplyr::filter(base::lengths(sf::st_touches(., focus_country)) > 0)


    # Crop specific extend of focus countries
    if (crop == TRUE) {
        # Focus country extent
        box <- sf::st_bbox(focus_country) %>%
            sf::st_as_sfc() %>%
            sf::st_buffer(
                dist = 1,
                endCapStyle = "SQUARE",
                joinStyle = "MITRE",
                mitreLimit = 2) %>%
            sf::st_as_sf()

        print(box)

        # Crop neighbors to extent
        neighbors <- neighbors %>%
            sf::st_crop(box)
    }

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
get_admin0 <- function(countries, scale = "medium", crs = 4326) {

    admin0 <- rnaturalearth::ne_countries(country = {{countries}},
                                          scale = {{scale}},
                                          returnclass = "sf") %>%
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
#' @param countries  List of the country names or sf object
#' @param mask       Should the extracted data match the exact boundary limits?
#' @param buffer     Extend AOI extent by x
#' @param terr       RasterLayer or Path to terrain raster file
#'
#' @return           spdf spatial dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' get_terrain(countries = list("Zambia"))
#' get_terrain(countries = list("Zambia"), mask = TRUE)
#' get_terrain(countries = list("Zambia"), buffer = .5, terr = "../../HDX_Data")
#' }
get_terrain <-
    function(countries = list("Zambia"),
             mask = FALSE,
             buffer = .1,
             terr = NULL) {

    # Params
    cntries <- {{countries}}

    # SFC Object
    aoi <- NULL

    if (base::is.null(cntries)) {
        base::cat(
            crayon::red(
                base::paste0("\ncountry(ies) is required",
                             " to extract Terrain RasterLayer\n")))

        return(NULL)
    }

    # Get country boundaries
    if ( "sf" %in% base::class(cntries) ) {
        aoi <- cntries
    }
    else {
        aoi <- get_admin0(countries = cntries)
    }

    # Raster Data
    # DEM File location
    dem_url <- "https://drive.google.com/drive/u/0/folders/1M02ToX9AnkozOHtooxU7s4tCnOZBTvm_"

    terr_ras <- NULL

    # Locate and retrieve terrain file
    if ( is.null(terr) ) {
        terr = "./GIS/"
    }

    # Use user provided rasterlayer
    if ( !is.null(terr) & "RasterLayer" %in% base::class(terr)) {
        terr_ras <- terr
    }

    # Read raster from local or special location
    if ( is.null(terr_ras) ) {

        # file path
        terr_file <- base::list.files(
            terr,
            pattern = "SR_LR.tif$",
            recursive = TRUE,
            full.names = TRUE
        )

        if ( length(terr_file) < 1 )
            base::stop(base::paste0("Could not find a TIFF file in: ",
                              terr, "\nDownload file from: ",
                              dem_url))

        # Read raster file
        terr_ras <- raster::raster(terr_file) %>%
            raster::crop(raster::extend(raster::extent(aoi), {{buffer}}))
    }

    # Crop raster to boundaries extent
    terr_ras <- terr_ras %>%
        raster::crop(raster::extend(raster::extent(aoi), {{buffer}}))

    # Crop to the exact limits if applicable
    if ( mask == TRUE )
        terr_ras <- terr_ras %>% raster::mask(aoi)

    # Convert raster data into a spatial data frame
    spdf <- terr_ras %>%
        as("SpatialPixelsDataFrame") %>%
        as.data.frame() %>%
        dplyr::rename(value = SR_LR)

    return(spdf)
}
