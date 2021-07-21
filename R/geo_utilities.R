#' @title View feature attributes
#'
#' @param geodata spatial data data
#' @param console view in console? default false
#' @export
#'
#' @examples
#' \dontrun{
#' gview(sf_data)
#' }
#'
dview <- function(geodata,
                  console = FALSE) {

    # check of data is sf or sfc
    base::stopifnot(base::any(class(geodata) %in% c('sf')))

    # Drop geometry and view data
    geodata <- geodata %>%
        sf::st_drop_geometry(x = .)

    # print of view
    if (console == TRUE) {
        base::print(geodata, n = Inf)
    } else {
        tibble::view(geodata)
    }
}




#' Plot sf features
#'
#' @param geodata r spatial data
#' @export
#' @examples
#' \dontrun{
#' gview(sf_data)
#' }
#'
gview <- function(geodata) {

    # check of data is sf or sfc
    base::stopifnot(base::any(class(geodata) %in% c('sf', 'sfg', 'sfc')))

    # get geom type
    geom_type <- sf::st_geometry_type(geodata, by_geometry = FALSE)

    # vizualize the object
    viz <- geodata %>%
        ggplot2::ggplot()

    # polygons
    if (geom_type %in% c("POLYGON", "MULTIPOLYGON"))
        viz <- viz + ggplot2::geom_sf(fill = NA, lwd = .3, color = "#6c6463")

    # lines
    if (geom_type %in% c("LINESTRING", "MULTILINESTRING"))
        viz <- viz + ggplot2::geom_sf(size = 1, color = "#6c6463")

    # points
    if (geom_type %in% c("POINT", "MULTIPOINT"))
        viz <- viz +
        ggplot2::geom_sf(shape = 21, size = 4, stroke = .3,
                         fill = "white", color = "#6c6463")

    # collection
    if (geom_type == "GEOMETRYCOLLECTION")
        viz <- viz + ggplot2::geom_sf()

    # Apply coordinates and theme
    viz <- viz +
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
#' @param crs coordinates reference system
#' @return simple feature class
#' @export
#' @examples
#' \dontrun{
#' get_admin1(countries = list("Zambia"))
#' }
#'
get_admin1 <- function(countries, crs = 4326) {

    admin1 <- rnaturalearth::ne_states(country = {{countries}},
                                       returnclass = "sf") %>%
        sf::st_transform(., crs = sf::st_crs({{crs}}))

    return(admin1)
}


#' @title Extract PEPFAR Orgunit Boundaries
#'
#' @description    PEPFAR VcPolygons are shared with orgunit uids only,
#'                 making hard for analysts to identify specific polygon
#'                 each orgunit level. This function extract orgunt attributes
#'                 from Datim and append them to the global shapefile, allowing
#'                 analysts to filter and work only with subset.
#'
#' @param spdf     PEPFAR Global Shapefile
#' @param country  Country name
#' @param level    Orgunit level
#' @param username Datim username
#' @param password Datim password
#' @param export   Export extract as shapefile?
#' @param name     Export filename
#'
#' @return         sf object with orgunit attributes
#' @export
#'
#' @examples
#' \dontrun {
#'
#'  library(gisr)
#'  library(sf)
#'  library(glamr)
#'
#'  cntry <- "Tanzania"
#'
#'  shp_pepfar <- return_latest(
#'      folderpath = si_path("path_vector"),
#'      pattern = "VcPepfarPolygons.*.shp",
#'      recursive = TRUE
#'    ) %>%
#'    read_sf()
#'
#'  cntry_level <- get_ouorglevel(cntry, org_type = "country")
#'
#'  shp_country <- extract_boundaries(
#'    spdf = shp_pepfar,
#'    country = cntry,
#'    level = cntry_level
#'  )
#'
#'  shp_country %>% gview()
#'
#'  psnu_level <- get_ouorglevel(cntry, org_type = "prioritization")
#'
#'  shp_psnu <- extract_boundaries(
#'    spdf = shp_pepfar,
#'    country = cntry,
#'    level = psnu_level
#'  )
#'
#'  shp_psnu %>% gview()
#' }
#'
extract_boundaries <-
    function(spdf, country,
             level = 3,
             username = NULL,
             password = NULL,
             export = FALSE,
             name = NULL) {

        # Params
        cntry <- {{country}}

        lvl <- {{level}}

        user <- base::ifelse(base::is.null(username),
                             glamr::datim_user(),
                             {{username}})

        pass <- base::ifelse(base::is.null(password),
                             glamr::datim_pwd(),
                             {{password}})

        #ou/country orgunit uid
        uid <- get_ouuid(operatingunit = cntry,
                         username = user,
                         password = pass)

        # list of orgs at the specified level
        orgs <- get_ouorgs(
            ouuid = uid,
            level = lvl,
            username = user,
            password = pass)

        # Check for valid data
        if (base::is.null(orgs)) {
            base::cat(crayon::red("\nNo geodata found for this request.\n"))

            return(NULL)
        }

        orgs <- orgs %>%
            dplyrr::mutate(org_level = lvl)

        # filter sp df
        spdf <- spdf %>%
            left_join(orgs, by = "uid") %>%
            filter(!is.na(orgunit))

        # Export
        if (export == TRUE & !is.null(name)) {
            # validate name
            name <- base::ifelse(!stringr::str_detect(name, ".shp$"),
                                 base::paste0(name, ".shp"),
                                 name)

            # Export shapefile
            sf::st_write(spdf,
                         delete_dsn = TRUE,
                         dsn = paste0(name, ".shp"))

        }

        return(spdf)
    }