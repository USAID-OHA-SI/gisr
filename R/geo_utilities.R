#' @title Get attributes from feature class
#'
#' @param  geodata spatial data
#' @return         attribures as data frame
#'
#' @export
#' @examples
#' \donttest{
#'  library(gisr)
#'
#'  # Admin level 0 [country] geodata
#'  adm0 <- get_admin0("Ghana")
#'
#'  # Extract attrs from geodata
#'  df <- attributes(geodata = adm0)
#'
#'  df %>% head()
#' }
#'
attributes <- function(geodata) {

    # check of data is sf or sfc
    base::stopifnot(base::any(class(geodata) %in% c('sf')))

    # Drop geometry and view data
    geodata %>% sf::st_drop_geometry(x = .)
}

#' @title View attributes from simple feature object
#'
#' @param geodata spatial data
#' @param console view in console? default false
#' @export
#'
#' @examples
#' \dontrun{
#'  library(gisr)
#'
#'  adm0 <- get_admin0("Ghana")
#'
#'  adm0 %>% dview(console = TRUE)
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


#' @title Plot sf features
#'
#' @param geodata spatial data as sf object
#' @param ...     arguments passed to geom_sf
#'
#' @export
#' @examples
#' \donttest{
#'  library(gisr)
#'
#'  adm0 <- get_admin0("Ghana")
#'
#'  adm0 %>%
#'   dplyr::select(name) %>%
#'   gview()
#' }
#'
gview <- function(geodata, ...) {

    # check of data is sf or sfc
    base::stopifnot(base::any(class(geodata) %in% c('sf', 'sfg', 'sfc')))

    # get geom type
    geom_type <- sf::st_geometry_type(geodata, by_geometry = FALSE)

    # vizualize the object
    viz <- geodata %>%
        ggplot2::ggplot()

    # polygons
    if (geom_type %in% c("POLYGON", "MULTIPOLYGON"))
        viz <- viz + ggplot2::geom_sf(fill = NA,
                                      lwd = .3,
                                      color = "#6c6463",
                                      ...)

    # lines
    if (geom_type %in% c("LINESTRING", "MULTILINESTRING")) {
        viz <- viz + ggplot2::geom_sf(size = 1,
                                      color = "#6c6463",
                                      ...)
    }

    # points
    if (geom_type %in% c("POINT", "MULTIPOINT")) {
        viz <- viz +
        ggplot2::geom_sf(shape = 21,
                         size = 4,
                         stroke = .3,
                         fill = "white",
                         color = "#6c6463",
                         ...)
    }

    # collection
    if (geom_type == "GEOMETRYCOLLECTION") {
        viz <- viz + ggplot2::geom_sf()
    }

    # Apply coordinates and theme
    viz <- viz +
        ggplot2::coord_sf() +
        ggplot2::theme_void()

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
#' \donttest{
#'  library(gisr)
#'
#'  cntry <- "Zambia"
#'
#'  # Country + neighbors boundaries
#'  cntries <- geo_neighbors(countries = cntry)
#'
#'  # Country + neighbors boundaries: crop to country extent
#'  cntries <- geo_neighbors(countries = cntry, crop = TRUE)
#'
#'  cntries %>%
#'    dplyr::select(name) %>%
#'    attributes()
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


#' @title Get admin level 0 boundary
#'
#' @description sf boundaries data for a given country
#'
#' @param countries list of country names
#' @param scale     spatial resolution of the geodata
#' @param crs       coordinates reference system
#'
#' @return          simple feature class
#' @export
#' @examples
#' \donttest{
#'   library(gisr)
#'
#'   get_admin0(countries = list("Zambia"))
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


#' @title Get admin level 1 boundaries
#'
#' @description sf boundaries data for a given country
#'
#' @param countries list of country names
#' @param crs coordinates reference system
#' @return simple feature class
#' @export
#' @examples
#' \donttest{
#'  library(gisr)
#'
#'  get_admin1(countries = list("Zambia"))
#' }
#'
get_admin1 <- function(countries, crs = 4326) {

    admin1 <- rnaturalearth::ne_states(country = {{countries}},
                                       returnclass = "sf") %>%
        sf::st_transform(., crs = sf::st_crs({{crs}}))

    return(admin1)
}


#' @title Generate a buffer around an Area of Interest
#'
#' @param aoi     Area of Interest as sf object
#' @param radius  Buffer redius in meters, default = 1000m
#' @param append  Should the buffered area be appended to the AOI? Default is TRUE
#'
#' @return simple feature class
#'
#' @export
#' @examples
#' \donttest{
#'  library(gisr)
#'
#'  adm <- get_admin0(countries = "Zambia")
#'
#'  adm %>% geo_fence(radius = 5000, append = TRUE) %>% gview()
#'
#'  adm %>% geo_fence(radius = 5000, append = FALSE) %>% gview()
#' }
#'
geo_fence <- function(aoi,
                      radius = 1000,
                      append = TRUE) {

    # CRS
    from_crs <- aoi %>% sf::st_crs()
    to_crs <- sf::st_crs(3857)

    if (base::is.null(from_crs)) {
        crayon::red("There are no coordinates system attached to your spatial data")
        return(NULL)
    }

    # Create buffer
    aoi_area <- aoi %>%
        sf::st_transform(crs = to_crs) %>%
        sf::st_buffer(dist = radius) %>%
        sf::st_transform(crs = from_crs)

    # clip off input
    if (!append) {
        aoi_area <- aoi_area %>% sf::st_difference(aoi)
    }

    return(aoi)
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
#' @return sf object with orgunit attributes
#' @export
#'
#' @examples
#' \dontrun{
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
            dplyr::mutate(org_level = lvl)

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


#' @title Extract Road Network data from OSM
#'
#' @param aoi     Area of Interest as sf object
#' @param radius  Buffer redius in meters, default = 1000m
#' @param clip    Should the output be clipped to the AOI? Default is FALSE
#'
#' @export
#'
#' @examples
#' \donttest{
#'
#'  library(gisr)
#'
#'  adm_zmb <- get_admin1(countries = "Zambia") %>%
#'    dplyr::select(name) %>%
#'    dplyr::filter(name == 'Lusaka')
#'
#'  adm_zmb %>%
#'    extract_roads() %>%
#'    gview()
#' }
#'
extract_roads <- function(aoi,
                          radius = NULL,
                          clip = FALSE) {

    # Buffer areas
    if (!base::is.null(radius)) {
        aoi <- aoi %>% geo_fence(radius = radius)
    }

    # Get area bounding box
    aoi_bbox <- aoi %>%
        sf::st_bbox()

    # Build query
    aoi_query <- aoi_bbox %>%
        base::unname() %>%
        osmdata::opq(bbox = .) %>%
        osmdata::add_osm_feature(key = "highway")

    # Query data
    sfdf <- aoi_query %>%
        osmdata::osmdata_sf()

    # Extract lines only
    sldf <- NULL

    if (!base::is.null(sfdf) & !base::is.null(sfdf$osm_lines)) {
        #sldf <- sfdf %>% osmdata::osm_lines()
        sldf <- sfdf$osm_lines
    }

    # Extract data matching aoi
    if (!base::is.null(sfdf) & clip == TRUE) {
        sldf <- sldf %>%
            sf::st_intersection(aoi)
    }

    # Notification for no-data
    if (base::is.null(sldf)) {
        crayon::red("No network data found for this area.")
    }

    return(sldf)
}


#' @title Download shapefile zipfile from googledrive
#'
#' @param country      PEPFAR Countryname
#' @param org_label    Orgunit label, default is set to country
#' @param drive_folder Googledrive id for all PEPFAR Spatial files
#' @param dest_file    Local directory where to download zipped shapefile
#' @param overwrite    Should the process overwrite existing files
#' @param unzip        Should the zipfile be unzipped
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   cntry <- "Zambia"
#'
#'   download_shapefiles(country = cntry)
#'
#'   download_shapefiles(country = cntry, org_label = "snu1", unzip = TRUE)
#'
#'   download_shapefiles(country = cntry, org_label = "psnu", unzip = TRUE)
#' }
#'
download_shapefiles <-
    function(country,
             org_label = "country",
             drive_folder = NULL,
             dest_file = NULL,
             overwrite = TRUE,
             unzip = FALSE) {

        # country
        cntry <- stringr::str_to_lower(country)

        # default google drive folder
        sp_folder <- "1KQACKdo7b-M_Un2Fe1x0ZSJkhkNNPTqa"

        # detault local folder
        dest_folder <- "./GIS"

        gdrive <- base::ifelse(base::is.null(drive_folder),
                               sp_folder,
                               drive_folder)

        # labels
        org_labels <- c("global", "region",
                        "country", "snu1", "psnu",
                        "community", "site")

        if (!org_label %in% org_labels) {
            base::message(glue::glue("{org_label} is not available. Valid options are: "))
            base::message(base::paste(org_labels, collapse = ", "))

            return(NULL)
        }

        # target sub-folder
        folder <- case_when(
            org_label == "global" ~ "Global",
            org_label == "region" ~ "Regional-Countries-Boundaries",
            org_label == "country" ~ "OU-Country-Boundaries",
            org_label == "snu1" ~ "SNU1",
            org_label == "psnu" ~ "PSNU",
            org_label == "community" ~ "Communities",
            org_label == "site" ~ "Sites",
            TRUE ~ NA_character_
        )

        # Download URL
        zfile_url <- "https://drive.google.com/uc?export=download&id="

        # authentication
        if (!glamr::is_loaded("email")) {
            glamr::load_secrets()
        }

        user <- base::getOption("email")

        if (base::is.null(user)) {
            base::message("Unable to identify current user. Please make your googledrive account is set to email")
            return(NULL)
        }

        googledrive::drive_auth(email = user)
        googlesheets4::gs4_auth(email = user)

        # List gdrive objects
        df_drives <- googledrive::drive_ls(path = googledrive::as_id(gdrive))

        if (base::nrow(df_drives) == 0) {
            base::message(glue::glue("{drive_folder} seems to be empty"))

            return(NULL)
        }

        # Identify sub-folder
        df_drive <- df_drives %>%
            dplyr::filter(str_detect(name, folder))

        if (base::nrow(df_drive) == 0) {
            base::message(glue::glue("Could not find a folder for {org_label}"))

            return(NULL)
        }

        if (base::nrow(df_drive) != 1) {
            base::message(glue::glue("There seems to be duplicate folders for {org_label}"))

            return(NULL)
        }

        # Identify zipfile
        df_files <- df_drive %>%
            dplyr::pull(id) %>%
            googledrive::as_id() %>%
            googledrive::drive_ls(path = ., pattern = cntry)

        if (base::nrow(df_files) == 0) {
            base::message(glue::glue("Could not find a match for {country} / {org_label}. Check if item was not removed or renamed."))

            return(NULL)
        }

        if (base::nrow(df_files) != 1) {
            base::message(glue::glue("There seems to be duplicate files for {country} / {org_label}. Check if item was not duplicated."))

            return(NULL)
        }

        zfile <- df_files %>%
            dplyr::pull(id) %>%
            base::paste0(zfile_url, .)

        base::print(glue::glue("Download link: {zfile}"))

        # Generate destination file
        if (base::is.null(dest_file) & base::dir.exists(dest_folder)) {
            dest_file <- base::file.path(dest_folder,
                                         glue::glue("{stringr::str_to_lower(country)}_{org_label}_shapefile.zip"))
        }

        if (base::is.null(dest_file)) {
            base::message("Could not identify the destination folder / file")
            return(NULL)
        }

        base::print(glue::glue("File is downloaded to: {dest_file}"))

        # Download zipped file to local drive
        zfile %>%
            googledrive::drive_download(
                file = .,
                path = dest_file,
                overwrite = overwrite)

        # Unzip file
        if (unzip == TRUE & base::file.exists(dest_file)) {
            zip::unzip(zipfile = dest_file,
                       exdir = base::dirname(dest_file))
        }

    }