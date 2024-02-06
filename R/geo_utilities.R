#' @title Get attributes from feature class
#'
#' @param  geodata spatial data
#' @return         attribures as data frame
#'
#' @export
#' @examples
#' \dontrun{
#'  library(gisr)
#'
#'  # Admin level 0 [country] geodata
#'  adm0 <- get_admin0("Ghana")
#'
#'  # Extract attrs from geodata
#'  .df <- gattributes(geodata = adm0)
#'
#'  head(.df)
#' }
#'
gattributes <- function(geodata) {

    geodata %>%
        sf::st_as_sf() %>%
        sf::st_drop_geometry(x = .) %>%
        tibble::as_tibble()
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
    geodata <- geodata %>% gattributes()

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
#' \dontrun{
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
#' \dontrun{
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
    world <- rnaturalearth::ne_countries(scale = {{scale}}, returnclass = "sf") %>%
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
#' \dontrun{
#'   library(gisr)
#'
#'   get_admin0(countries = list("Zambia"))
#' }
#'
get_admin0 <- function(countries, scale = "medium", crs = 4326) {

    admin0 <- rnaturalearth::ne_countries(
        country = {{countries}},
        scale = {{scale}},
        returnclass = "sf"
      ) %>%
      sf::st_transform(., crs = sf::st_crs({{crs}}))

    return(admin0)
}


#' @title Get admin level 1 boundaries
#'
#' @description sf boundaries data for a given country
#'
#' @param countries list of country names
#' @param crs coordinates reference system
#' @return simple feature class
#'
#' @export
#' @examples
#' \dontrun{
#'  library(gisr)
#'
#'  get_admin1(countries = list("Zambia"))
#' }
#'
get_admin1 <- function(countries, crs = 4326) {

    admin1 <- rnaturalearth::ne_states(
        country = {{countries}},
        returnclass = "sf"
      ) %>%
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
#' \dontrun{
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
        aoi_area <- sf::st_difference(aoi_area, aoi)
    }

    return(aoi)
}




#' @title Get PEPFAR Visual Crossing Polygons
#'
#' @param folderpath     Path to PEPFAR Global Shapefile
#' @param name           Name or pattern of shapefile
#'
#' @return sf object
#'
#' @export
#' @examples
#' \dontrun{
#'  library(gisr)
#'
#'  shp_pepfar <- get_vcpolygons()
#'  shp_pepfar <- get_vcpolygons(folderpath = glamr::si_path("path_vector"))
#'  shp_pepfar <- get_vcpolygons(folderpath = "./GIS", name = "VcPepfarPolygons.shp")
#' }
#'
get_vcpolygons <- function(folderpath, name = NULL) {

    # filename
    if (base::is.null(name)) {
        name <- "^VcPepfarPolygons.*.shp$"
    }

    # file full path
    shp_pepfar <- glamr::return_latest(
        folderpath = folderpath,
        pattern = name,
        recursive = TRUE
    )

    # Read files
    sf::read_sf(shp_pepfar)
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
#'  shp_pepfar <- get_vcpolygons(folderpath = glamr::si_path("path_vector")) %>%
#'
#'  shp_country <- extract_boundaries(
#'    spdf = shp_pepfar,
#'    country = cntry,
#'    level = 3
#'  )
#'
#'  shp_country %>% gview()
#'
#'
extract_boundaries <-
    function(spdf, country,
             level = 3,
             username,
             password,
             export = FALSE,
             name = NULL) {

        # account details
        accnt <- grabr::lazy_secrets("datim", username, password)

        #ou/country orgunit uid
        uid <- grabr::get_ouuid(operatingunit = country,
                                username = accnt$username,
                                password = accnt$password)

        # list of orgs at the specified level
        orgs <- grabr::get_ouorgs(
            ouuid = uid,
            level = level,
            username = accnt$username,
            password = accnt$username
        )

        # Check for valid data
        if (base::is.null(orgs)) {
            base::cat(crayon::red("\nNo geodata found for this request.\n"))

            return(NULL)
        }

        orgs <- orgs %>% dplyr::mutate(orgunit_level = level)

        # filter spdf
        spdf <- spdf %>%
            dplyr::left_join(orgs, by = "uid") %>%
            dplyr::filter(!is.na(orgunit))

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


#' Extract Country Polygons
#'
#' @param spdf  VcPolygons data as Spatial Data Frame
#' @param cntry Country name
#' @param attrs Country orgunits, output of `grabr::datim_orgunits(cntry, reshape = TRUE)`
#'
#' @return list of spatial data frames
#' @export
#'
#' @examples
#' \dontrun{
#'
#' cntry = "Nigeria"
#'
#' spdf <- gisr::get_vcpolygons(path = glamr::si_path("path_vector"), name = "VcPepfarPolygons.shp")
#'
#' df_attrs <- grabr::get_attributes(cntry)
#'
#' cntry_polygons(spdf = spdf, cntry = "Zambia", attrs = df_attrs)
#'
#' }
#'
cntry_polygons <- function(spdf, cntry, attrs) {

    # Append attrs to boundaries
    spdf <- spdf %>% dplyr::left_join(attrs, by = c("uid" = "orgunituid"))

    # Get distinct labels - boundary names
    labels <- attrs %>%
        dplyr::distinct(label) %>%
        dplyr::pull()

    # Check for valid columns
    if(!"label" %in% base::names(spdf))
        base::stop("ERROR - Unable to identify 'label' attribute")

    # Extract distinct boundaries
    cntry_geo <- labels %>%
        purrr::map(function(.x) {
            dplyr::filter(spdf, label == .x)
        })

    # add labels
    names(cntry_geo) <- labels

    return(cntry_geo)
}

#' @title Generate Point Spatial DataFrame
#'
#' @param .data  Location data as a data frame, use `extract_facilities`
#' @param lat    Column name for latitude, default value is latitude
#' @param long   Column name for longitude, default value is longitude
#' @param crs    Coordinate Reference System, default value is EPSG Code for WGS 1984
#'
#' @return list of spatial data frames
#' @export
#'
#' @examples
#' \dontrun{
#'
#' cntry <- "Ethiopia"
#' level_fac <- get_ouorglevel(operatingunit = cntry, org_type = "facility")
#' df_facs <- extract_locations(country = cntry, level = level_fac)
#' df_facs <- df_facs %>% extract_facilities()
#' df_locs <- df_facs %>% select(-c(geom_type:nested))
#'
#' spdf <- spdf_points(.data = df_locs)
#' }
#'
#'
spdf_points <- function(.data,
                         lat = "latitude",
                         long = "longitude",
                         crs = 4326) {

    # Spatial file
    spdf <- NULL

    # check for lat/long columns
    if (lat %in% names(.data)) {

        spdf <- .data %>%
            dplyr::filter(dplyr::across(dplyr::all_of(c(lat, long)), ~ !base::is.na(.x))) %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(c(lat, long)), ~ base::as.numeric(.x)))


        spdf <- spdf %>%
            sf::st_as_sf(coords = c(long, lat),
                         crs = sf::st_crs(crs))

        # TODO: Shapefiles columns have a max - Move this outside
        # spdf <- spdf %>%
        #     dplyr::rename(ou_iso = operatingunit_iso,
        #                   ou = operatingunit,
        #                   cntry_iso = countryname_iso,
        #                   cntry = countryname)

        return(spdf)

    }

    base::message("No location columns found. Consider changing lat/long default values")
}


#' @title Save spatial data as shapefile
#'
#' @param spdf sf object
#' @param name filename with full path
#'
#' @return boolean
#'
#' @examples
#' \dontrun{
#'  library(gisr)
#'  library(sf)
#'
#'  shp <- get_admin0(countries = "Nigeria")
#'
#'  export_spdf(spdf = shp, name = "./GIS/nga_country_boundaries")
#'  export_spdf(spdf = shp, name = "./GIS/nga_country_boundaries.shp")
#' }
#'
export_spdf <- function(spdf, name) {

    # Check directory
    dir <- base::dirname(name)

    if (!base::dir.exists(dir)) {
        base::message(glue::glue("{dir} does not seem to exist."))
    }

    name <- base::ifelse(!stringr::str_detect(name, ".shp$"),
                         base::paste0(name, ".shp"),
                         name)

    delete <- base::ifelse(file.exists(name), TRUE, FALSE)

    # Write spdf to local file
    sf::st_write(obj = spdf,
                 dsn = name,
                 delete_dsn = delete)

}


#' @title Export spatial data as shapefile with flags
#'
#' @param spdf sf object
#' @param name filename with full path
#'
#' @return boolean
#' @export
#'
#' @examples
#' \dontrun{
#'  library(gisr)
#'  library(sf)
#'
#'  shp <- get_admin0(countries = "Nigeria")
#'
#'  export_spdf(spdf = shp, name = "./GIS/nga_country_boundaries")
#'  export_spdf(spdf = shp, name = "./GIS/nga_country_boundaries.shp")
#' }
#'
spdf_export <- function(spdf, name) {

    cols_check <- base::names(spdf) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(len = nchar(value)) %>%
        dplyr::filter(len > 10)

    if (base::nrow(cols_check) > 0) {
        base::message(base::paste0("Consider shortening these columns to 10 characters to adhere to shapefile specs: ",
                                   base::paste0(cols_check$value, collapse = ", ")))
    }

    # Redirect to actual export after alerting user
    export_spdf(spdf, name)
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
#' \dontrun{
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
#' @param dest_file    Full file name where to download zipped shapefile
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
        #cntry <- stringr::str_to_lower(country)
        cntry <- janitor::make_clean_names(country)
        label <- stringr::str_to_lower(org_label)

        # default google drive folder
        sp_folder <- "1KQACKdo7b-M_Un2Fe1x0ZSJkhkNNPTqa"

        # default local folder
        dest_folder <- glamr::si_path("path_vector") #"./GIS"

        gdrive <- base::ifelse(base::is.null(drive_folder),
                               sp_folder,
                               drive_folder)

        # labels
        org_labels <- c("global", "region",
                        "country", "snu1", "psnu", "prioritization",
                        "community", "site")

        if (!label %in% org_labels) {
            base::message(glue::glue("{org_label} is not available. Valid options are: "))
            base::message(base::paste(org_labels, collapse = ", "))

            return(NULL)
        }

        # target sub-folder
        folder <- dplyr::case_when(
            label == "global" ~ "Global",
            label == "region" ~ "Regional-Countries-Boundaries",
            label == "country" ~ "OU-Country-Boundaries",
            label == "snu1" ~ "SNU1",
            label == "psnu" ~ "PSNU",
            label == "prioritization" ~ "PSNU",
            label == "community" ~ "Communities",
            label == "site" ~ "Sites",
            TRUE ~ NA_character_
        )

        # Download URL
        zfile_url <- "https://drive.google.com/uc?export=download&id="

        # authentication
        if (is.null(getOption("email"))) {
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
            dplyr::filter(stringr::str_detect(name, folder))

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
        if (base::is.null(dest_file) && base::dir.exists(dest_folder)) {
            dest_file <- base::file.path(dest_folder,
                                         glue::glue("{cntry}_{org_label}_shapefile.zip"))
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
