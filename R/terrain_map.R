#' @title Generate a terrain map
#'
#' @param countries     List of countries to map
#' @param adm0          Admin 0 boundaries, optional sf geodata
#' @param adm1          Admin 1 boundaries, optional sf geodata
#' @param add_neighbors Should the map include the neighbor countries
#' @param add_labels    Add neighbors countries's names (works only when add_neighbors is TRUE)
#' @param mask          Should the extracted data match the exact boundary limits?
#' @param terr          RasterLayer dataset or Path for terrain raster file
#' @return ggplot basemap
#' @export
#' @examples
#' \dontrun{
#' library(gisr)
#' gisr::terrain_map(countries = list("Zambia"))
#' gisr::terrain_map(countries = list("Zambia"), add_neighbors = TRUE)
#' }
#'
terrain_map <-
    function(countries,
             adm0 = NULL,
             adm1 = NULL,
             add_neighbors = FALSE,
             add_labels = FALSE,
             mask = FALSE,
             terr = NULL) {

    # Country name(s)
    cntries <- {{countries}}

    # Country boundaries:
    # 1) Get from NE
    if (!is.null(adm0) | !is.null(adm1)) {
        admin0 <- adm0
        admin1 <- adm1
    }
    else {#2) Provide sf objects
        admin0 <- get_admin0(cntries)
        admin1 <- get_admin1(cntries)
    }

    # Get neighbors id needed
    nghbrs <- NULL

    # Validate options
    if (TRUE == {{add_neighbors}} & is.null(cntries)) {
        base::cat(
            crayon::red(
                base::paste0("\ncountry(ies) is required when",
                             " adding neighbors\n")))

        return(NULL)
    }

    # Get neighbors
    if ( TRUE == {{add_neighbors}} & !is.null(cntries) ) {

        # Get neighbors
        nghbrs <- geo_neighbors(countries = cntries, crop = TRUE)

        cntries <- nghbrs
    }

    # Get terrain raster
    spdf <- get_terrain(countries = cntries,
                        mask = {{mask}},
                        terr = {{terr}})

    # check
    if (base::is.null(spdf)) {
        base::cat(
            crayon::red(
                base::paste0("\nCould not extract terrain data.",
                             " Your AOI may be invalid.\n")))

        return(NULL)
    }


    # Plot the map

    # Get basemap
    p <- ggplot2::ggplot() +
        ggplot2::geom_tile(data = dplyr::filter(spdf, value < 210),
                           ggplot2::aes(x = x, y = y, alpha = value)) +
        ggplot2::scale_alpha(name = "", range = c(0.6, 0), guide = "none")

    # Add neighbors
    if ( !is.null(nghbrs) )
        p <- p +
            ggplot2::geom_sf(data = nghbrs,
                             fill = "#d9d9d9",
                             alpha = 0.35,
                             size = 0.25,
                             colour = "#6d6e71")

    if ( !is.null(nghbrs) & add_labels == TRUE)
        p <- p + ggplot2::geom_sf_text(data = nghbrs,
                                       ggplot2::aes(label = sovereignt),
                                       family = "Source Sans Pro" )

    # Add country layers
    p <- p +
        ggplot2::geom_sf(data = admin0,
                         colour = "white",
                         fill = "grey93",
                         size = 2,
                         alpha = 0.25) +
        ggplot2::geom_sf(data = admin0,
                         colour = "black",
                         fill = "NA")

    # Add admin 1 layer
    if ( !is.null(admin1) )
        p <- p + ggplot2::geom_sf(data = admin1,
                                  fill = "NA",
                                  linetype = "dotted")

    # Zoom to South Africa mainland
    if ("south africa" %in% stringr::str_to_lower(cntries))
        p <- p +
        ggplot2::xlim(15, 35) +
        ggplot2::ylim(-38, -20)

    # apply theme
    p <- p + ggplot2::theme_void()

    return(p)
}



#' @title Get Terrain Raster dataset
#'
#' @param path Path to raster file, default will be `si_path('path_raster')`
#' @param name Name of the raster file (with extension), default is set to terrain raster `SR_LR.tif`
#' @param ...  Additional arguments to be passed to `base::list.files`. Eg: `Use ignore.case = TRUE` for non case sensitive search
#'
#' @return RasterLayer
#' @export
#'
#' @examples
#' \dontrun{
#' get_raster(terr_path = glamr::si_path("path_raster"))
#' }
#'
get_raster <- function(path = NULL, name = NULL, ...) {

        # Check params
        if (base::is.null(path)) {
            path <- glamr::si_path("path_raster")
        }

        if (base::is.null(name)) {
            name <- "SR_LR.tif"
        }

        # Check directory
        if (!base::dir.exists(path)) {
            stop(base::cat("\nInvalid terrain directory: ",
                           crayon::red(path),
                           "\n"))
        }

        # Identify file path
        terr_file <- base::list.files(
            path = path,
            pattern = base::paste0(name, "$"),
            recursive = TRUE,
            full.names = TRUE,
            ...
        )

        # Check file
        if (!base::file.exists(terr_file)) {
            stop(base::cat("\nFile does not exist: ", terr_file, "\n"))
        }

        # Read file content
        ras <- raster::raster(terr_file)

        return(ras)
    }


#' @title Get terrain data for an AOI (Countries)
#'
#' @param countries  List of the country names or sf object
#' @param mask       Should the extracted data match the exact boundary limits?
#' @param buffer     Extend AOI extent by x
#' @param terr       RasterLayer or Path to terrain raster file
#' @importFrom       methods as
#' @return           spdf spatial dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'  library(gisr)
#'  library(sf)
#'
#'  get_terrain(countries = list("Zambia"))
#'  get_terrain(countries = list("Zambia"), mask = TRUE)
#'  get_terrain(countries = list("Zambia"), buffer = .5, terr = "../../HDX_Data")
#'
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

        # Check AOI
        if (base::is.null(cntries)) {
            base::cat(
                crayon::red(
                    base::paste0("\ncountry(ies) is required",
                                 " to extract Terrain RasterLayer\n")))

            return(NULL)
        }

        # Country boundaries
        if ( "sf" %in% base::class(cntries) ) {
            aoi <- cntries

        } else {
            # check if country is a character and a valid ne name
            if (base::is.character(cntries) & cntries %in% glamr::pepfar_country_xwalk$sovereignt) {
                aoi <- get_admin0(countries = cntries)
            }
            else {
                stop("Countries list does not seems to match Natural Earth Countries")
            }
        }

        # Raster Data
        # DEM File location
        dem_url <- "https://drive.google.com/drive/u/0/folders/1M02ToX9AnkozOHtooxU7s4tCnOZBTvm_"

        terr_ras <- NULL
        terr_path <- NULL  #"./GIS/"

        # Locate and retrieve terrain file
        if ( base::is.null(terr) ) {

            terr_path = glamr::si_path("path_raster")

            if ( base::is.null(terr_path) ) {
                stop("Global Raster path is not set. Please use glamr::set_paths() and add a value for path_raster")

            } else if ( !base::is.null(terr_path) & !base::dir.exists(terr_path) ) {
                stop(base::paste0("Path: ", terr_path, " does not exist"))
            }
        }

        # Use user provided rasterlayer
        if ( !base::is.null(terr) & "RasterLayer" %in% base::class(terr)) {
            terr_ras <- terr
        }

        # Read raster from local or special location
        if ( base::is.null(terr_ras)) {

            # file path
            terr_file <- base::list.files(
                terr_path,
                pattern = "SR_LR.tif$",
                recursive = TRUE,
                full.names = TRUE
            )

            if ( length(terr_file) == 0 )
                base::stop(base::paste0("Could not locate a TIFF file in: ",
                                        terr_path, "\nDownload file from: ",
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
            as(., "SpatialPixelsDataFrame") %>%
            base::as.data.frame() %>%
            dplyr::rename(value = SR_LR)

        return(spdf)
    }


#' @title Get Basemap
#'
#' @param spdf        PEPFAR ORGs Spatial Data
#' @param country     OU or Country Name
#' @param terr        RasterLayer
#' @param add_admins  Should the sub-admins be added? Default is false
#' @return            ggplot plot of base map
#'
#' @examples
#' \dontrun{
#' library(gisr)
#' library(sf)
#'
#'  shp <- get_pepfar_shp(shp_path = glamr::si_path("path_vector"), add_attr = TRUE)
#'  ras <- get_raster(terr_path = glamr::si_path("path_raster"))
#'
#'  get_basemap(spdf = shp, country = "Nigeria", terr = ras)
#' }
#'
get_basemap <-
    function(spdf,
             country = NULL,
             terr = NULL,
             add_admins = FALSE) {

        # Params
        df_geo <- {{spdf}}
        cntry <- {{country}}
        dta_raster <- {{terr}}

        # Check for valid attributes
        base::stopifnot(base::all(c("id",
                                    "name",
                                    "label",
                                    "countryname",
                                    "operatingunit") %in% names(spdf)))

        # Filter by OU / Country
        if (!is.null(cntry)) {
            df_geo <- df_geo %>%
                dplyr::filter(countryname == cntry)

            base::stopifnot(base::nrow(df_geo) == 0)
        }

        # Transform geodata
        # df_geo <- df_geo %>%
        #     sf::st_as_sf() %>%
        #     sf::st_transform(., crs = sf::st_crs(4326)) %>%
        #     sf::st_zm()

        # Get country boundaries
        df_geo0 <- df_geo %>%
            dplyr::filter(label == "country")

        # Get snu1 or psnu boundaries
        df_geo1 <- df_geo %>%
            dplyr::filter(label == "snu1")

        # Terrain
        if (is.null(dta_raster)) {
            # Get raster file
            dta_raster <- get_raster()
        }

        # Crop
        terr <- dta_raster %>%
            raster::crop(x = ., y = raster::extend(raster::extent(df_geo0), .2)) %>%
            raster::mask(x = ., mask = df_geo0)

        # Convert raster data into a spatial data frame
        trdf <- terr %>%
            as("SpatialPixelsDataFrame") %>%
            as.data.frame() %>%
            dplyr::rename(value = SR_LR) %>%
            dplyr::filter(value < 210)

        # Basemap
        m <- ggplot2::ggplot() +
            ggplot2::geom_tile(data = trdf, aes(x, y, alpha = value)) +
            ggplot2::scale_alpha(name = "", range = c(0.6, 0), guide = "none") +
            ggplot2::geom_sf(
                data = df_geo0,
                colour = "white",
                fill = grey10k,
                size = 2,
                alpha = .25
            )

        # Add sub-admins boundaries
        if (add_admins & nrow(df_geo1) > 0) {
            m <- m +
                ggplot2::geom_sf(
                    data = df_geo1,
                    fill = "NA",
                    linetype = "dotted",
                    size = .5
                )
        }

        # Add country boundaries
        m <- m +
            ggplot2::geom_sf(
                data = df_geo0,
                colour = grey10k,
                fill = "NA",
                size = 1
            ) +
            ggplot2::geom_sf(
                data = df_geo0,
                colour = grey90k,
                fill = "NA",
                size = .3
            ) +
            ggplot2::theme_void()

        # Zoom to South Africa mainland
        if ("south africa" == tolower(country)) {
            m <- m +
                ggplot2::xlim(15, 35) +
                ggplot2::ylim(-38,-20)
        }

        return(m)
    }
