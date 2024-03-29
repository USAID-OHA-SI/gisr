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

    # Get terrain raster
    spdf <- get_terrain(countries = cntries,
                        terr = {{terr}},
                        mask = {{mask}})

    # Get neighbors
    if ( TRUE == {{add_neighbors}} & !is.null(cntries) ) {
      # Get neighbors
      adms <- get_nepolygons()
      nghbrs <- geo_neighbors(src = adms, countries = cntries, crop = TRUE)
      cntries <- nghbrs
    }

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
                       linewidth = 0.25,
                       colour = "#6d6e71")

    if ( !is.null(nghbrs) & add_labels == TRUE)
      p <- p +
      ggplot2::geom_sf_text(data = nghbrs,
                            ggplot2::aes(label = sovereignt),
                            family = "Source Sans Pro")

    # Add country layers
    p <- p +
      ggplot2::geom_sf(data = admin0,
                       colour = "white",
                       fill = "grey93",
                       linewidth = 2,
                       alpha = 0.25) +
      ggplot2::geom_sf(data = admin0,
                       colour = "black",
                       fill = "NA")

    # Add admin 1 layer
    if ( !is.null(admin1) )
      p <- p +
      ggplot2::geom_sf(data = admin1,
                       fill = "NA",
                       linetype = "dotted")

    # Zoom to South Africa mainland
    if (base::is.character(cntries)) {
      if ("south africa" %in% stringr::str_to_lower(cntries)) {
        p <- p +
          ggplot2::xlim(15, 35) +
          ggplot2::ylim(-38, -20)
      }
    }

    # apply theme
    p <- p + ggplot2::theme_void()

    return(p)
  }



#' @title Get Terrain Raster dataset
#'
#' @param folderpath Path to raster file, default will be `si_path('path_raster')`
#' @param name Name of the raster file (with extension), default is set to terrain raster `SR_LR.tif`
#' @param rename Should the RasterLayer be renamed? If yes, the name is changed to `value`
#' @param ...  Additional arguments to be passed to `base::list.files`. Eg: `Use ignore.case = TRUE` for non case sensitive search
#'
#' @return RasterLayer
#' @export
#'
#' @examples
#' \dontrun{
#'  library(glamr)
#'  library(gisr)
#'
#'  get_raster()
#'
#'  get_raster(name = "sample.tif")
#'  get_raster(path = "./geodata/raster", name = "sample.tif")
#' }
#'
get_raster <- function(folderpath,
                       name = NULL,
                       rename = FALSE,
                       ...) {

  # Check parameters

  # File name
  if (base::is.null(name)) {
    name <- "SR_LR.tif"
  }

  # Check directory
  if (!base::dir.exists(folderpath)) {
    stop(base::cat("\nInvalid directory: ", crayon::red(folderpath), "\n"))
  }

  # Identify file path
  terr_file <- base::list.files(
    path = folderpath,
    pattern = base::paste0(name, "$"),
    recursive = TRUE,
    full.names = TRUE,
    ...
  )

  # Check file
  if (base::length(terr_file) == 0) {
    stop(base::cat(crayon::red("\nNo raster file found\n")))
  } else if (base::length(terr_file) > 1) {
    stop(base::cat(crayon::red("\nMultiple raster files found: ", base::paste(terr_file, collapse = ','), "\n")))
  }

  if (base::length(terr_file) == 1 & !base::file.exists(terr_file)) {
    stop(base::cat("\nFile does not exist: ", terr_file, "\n"))
  }

  # Read file content
  ras <- terra::rast(terr_file)

  if (rename) {
    names(ras) <- "value"
  }

  return(ras)
}


#' @title Extract raster data for an AOI (Countries)
#'
#' @param countries  List of the country names or sf object
#' @param ras        RasterLayer or Path to raster file
#' @param mask       Should the extracted data match the exact boundary limits?
#' @param buffer     Extend AOI extent by x
#'
#' @importFrom       methods as
#'
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
#' }
#'
extract_raster <-
  function(countries, ras,
           mask = FALSE,
           buffer = .1) {

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
      } else {
        stop("Countries list does not seems to match Natural Earth Countries")
      }
    }

    # Raster Data
    dta_ras <- NULL
    ras_path <- NULL  #"./GIS/"

    # Locate and retrieve terrain file
    if (!base::is.null(ras) && "character" %in% base::class(ras) && base::dir.exists(ras)) {
      dta_ras <- get_raster(folderpath = ras, rename = TRUE)
    } else if (!base::is.null(ras) && "RasterLayer" %in% base::class(ras)) {
      dta_ras <- ras
    }

    # Validate RasterLayer
    if ( base::is.null(dta_ras)) {
      base::stop("Invalid raster data / path. Download raster file into si_path('path_raster')")
    }

    # Crop raster to boundaries extent
    aoi_ras <- dta_ras %>%
      terra::crop(terra::extend(terra::ext(aoi), {{buffer}}))

    # Crop to the exact limits if applicable
    if ( mask == TRUE ) {
      aoi_ras <- aoi_ras %>% terra::mask(aoi)
    }

    # Rename RasterLayer
    if (!"value" %in% names(aoi_ras)) {
      names(aoi_ras) <- "value"
    }

    # Convert raster data into a spatial data frame
    spdf <- aoi_ras %>%
      terra::as.data.frame(xy=TRUE)

    return(spdf)
  }


#' @title Get terrain data for an AOI (Countries)
#'
#' @note `get_terrain()` will evantually be replaced by `extract_raster()`
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
#' }
#'
get_terrain <-
  function(countries, terr,
           mask = FALSE,
           buffer = .1) {

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
      } else {
        stop("Countries list does not seems to match Natural Earth Countries")
      }
    }

    # Raster Data
    terr_ras <- NULL

    # Locate and retrieve terrain file
    if (!base::is.null(terr) && "character" %in% base::class(terr) && dir.exists(terr)) {
      terr_ras <- get_raster(folderpath = terr, rename = TRUE)
    } else if (!base::is.null(terr) && "SpatRaster" %in% base::class(terr)) {
      terr_ras <- terr
    }

    # Validate RasterLayer
    if ( base::is.null(terr_ras)) {
      base::stop("Invalid terrain data / path. Download terrain file into si_path('path_raster')")
    }

    # Crop raster to boundaries extent
    terr_ras <- terr_ras %>%
      terra::crop(terra::extend(terra::ext(aoi), {{buffer}}))

    # Crop to the exact limits if applicable
    if ( mask == TRUE ) {
      terr_ras <- terr_ras %>% terra::mask(aoi)
    }

    # Rename RasterLayer
    if (!"value" %in% names(terr_ras)) {
      names(terr_ras) <- "value"
    }

    # Convert raster data into a spatial data frame
    spdf <- terr_ras %>%
      terra::as.data.frame(xy=TRUE)

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
  function(spdf, terr,
           country = NULL,
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
    if (is.null(dta_raster) && "character" %in% base::class(dta_raster) && dir.exists(dta_raster) ) {
      # Get raster file
      dta_raster <- get_raster(folderpath = terr)
    }

    # Crop
    terr <- dta_raster %>%
      terra::crop(x = ., y = terra::extend(terra::ext(df_geo0), .2)) %>%
      terra::mask(x = ., mask = df_geo0)

    # Convert raster data into a spatial data frame
    trdf <- terr %>%
      terra::as.data.frame(xy=TRUE) %>%
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
        linewidth = 1
      ) +
      ggplot2::geom_sf(
        data = df_geo0,
        colour = grey90k,
        fill = "NA",
        linewidth = .3
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
