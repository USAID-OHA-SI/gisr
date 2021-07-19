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

    # Get neighors
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
        ggplot2::scale_alpha(name = "", range = c(0.6, 0), guide = F)

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
#' @param terr_path path to terrain raster file
#' @param name Name of the raster file
#'
#' @return RasterLayer
#' @export
#'
#' @examples
#' \dontrun{
#' get_raster(terr_path = glamr::si_path("path_raster"))
#' }
#'
get_raster <-
    function(terr_path = "../../GEODATA/RASTER",
             name = "SR_LR.tif") {

        # Check directory
        if (!base::dir.exists(terr_path))
            stop(base::cat("\nInvalid terrain directory: ",
                           crayon::red(terr_path),
                           "\n"))

        # Identify file path
        terr_file <- base::list.files(
            path = terr_path,
            pattern = base::paste0(name, "$"),
            recursive = TRUE,
            full.names = TRUE
        )

        # Check file
        if (!base::file.exists(terr_file))
            stop(base::cat("\nFile does not exist: ", terr_file, "\n"))

        # Read file content
        ras <- raster::raster(terr_file)

        return(ras)
    }
