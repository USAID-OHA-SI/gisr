#' Generate a terrain map
#'
#' @param countries list of countries to map
#' @param adm0 Admin 0 boundaries, optional sf geodata
#' @param adm1 Admin 1 boundaries, optional sf geodata
#' @param add_neighbors should the map include the neighbor countries
#' @param terr_path path for terrain raster file
#' @param mask should the extracted data match the exact boundary limits?
#' @return
#' @export
#' @examples
#' \dontrun{
#' terrain_map(countries = list("Zambia"))
#' terrain_map(countries = list("Zambia"), add_neighbors = TRUE)
#' }
#'
terrain_map <- function(countries,
                        adm0 = NULL,
                        adm1 = NULL,
                        add_neighbors = FALSE,
                        terr_path = NULL,
                        mask = FALSE) {
    # Country name(s)
    cntries <- {{countries}}

    # Country boundaries
    if (!is.null(adm0) & !is.null(adm1)) {
        admin0 <- adm0
        admin1 <- adm1
    }
    else {
        admin0 <- get_admin0(cntries)
        admin1 <- get_admin1(cntries)
    }

    # Get neighbors id needed
    nghbrs <- NULL

    if ( TRUE == {{add_neighbors}} & !is.null(cntries) ) {

        nghbrs <- geo_neighbors(cntries)

        cntries <- nghbrs %>%
            sf::st_set_geometry(NULL) %>%
            dplyr::pull(sovereignt)
    }

    ## Get terrain data
    spdf <- get_terrain(countries = cntries,
                        mask = {{mask}},
                        terr_path = {{terr_path}})

    # Plot the map

    # Get basemap
    p <- ggplot2::ggplot() +
        ggplot2::geom_tile(data = dplyr::filter(spdf, value < 210), ggplot2::aes(x = x, y = y, alpha = value)) +
        ggplot2::scale_alpha(name = "", range = c(0.6, 0), guide = F)

    # Add neighbors
    if ( !is.null(nghbrs) )
        p <- p +
            ggplot2::geom_sf(data = nghbrs, fill = "#d9d9d9", alpha = 0.35, size = 0.25, colour = glitr::grey70k) +
            ggplot2::geom_sf_text(data = nghbrs, ggplot2::aes(label = sovereignt), family = "Source Sans Pro" )

    # Add country and admin 1 layers
    p <- p +
        ggplot2::geom_sf(data = admin0, colour = "white", fill = "grey93", size = 2, alpha = 0.25) +
        ggplot2::geom_sf(data = admin0, colour = "black", fill = "NA") +
        ggplot2::geom_sf(data = admin1, fill = "NA", linetype = "dotted")

    # Zoom to South Africa mainland
    if ("south africa" %in% tolower(cntries))
        p <- p +
            ggplot2::coord_sf(xlim = c(), ylim = c())

    # apply theme
    p <- p + glitr::si_style() +
        ggplot2::theme_void()

    return(p)
}