#' Generate a terrain map
#'
#' @param countries list of countries to map
#' @param terr_path path for terrain raster file
#' @param add_neighbors should the map include the neighbor countries
#' @return
#' @export
#' @examples
#' \dontrun{
#' terrain_map(countries = list("Zambia"))
#' terrain_map(countries = list("Zambia"), add_neighbors = TRUE)
#' }
#'
terrain_map <- function(countries,
                        terr_path = NULL,
                        add_neighbors = FALSE) {

    cntries <- {{countries}}

    admin0 <- get_admin0(cntries)
    admin1 <- get_admin1(cntries)

    nghbrs <- NULL

    if ( TRUE == {{add_neighbors}} ) {

        nghbrs <- geo_neighbors(cntries)

        cntries <- nghbrs %>%
            sf::st_set_geometry(NULL) %>%
            dplyr::pull(sovereignt)
    }

    ## Get terrain data
    spdf <- get_terrain(countries = cntries, terr_path = {{terr_path}})

    # Set the map range for centering the map in the ggplot call
    mapRange <- c(range(sf::st_coordinates(admin0)[, 1]), range(sf::st_coordinates(admin0)[, 2]))

    # Plot the map
    p <- ggplot2::ggplot() +
        ggplot2::geom_tile(data = dplyr::filter(spdf, value < 210), ggplot2::aes(x = x, y = y, alpha = value)) +
        ggplot2::scale_alpha(name = "", range = c(0.6, 0), guide = F)

    if ( !is.null(nghbrs) )
        p <- p +
            ggplot2::geom_sf(data = nghbrs, fill = "#d9d9d9", alpha = 0.35, size = 0.25, colour = glitr::grey70k) +
            ggplot2::geom_sf_text(data = nghbrs, ggplot2::aes(label = sovereignt), family = "Source Sans Pro" )

    p <- p +
        ggplot2::geom_sf(data = admin0, colour = "white", fill = "grey93", size = 2, alpha = 0.25) +
        ggplot2::geom_sf(data = admin0, colour = "black", fill = "NA") +
        ggplot2::geom_sf(data = admin1, fill = "NA", linetype = "dotted") +
        glitr::si_style() +
        ggplot2::theme_void()

    return(p)
}