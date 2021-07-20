#' @title Generate administrative boundaries map
#'
#' @param countries list of countries to map
#' @param add_neighbors should the map include the neighbor countries
#' @return ggplot admin map
#' @export
#' @examples
#' \dontrun{
#' library(gisr)
#' admins_map(countries = list("Zambia"))
#' admins_map(countries = list("Zambia"), add_neighbors = TRUE)
#' }
#'
admins_map <- function(countries,
                       add_neighbors = FALSE) {

    cntries <- {{countries}}
    admin0 <- get_admin0(cntries)
    admin1 <- get_admin1(cntries)

    nghbrs <- NULL

    if ( TRUE == {{add_neighbors}} ) {

        nghbrs <- geo_neighbors(cntries)

        cntries <- nghbrs %>%
            sf::st_drop_geometry() %>%
            dplyr::pull(sovereignt)
    }

    # Plot the map
    p <- ggplot2::ggplot()

    if ( !is.null(nghbrs) ) {
        p <- p +
            ggplot2::geom_sf(data = nghbrs,
                             fill = "#d9d9d9",
                             alpha = 0.35,
                             size = 0.25,
                             colour = "#6d6e71") +
            ggplot2::geom_sf_text(data = nghbrs,
                                  ggplot2::aes(label = sovereignt),
                                  family = "Source Sans Pro")
    }

    p <- p +
        ggplot2::geom_sf(data = admin0,
                         colour = "white",
                         fill = "grey93",
                         size = 2,
                         alpha = 0.25) +
        ggplot2::geom_sf(data = admin0,
                         colour = "black",
                         fill = "NA") +
        ggplot2::geom_sf(data = admin1,
                         fill = "NA",
                         linetype = "dotted") +
        ggplot2::theme_void()

    return(p)
}