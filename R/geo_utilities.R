#' Plot polygon features
#'
#' @param geodata r spatial data
#' @export
#' @example
#' geo_viz(data)
#'
geo_viz <- function(geodata) {

    viz <- geodata %>%
        ggplot2::ggplot() +
        ggplot2::geom_sf(fill=NA, lwd=.3, color="#BCBEC0") +
        ggplot2::coord_sf() +
        ggplot2::theme_void()

    print(viz)

    return(viz)
}

