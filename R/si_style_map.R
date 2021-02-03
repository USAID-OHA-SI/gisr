#' @title Standard SI Style for Thematic Maps
#'
#' @param ... Any other ggplot theme arguments
#'
#' @description This style borrows from Timo Grossenbacher's thematic maps to match OHA/SIEI Viz Products
#' @export
#' @examples
#' \dontrun{
#' get_admin0(countries = "Kenya") %>%
#'   ggplot() +
#'   geom_sf(fill = glitr::grey10k) +
#'   labs(
#'     title = "COUNTRY ABC - Sample thematic map",
#'     subtitle = "Region XYZ is over testing your males",
#'     caption = paste0("OHA/SIEI - Data from DATIM FY?? Q?, ", Sys.Date())
#'   ) +
#'   si_style_map()
#' }
#'
si_style_map <- function(...) {

    # Start with a clean ggplot theme
    ggplot2::theme_void() +
        # Add only map related elements
        ggplot2::theme(
            # define font family & color
            text = ggplot2::element_text(family = "Source Sans Pro", color = glitr::color_plot_text),
            plot.margin = ggplot2::unit(c(5, 5, 5, 5), "pt"),
            panel.border = ggplot2::element_blank(),
            panel.spacing = ggplot2::unit(2, "lines"),
            # legend
            legend.position = "bottom", #or use c(x,y)
            legend.justification = "center",
            legend.direction = "horizontal",
            legend.title = ggplot2::element_blank(), # better to insert [legend title[] in the the plot [subtitle]
            legend.text = ggplot2::element_text(size = 9, hjust = 0.5),
            legend.key.width = ggplot2::unit(2, "cm"),
            # Plot headers + notes
            plot.title = ggplot2::element_text(
                size = 14,
                face = "bold",
                hjust = 0.5,
                color = glitr::color_title,
                margin = ggplot2::margin(b = 5, unit = "pt")),
            plot.subtitle = ggplot2::element_text(
                size = 12,
                hjust = 0.5,
                color = glitr::color_title,
                margin = ggplot2::margin(b = 5, unit = "pt")),
            plot.caption = ggplot2::element_text(
                size = 9,
                hjust = 0.5,
                face = "italic",
                margin = ggplot2::margin(t = 5, b = 10, unit = "pt"),
                color = glitr::color_caption),
            ...
        )
}