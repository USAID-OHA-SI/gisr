#' Report locations data completeness
#'
#' @param cntry Country name
#' @param targets Latest MER Results / Targets
#' @param user Datim account username
#' @param pass Datim account password (glamr::mypwd is recommended)
#' @param terr_path Path to terrain raster data
#' @param output_folder Output folder
#' @export
#' @examples
#' \dontrun{
#' generate_sites_report(cntry = "saturn", )
#' }
#'
generate_facilities_report <- function(cntry, targets, user, pass,
                                  terr_path = NULL, output_folder = NULL) {

    # Extract Site Location Data
    sites <- extract_locations(country = {{cntry}}, username = {{user}}, password = {{pass}}) %>%
        extract_facilities(targets = {{targets}})

    # Check data
    if (is.null(sites) | nrow(sites) == 0) {

        cat("\n", cat(Wavelength::paint_red({{cntry}})), "\n")
    }

    # Map sites locations
    viz_map <- NULL

    if ( !is.null(terr_path) ) {
        viz_map <- sites %>%
            explore_facilities(cntry = {{cntry}}, terr_path = {{terr_path}})
    }
    else {
        viz_map <- sites %>%
            explore_facilities(cntry = {{cntry}})
    }

    # Plot completeness
    viz_bar <- sites %>%
        assess_facilities()

    # Combine plots
    viz <- (viz_map + viz_bar) +
        patchwork::plot_layout(widths = c(2,1)) +
        patchwork::plot_annotation(
            title = toupper({{cntry}}),
            subtitle = "Facilities location data availability",
            theme = ggplot2::theme(
                plot.title = element_text(hjust = .5),
                plot.subtitle = element_text(hjust = .5)
            )
        )

    # Export viz as png file
    if ( !is.null(output_folder) ) {
        ggplot2::ggsave(
            filename = paste0({{output_folder}}, "/", {{cntry}}, " - Sites location data availability.png"),
            scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
    }

    return(viz)
}
