#' Report locations data completeness
#'
#' @param cntry Country name
#' @param mer_sites Latest MER Sites Results / Targets [cols: orgunituid, sitename]
#' @param user Datim account username
#' @param pass Datim account password (glamr::mypwd is recommended)
#' @param terr_path Path to terrain raster data
#' @param output_folder Output folder
#' @export
#' @examples
#' \dontrun{
#' generate_sites_report(cntry = "saturn", mer_sites = sites)
#' }
#'
generate_facilities_report <- function(cntry, mer_sites, user, pass,
                                  terr_path = NULL, output_folder = NULL) {

    # Extract Site Location Data
    sites <- extract_locations(country = {{cntry}}, username = {{user}}, password = {{pass}}) %>%
        extract_facilities(mer_sites = {{mer_sites}})

    # Check data
    if (is.null(sites) | nrow(sites) == 0) {
        cat("\n", crayon::red({{cntry}}), "\n")
    }

    # Map sites locations
    viz_map <- sites %>%
        explore_facilities(cntry = {{cntry}}, terr_path = {{terr_path}})

    # Plot completeness
    viz_bar <- sites %>%
        assess_facilities()

    # Combine plots
    viz <- NULL

    if ( !is.null(viz_bar) ) {

        viz <- patchwork::wrap_plots(viz_map, viz_bar, nrow = 1, widths = c(2,1)) +
            patchwork::plot_annotation(
                title = toupper({{cntry}}),
                subtitle = "Facilities location data availability",
                theme = ggplot2::theme(
                    plot.title = element_text(hjust = .5),
                    plot.subtitle = element_text(hjust = .5)
                )
            )
    }
    else {
        cat("\n", crayon::red({{cntry}}), " - bar chart is null\n")

        viz <- viz_map +
            patchwork::plot_annotation(
                title = toupper({{cntry}}),
                subtitle = "Facilities location data availability",
                theme = ggplot2::theme(
                    plot.title = element_text(hjust = .5),
                    plot.subtitle = element_text(hjust = .5)
                )
            )
    }

    # Print viz
    print(viz)

    # Export viz as png file
    if ( !is.null(output_folder) ) {
        ggplot2::ggsave(
            filename = paste0({{output_folder}}, "/", {{cntry}}, " - Sites location data availability.png"),
            plot = last_plot(), scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
    }

    return(viz)
}
