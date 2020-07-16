#' Map the distribution of OVC_HIVSTAT_POS PSNU Results
#'
#' @param country Operating Unit name
#' @param fy Fiscal Year (4 digit)
#' @param df_psnu PSNU by IM Dataset (data frame)
#' @param geo_psnu PSNU Geodata (sf data frame)
#' @param terr_path Path to terrain raster file
#' @export
#' @examples
#' \dontrun{
#' spdist_ovc_hivstat_pos(country = "saturn", fy = 3030,
#'     df_psnu = psnu_by_im, geo_psnu = saturn_districts)
#' spdist_ovc_hivstat_pos(country = "saturn", fy = 3030,
#'     df_psnu = psnu_by_im, geo_psnu = saturn_districts, terr_path = path_to_terr)
#' }
#'
spdist_ovc_hivstat_pos <- function(country, fy, df_psnu, geo_psnu, terr_path = NULL) {

    # Subset data to OU, FY and Indicator
    df <- df_psnu %>%
        dplyr::filter(
            fundingagency == "USAID",
            operatingunit == {{country}},
            fiscal_year == {{fy}},
            indicator == 'OVC_HIVSTAT_POS'
        ) %>%
        dplyr::select(fiscal_year, snu1, psnu, psnuuid, primepartner, mech_code, mech_name, cumulative)


    # Get Admin1 Spatial Data
    adm1 <- get_admin1({{country}})

    # Join data to Spatial Data
    df <- geo_psnu %>%
        dplyr::left_join(df, by = c("uid" = "psnuuid")) %>%
        dplyr::filter(!is.na(cumulative))

    # Plot a bar chart
    pos_bar <- df %>%
        dplyr::mutate(label = paste0(psnu, " (", cumulative, ")")) %>%
        ggplot2::ggplot(
            aes(x = reorder(label, cumulative),
                y = cumulative,
                fill = cumulative
            )
        ) +
        ggplot2::geom_col(show.legend = F) +
        ggplot2::scale_fill_viridis_c(option = "D", direction = -1) +
        ggplot2::scale_y_continuous(position = "right") +
        ggplot2::labs(x = "", y="") +
        ggplot2::coord_flip() +
        glitr::si_style_nolines() +
        ggplot2::theme(panel.grid.major.x = element_line(size = .2, color = glitr::grey10k))

    # Plot the map
    pos_m <- NULL

    if (is.null(terr_path)) {
        pos_m <- admins_map(countries = {{country}})
    }
    else {
        pos_m <- terrain_map(countries = {{country}}, terr_path = {{terr_path}}, mask = TRUE)
    }

    pos_m <- pos_m +
        ggplot2::geom_sf(data = df, aes(fill = cumulative, label = psnu), lwd = .1, color = glitr::grey10k) +
        ggplot2::geom_sf(data = adm1, fill = NA, linetype = "dotted") +
        ggplot2::geom_sf_text(data = adm1, aes(label = name), color = glitr::grey80k, size = 3) +
        ggplot2::scale_fill_viridis_c(direction = -1, na.value = glitr::grey30k) +
        ggplot2::coord_sf() +
        si_style_map() +
        ggplot2::theme(
            legend.position =  c(.9, .2),
            legend.direction = "vertical",
            legend.key.width = ggplot2::unit(.5, "cm"),
            legend.key.height = ggplot2::unit(1, "cm")
        )

    # Combine map + chart
    viz <- (pos_m + pos_bar) +
        patchwork::plot_layout(widths = c(1,1)) +
        patchwork::plot_annotation(
            title = paste0(toupper({{country}}), " - OVC_HIVSTAT_POS Results"),
            subtitle = "Districts hightlighted in grey not part of the PSNUs",
            caption = paste0("OHA/SIEI - DATIM ", {{fy}}, " PSNU x IMs Data, procuded on ", Sys.Date())
        )

    # Print and return images
    print(viz)

    return(viz)
}