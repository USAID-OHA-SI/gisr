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


#' Map the distribution of OVC TX_CURR Achievement by PSNU
#'
#' @param country Operating Unit name
#' @param fy Fiscal Year (4 digit)
#' @param df_psnu PSNU by IM Dataset (data frame)
#' @param geo_psnu PSNU Geodata (sf data frame)
#' @param terr_path Path to terrain raster file
#' @export
#' @examples
#' \dontrun{
#' spdist_ovc_hivstat_pos(country = "saturn", fy = 3030, df_psnu = psnu_by_im, geo_psnu = saturn_districts)
#' spdist_ovc_hivstat_pos(country = "saturn", fy = 3030, df_psnu = psnu_by_im, geo_psnu = saturn_districts, terr_path = path_to_terr)
#' }
#'
spdist_ovc_tx_curr <- function(country, fy, df_psnu, geo_psnu, terr_path = NULL) {

    # Subset data to OU, FY and Indicator
    df <- df_psnu %>%
        dplyr::filter(
            fundingagency == "USAID",
            operatingunit == {{country}},
            fiscal_year == {{fy}},
            indicator == 'TX_CURR',
            disaggregate == 'Age/Sex/HIVStatus'
        ) %>%
        select(snu1, psnu, psnuuid, primepartner, mech_code, mech_name, disaggregate, trendsfine, targets:cumulative) %>%
        mutate(age_group = ifelse(trendsfine %in% c("<01", "01-09", "10-14", "15-19"), "OVC", "Other")) %>%
        relocate(age_group, .after = trendsfine) %>%
        filter(age_group == 'OVC') %>%
        group_by(snu1, psnu, psnuuid, age_group) %>%
        summarise_at(.vars = vars(targets:cumulative), .funs = sum, na.rm = TRUE) %>%
        ungroup() %>%
        mutate(
            ovc_tx_curr_ach = round(cumulative / targets * 100),
            ovc_tx_curr_cat = cut(ovc_tx_curr_ach, breaks = c(-Inf, 49, 89, Inf), labels = c("<50%", "50-89%", "+90%"))
        ) %>%
        arrange(snu1, psnu, desc(ovc_tx_curr_ach)) %>%
        dplyr::mutate(label = paste0(psnu, " (", ovc_tx_curr_ach, ")"))


    # Get Admin1 Spatial Data
    adm1 <- gisr::get_admin1({{country}})

    # Join data to Spatial Data
    df <- geo_psnu %>%
        dplyr::left_join(df, by = c("uid" = "psnuuid")) %>%
        dplyr::filter(!is.na(psnu))

    # Plot a bar chart
    tx_curr_bar <- df %>%
        ggplot2::ggplot(
            aes(x = reorder(label, ovc_tx_curr_ach),
                y = ovc_tx_curr_ach,
                fill = ovc_tx_curr_cat
            )
        ) +
        ggplot2::geom_point(aes(size = targets, color = ovc_tx_curr_cat), alpha = 2/3, show.legend = F) +
        ggplot2::scale_color_manual(values = c("#d7191c", "#dfc27d", "#008837")) +
        ggplot2::scale_y_continuous(position = "right", labels = function(x) {paste0(x, "%")}) +
        ggplot2::labs(x = "", y="") +
        ggplot2::coord_flip() +
        glitr::si_style_nolines() +
        ggplot2::theme(panel.grid.major.x = element_line(size = .2, color = grey10k))

    # Plot the map
    tx_curr_m <- NULL

    if (is.null(terr_path)) {
        tx_curr_m <- gisr::admins_map(countries = {{country}})
    }
    else {
        tx_curr_m <- gisr::terrain_map(countries = {{country}}, terr_path = {{terr_path}}, mask = TRUE)
    }

    tx_curr_m <- tx_curr_m +
        ggplot2::geom_sf(data = df, aes(fill = ovc_tx_curr_cat, alpha = 2/3), lwd = .1, color = grey10k) +
        ggplot2::geom_sf(data = adm1, fill = NA, linetype = "dotted") +
        ggplot2::geom_sf_text(data = adm1, aes(label = name), color = grey80k, size = 3) +
        ggplot2::scale_fill_manual(values = c("#d7191c", "#dfc27d", "#008837")) +
        ggplot2::coord_sf() +
        gisr::si_style_map() +
        guides(fill = guide_legend(override.aes = list(alpha = 2/3))) +
        ggplot2::theme(
            legend.position =  c(.9, .2),
            legend.direction = "vertical",
            legend.key.width = ggplot2::unit(.5, "cm"),
            legend.key.height = ggplot2::unit(1, "cm")
        )

    # Combine map + chart
    viz <- (tx_curr_m + tx_curr_bar) +
        plot_layout(widths = c(1,1)) +
        plot_annotation(
            title = paste0(toupper({{country}}), " - TX_CURR Achievement by PSNU (Age < 20)"),
            subtitle = "The size of the circles represent the targets and the colors the achievements",
            caption = paste0("OHA/SIEI - DATIM ", {{fy}}, " PSNU x IMs Data, procuded on ", Sys.Date())
        )

    # Print and return images
    print(viz)

    return(viz)
}

