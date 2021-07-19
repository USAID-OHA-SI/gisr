#' Explore facility locations
#'
#' @param .data Datim organisation units data frame
#' @param cntry Country name
#' @param terr_path Path to terrain raster dataset
#' @export
#' @examples
#' \dontrun{
#' explore_facilities(df_sites, cntry = "saturn")
#' df_sites %>% explore_facilities("saturn")
#' }
#'
explore_facilities <- function(.data, cntry, terr_path = NULL) {

    # Make sure to use rnaturalearth version of the name
    country <- dplyr::case_when(
        cntry == "Cote d'Ivoire" ~ "Ivory Coast",
        cntry == "Eswatini" ~ "Swaziland",
        cntry == "Tanzania" ~ "United Republic of Tanzania",
        TRUE ~ {{cntry}}
    )

    # Count non valid lat/lon
    na_sites <- .data %>%
        dplyr::filter(is.na(longitude) | is.na(latitude)) %>%
        nrow()

    if (na_sites > 0) {
        cat(paste0("\nThere are missing lat/lon: ", crayon::red(na_sites), "\n"))
    }

    # Get a basemap: terrain or country boundaries
    if (!is.null(terr_path)) {

        m <- gisr::terrain_map(countries = country,
                               terr_path = {{terr_path}},
                               mask = TRUE)
    }
    else {

        m <- ggplot2::ggplot() +
            ggplot2::geom_sf(data = gisr::get_admin0(countries = country), fill = NA)
    }

    # Overlay facility data, if data exists
    if ( nrow(.data) > 0 ) {

        m <-  m +
            ggplot2::geom_sf(
                data = .data %>%
                    dplyr::filter(!is.na(longitude) | !is.na(latitude)) %>%
                    sf::st_as_sf(coords = c("longitude", "latitude"),
                                 crs = 4326),
                shape = 21,
                size = 3,
                colour = "white",
                fill = "#808080",
                stroke = .5,
                alpha = 2/3
            ) +
            ggplot2::coord_sf() +
            ggplot2::theme_void()
    }
    else {

        m <-  m +
            ggplot2::coord_sf() +
            ggplot2::theme_void()
    }

    return(m)
}


#' Assess facility geo-location reporting levels
#'
#' @param .data Datim organisation units data frame
#' @export
#' @examples
#' \dontrun{
#' assess_facilities(df_sites)
#' df_sites %>% assess_facilities()
#' }
assess_facilities <- function(.data) {

    if ( !base::is.data.frame(.data) | base::nrow(.data) == 0 ) {
        return(NULL)
    }

    p <- .data %>%
        dplyr::mutate(
            valid_geom = ifelse(is.na(latitude) | is.na(longitude),
                                "Missing",
                                "Available"),
            valid_geom = base::as.factor(valid_geom)) %>%
        dplyr::group_by(valid_geom) %>%
        dplyr::tally() %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            p = round(n / sum(n) * 100),
            t = 100
        ) %>%
        ggplot2::ggplot(aes(valid_geom, p, fill = valid_geom, label = p)) +
        ggplot2::geom_hline(yintercept = 0, color = "#d1d3d4") +
        ggplot2::geom_hline(yintercept = 25, color = "#d1d3d4") +
        ggplot2::geom_hline(yintercept = 50, color = "#d1d3d4") +
        ggplot2::geom_hline(yintercept = 75, color = "#d1d3d4") +
        ggplot2::geom_hline(yintercept = 100, color = "#d1d3d4") +
        ggplot2::geom_col(aes(y = t), fill = "#E6E7E8") +
        ggplot2::geom_col(position = position_dodge(), show.legend = FALSE) +
        ggplot2::geom_label(aes(label = format(n, big.mark = ",", scientific = FALSE)),
                            color = "white", show.legend = FALSE) +
        ggplot2::scale_y_continuous(labels = function(n){base::paste0(n, "%")}) +
        ggplot2::scale_fill_manual(values = c("#66c2a5", "#fc8d62")) +
        ggplot2::labs(title = "", x = "", y = "") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank()
        )

    return(p)
}

