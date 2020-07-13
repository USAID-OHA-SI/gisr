#' Extract location data
#'
#' @param country PEPFAR Operating Unit or Regional Countries
#' @param username Datim Account Username
#' @param password Datim Account Key
#' @export
#' @examples
#' \dontrun{
#' extract_locations("saturn", "my_username", "my_password")
#' }
extract_locations <- function(country, username, password) {

    baseurl = "https://final.datim.org/"

    cntry <- {{country}}
    user <- {{username}}
    key <- {{password}}

    # Get country uid
    ouuid <- get_ouuid(cntry, username = user, password = key)

    # Get country org levels
    ou_levels <- Wavelength::identify_levels(ou = cntry, username = user, password = key) %>%
        dplyr::relocate(dplyr::last_col(), .after = name4) %>%
        tidyr::gather(key = "label", value = "level", -c(1:5))

    # Query OU Location data
    df <- baseurl %>%
        paste0("api/organisationUnits?filter=path:like:", ouuid,
               "&fields=id,name,path,level,geometry&paging=false&format=json") %>%
        httr::GET(httr::authenticate(user, key)) %>%
        httr::content("text") %>%
        jsonlite::fromJSON(flatten = T) %>%
        purrr::pluck("organisationUnits") %>%
        tibble::as_tibble() %>%
        dplyr::rename(
            geom_type = geometry.type,          # NA or Geometry Type Value
            coordinates = geometry.coordinates  # NA or list of 2 or more
        ) %>%
        dplyr::mutate(
            gid = row_number(),  # Geom ID (unique accross gid1 & gid2)
            # gid1 = row_number(), # Geom ID
            # gid2 = 1,            # Sub Geom ID (for MultiPolygon or MultiPoint)
            nodes = as.integer(lengths(coordinates) / 2), # Geom is a pair of lon / lat
            nested = lapply(coordinates, function(x) return(is.list(x))),
            geom_type = ifelse(nested == TRUE & geom_type != "MultiPolygon", 'MultiPolygon', geom_type)
        ) %>%
        dplyr::relocate(coordinates, .after = last_col())

    # Flag org categories
    df <- df %>%
        left_join(ou_levels, by = "level") %>%
        dplyr::select(operatingunit = name3, country_name, label, level:coordinates) %>%
        dplyr::mutate(
            label = ifelse(is.na(label) & level == 4, "SNU1", label),
            geom_type = case_when(
                is.na(geom_type) & label == "facility" ~ "Point",
                is.na(geom_type) & label != "facility" ~ "Polygon",
                TRUE ~ geom_type
            ),
            geom_type = ifelse(label != "facility" & nested == TRUE, "MultiPolygon", geom_type)
        )

    return(df)
}


#' Extract facility sites
#'
#' @param .data Datim organisation units data frame
#' @param targets Data Frame of MER Results / Targets
#' @export
#' @examples
#' \dontrun{
#' extract_facilities(df_orgunits)
#' df_orgunits %>% extract_facilities()
#' }
#'
extract_facilities <- function(.data, targets = NULL) {

    .data <- .data %>%
        dplyr::filter(label == "facility") %>%
        tidyr::unnest_wider(coordinates) %>%
        janitor::clean_names() %>%
        dplyr::rename(longitude = "x1", latitude = "x2")

    if ( !is.null(targets) ) {

        .data <- .data %>%
            dplyr::left_join(
                targets %>%
                    dplyr::filter(!is.na(mer_results)) %>%
                    dplyr::distinct(orgunit, orgunituid),
                by = c("id" = "orgunituid")
            ) %>%
            dplyr::filter(!is.na(orgunit)) %>%
            dplyr::select(-orgunit)
    }

    return(.data)
}