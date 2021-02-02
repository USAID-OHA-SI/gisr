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
extract_locations <-
    function(country,
             level = NULL,
             add_geom = TRUE,
             username, password) {

    baseurl = "https://final.datim.org/"

    cntry <- {{country}}
    lvl <- {{level}}

    user <- {{username}}
    key <- {{password}}

    # Get country uid
    ouuid <- get_ouuid(cntry, username = user, password = key)

    # Get country org levels
    ou_levels <- Wavelength::identify_levels(ou = cntry, username = user, password = key) %>%
        dplyr::relocate(dplyr::last_col(), .after = name4) %>%
        tidyr::gather(key = "label", value = "level", -c(1:5))

    # Query OU Location data
    url <- baseurl %>%
        paste0("api/organisationUnits?",
               "fields=id,name,path,level,geometry",
               "&filter=path:like:", ouuid)

    # Filter specific levels
    if (!is.null(lvl)) {

        url <- url %>%
            paste0(url,
                   "&filter=level:eq:", lvl,
                   "&paging=false&format=json")

        ou_levels <- ou_levels %>% filter(level == lvl)
    }

    # Check levels
    if (base::nrow(ou_levels) == 0) {
        base::cat(crayon::red("\nNo records found!\n"))

        return(NULL)
    }

    # Query OU Location data
    df <- url %>%
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
            gid = dplyr::row_number(),  # Geom ID (unique accross gid1 & gid2)
            # gid1 = row_number(), # Geom ID
            # gid2 = 1,            # Sub Geom ID (for MultiPolygon or MultiPoint)
            nodes = base::as.integer(base::lengths(coordinates) / 2), # Geom is a pair of lon / lat
            nested = base::lapply(coordinates, function(x) return(is.list(x))),
            geom_type = base::ifelse(nested == TRUE & geom_type != "MultiPolygon", 'MultiPolygon', geom_type)
        ) %>%
        dplyr::relocate(coordinates, .after = last_col())

    # Flag org categories
    df <- df %>%
        left_join(ou_levels, by = "level") %>%
        dplyr::select(operatingunit = name3, country_name, label, level:coordinates) %>%
        dplyr::mutate(
            label = ifelse(is.na(label) & level == 4, "snu1", label),
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
#' @param mer_sites Data Frame of MER Sites by IM with Results and/or Targets [cols: orgunituid, sitename]
#' @export
#' @examples
#' \dontrun{
#' extract_facilities(df_orgunits)
#' df_orgunits %>% extract_facilities()
#' }
#'
extract_facilities <- function(.data, mer_sites = NULL) {

    .data <- .data %>%
        dplyr::filter(label == "facility") %>%
        tidyr::unnest_wider(coordinates) %>%
        janitor::clean_names() %>%
        dplyr::rename(longitude = "x1", latitude = "x2")

    if ( !is.null(mer_sites) & "orgunituid" %in% names(mer_sites) & "sitename" %in% names(mer_sites) ) {

        .data <- .data %>%
            dplyr::left_join(mer_sites, by = c("id" = "orgunituid")) %>%
            dplyr::filter(!is.na(sitename)) %>%
            dplyr::select(-sitename)
    }
    else {
        cat(Wavelength::paint_red("\nMER Sites are missing or invalid. Make sure to include orgunituid and sitename columns."))
    }

    return(.data)
}