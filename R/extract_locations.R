#' Extract location data
#'
#' @param country  PEPFAR Operatingunit
#' @param level    PEPFAR Org Level
#' @param add_geom Include geometry column
#' @param username Datim Account Username
#' @param password Datim Account password
#' @param base_url Datim URL
#' @export
#' @examples
#' \dontrun{
#' extract_locations("saturn", "my_username", "my_password")
#' }
extract_locations <-
    function(country,
             level = NULL,
             add_geom = TRUE,
             username = NULL,
             password = NULL,
             base_url = NULL) {

    # API
    baseurl = "https://final.datim.org/"

    if(!base::is.null(base_url))
        baseurl <- base_url

    # OU / Country / levels
    cntry <- {{country}}
    lvl <- {{level}}

    # Access
    user <- base::ifelse(base::is.null(username),
                         glamr::datim_user(),
                         {{username}})

    pass <- base::ifelse(base::is.null(password),
                         glamr::datim_pwd(),
                         {{password}})

    # Get country uid
    ouuid <- get_ouuid(cntry, username = user, password = pass)

    # Get country org levels
    ou_levels <- get_levels(username = user, password = pass) %>%
        tidyr::pivot_longer(cols = country:prioritization,
                            names_to = "label",
                            values_to = "level") %>%
        dplyr::filter(stringr::str_to_lower(countryname) == stringr::str_to_lower(cntry))

    # Check levels
    if (base::nrow(ou_levels) == 0) {
        base::cat(crayon::red(glue::glue("\nUnable to retrieve org levels for [{cntry}]!\n")))
        return(NULL)
    }

    # Query OU Location data
    url <- baseurl %>%
        base::paste0("api/organisationUnits?fields=id,name,path,level")

    # Include geometry columns if needed
    geom_cols <- NULL

    if (add_geom == TRUE) {
       url <- url %>% base::paste0(",geometry")
       geom_cols <- c("geometry.type", "geometry.coordinates")
    }

    # filter by ou/country uid, note: regional countries will also extract region info
    url <- url %>% base::paste0("&filter=path:like:", ouuid)

    # Filter specific level
    if (!is.null(lvl)) {
        url <- url %>% base::paste0("&filter=level:eq:", lvl)
        ou_levels <- ou_levels %>% dplyr::filter(level == lvl)
    }

    # Remove pages and format as json
    url <- url %>% base::paste0("&paging=false&format=json")

    # Query OU Location data
    df <- url %>%
        httr::GET(httr::authenticate(user, pass)) %>%
        httr::content("text") %>%
        jsonlite::fromJSON(flatten = T) %>%
        purrr::pluck("organisationUnits") %>%
        tibble::as_tibble()

    # Check presence of geom columns
    if (add_geom == TRUE) {
        if(base::is.null(geom_cols) | (!base::is.null(geom_cols) & !geom_cols[1] %in% base::names(df))) {
            add_geom = FALSE
        }
    }

    # Unpack geometry
    if (add_geom == TRUE) {
        df <- df %>%
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
    }

    # Flag org levels
    df <- df %>% dplyr::left_join(ou_levels, by = "level")

    # Parse geom
    if (add_geom == TRUE) {
        df <- df %>%
            dplyr::select(operatingunit_iso,
                          countryname_iso,
                          operatingunit,
                          countryname,
                          label,
                          level:coordinates) %>%
            dplyr::mutate(
                geom_type = dplyr::case_when(
                    base::is.na(geom_type) & label == "facility" ~ "Point",
                    base::is.na(geom_type) & label != "facility" ~ "Polygon",
                    TRUE ~ geom_type
                ),
                geom_type = dplyr::if_else(label != "facility" & nested == TRUE, "MultiPolygon", geom_type)
            )
    }

    # iso code
    iso <- df %>%
        dplyr::distinct(operatingunit_iso) %>%
        dplyr::pull() %>%
        dplyr::first()

    ou <- df %>%
        dplyr::filter(!is.na(operatingunit)) %>%
        dplyr::distinct(operatingunit) %>%
        dplyr::pull() %>%
        dplyr::first()

    # Cleanup data
    df <- df %>%
        dplyr::mutate(
            operatingunit = dplyr::if_else(base::is.na(operatingunit), ou, operatingunit),
            countryname = dplyr::if_else(base::is.na(countryname), cntry, countryname),
            operatingunit_iso = dplyr::if_else(base::is.na(operatingunit_iso), iso, operatingunit_iso),
            countryname_iso = dplyr::if_else(base::is.na(countryname_iso), iso, countryname_iso),
            label = dplyr::if_else(base::is.na(label) & level == 4, "snu1", label),
            label = dplyr::if_else(base::is.na(label) & level == 5, "snu2", label))

    return(df)
}


#' Extract facility sites
#'
#' @param .data Datim organisation units data frame
#' @param mer_sites Data Frame of MER Sites by IM with Results and/or Targets (cols: orgunituid, sitename)
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

    if ( !is.null(mer_sites) & ("orgunituid" %in% names(mer_sites)) & ("sitename" %in% names(mer_sites)) ) {

        .data <- .data %>%
            dplyr::left_join(mer_sites, by = c("id" = "orgunituid")) %>%
            dplyr::filter(!is.na(sitename)) %>%
            dplyr::select(-sitename)
    }
    else {
        cat(crayon::red("\nMER Sites are missing or invalid. Make sure to include orgunituid and sitename columns."))
    }

    return(.data)
}


