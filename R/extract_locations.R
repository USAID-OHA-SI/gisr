#' Extract location data
#'
#' @param country  PEPFAR Operating Unit or Country name
#' @param username Datim Account Username
#' @param password Datim Account password
#' @param level    PEPFAR Org Level, optional
#' @param add_geom Include geometry column, default value is true
#' @param base_url Datim URL
#'
#' @return A dataframe or Null if not match
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' extract_locations("<saturn>", "<my_username>", "<my_password>")
#' }
#'
extract_locations <-
    function(country, username, password,
             level = NULL,
             add_geom = TRUE,
             base_url = NULL) {

    # API
    baseurl = "https://final.datim.org/"

    if(!base::is.null(base_url))
        baseurl <- base_url

    # OU / Country / levels
    cntry <- {{country}}
    lvl <- {{level}}

    # Get country uid
    ouuid <- get_ouuid(cntry, username, password, baseurl)

    # Get country org levels
    ou_levels <- get_levels(
        username = username,
        password = password,
        reshape = TRUE
      ) %>%
      clean_levels() %>%
      dplyr::filter(
        stringr::str_to_lower(countryname) == stringr::str_to_lower(cntry) |
          stringr::str_to_lower(operatingunit) == stringr::str_to_lower(cntry))

    # UID and Check levels
    if (is.null(ouuid) | base::nrow(ou_levels) == 0) {
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

      if (lvl %in% ou_levels$level) {
        url <- url %>% base::paste0("&filter=level:eq:", lvl)
        ou_levels <- ou_levels %>% dplyr::filter(level == lvl)
      }
    }

    # Remove pages and format as json
    url <- url %>% base::paste0("&paging=false&format=json")

    # Query OU Location data
    df <- url %>%
        httr::GET(httr::authenticate(user = username, password = password)) %>%
        httr::content("text") %>%
        jsonlite::fromJSON(flatten = T) %>%
        purrr::pluck("organisationUnits") %>%
        tibble::as_tibble()

    # Check presence of geom columns - some countries do not have geometry
    if (add_geom == TRUE & !any(stringr::str_detect(names(df), "geometry"))) {
        add_geom = FALSE
    }

    # Unpack geometry
    if (add_geom == TRUE) {
        df <- df %>%
            dplyr::rename(
                geom_type = geometry.type,          # NA or Geometry Type Value
                coordinates = geometry.coordinates  # NA, vector or list of 2 or more vectors
            ) %>%
            dplyr::mutate(
                gid = dplyr::row_number(),  # Geom ID (unique accross gid1 & gid2)
                # gid1 = row_number(), # Geom ID
                # gid2 = 1,            # Sub Geom ID (for MultiPolygon or MultiPoint)
                nodes = base::as.integer(base::lengths(coordinates) / 2), # Geom is a pair of lon / lat
                nested = base::lapply(coordinates, function(x) return(is.list(x))),
                geom_type = base::ifelse(
                  nested == TRUE & geom_type != "MultiPolygon",
                  'MultiPolygon',
                  geom_type
                )
            ) %>%
            dplyr::relocate(coordinates, .after = last_col())
    }

    # Flag org levels
    df <- df %>%
      dplyr::left_join(ou_levels,
                       by = "level",
                       relationship = "many-to-many")

    # Parse geom
    if (add_geom == TRUE) {
        df <- df %>%
            dplyr::select(operatingunit_iso,
                          operatingunit,
                          country_iso,
                          countryname,
                          label,
                          level:coordinates) %>%
            dplyr::mutate(
                geom_type = dplyr::case_when(
                    base::is.na(geom_type) & label == "facility" ~ "Point",
                    base::is.na(geom_type) & label != "facility" ~ "Polygon",
                    TRUE ~ geom_type
                ),
                geom_type = dplyr::case_when(
                  label != "facility" & nested == TRUE ~ "MultiPolygon",
                  TRUE ~ geom_type
                )
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
            country_iso = dplyr::if_else(base::is.na(country_iso), iso, country_iso))

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


