#' Extract location data
#'
#' @param country  PEPFAR Operating Unit or Country name
#' @param username Datim Account Username
#' @param password Datim Account password
#' @param level    PEPFAR Org Level, optional
#' @param add_geom Include geometry column, default value is true
#' @param baseurl Datim URL
#'
#' @return A dataframe or Null if not match
#'
#' @examples
#' \dontrun{
#'
#' extract_locations("<saturn>", "<my_username>", "<my_password>")
#' }
#'
extract_locations <-
    function(country,
             username,
             password,
             level = NULL,
             add_geom = TRUE,
             baseurl = NULL) {

    # Account details
    accnt <- grabr::lazy_secrets("datim", username, password)

    # Depreciation
    .Deprecated(
      new = "datim_pull_hierarchy",
      package = "gisr",
      msg = "All DATIM API related functions have been moved to `grabr` package"
    )

  # Ignore level param
  if (!is.null(level)) usethis::ui_warn("`level` filter is being disregarded at the moment. Consider filtering the output.")

  # get country uid
  uid <- grabr::get_ouuid(country)

  # pass request to grabr
  grabr::datim_pull_hierarchy(ou_uid = uid,
                              username = username,
                              password = password,
                              add_geom = add_geom)
}


#' Extract facility sites
#'
#' @param .data Datim organisation units data frame - with label and coordinates columns
#'
#' @examples
#' \dontrun{
#'   grabr::datim_pull_hierarchy(...) %>%
#'     extract_facilities()
#' }
#'
extract_facilities <- function(.data) {

  ## Check input df structure
  req_cols <- c("label", "coordinates")

  if (!all(req_cols %in% req_cols))
    usethis::ui_warn("Input dataset is missing key variable(s): label, coordinates")

  return(.data)

  # Filter dataset and unpack coordinates
  .data %>%
    dplyr::filter(label == "facility") %>%
    tidyr::unnest_wider(coordinates, names_sep = "_") %>%
    janitor::clean_names() %>%
    dplyr::rename(longitude = "coordinates_1", latitude = "coordinates_2")
}


