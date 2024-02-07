#' @title Get Attributes Data for Orgunit Boundaries
#' @note  This will attempt to read data from local directory when folderpath is not set to null.
#' If null, username and password will be required
#'
#' @param country    OU/country
#' @param username   Datim username
#' @param password   Datim password
#' @param folderpath Local directory of files
#'
#' @return           OU Orgunit level as df
#' @export
#'
#' @examples
#' \dontrun{
#'  library(gisr)
#'
#'  get_attributes(country = "Nigeria")
#' }
#'
get_attributes <- function(country,
                           username,
                           password,
                           folderpath = NULL,
                           search = "orghierarchy",
                           baseurl = NULL) {

  file_pattern <- glue::glue("{search}.*{stringr::str_to_lower(country)}.*.csv$|{stringr::str_to_lower(country)}.*{search}.*.csv$")

  file_attrs <- NULL

  # Attempt to locate file from local drive
  if (!base::is.null(folderpath)) {

    base::message(base::paste0("Searching for: ", file_pattern))

    file_attrs <- glamr::return_latest(
      folderpath,
      pattern = file_pattern,
      recursive = TRUE)
  }

  # Attempt to read attrs from local drive
  if (!base::is.null(file_attrs) | base::length(file_attrs) != 0) {

    base::message(glue::glue("Reading from: {basename(file_attrs)}"))

    df_attrs <- file_attrs %>%
      purrr::map(~readr::read_csv(file = .x, col_types = c(.default = "c"))) %>%
      dplyr::bind_rows()

    return(df_attrs)
  }

  # Account details
  accnt <- grabr::lazy_secrets("datim", username , password)

  if (is.null(baseurl)) baseurl <- "https://final.datim.org/"

  # Get attrs from datim
  grabr::datim_orgunits(
    cntry = country,
    username = accnt$username,
    password = accnt$password,
    reshape = TRUE,
    baseurl = baseurl
  )
}


#' @title Extract Orgunit Boundaries Attributes
#'
#' @param country    OU/country
#' @param folderpath Local directory of files
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#'  library(tidyverse)
#'  library(gisr)
#'
#'  extract_attributes(country = "Nigeria")
#'  extract_attributes(country = "Nigeria", folderpath = glamr::si_path("path_vector"))
#' }
#'
extract_attributes <-
  function(country,
           folderpath = NULL) {

    # local dir & filename
    dir <- folderpath

    if (!base::is.null(dir) && !base::dir.exists(dir)) {
      base::message(glue::glue("Directory does not exist: {folderpath}"))
      base::stop("Invalid folderpath")
    }

    # Default folder
    path_vector <- glamr::si_path("path_vector")

    if (base::is.null(dir) && !base::is.null(path_vector) && base::dir.exists(path_vector)) {
      base::message(glue::glue("Data will be extracted to: {path_vector}"))
      dir <- path_vector
    }

    # No folder identified
    if (is.null(dir)) {

      base::message("Destination folder is not set")
      base::message("Consider using glamr::set_paths(folderpath_vector = '../<folder>')")

      stop("folderpath is not set")
    }

    filename <- base::file.path(dir,
                                glue::glue("orghierarchy - ",
                                "{stringr::str_to_lower(country)} - ",
                                "{base::format(base::Sys.Date(), '%Y%m%d')}",
                                ".csv"))

    #get and save country attributes
    df_attrs <- base::tryCatch(
      get_attributes(country = country),
      error = function(err) {
        base::message("See error(s) below:")
        base::print(err)
        return(NULL)
      },
      warning = function(warn) {
        base::message("See warning(s) below:")
        base::print(warn)
      }
    )

    #Write to csv file
    if (!base::is.null(df_attrs) && base::nrow(df_attrs) > 0) {
      base::message(glue::glue("Found {base::nrow(df_attrs)} records for {country}"))
      base::message(glue::glue("Writing data to: {base::basename(filename)}"))
      readr::write_csv(x = df_attrs, file = filename)
    } else {
      base::message(glue::glue("No data found for: {country}"))
    }
  }


#' @title Compress all shapefile components into a zipped file
#'
#' @param filename    Shapefile full name
#' @param dest_folder Where to place the zipped files
#'
#' @return            Boolean
#' @export
#'

zip_shapefiles <-
  function(filename,
           dest_folder = NULL) {

    # File pattern
    fileparts <- base::basename(filename) %>%
      stringr::str_remove(".shp$")

    # Where to place the zipped file
    if (base::is.null(dest_folder)) {
      dest_folder <- base::dirname(filename)
    }

    # Files to be zipped
    zipfiles <- base::list.files(path = dest_folder,
                                 pattern = fileparts,
                                 full.names = TRUE)

    # Zip files
    zip::zip(zipfile = base::file.path(dest_folder, base::paste0(fileparts, ".zip")),
             files = zipfiles,
             mode = "cherry-pick")

  }



