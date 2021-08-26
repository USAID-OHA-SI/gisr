#' @title Get Org UIDS
#' @note Use with caution. Use `get_ouorguids()` for levels below 3
#'
#' @param level    Org level
#' @param username DATIM Username, recommend using `datim_user()`
#' @param password DATIM password, recommend using `datim_pwd()`
#' @param baseurl base url for the API, default = https://final.datim.org/
#'
#' @return ORG UIDS as tibble
#'
#'
#' @examples
#' \dontrun{
#'  library(gisr)
#'
#'  # All orgunit level 3 uids + names
#'  orgs <- get_orguids(level = 3)
#' }
#'
get_orguids <-
  function(level = 3,
           username = NULL,
           password = NULL,
           baseurl = "https://final.datim.org/"){

    # Params
    lvl <- {{level}}

    user <- base::ifelse(base::is.null({{username}}),
                         glamr::datim_user(), {{username}})

    pass <- base::ifelse(base::is.null({{password}}),
                         glamr::datim_pwd(), {{password}})

    # Query ou
    orgs <- baseurl %>%
      paste0("api/organisationUnits",
             "?filter=level:eq:", lvl) %>%
      httr::GET(httr::authenticate(user, pass)) %>%
      httr::content("text") %>%
      jsonlite::fromJSON(flatten = TRUE) %>%
      purrr::pluck("organisationUnits") %>%
      dplyr::rename(uid = id, orgunit = displayName) %>%
      tibble::as_tibble()

    return(orgs)
  }


#' @title Get list of OU Orgs at specific level
#' @note  Use `get_orguids()` for levels above 4
#'
#' @param ouuid    OU uid
#' @param level    org level
#' @param username DATIM Username
#' @param password DATIM password, recommend using `mypwd()`
#' @param baseurl  base url for the API, default = https://final.datim.org/
#'
#' @return ORG UIDS as tibble
#'
#'
#' @examples
#' \dontrun{
#'
#'  library(gisr)
#'
#'  cntry <- "Zambia"
#'
#'  uid <- get_ouuid(cntry)
#'
#'  lvl <- get_ouorglevel(cntry, org_type = "prioritization")
#'
#'  orgs <- get_ouorgs(ouuid = uid, level = lvl)
#' }
#'
get_ouorgs <-
  function(ouuid,
           level = 4,
           username = NULL,
           password = NULL,
           baseurl = "https://final.datim.org/"){

    # Params
    uid <- {{ouuid}}

    lvl <- {{level}}

    user <- base::ifelse(base::is.null({{username}}),
                         glamr::datim_user(), {{username}})

    pass <- base::ifelse(base::is.null({{password}}),
                         glamr::datim_pwd(), {{password}})

    # Query ou
    orgs <- baseurl %>%
      paste0("api/organisationUnits",
             "?filter=level:eq:", lvl,
             "&filter=path:like:", uid,
             "&paging=false&format=json") %>%
      httr::GET(httr::authenticate(user, pass)) %>%
      httr::content("text") %>%
      jsonlite::fromJSON(flatten = TRUE) %>%
      purrr::pluck("organisationUnits")

    # Check data
    if (base::is.null(orgs)) {
      base::cat(
        crayon::red(
          paste0("\nNo orgunits found for uid = ",
                 uid, " & level = ", lvl, "\n")))

      return(NULL)
    }

    # Clean up
    orgs <- orgs %>%
      dplyr::rename(uid = id, orgunit = displayName) %>%
      tibble::as_tibble()

    return(orgs)
  }


#' @title Get OU Org UIDS
#'
#' @param add_details Add countries for regional ou, default is false
#' @param username    DATIM Username, recommend using `datim_user()`
#' @param password    DATIM password, recommend using `datim_pwd()`
#' @param baseurl     base url for the API, default = https://final.datim.org/
#'
#' @return OU UIDS as tibble
#'
#'
#' @examples
#' \dontrun{
#'  library(gisr)
#'
#'  # OU Org UIDs
#'  ous <- get_ouuids()
#' }
#'
get_ouuids <-
  function(add_details = FALSE,
           username = NULL,
           password = NULL,
           baseurl = "https://final.datim.org/"){

    # Params
    user <- base::ifelse(base::is.null({{username}}),
                         glamr::datim_user(), {{username}})

    pass <- base::ifelse(base::is.null({{password}}),
                         glamr::datim_pwd(), {{password}})

    # Query ou
    ous <- get_orguids(level = 3,
                       username = user,
                       password = pass,
                       baseurl = baseurl) %>%
      dplyr::rename(operatingunit = orgunit)

    # Add details if needed
    if (add_details == TRUE) {

      # Query R OUs / Countries
      countries <- ous %>%
        dplyr::filter(stringr::str_detect(operatingunit, " Region$")) %>%
        base::split(1:base::nrow(.)) %>%
        purrr::map_dfr(function(obj){

          cntries <- get_ouorgs(obj$uid, 4) %>%
            dplyr::rename(countryname = orgunit) %>%
            dplyr::mutate(operatingunit = obj$operatingunit) %>%
            dplyr::relocate(operatingunit, .after = 1)

          return(cntries)
        })

      # Combine
      ous <- ous %>%
        dplyr::mutate(
          countryname = dplyr::case_when(
            stringr::str_detect(operatingunit, " Region$") == TRUE ~ NA_character_,
            TRUE ~ operatingunit)) %>%
        dplyr::bind_rows(countries) %>%
        dplyr::arrange(operatingunit, countryname)
    }

    return(ous)
  }


#' @title Get Operatingunit / Country Org UID
#'
#' @param operatingunit Operatingunit name
#' @param username      Datim Account username, recommend using `datim_user()`
#' @param password      Datim Account Password, recommend using `datim_pwd()`
#'
#' @return uid
#'
#'
#' @examples
#' \dontrun{
#'   library(gisr)
#'
#'   # get orgunit for specific OU/Country: kenya
#'   get_ouuid(operatingunit = "Kenya")
#' }
#'
get_ouuid <-
  function(operatingunit,
           username = NULL,
           password = NULL) {

    # Params
    ou <- stringr::str_to_upper({{operatingunit}})

    user <- base::ifelse(base::is.null(username),
                         glamr::datim_user(),
                         {{username}})

    pass <- base::ifelse(base::is.null(password),
                         glamr::datim_pwd(),
                         {{password}})

    # Get all ou uids
    ous <- get_ouuids(
        add_details = TRUE,
        username = user,
        password = pass) %>%
      dplyr::filter(
        stringr::str_to_upper(operatingunit) == ou |
          stringr::str_to_upper(countryname) == ou)


    if (base::nrow(ous) == 0) {
      base::cat("\nInvalid PEPFAR Operatingunit / Countryname: ",
                crayon::red(ou, "\n"))

      return(NULL)
    }

    # OU/Country uid
    if (stringr::str_detect(ou, " region")) {
      ous <- ous %>% dplyr::filter(is.na(countryname))
    }

    # Get uid
    ouuid <- ous %>%
      dplyr::pull(uid) %>%
      dplyr::first()

    return(ouuid)
  }


#' @title Get all orgunits levels in org hierarchy
#' @note  Same as `glamr::identify_levels()` or `Wavelength::identify_levels()`
#'
#' @param username DATIM username, recommed using `datim_user()`
#' @param password DATIM password, recommend using `datim_pwd()`
#' @param baseurl  base API url, default = https://final.datim.org/
#'
#' @return df
#'
#'
#' @examples
#' \dontrun{
#'   library(gisr)
#'
#'   # Get PEPFAR Org Levels
#'   get_levels()
#'  }
#'
get_levels <-
  function(username = NULL,
           password = NULL,
           baseurl = "https://final.datim.org/"){

    # Params
    user <- base::ifelse(base::is.null(username),
                         glamr::datim_user(),
                         {{username}})

    pass <- base::ifelse(base::is.null(password),
                         glamr::datim_pwd(),
                         {{password}})

    # Query data
    df_levels <- baseurl %>%
      paste0(.,"api/dataStore/dataSetAssignments/orgUnitLevels") %>%
      httr::GET(httr::authenticate(user, pass)) %>%
      httr::content("text") %>%
      jsonlite::fromJSON(flatten = TRUE) %>%
      purrr::map_dfr(dplyr::bind_rows) %>%
      dplyr::mutate_if(is.character, ~ dplyr::na_if(., ""))

    # Adjust for non-regional missions
    df_levels <- df_levels %>%
      dplyr::mutate(name4 = ifelse(is.na(name4), name3, name4),
                    iso4 = ifelse(is.na(iso4), iso3, iso4))

    # rename
    df_levels <- df_levels %>%
      dplyr::rename(operatingunit = name3,
                    countryname = name4,
                    operatingunit_iso = iso3,
                    countryname_iso = iso4)

    return(df_levels)
  }


#' Get OU Org level
#'
#' @param operatingunit Operatingunit name
#' @param country       Country name (default = Operatingunit)
#' @param org_type      Orgunit type (country_lvl, prioritization, community, facility_lvl)
#' @param username      Datim Account username
#' @param password      Datim Account Password
#'
#' @return uid
#'
#'
#' @examples
#' \dontrun{
#'  library(gisr)
#'
#'  cntry <- "Zambia"
#'
#'  # Get country org level
#'  get_ouorglevel(cntry)
#'
#'  # Get community org level
#'  get_ouorglevel(cntry, org_type = "community")
#' }
#'
get_ouorglevel <-
  function(operatingunit,
           country = NULL,
           org_type = "prioritization",
           username = NULL,
           password = NULL) {

    # params
    ou = {{operatingunit}}

    cntry <- base::ifelse(base::is.null(country), ou, {{country}})

    type <- {{org_type}}

    user <- base::ifelse(base::is.null(username),
                         glamr::datim_user(),
                         {{username}})

    pass <- base::ifelse(base::is.null(password),
                         glamr::datim_pwd(),
                         {{password}})

    # Levels
    df_lvls <- get_levels(user, pass)

    # level name
    if (!stringr::str_to_lower(type) %in% base::names(df_lvls)) {
      base::cat(base::paste0("\nOrg_type is not available: ",
                             crayon::red(type), "\n"))

      return(NULL)
    }

    # filter ou/country
    df_lvls <- df_lvls %>%
      dplyr::filter(operatingunit == ou,
                    countryname == cntry)

    # records
    if (nrow(df_lvls) == 0) {
      base::cat(crayon::red("\nThere is no match for ou/country options\n"))

      return(NULL)
    }

    # Level
    lvl <- df_lvls %>% dplyr::pull(type)

    return(lvl)
  }


#' Get Orgs uids by level
#'
#' @param ouuid        Operatingunit uid
#' @param level        Orgunit level
#' @param username     Datim Account username
#' @param password     Datim Account Password
#'
#' @return             list of uids
#'
#'
#' @examples
#' \dontrun{
#'  library(gisr)
#'
#'  # Set country of interest
#'  cntry <- "Zambia"
#'
#'  # Get OU/Country orgunit uid
#'  uid <- get_ouuid(cntry)
#'
#'  # Get org level for psnu
#'  lvl <- get_ouorglevel(cntry, org_type = "prioritization")
#'
#'  # Retreived all uids for level 4 (SNU1)
#'  get_ouorguids(ouuid = uid, level = 4)
#' }
#'
get_ouorguids <-
  function(ouuid, level,
           username = NULL,
           password = NULL) {

    # params
    uid <- {{ouuid}}

    lvl <- {{level}}

    user <- base::ifelse(base::is.null(username),
                         glamr::datim_user(),
                         {{username}})

    pass <- base::ifelse(base::is.null(password),
                         glamr::datim_pwd(),
                         {{password}})

    # Query orgunits
    orgs <- get_ouorgs(ouuid = uid,
                       level = lvl,
                       username = user,
                       password = pass)

    # Check data
    if (base::is.null(orgs)) {
      base::cat(
        crayon::red(
          paste0("\nNo org uids found\n")))

      return(NULL)
    }

    # extract list of uids
    lvl_uids <- orgs %>% dplyr::pull(uid)

    # return
    return(lvl_uids)
  }


#' @title Get Attributes Data for Orgunit Boundaries
#'
#' @param country    OU/country
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
                           folderpath = NULL) {

  file_pattern <- base::paste0("^orghierarchy - ",
                         stringr::str_to_lower(country),
                         " - \\d{8}.csv$")

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
      readr::read_csv(file = ., col_types = c(.default = "c"))

    return(df_attrs)
  }

  base::message(country)

  # Get attrs from datim
  df_attrs <- extract_locations(country = country, add_geom = FALSE)

  labels <- df_attrs %>%
    dplyr::distinct(label) %>%
    dplyr::pull()

  # Use psnu as snu1
  if (!"snu1" %in% labels) {
    df_attrs <- df_attrs %>%
      dplyr::filter(label == "prioritization") %>%
      dplyr::mutate(label = "snu1") %>%
      dplyr::bind_rows(df_attrs, .)
  }

  # Filter out facilities and communities
  df_attrs <- df_attrs %>%
    dplyr::select(-path) %>%
    dplyr::filter(label != "facility") %>%
    dplyr::select(id, level, label, name,
                  operatingunit_iso, operatingunit,
                  countryname_iso, countryname)

  return(df_attrs)
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


#' @title Save shapefile
#'
#' @param spdf sf object
#' @param name filename with full path
#'
#' @return boolean
#' @export
#'
#' @examples
#' \dontrun{
#'  library(gisr)
#'  library(sf)
#'
#'  shp <- get_admin0(countries = "Nigeria")
#'
#'  export_spdf(spdf = shp, name = "./GIS/nga_country_boundaries")
#'  export_spdf(spdf = shp, name = "./GIS/nga_country_boundaries.shp")
#' }
#'
export_spdf <- function(spdf, name) {

  # Check directory
  dir <- base::dirname(name)

  if (!base::dir.exists(dir)) {
    base::message(glue::glue("{dir} does not seem to exist."))
  }

  name <- base::ifelse(!stringr::str_detect(name, ".shp$"),
                 base::paste0(name, ".shp"),
                 name)

  delete <- base::ifelse(file.exists(name), TRUE, FALSE)

  sf::st_write(obj = spdf,
               dsn = name,
               delete_dsn = delete)

}


#' @title Compress all shapefile components into a zipped file
#'
#' @param filename    Shapefile full name
#' @param dest_folder Where to place the zipped files
#' @return            Boolean
#' @export
#'
#' @examples
#' \dontrun{
#'  library(gisr)
#'  library(sf)
#'
#'  shp <- get_admin0(countries = "Nigeria")
#'
#'  fname <- ./GIS/nga_country_boundaries"
#'
#'  export_spdf(spdf = shp, name = fname)
#'
#'  zip_shapefiles(filename = fname, dest_folder = "./GIS/)
#' }
#'
zip_shapefiles <-
  function(filename,
           dest_folder = NULL) {

    # File pattern
    fileparts <- base::basename(filename) %>%
      stringr::str_remove(".shp$")

    # Where to place the zipped file
    if (base::is.null(dest_folder)) {
      dest_folder <- filename %>%
        base::dirname()
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



