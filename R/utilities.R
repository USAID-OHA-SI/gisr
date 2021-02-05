#' @title Get Org UIDS
#' @note Use with caution. Use `get_ouorguids()` for levels below 3
#'
#' @param level    Org level
#' @param username DATIM Username, recommend using `datim_user()`
#' @param password DATIM password, recommend using `datim_pwd()`
#' @param baseurl base url for the API, default = https://final.datim.org/
#'
#' @return ORG UIDS as tibble
#' @export
#'
#' @examples
#' \dontrun{
#'  orgs <- get_orguids(level = 3)}
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


#' @title Get OU Orgs by level
#' @note  Use `get_orguids()` for levels above 4
#'
#' @param ouuid    OU uid
#' @param level    org level
#' @param username DATIM Username
#' @param password DATIM password, recommend using `mypwd()`
#' @param baseurl base url for the API, default = https://final.datim.org/
#'
#' @return ORG UIDS as tibble
#' @export
#'
#' @examples
#' \dontrun{
#'  orgs <- get_ouorgs(ouuid = "<ou-uid-goes-here>", level = 4)}
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
      purrr::pluck("organisationUnits") %>%
      dplyr::rename(uid = id, orgunit = displayName) %>%
      tibble::as_tibble()

    return(orgs)
  }


#' Get OU UIDS
#'
#' @param username DATIM Username, recommend using `datim_user()`
#' @param password DATIM password, recommend using `datim_pwd()`
#' @param baseurl base url for the API, default = https://final.datim.org/
#'
#' @return OU UIDS as tibble
#' @export
#'
#' @examples
#' \dontrun{
#'  ous <- get_ouuids() }
#'
get_ouuids <-
  function(username = NULL,
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

    return(ous)
  }


#' Get OU UID
#'
#' @param operatingunit Operatingunit name
#' @param username      Datim Account username, recommend using `datim_user()`
#' @param password      Datim Account Password, recommend using `datim_pwd()`
#'
#' @return uid
#' @export
#'
#' @examples
#' \dontrun{
#' get_ouuid(operatingunit = "Kenya")
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
    ous <- get_ouuids(username = user,
                      password = pass)

    ous <- ous %>%
        dplyr::filter(stringr::str_to_upper(operatingunit) == ou)


    if (base::nrow(ous) == 0) {
      base::cat("\nInvalid PEPFAR Operatingunit: ",
                crayon::red(stringr::str_to_upper(cntry), "\n"))

      return(NULL)
    }

    # OU uid
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
#' @param baseurl base API url, default = https://final.datim.org/
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#'   get_levels() }
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
#' @param org_type      Orgunit type [country_lvl, prioritization, community, facility_lvl]
#' @param username      Datim Account username
#' @param password      Datim Account Password
#'
#' @return uid
#' @export
#'
#' @examples
#' \dontrun{
#' get_ouorglevel("Zambia")
#' get_ouorglevel("Zambia", org_type = "community")
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
#' @return list of uids
#' @export
#'
#' @examples
#' \dontrun{
#' get_ouorgsuids("<ou-uid-goes-here>", level = 4)
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
    lvl_uids <- get_ouorgs(ouuid = uid,
                             level = lvl,
                             username = user,
                             password = pass) %>% pull(uid)

    # return
    return(lvl_uids)
  }


