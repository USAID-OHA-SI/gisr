## PROJECT: Geospatial Analytics Utility functions
## PURPOSE: Extract OperatingUnit boundaries from PEPFAR VcPolygons
## AUTHOR:  B.Kagniniwa | USAID
## DATE:    2021-02-04
## UPDATED: 2021-07-28


# Libraries ----

library(tidyverse)
library(glamr)
library(grabr)
library(gisr)
library(sf)
library(zip)
library(glue)

# Functions ----

#' @title Move files to another directory
#'
#'
move_files <- function(from,
                       to = "archive",
                       add_dest = TRUE,
                       name = ".csv$") {

  # Check origin path
  if (!dir.exists(from))
    stop(glue("Invalid directory: {from}"))

  # Check destination path
  dest_path <- file.path(from, to)

  if (!dir.exists(to) & !dir.exists(dest_path) & add_dest == TRUE)
    dir.create(dest_path, recursive = TRUE)

  if (dir.exists(to))
    dest_path <- to

  # Files to be moved
  mfiles <- from %>% list.files(pattern = name, full.names = TRUE)

  if (length(mfiles) == 0)
    stop(glue("No matching files found from directory: {from}"))

  # Move files
  mfiles %>%
    map_chr(~fs::file_move(
      path = .x,
      new_path = file.path(dest_path, basename(.x))))

  # Check for errors
  errors <- from %>% list.files(pattern = name, full.names = TRUE)

  # Notifications
  if(length(errors) > 0) {
    message("Error - These files were not moved:")
    print(errors)
  }
}

#dir_ou_global %>% move_files(name = "^pepfar")

#' @title Clean names for shp
#'
#'
clean_names4shp <- function(.data, cols = NULL) {

  .data %>%
    rename_with(.cols = starts_with("operatingunit"),
                .fn = ~str_replace(., "operatingunit", "ou")) %>%
    rename_with(.cols = ends_with("countryname"),
                .fn = ~str_replace(., "countryname", "cntry_name")) %>%
    rename_with(.cols = starts_with("countryname_"),
                .fn = ~str_replace(., "countryname_", "cntry"))
}

# Variables ----

  #cntry <- "Nigeria"
  #cntry <- "Zambia"
  #cntry <- "Eswatini"
  ou <- "West Africa Region"
  cntry <- "Liberia"

  file_shp <- return_latest(
      folderpath = glamr::si_path("path_vector"),
      pattern = "VcPepfarPolygons.shp$",
      recursive = TRUE
    )

  # Ou folder
  dir_ou_global <- paste0(si_path("path_vector"), "/OU-Global")

  if (!dir.exists(dir_ou_global))
    dir.create(dir_ou_global)

  # Ou folder
  dir_ou_bndries <- paste0(si_path("path_vector"), "/OU-Country-Boundaries")

  if (!dir.exists(dir_ou_bndries))
    dir.create(dir_ou_bndries)

  # SNU folder
  dir_snu_bndries <- paste0(si_path("path_vector"), "/OU-SNUs")

  if (!dir.exists(dir_snu_bndries))
    dir.create(dir_snu_bndries)

  # psnu folder
  dir_psnu_bndries <- paste0(si_path("path_vector"), "/OU-PSNUs")

  if (!dir.exists(dir_psnu_bndries))
    dir.create(dir_psnu_bndries)

  # communities folder
  dir_comm_bndries <- paste0(si_path("path_vector"), "/OU-Communities")

  if (!dir.exists(dir_comm_bndries))
    dir.create(dir_comm_bndries)

  # Country specific
  dir_ou_bndries <- paste0(si_path("path_vector"), "/OU-Boundaries")

  if (!dir.exists(dir_ou_bndries))
    dir.create(dir_ou_bndries)

# Functions ----

  #' Extract boundaries
  extract_boundaries <-
    function(spdf, country,
             level = 3,
             username = NULL,
             password = NULL,
             export = FALSE,
             name = NULL) {

      # Params
      cntry <- {{country}}

      lvl <- {{level}}

      user <- base::ifelse(base::is.null(username),
                           glamr::datim_user(),
                           {{username}})

      pass <- base::ifelse(base::is.null(password),
                           glamr::datim_pwd(),
                           {{password}})

      #ou uid
      uid <- get_ouuid(operatingunit = cntry,
                       username = user,
                       password = pass)

      # list of orgs at the specified level
      orgs <- get_ouorgs(
          ouuid = uid,
          level = lvl,
          username = user,
          password = pass,
          baseurl = "https://datim.org/")

      # Check for valid data
      if (is.null(orgs)) {
        base::cat(crayon::red("\nNo geodata found for this request.\n"))

        return(NULL)
      }

      orgs <- orgs %>%
        mutate(org_level = lvl)

      # filter sp df
      spdf <- spdf %>%
        left_join(orgs, by = "uid") %>%
        filter(!is.na(orgunit))

      # Export
      if (export == TRUE & !is.null(name)) {
        sf::st_write(spdf,
                     delete_dsn = TRUE,
                     dsn = paste0(name, ".shp"))

      }

      return(spdf)
    }

  file_shp %>%
    read_sf() %>%
    extract_boundaries(cntry)

  #' @title Get OU/Country orgs attributes
  #'
  #' @param country OU/Country name
  #' @param username Datim Username
  #' @param password Datim Password
  #'
  get_attributes <- function(country,
                             username = NULL,
                             password = NULL) {

    print(country)

    locs <- extract_locations(country = country, add_geom = FALSE)

    labels <- locs %>%
      distinct(label) %>%
      pull()

    # Use psnu as snu1
    if (!"snu1" %in% labels) {
      df_psnu <- locs %>%
        filter(label == "prioritization") %>%
        mutate(label = "snu1")

      locs <- locs %>%
        bind_rows(df_psnu)
    }

    # Filter out facilities and communities
    locs <- locs %>%
      select(-path) %>%
      filter(!label %in% c("facility", "community"))

    return(locs)
  }


  #' @title Save shapefile
  #'
  #' @param spdf sf object
  #' @param name filename with full path
  #'
  #' @return boolean
  #'
  export_spdf <- function(spdf, name) {

    name <- ifelse(!str_detect(name, ".shp$"),
                   paste0(name, ".shp"),
                   name)

    delete <- ifelse(file.exists(name), TRUE, FALSE)

    sf::st_write(obj = spdf,
                 dsn = name,
                 delete_dsn = delete)

  }

  #' @title Zip shapefile
  #'
  #' @param filename    Shapefile full name
  #' @param dest_folder Where to place the zipped files
  #'
  zip_shapefiles <-
    function(filename,
             dest_folder = NULL) {

      # File pattern
      fileparts <- base::basename(filename) %>% str_remove(".shp")

      # Where to place the zipped file
      if (is.null(dest_folder)) {
        dest_folder <- filename %>% str_remove(basename(.))
      }

      # Files to be zipped
      zipfiles <- base::dir(path = dest_folder,
                            pattern = fileparts,
                            full.names = TRUE)

      # Zip files
      zip::zip(zipfile = file.path(dest_folder, paste0(fileparts, ".zip")),
               files = zipfiles,
               mode = "cherry-pick")

    }


# Data ----

  # Get OUs
  #ouuids <- get_ouuids()
  ouuids <- get_ouuids(add_details = TRUE)

  # Get OU UID
  ouuid <- get_ouuid(operatingunit = cntry)


  # Levels
  df_levels <- get_levels(
    username = datim_user(),
    password = datim_pwd()
  )

  df_ou_levels <- df_levels %>%
    filter(operatingunit == ou, countryname == cntry)

  df_ou_levels$country
  df_ou_levels$prioritization
  df_ou_levels$snu1 # this does not exist in api/dataStore/dataSetAssignments/orgUnitLevels
  df_ou_levels$community
  df_ou_levels$facility

  # Geodata
  pepfar_polygons <- file_shp %>% read_sf()

  # Operatingunit boundaries
  spdf_ou <- pepfar_polygons %>% filter(uid == ouuid)

  spdf_ou %>% gview()


  # Org Hirarchy
  df_cntry_locs <- ouuids %>%
    filter(!is.na(country)) %>%
    pull(country) %>%
    map_dfr(~extract_locations(country = .x, add_geom = FALSE)) %>%
    separate(path, into = paste0("path", 0:max(.$level)), sep = "/") %>%
    rename(
      uid = id,
      global_uid = path1,
      region_uid = path2,
      ou_uid = path3) %>%
    rename_with(.cols = starts_with("operatingunit"),
                .fn = ~str_replace(., "operatingunit", "ou")) %>%
    rename_with(.cols = ends_with(c("countryname", "country_name")),
                .fn = ~str_replace(., "countryname|country_name", "country")) %>%
    rename_with(.cols = starts_with(c("countryname_", "country_name_")),
                .fn = ~str_replace(., "countryname_|country_name_", "cntry")) %>%
    select(-starts_with("path")) %>%
    relocate(level, .after = last_col())


  df_locs <- ouuids %>%
    filter(!str_detect(operatingunit, " Region$")) %>%
    pull(operatingunit) %>%
    map_dfr(.x, .f = ~get_attributes(country = .x)) %>%
    filter(str_detect(name, "_Military", negate = T)) %>%
    rename_with(.cols = starts_with("operatingunit"),
                .fn = ~str_replace(., "operatingunit", "ou")) %>%
    rename_with(.cols = ends_with(c("countryname", "country_name")),
                .fn = ~str_replace(., "countryname|country_name", "country")) %>%
    rename_with(.cols = starts_with(c("countryname_", "country_name_")),
                .fn = ~str_replace(., "countryname_|country_name_", "cntry"))


  # Extract boundaries for all ou levels

  # OU Only
  extract_boundaries(
      spdf = pepfar_polygons,
      country = cntry,
      level = 4
    ) %>%
    gview()

  orgs <- get_ouorgs(
    ouuid = get_ouuid(cntry),
    level = 5,
    username = datim_user(),
    password = datim_pwd(),
    baseurl = "https://datim.org/")

  orgs %>%
    pull(uid)

  pepfar_polygons %>%
    dplyr::filter(uid %in% orgs$uid)

  # OU - all levels
  df_ou_levels %>%
    pivot_longer(cols = country:last_col(),
                 names_to = "label",
                 values_to = "level") %>%
    filter(label != "facility") %>%
    select(label, level) %>%
    arrange(level) %>%
    pwalk(function(label, level) {

      print(glue("{label} => {level}"))

      lvl_spdf <- extract_boundaries(spdf = pepfar_polygons,
                                     country = cntry,
                                     level = level)

      lvl_spdf <- lvl_spdf %>%
        mutate(org_label = label,
               country = cntry)

      shp_name <- file.path(
        dir_ou_bndries,
        cntry,
        paste0(
          str_to_lower(str_replace_all(cntry, " ", "_")),
          "_", label, ".shp"
        )
      )

      print(shp_name)

      export_spdf(lvl_spdf, shp_name)

      zip_shapefiles(basename(shp_name),
                     dest_folder = file.path(dir_ou_bndries, cntry))
    })

  # Export all OU Boundaries

  ous <- get_levels(
      username = glamr::datim_user(),
      password = glamr::datim_pwd()
    ) %>%
    distinct(operatingunit) %>%
    pull(operatingunit)

  ## Save as individual shapefile
  ous %>%
    map(.x, .f = ~ extract_boundaries(spdf = pepfar_polygons,
                                      country = .x,
                                      level = 3,
                                      export = TRUE,
                                      name = file.path(
                                        si_path("path_vector"),
                                        dir_ou_bndries,
                                        str_replace_all(.x, " ", "_") %>%
                                          str_to_lower())))

  ## Save as single shapefile
  shp_ous <- ous %>%
    map(.x, .f = ~ extract_boundaries(
        spdf = pepfar_polygons,
        country = .x,
        level = 3
      )) %>%
    bind_rows()

  shp_ous_name <- file.path(dir_ou_global, "pepfar_operatingunits.shp")

  export_spdf(shp_ous, shp_ous_name)

  zip_shapefiles(basename(shp_ous_name), dir_ou_global)


  # Export Country Boundaries

  countries <- glamr::get_outable(
      username = glamr::datim_user(),
      password = glamr::datim_pwd()
    ) %>%
    select(starts_with(c("oper", "coun")))

  shp_cntries <- pepfar_polygons %>%
    left_join(countries, by = c("uid" = "countryname_uid")) %>%
    filter(!is.na(countryname))

  shp_cntries %>% filter(countryname %in% cntries) %>%
    st_drop_geometry()

  #shp_cntries %>% gview()

  shp_cntries_name <- file.path(dir_ou_global, "pepfar_countries.shp")

  export_spdf(shp_cntries, shp_cntries_name)

  zip_shapefiles(basename(shp_cntries_name), dir_ou_global)


  # Export all psnu Boundaries - exclude Regional OUs
  ous2 <- ous %>%
    str_subset(pattern = " Region$", negate = TRUE)

  psnu_levels <- ous2 %>%
    map(.x, .f = ~get_ouorglevel(
      operatingunit = .x,
      org_type = "prioritization")) %>%
    unlist()

    map2(ous2, psnu_levels, .x, .y,
         .f = ~ extract_boundaries(spdf = pepfar_polygons,
                                    country = .x,
                                    level = .y,
                                    export = TRUE,
                                    name = file.path(
                                      dir_psnu_bndries,
                                      paste0(str_replace_all(.x, " ", "_") %>%
                                             str_to_lower(), "_psnu"))))

    # Combine all psnus
    spdf_psnus <- list.files(
      path = dir_psnu_bndries,
      pattern = ".*_psnu.shp",
      full.names = TRUE) %>%
      map_dfr(read_sf)

    spdf_psnus %>%
      ggplot() +
      geom_sf() +
      si_style_map()

    spdf_psnus %>%
      export_spdf(name = file.path(dir_psnu_bndries, "global_psnus.shp"))


    # Zip shapefiles

    # OU Boundaries
    dir(path = dir_ou_bndries,
        pattern = ".shp",
        full.names = TRUE) %>%
      map(.x, .f = ~ zip_shapefiles(filename = .x))

    # PSNU Boundaries
    dir(path = dir_psnu_bndries,
        pattern = ".shp",
        full.names = TRUE) %>%
      map(.x, .f = ~ zip_shapefiles(filename = .x))

    dir(path = dir_psnu_bndries,
        pattern = "global_psnus.shp",
        full.names = TRUE) %>%
      map(.x, .f = ~ zip_shapefiles(filename = .x))


  # SNU1

    # Extract country location attributes
    cntry_attrs <- get_attributes(country = cntry)

    # Identify SNU1 level
    lvl_snu1 <- cntry_attrs %>%
      filter(label == 'snu1') %>%
      pull(level) %>%
      first()

    snu1_filename <- cntry %>%
      str_replace_all(" ", "_") %>%
      str_replace_all("[^[:alnum:]]", "_") %>%
      str_to_lower() %>%
      paste0("_snu1") %>%
      file.path(dir_snu_bndries, .)

    extract_boundaries(spdf = pepfar_polygons,
                       country = cntry,
                       level = lvl_snu1,
                       export = TRUE,
                       name = snu1_filename)

    zip_shapefiles(filename = snu1_filename)

    # Extract all snu1

    snu1_levels <- ous %>%
      str_subset(pattern = " Region$", negate = TRUE) %>%
      map_dfr(~get_attributes(country = .x))

    snu1_levels <- snu1_levels %>%
      filter(label %in% c("snu1", "prioritization")) %>%
      group_by(countryname) %>%
      mutate(no_snu1 = identical(id[label == 'snu1'],
                                 id[label == 'prioritization'])) %>%
      ungroup() %>%
      filter(no_snu1 == FALSE, label == "snu1") %>%
      distinct(countryname, level) %>%
      select(countryname, level)

    extract_snu1 <- function(countryname, level){
      print(glue::glue("{countryname}: {level}"))

      fname <- countryname %>%
        str_replace_all(" ", "_") %>%
        str_replace_all("[^[:alnum:]]", "_") %>%
        str_to_lower() %>%
        paste0("_snu1") %>%
        file.path(dir_snu_bndries, .)

      extract_boundaries(spdf = pepfar_polygons,
                         country = countryname,
                         level = level,
                         export = TRUE,
                         name = fname)

      return(countryname)
    }

    snu1_levels %>%
      pmap(extract_snu1)

    # Zip snu1 shapefiles

    list.files(path = dir_snu_bndries,
               pattern = ".shp$",
               full.names = TRUE) %>%
      map(~zip_shapefiles(filename = .x))

