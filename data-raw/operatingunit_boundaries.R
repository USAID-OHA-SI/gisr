## PROJECT: Geospatial Analytics Utility functions
## PURPOSE: Extract OperatingUnir boundaries from PEPFAR VcPolygons
## AUTHOR:  B.Kagniniwa | USAID
## DATE:    2021-02-04
## UPDATED: 2021-03-12


# Libraries ----

library(tidyverse)
library(ggflags)
library(glamr)
library(gisr)
library(glitr)
library(scales)
library(sf)
library(zip)

# Variables ----

  cntry <- "Nigeria"

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
  dir_ou_bndries <- paste0(si_path("path_vector"), "/OU-Boundaries")

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
          password = pass)

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

    locs <- extract_locations(country = ou, add_geom = FALSE)

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
  ouuids <- get_ouuids()
  ouuids2 <- get_ouuids(add_details = TRUE)

  # Get OU UID
  ouuid <- get_ouuid(operatingunit = cntry)


  # Levels
  levels <- get_levels(username = datim_user(),
                       password = datim_pwd()) %>%
    filter(operatingunit == cntry)

  levels$country
  levels$prioritization
  #levels$snu1
  levels$community
  levels$facility

  # Geodata
  pepfar_polygons <- file_shp %>% read_sf()

  # Operatingunit boundaries
  spdf_ou <- pepfar_polygons %>% filter(uid == ouuid)

  spdf_ou %>% gview()


  # SNU1
  df_locs <- extract_locations(country = cntry, add_geom = FALSE) %>%
    separate(path,
             into = paste0("path", 0:max(.$level)),
             sep = "/",
             remove = FALSE) %>%
    select(-path0) %>%
    rename(
      global_uid = path1,
      region_uid = path2,
      operatingunit_uid = path3) %>%
    pivot_wider(names_from = label,
                values_from = level)


  df_locss <- ouuids %>%
    filter(!str_detect(operatingunit, " Region$")) %>%
    pull(operatingunit) %>%
    map_dfr(.x, .f = ~get_attributes(country = .x))


  # psnu boundaries: uid lookup
  psnu_uids <- get_ouorguids(ouuid = ouuid,
                             level = levels$prioritization)

  spdf_psnu <- pepfar_polygons %>%
    filter(uid %in% psnu_uids)

  spdf_psnu %>% gview()

  # psnu boundaries: join & filter
  psnus <- get_ouorgs(
      ouuid = ouuid,
      level = levels$prioritization
    ) %>%
    mutate(ou = cntry,
           org_type = "psnu",
           org_level = levels$prioritization)

  psnus %>% glimpse()

  spdf_psnu <- pepfar_polygons %>%
    left_join(psnus, by = "uid") %>%
    filter(!is.na(orgunit))

  spdf_psnu %>% gview()


  # communities boundaries: uid lookup
  comm_uids <- get_ouorguids(ouuid = ouuid,
                             level = levels$community)

  spdf_comm <- pepfar_polygons %>%
    filter(uid %in% comm_uids)

  spdf_comm %>% gview()

  # communities boundaries: join & filter
  comms <- get_ouorgs(
      ouuid = ouuid,
      level = levels$community
    ) %>%
    mutate(ou = cntry,
           org_type = "community",
           org_level = levels$community)

  spdf_comm %>% glimpse()

  spdf_comm <- pepfar_polygons %>%
    left_join(comms, by = "uid") %>%
    filter(!is.na(orgunit))

  spdf_comm %>% gview()

  # Extract boundaries for all ou levels

  # OU Only
  extract_boundaries(
      spdf = pepfar_polygons,
      country = "Guinea", #cntry,
      level = 6
    ) %>%
    gview()

  # OU - all levels
  levels %>%
    pivot_longer(cols = country:last_col(),
                 names_to = "level",
                 values_to = "value") %>%
    filter(level != "facility") %>% #View()
    pull(value) %>%
    sort() %>%
    map(.x, .f = ~ extract_boundaries(spdf = pepfar_polygons,
                                      country = cntry,
                                      level = .x))


  # Export all OU Boundaries

  ous <- get_levels(
      username = glamr::datim_user(),
      password = glamr::datim_pwd()
    ) %>% #View()
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

  shp_ous %>% gview()

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
                                      si_path("path_vector"),
                                      dir_psnu_bndries,
                                      paste0(str_replace_all(.x, " ", "_") %>%
                                             str_to_lower(), "_psnu"))))


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


  # SNU1

  df_locs <- extract_locations()

