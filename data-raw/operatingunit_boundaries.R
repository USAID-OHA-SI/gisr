## PROJECT: Geospatial Analytics Utility functions
## PURPOSE: Extract OperatingUnit boundaries from PEPFAR VcPolygons
## AUTHOR:  B.Kagniniwa | USAID
## DATE:    2021-02-04
## UPDATED: 2023-07-24


# Libraries ----

  library(tidyverse)
  library(glamr)
  library(grabr)
  library(gisr)
  library(sf)
  library(zip)
  library(glue)
  library(googledrive)

# Variables ----

  # Fiscal year
  curr_fy <- "FY23"

  #cntry <- "Nigeria"
  cntry <- "Zambia"
  #cntry <- "Eswatini"
  #ou <- "West Africa Region"
  #cntry <- "Liberia"

  file_shp <- return_latest(
      folderpath = glamr::si_path("path_vector"),
      pattern = "VcPepfarPolygons.shp",
      recursive = TRUE
    )

  # Ou folder
  dir_ou_global <- paste0(si_path("path_vector"), "/OU-Global")

  if (!dir.exists(dir_ou_global))
    dir.create(dir_ou_global)

  # Country specific
  dir_ou_bndries <- paste0(si_path("path_vector"), "/OU-Boundaries")

  if (!dir.exists(dir_ou_bndries))
    dir.create(dir_ou_bndries)

  # Ou/Country folder
  dir_cntry_bndries <- paste0(si_path("path_vector"), "/OU-Country-Boundaries")

  if (!dir.exists(dir_cntry_bndries))
    dir.create(dir_cntry_bndries)

  # Ou/Countries folder
  dir_cntries_bndries <- paste0(si_path("path_vector"), "/OU-Countries-Boundaries")

  if (!dir.exists(dir_cntries_bndries))
    dir.create(dir_cntries_bndries)

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

  # facilities folder
  dir_fac_bndries <- paste0(si_path("path_vector"), "/OU-Facilities")

  if (!dir.exists(dir_fac_bndries))
    dir.create(dir_fac_bndries)

  # Ref to all paths
  dir_ref <- list(
    "global" = dir_ou_global,
    "country" = dir_cntry_bndries,
    "countries" = dir_cntries_bndries,
    "snu1" = dir_snu_bndries,
    "snu2" = dir_snu_bndries,
    "prioritization" = dir_psnu_bndries,
    "community" = dir_comm_bndries,
    "facility" = dir_fac_bndries
  )

  # gdrive sp. files folder
  spfiles <- "1KQACKdo7b-M_Un2Fe1x0ZSJkhkNNPTqa"

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


# Data ----

  # Get OUs

  #ouuids <- get_ouuids()

  ouuids <- get_ouuids(add_details = TRUE)

  ouuids <- ouuids %>%
    mutate(
      country = case_when(
        is.na(countryname) ~ operatingunit,
        TRUE ~ countryname
      )
    )

  # Get OU UID
  ouuid <- get_ouuid(operatingunit = cntry)

  # Levels
  df_levels <- get_levels(
    username = datim_user(),
    password = datim_pwd()
  )

  df_levels2 <- get_levels(
      username = datim_user(),
      password = datim_pwd(),
      reshape = T
    ) %>%
    clean_levels()


  df_ou_levels <- df_levels %>%
    filter(operatingunit == cntry | countryname == cntry)

  df_ou_levels2 <- df_levels2 %>%
    filter(operatingunit == cntry | countryname == cntry)

  df_ou_levels$country
  df_ou_levels$prioritization
  #df_ou_levels$snu1 # this does not exist in api/dataStore/dataSetAssignments/orgUnitLevels
  df_ou_levels$community
  df_ou_levels$facility

  df_ou_levels2 <- df_levels2 %>%
    filter(operatingunit == cntry | countryname == cntry)

  # Geodata
  pepfar_polygons <- file_shp %>% read_sf()

  # Operatingunit boundaries
  pepfar_polygons %>%
    filter(uid == ouuid) %>%
    gview()

  # Orgunits
  df_reg_locs <- ouuids %>%
    filter(!is.na(country)) %>%
    pull(country) %>%
    map_dfr(~extract_locations(country = .x,
                               username = datim_user(),
                               password = datim_pwd(),
                               add_geom = FALSE))

  #df_reg_locs %>% glimpse()

  df_reg_locs <- df_reg_locs %>%
    separate(path, into = paste0("path_", 0:max(.$level)), sep = "/", fill = "right")


  # Extract boundaries for all ou levels

  # OU Only
  extract_boundaries(
      spdf = pepfar_polygons,
      country = cntry,
      level = 4
    ) %>%
    gview()

  # OU - all levels
  df_levels2 %>%
    filter(label != "facility") %>%
    select(countryname, label, level) %>%
    arrange(countryname, level) %>%
    #filter(countryname == cntry) %>%
    pwalk(possibly(function(countryname, label, level) {

      print(glue("{label} => {level}"))

      # Export cntry boundaries from the global file
      lvl_spdf <- extract_boundaries(spdf = pepfar_polygons,
                                     country = countryname,
                                     level = level)

      # Add more attributes
      lvl_spdf <- lvl_spdf %>%
        mutate(org_label = label,
               org_level = level,
               country = countryname)

      if (nrow(lvl_spdf) == 0) return(NULL)

      # Add cntry sub-directory
      dir_cntry <- file.path(dir_ref[label], countryname)

      if (!dir.exists(dir_cntry)) dir.create(dir_cntry)

      # shapefile name
      shp_name <- file.path(
        dir_ref[label],
        countryname,
        paste0(
          str_to_lower(str_replace_all(countryname, " ", "_")),
          "_", label, ".shp"
        )
      )

      print(shp_name)

      # Export shapefile
      spdf_export(lvl_spdf, shp_name)

      # Compress shapefile
      zip_shapefiles(filename = basename(shp_name), dest_folder = dir_cntry)
    }))


  ## Save OU Shapefile as a single file

  shp_ous <- ouuids %>%
    filter(operatingunit == country) %>%
    select(ou = operatingunit, uid) %>%
    left_join(pepfar_polygons, .,by = "uid") %>%
    filter(!is.na(ou))

  shp_ous_name <- file.path(dir_ref["global"], "pepfar_operatingunits.shp")

  spdf_export(shp_ous, shp_ous_name)

  base::list.files(path = dir_ref$global,
                   pattern = str_remove(basename(shp_ous_name), ".shp"),
                   full.names = TRUE)

  zip_shapefiles(filename = basename(shp_ous_name), dest_folder = dir_ref$global)


  # Export Country Boundaries as a single shapefile

  shp_cntries <- ouuids %>%
    filter(str_detect(country, "Region$", negate = TRUE)) %>%
    select(ou = operatingunit, country, uid) %>%
    left_join(pepfar_polygons, ., by = "uid") %>%
    filter(!is.na(country))

  shp_cntries_name <- file.path(dir_ou_global, "pepfar_countries.shp")

  spdf_export(shp_cntries, shp_cntries_name)

  zip_shapefiles(basename(shp_cntries_name), dir_ref$global)

  # Combine all psnus

  spdf_psnus <- list.files(
    path = dir_psnu_bndries,
    pattern = ".*_prioritization.shp",
    recursive = TRUE,
    full.names = TRUE) %>%
    map_dfr(read_sf)

  spdf_psnus %>% gview

  shp_psnus_name <- file.path(dir_ou_global, "global_psnus.shp")

  spdf_export(spdf_psnus, shp_psnus_name)

  zip_shapefiles(basename(shp_psnus_name), dir_ref$global)


  # Zip all shapefiles

  # OU Boundaries
  dir(path = dir_cntry_bndries,
      pattern = ".*_country.shp",
      recursive = TRUE,
      full.names = TRUE) %>%
    map(.x, .f = ~ zip_shapefiles(filename = .x))

  # PSNU Boundaries
  dir(path = dir_psnu_bndries,
      pattern = ".*_prioritization.shp",
      recursive = TRUE,
      full.names = TRUE) %>%
    map(.x, .f = ~ zip_shapefiles(filename = .x))

  # Community Boundaries
  dir(path = dir_comm_bndries,
      pattern = ".*_community.shp",
      recursive = TRUE,
      full.names = TRUE) %>%
    map(.x, .f = ~ zip_shapefiles(filename = .x))

  # Relocate all OUs files together

  df_levels2 %>%
    select(countryname, label) %>%
    pwalk(possibly(function(countryname, label) {

      # Source of zip
      dir_src <- file.path(dir_ref[label], countryname)

      # Add cntry sub-directory
      dir_dest <- file.path(dir_ref$countries, countryname)

      if (!dir.exists(dir_dest)) dir.create(dir_dest)

      # Ref to zipped files
      file_src <- list.files(
        path = dir_src,
        pattern = ".zip$",
        full.names = TRUE
      )

      print(glue("{countryname}: {label} - {file_src}"))

      # Copy files over
      if (!is.null(file_src) & file.exists(file_src)) {

        file_new <- file.path(dir_dest, basename(file_src))

        print(glue("Destination: {file_new}"))

        fs::file_copy(
          path = file_src,
          new_path = file_new,
          overwrite = TRUE
        )
      }
      else {
        print("Source file not found")
      }

    }))

  # Push all zip files to google drive

  googledrive::drive_auth(email = pano_user())

  df_sp_drive <- spfiles %>%
    as_id() %>%
    drive_ls() %>%
    select(-drive_resource) %>%
    arrange(name)

  drive_ou_bndries <- df_sp_drive %>%
    filter(str_detect(name, "OU and Country-Boundaries")) %>%
    pull(id) %>%
    as_id()

  drive_curr_fy <- drive_mkdir(name = curr_fy,
                               path = drive_ou_bndries,
                               overwrite = FALSE)

  df_levels %>%
    pull(countryname) %>%
    walk(possibly(function(.x){

      dir_cntry <- file.path(dir_cntries_bndries, .x)

      name = str_replace(.x, "'", "-")

      print(name)

      # Do not let the connection stale
      googledrive::drive_auth(email = pano_user())

      drive_dest <- drive_mkdir(name = name,
                                path = as_id(drive_curr_fy$id),
                                overwrite = TRUE)

      dir_cntry %>%
        list.files(pattern = ".zip$",
                   full.names = TRUE) %>%
        walk(possibly(function(.y) {
          print(.y)

          drive_upload(
            media = .y,
            path = as_id(drive_dest$id),
            name = basename(.y)
          )
        }))
    }))


