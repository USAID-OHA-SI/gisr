## PROJECT: Geospatial Analytics Utility functions
## PURPOSE: Tag PEPFAR OrgHierarchy Shapefile
## AUTHOR:  B.Kagniniwa | USAID
## DATE:    2021-02-02


# Libraries ----

library(Wavelength)
library(glamr)
library(tidyverse)
library(sf)

# Variables ----

  cntry <- "Zambia"

  file_shp <- list.files(
      path = glamr::si_path("path_vector"),
      pattern = "^VcPepfarPolygons.shp",
      recursive = T,
      full.names = T
    ) %>%
    sort() %>%
    last()

# Get OU UID
  z_uid <- get_ouuid(operatingunit = cntry,
                     username = glamr::datim_user(),
                     password = glamr::datim_pwd())


#Levels
  levels <- get_levels(username = datim_user(),
                       password = datim_pwd())

  levels %>% glimpse()

# List of Regions
  regions <- get_orguids(level = 2)

# List of reional ou uids
  reg_ous <- get_ouuids() %>%
    dplyr::filter(str_detect(operatingunit, " Region$"))


# PEPFAR Shapefile

  # Read global shapefile
  sf_pepfar <- file_shp %>%
    read_sf()

  sf_pepfar %>% glimpse()

  # Extract region/countries boundaries

  # Asia
  ar_countries <- reg_ous %>%
    filter(str_detect(operatingunit, "^Asia")) %>%
    pull(uid) %>%
    get_ouorguids(ouuid = ., level = 4) %>%
    mutate(ou = "Asia Region") %>%
    rename(country = orgunit) %>%
    relocate(country, .after = last_col())


  sf_ar_countries <- sf_pepfar %>%
    left_join(ar_countries, by = "uid") %>%
    filter(!is.na(country))

  st_write(sf_ar_countries,
           delete_dsn = TRUE,
           dsn = file.path(glamr::si_path("path_vector"),
                           "AsiaRegion/ar_countries.shp"))

  # West Africa
  war_countries <- reg_ous %>%
    filter(str_detect(operatingunit, "^West A")) %>%
    pull(uid) %>%
    get_ouorguids(ouuid = ., level = 4) %>%
    mutate(ou = "West Africa Region") %>%
    rename(country = orgunit) %>%
    relocate(country, .after = last_col())

  sf_war_countries <- sf_pepfar %>%
    left_join(war_countries, by = "uid") %>%
    filter(!is.na(country))

  st_write(sf_war_countries,
           delete_dsn = TRUE,
           dsn = file.path(glamr::si_path("path_vector"),
                           "WestAfricaRegion/war_countries.shp"))

  # Western Hemisphere Region
  whr_countries <- reg_ous %>%
    filter(str_detect(operatingunit, "^Western")) %>%
    pull(uid) %>%
    get_ouorguids(ouuid = ., level = 4) %>%
    mutate(ou = "Western Hemisphere Region") %>%
    rename(country = orgunit) %>%
    relocate(country, .after = last_col())

  sf_whr_countries <- sf_pepfar %>%
    left_join(whr_countries, by = "uid") %>%
    filter(!is.na(country))

  # All XWH Countries
  st_write(sf_whr_countries,
           delete_dsn = TRUE,
           dsn = file.path(glamr::si_path("path_vector"),
                           "WesternHemisphereRegion/whr_countries.shp"))

  # Limited list of XWH Countries
  st_write(sf_whr_countries %>%
             filter(country %in% c('Brazil',
                                   'El Salvador',
                                   'Guatemala',
                                   'Guyana',
                                   'Honduras',
                                   'Jamaica',
                                   'Nicaragua',
                                   'Panama',
                                   'Trinidad and Tobago')),
           delete_dsn = TRUE,
           dsn = file.path(glamr::si_path("path_vector"),
                           "WesternHemisphereRegion/whr_limited_countries.shp"))




