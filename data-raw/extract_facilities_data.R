## PROJECT: Geospatial Analytics Utility functions
## PURPOSE: Extract OU/Country Specific Facilities Data
## AUTHOR:  B.Kagniniwa | USAID
## DATE:    2022.04.25


# Libraries ----

  library(tidyverse)
  library(glamr)
  library(gisr)
  library(sf)
  library(sp)
  library(glue)

# DIRs ----

  dir_sites <- Sys.Date() %>%
    glamr::convert_date_to_qtr() %>%
    str_sub(1, 4)

  dir_data <- si_path("path_vector") %>%
    paste0("/OU-Sites/", dir_sites)

  dir.create(dir_data)

  dir_geodata <- paste0(dir_data, "/SHP")

  dir.create(dir_geodata)

# DATA - Extract OU Data ----

  cntry <- "Ethiopia"

  level_fac <- get_ouorglevel(operatingunit = cntry, org_type = "facility")

  df_facs <- extract_locations(country = cntry, level = level_fac)

  df_facs <- df_facs %>% extract_facilities()

  df_facs %>%
    select(-c(geom_type:nested)) %>%
    write_csv(file = paste0(dir_data, "/",
                            cntry,
                            " - facilities_locations_extract_",
                            format(Sys.Date(), "%Y-%m-%d"),
                            ".csv"),
              na = "")

# Batch Locations ----

  pull_facilities <- function(ou, cntry = NULL, dest = NULL) {

    print(glue("Ou = {ou}, Cntry = {cntry}"))

    if (is.null(cntry)) {
      cntry = ou
    }

    # Facility org level
    lvl <- get_ouorglevel(operatingunit = ou, country = cntry, org_type = "facility")

    print(glue("Facility level = {lvl}"))

    # Location Data
    locs <- extract_locations(country = cntry, level = lvl)

    # check location details
    if (any(str_detect(names(locs), "coordinates"))) {
      print("unpacking coordinates")

      locs <- locs %>%
        extract_facilities() %>%
        select(-c(path:nested))
    }

    # Export
    if (nrow(locs) > 0 & !is.null(dest)) {

      filename <- paste0(dest, "/",
                         cntry,
                         " - facilities_locations_",
                         format(Sys.Date(), "%Y-%m-%d"),
                         ".csv")

      print(glue("Exporting data to: {filename}"))

      locs %>% write_csv(file = filename, na = "")
    }

    return(locs)
  }

  pull_facilities("Nigeria")


# Export OU / Country Facilities Locations data ----

  glamr::pepfar_country_list %>%
    select(operatingunit, countryname) %>%
    pwalk(.f = ~pull_facilities(ou = .x, cntry = .y, dest = dir_data))

# Create Shapefiles ----

  generate_shp <- function(df_locs,
                         lat = "latitude",
                         long = "longitude") {

    # Spatial file
    spdf <- NULL

    # check for lat/long columns
    if (lat %in% names(df_locs)) {

      spdf <- df_locs %>%
        filter(across(all_of(c(lat, long)), ~ !is.na(.x))) %>%
        mutate(across(all_of(c(lat, long)), ~ as.numeric(.x)))


      spdf <- spdf %>% st_as_sf(coords = c(long, lat), crs = st_crs(4326))

      # Shapefiles columns have a max
      spdf <- spdf %>%
        rename(ou_iso = operatingunit_iso,
               ou = operatingunit,
               cntry_iso = countryname_iso,
               cntry = countryname)

    } else {
      print("No location columns found. Consider changing lat/long default values")
    }

    return(spdf)
  }

# 1 country at the time ----

  list.files(path = dir_data,
             pattern = ".*facilities_locations_\\d{4}.*.csv",
             full.names = TRUE) %>%
    walk(function(.x) {

      name <- basename(.x) %>%
        str_remove(".csv$") %>%
        str_replace_all("\'", "_")

      print(.x)

      df_locs <- .x %>% read_csv(col_types = c(.default = "c"))

      print(nrow(df_locs))

      spdf <- generate_shp(df_locs = df_locs)

      if (!is.null(spdf)) {
        spdf %>% export_spdf(name = file.path(dir_geodata, name))

        file.path(dir_geodata, name) %>% zip_shapefiles()
      }
    })

# Global facility shapefile ----

  df_locss <- list.files(
      path = dir_data,
      pattern = ".*facilities_locations_\\d{4}.*.csv",
      full.names = TRUE
    ) %>%
    map_dfr(function(.x) {

      name <- basename(.x) %>%
        str_remove(".csv$") %>%
        str_replace_all("\'", "_")

      print(.x)

      df_locs <- .x %>%
        read_csv(col_types = c(.default = "c"))

      return(df_locs)
    })

# Save global CSV

  df_locss %>%
    write_csv(file = paste0(dir_data, "/",
                            "Global - facilities_locations_",
                            format(Sys.Date(), "%Y-%m-%d"),
                            ".csv"),
              na = "")

# global SHP

  spdf_global <- df_locss %>% generate_shp(df_locs = .)


# Save global SHP

  spdf_global %>%
    export_spdf(name = paste0(dir_geodata, "/",
                              "Global - facilities_locations_",
                              format(Sys.Date(), "%Y-%m-%d")))
# Zip global SHP

  file.path(dir_geodata,
            paste0("Global - facilities_locations_",
                   format(Sys.Date(), "%Y-%m-%d"))) %>%
    zip_shapefiles()
