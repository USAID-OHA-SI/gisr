## PROJECT: Geospatial Analytics Utility functions
## PURPOSE: Create a Terrain Topographic Basemap
## AUTHOR:  B.Kagniniwa | USAID
## DATE:    2021-02-03
## UPDATE:  2024-02-08


# Libraries ----

  library(tidyverse)
  library(glamr)
  library(gisr)
  library(sf)

# Variables ----

  cntry <- "Nigeria"

  file_raster <- list.files(
      path = glamr::si_path("path_raster"),
      pattern = "^SR_LR.tif$",
      recursive = T,
      full.names = T
    )


# Datasets

  # RasterLayer
  ras <- gisr::get_raster(folderpath = glamr::si_path("path_raster"))

  # Global polygons
  shp_ne <- get_nepolygons()

  shp_pepfar <- get_vcpolygons(folderpath = si_path("path_vector"))

  # Country boundaries
  adm0 <- gisr::get_admin0(cntry)
  adm1 <- gisr::get_admin1(cntry)

  # Neighbors
  nghbrs <- geo_neighbors(src = shp_ne, countries = cntry, crs = 3857)
  nghbrs2 <- geo_neighbors(src = shp_ne, countries = cntry, crs = 3857, crop = TRUE)

# VIZ ----

  # View admins

  adm0 %>% gview()

  adm1 %>% gview()

  nghbrs2 %>% gview()

  # Add neighbor countries
  adm1 %>%
    gview() +
    geom_sf(data = nghbrs2, fill = glitr::grey10k, linewidth = .5, color = "white") +
    geom_sf(data = adm0, fill = NA, linewidth = 2, color = "white") +
    geom_sf(data = adm0, fill = NA, linewidth = .6, color = glitr::grey70k) +
    geom_sf_text(data = nghbrs2,
                 aes(label = sovereignt),
                 color = glitr::grey80k,
                 size = 4)

  # Get country specific terrain data
  ras_aoi <- get_terrain(countries = cntry, terr = si_path("path_raster"))


  # Take advantage of terrain data
  terrain_map(countries = cntry,
              terr = si_path("path_raster"))

  # Mask terrain behind country boundaries
  terrain_map(countries = cntry,
              mask = TRUE,
              terr = si_path("path_raster"))

  # Mask terrain and provide your own RastLayer
  terrain_map(countries = cntry,
              mask = TRUE,
              terr = ras)

  # Provide your own polygons and RastLayer
  terrain_map(countries = cntry,
              adm0 = adm0,
              mask = TRUE,
              terr = ras)

  terrain_map(countries = cntry,
              adm0 = adm0,
              adm1 = adm1,
              mask = TRUE,
              terr = ras)

  # Add neighbor countries
  terrain_map(countries = cntry,
              add_neighbors = TRUE,
              mask = TRUE,
              terr = si_path("path_raster"))

  terrain_map(countries = cntry,
              add_neighbors = TRUE,
              mask = TRUE,
              terr = ras)

  terrain_map(countries = cntry,
              add_neighbors = TRUE,
              mask = FALSE,
              terr = ras)

  # Add neighbor countries with La
  terrain_map(countries = cntry,
              add_neighbors = TRUE,
              add_labels = TRUE,
              mask = TRUE,
              terr = ras)








