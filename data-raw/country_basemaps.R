## PROJECT: Geospatial Analytics Utility functions
## PURPOSE: Create a Terrain Topographic Basemap
## AUTHOR:  B.Kagniniwa | USAID
## DATE:    2021-02-03


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

  # Country boundaries
  adm0 <- gisr::get_admin0(cntry)
  adm1 <- gisr::get_admin1(cntry)

  # Neighbors
  nghbrs <- geo_neighbors(cntry)

  nghbrs2 <- geo_neighbors(cntry, crop = TRUE)

  # RasterLayer
  ras <- gisr::get_raster(terr_path = glamr::si_path("path_raster"))

  # Get country extent & buffer by 50km

  box <- adm0 %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_transform(crs = 3857) %>%
    st_buffer(dist = 50000,
              endCapStyle = "SQUARE",
              joinStyle = "MITRE",
              mitreLimit = 2) %>%
    st_as_sf()

  # Crop neighors by country extent
  nghbrs2 <- nghbrs %>%
    st_transform(crs = 3857) %>%
    st_crop(box) %>%
    st_transform(crs = 4326)

# VIZ ----

  # View admins

  adm0 %>% gview()

  adm1 %>% gview()

  nghbrs2 %>% gview()

  # Add neighbor countries
  adm1 %>%
    gview() +
    geom_sf(data = nghbrs2, fill = glitr::grey10k, lwd = .2, color = "white") +
    geom_sf(data = adm0, fill = NA, size = 2, color = "white") +
    geom_sf(data = adm0, fill = NA, size = .6, color = glitr::grey70k) +
    geom_sf_text(data = nghbrs2,
                 aes(label = sovereignt),
                 color = glitr::grey80k,
                 size = 3)


  # Take advantage of terrain data
  terrain_map(countries = cntry,
              terr = si_path("path_raster"))

  # Mask terrain behind country boundaries
  terrain_map(countries = cntry,
              mask = TRUE,
              terr = si_path("path_raster"))

  # Mask terrain and provide your own rasterlayer
  terrain_map(countries = cntry,
              mask = TRUE,
              terr = ras)

  # Provide your own polygons and rasterlayer
  # TODO: re-test this
  terrain_map(countries = cntry,
              adm0 = adm0,
              #adm1 = adm1,
              mask = TRUE,
              terr = ras)

  # Add neighbor countries
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








