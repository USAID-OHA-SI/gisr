## PROJECT: Geospatial Analytics Utility functions
## PURPOSE: Create a Terrain Topographic Basemap
## AUTHOR:  B.Kagniniwa | USAID
## DATE:    2021-02-03


# Libraries ----

  library(tidyverse)
  library(gisr)

# Variables ----

  cntry <- "Nigeria"

  file_raster <- list.files(
      path = glamr::si_path("path_raster"),
      pattern = "^SR_LR.tif$",
      recursive = T,
      full.names = T
    )



cntry <- "Zambia"

z_adm0 <- gisr::get_admin0(cntry)
z_nghbrs <- geo_neighbors(cntry, crop = TRUE)

z_nghbrs2 %>% glimpse()

z_adm0 %>% geo_viz()

z_nghbrs %>% geo_viz()

box <- st_bbox(z_adm0) %>%
  st_as_sfc() %>%
  st_buffer(dist = 1,
            endCapStyle = "SQUARE",
            joinStyle = "MITRE",
            mitreLimit = 2) %>%
  st_as_sf()

z_nghbrs2 <- z_nghbrs %>%
  st_crop(box)

z_nghbrs2 %>%
  geo_viz() +
  geom_sf_text(data = z_nghbrs2, aes(label = sovereignt))


box %>% geo_viz()

box %>%
  geo_viz() +
  geom_sf(data = z_adm0, fill = NA)


# Raster

ras <- get_raster()

class(ras)


## Base map

terrain_map("Zambia",
            terr = si_path("path_raster"))

terrain_map("Zambia",
            mask = TRUE,
            terr = ras)

terrain_map("Zambia",
            mask = TRUE,
            terr = si_path("path_raster"))


terrain_map("Zambia",
            add_neighbors = TRUE,
            terr = si_path("path_raster"))


terrain_map("Zambia",
            add_neighbors = TRUE,
            terr = ras)

terrain_map("Zambia",
            add_neighbors = TRUE,
            add_labels = TRUE,
            terr = ras)






