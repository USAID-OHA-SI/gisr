# gisr
OHA Geospatial Analytics Utilities

## Installation

```{r}
install.packages("devtools")
devtools::install_github("USAID-OHA-SI/gisr")
```
    
## Get admin boundaries from [GADM](https://gadm.org/download_country_v3.html) through Raster package

```{r}
library(gisr)

tgo0 <- get_adm_boundaries("TGO", adm_level=0, geo_path="./GIS")
tgo1 <- get_adm_boundaries("TGO", adm_level=1, geo_path="./GIS")
```

## Get admin boundaries from [Natural Earth Data](https://www.naturalearthdata.com/)

```{r}
library(tidyverse)
library(sf)
library(gisr)

zambia0 <- get_admin0("Zambia") 
zambia1 <- get_admin1("Zambia") 

zambia_neighbors <- geo_neighbors("Zambia") 

ggplot(data = zambia_neighbors) +
        geom_sf(fill = NA) +
        geom_sf(data = zambia1, fill = gray(.92), lty = "dashed") +
        geom_sf(data = zambia0, fill = NA) +
        geom_sf_text(aes(label = sovereignt), size = 3) +
        theme_void()
```


## Plot a terrain map from local DEM file

```{r}
library(tidyverse)
library(sf)
library(gisr)

dem_dir <- "./GIS"

z_map1 <- terrain_map("Zambia", terr_path = dir_terr)

print(z_map1)

z_map2 <- terrain_map("Zambia", terr_path = dir_terr, add_neighbors = TRUE) 
    
print(z_map2)

```



---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*