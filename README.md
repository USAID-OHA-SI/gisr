<img src='man/figures/logo.png' align="right" height="120" />

[![R-CMD-check](https://github.com/USAID-OHA-SI/gisr/workflows/R-CMD-check/badge.svg)](https://github.com/USAID-OHA-SI/gisr/actions)

# gisr
OHA Geospatial Analytics Utilities

## Installation

```{r}
install.packages("devtools")
devtools::install_github("USAID-OHA-SI/gisr")
```
    
## Get admin boundaries 
 
Get admin boundaries from [GADM](https://gadm.org/download_country_v3.html) through Raster package

```{r}
library(gisr)

tgo0 <- get_adm_boundaries("TGO", adm_level=0, geo_path="./GIS")
tgo1 <- get_adm_boundaries("TGO", adm_level=1, geo_path="./GIS")
```

## Get admin boundaries 

Get target country admin boundaries + neighbor countries data from [Natural Earth Data](https://www.naturalearthdata.com/)


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


## Plot a terrain map 

Create a terrain map with vector + raster data from local DEM Tiff file

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


## Plot an admin map 

Create an administrative map with vector data from RNaturalEarth

```{r}
library(tidyverse)
library(sf)
library(glitr)
library(gisr)

z_map1 <- admins_map("Zambia")

print(z_map1)

z_map2 <- admins_map("Zambia", add_neighbors = TRUE) 
    
print(z_map2)

```

## Apply SI Style Guide to maps 

Create an administrative map with vector data from RNaturalEarth and apply SI Style

```{r}
library(extrafont)
library(tidyverse)
library(gisr)
library(sf)
library(glitr)


sfdf <- gisr::get_admin1("Nigeria") %>%
 select(name) %>%
 mutate(value = runif(nrow(.), 0, 1))

ggplot() +
   geom_sf(data = sfdf,
           aes(fill = value),
           color = grey10k,
           size = .1) +
   scale_fill_si(palette = "genoas",
                 discrete = FALSE,
                 limits = c(0, 1),
                 labels = scales::percent) +
   labs(title = "NIGERIA - % OF PLHIV BY STATE",
        subtitle = "States from XYZ Region are the most hit by HIV/AIDS",
        caption = base::paste0("Produced by OHA/SIEI/SI, ON ", base::Sys.Date())) +
  si_style_map()
  

```

![image](https://user-images.githubusercontent.com/3952707/125997981-73a84f04-5f23-48f4-a77a-fceb4b158f76.png)

---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*
