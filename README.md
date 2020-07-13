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


## Assess Facilities Location data

Extract Country OrgUnit Data and Assess Facalities Location data availability

```{r}
library(tidyverse)
library(gisr)

# Get results / targets data for country x

ken_targets <- list.files(path = "../path-to-mer-data",
                         pattern = "^HFR_FY20Q\\d{1}_KEN_\\d*_DATIM_\\d{8}.csv$",
                         full.names = TRUE) %>% 
                         map_dfr(read_csv)
                         
# Generate a report: Map + Bar chart showing available & missing location data

## 1) No basemap
generate_facilities_report(cntry = "Kenya",
                      targets = ken_targets,
                      user = "<username>",
                      pass = glamr::mypwd("<credential-key>""))

## 2) With basemap
generate_facilities_report(cntry = "Kenya",
                      targets = ken_targets,
                      user = "<username>",
                      pass = glamr::mypwd("<credential-key>""),
                      terr_path = "<../path-to-terrain-raster-data>",
                      output_folder = "<./path-to-ouput-folder>")
                      
```

---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*