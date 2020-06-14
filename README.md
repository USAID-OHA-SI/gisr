# gisr
OHA Geospatial Analytics Utilities

## Installation

```{r}
install.packages("devtools")
devtools::install_github("USAID-OHA-SI/gisr")
```
    
## Get admin boundaries for togo

```{r}
library(gisr)

tgo0 <- get_adm_boundaries("TGO", adm_level=0, geo_path="./GIS")
tgo1 <- get_adm_boundaries("TGO", adm_level=1, geo_path="./GIS")
```



---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*