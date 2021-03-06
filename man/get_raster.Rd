% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/terrain_map.R
\name{get_raster}
\alias{get_raster}
\title{Get Terrain Raster dataset}
\usage{
get_raster(terr_path = "../../GEODATA/RASTER", name = "SR_LR.tif")
}
\arguments{
\item{terr_path}{path to terrain raster file}

\item{name}{Name of the raster file}
}
\value{
RasterLayer
}
\description{
Get Terrain Raster dataset
}
\examples{
\dontrun{
get_raster(terr_path = glamr::si_path("path_raster"))
}

}
