% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo_utilities.R
\name{spdf_export}
\alias{spdf_export}
\title{Export spatial data as shapefile with flags}
\usage{
spdf_export(spdf, name)
}
\arguments{
\item{spdf}{sf object}

\item{name}{filename with full path}
}
\value{
boolean
}
\description{
Export spatial data as shapefile with flags
}
\examples{
\dontrun{
 library(gisr)
 library(sf)

 shp <- get_admin0(countries = "Nigeria")

 export_spdf(spdf = shp, name = "./GIS/nga_country_boundaries")
 export_spdf(spdf = shp, name = "./GIS/nga_country_boundaries.shp")
}

}
