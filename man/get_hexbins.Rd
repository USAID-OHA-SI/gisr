% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_hexbins.R
\name{get_hexbins}
\alias{get_hexbins}
\title{Get an hexbins polygon feature class for country ABC}
\usage{
get_hexbins(country_code, adm_level = 0, geo_path = NULL, size = NULL)
}
\arguments{
\item{country_code}{iso3 code}

\item{adm_level}{country administrative unit level}

\item{geo_path}{path to geodata}

\item{size}{size of each hex bin in meters}
}
\value{
country hex polygon as feature class
}
\description{
Get an hexbins polygon feature class for country ABC
}
\examples{
\dontrun{
get_hexbins('LSO', 1)
get_hexbins('UGA', 2)
}

}
