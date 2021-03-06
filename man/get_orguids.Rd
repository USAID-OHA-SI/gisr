% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{get_orguids}
\alias{get_orguids}
\title{Get Org UIDS}
\usage{
get_orguids(
  level = 3,
  username = NULL,
  password = NULL,
  baseurl = "https://final.datim.org/"
)
}
\arguments{
\item{level}{Org level}

\item{username}{DATIM Username, recommend using `datim_user()`}

\item{password}{DATIM password, recommend using `datim_pwd()`}

\item{baseurl}{base url for the API, default = https://final.datim.org/}
}
\value{
ORG UIDS as tibble
}
\description{
Get Org UIDS
}
\note{
Use with caution. Use `get_ouorguids()` for levels below 3
}
\examples{
\dontrun{
 orgs <- get_orguids(level = 3)}

}
