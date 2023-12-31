% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/searchId.R
\name{searchId}
\alias{searchId}
\title{function for search SENAMHI stations information by station ID.}
\usage{
searchId(x, stations)
}
\arguments{
\item{x}{character or number; optional character vector to filter results by station ID.}

\item{stations}{stations information of SENAMHI web scraping}
}
\description{
function for search SENAMHI stations information by station name inside SENAMHI data web.
}
\examples{

x <- 114
stations <- stations()

searchId(x, stations)

}
\author{
Geomar Perales Apaico
}
