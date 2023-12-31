% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LOCATIONS_location_manager.R
\name{is.location.valid}
\alias{is.location.valid}
\title{is.location.valid}
\usage{
is.location.valid(locations, suggest.options = F)
}
\arguments{
\item{suggest.options}{A boolean indicating whether to check the aliases for potential matches}

\item{location}{A character vector representing potential locations}
}
\value{
A vector of boolean values whether the passed-in value is a location codes.  If false, this function will display a list of possibilities.
}
\description{
Check to see if the passed-in value matches any location code or alias
}
