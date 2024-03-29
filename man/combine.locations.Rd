% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LOCATIONS_location_manager.R
\name{combine.locations}
\alias{combine.locations}
\title{combine.locations}
\usage{
combine.locations(sub.locations, new.location.code, new.location.name, type)
}
\arguments{
\item{sub.locations}{A character vector of location codes we wish to combine into a new location.  Will fail on unknown codes.}

\item{new.location.code}{The location code for the new location we are creating, without the type prefix}

\item{new.location.name}{The location name for the new location we are creating.}

\item{type}{The type of the new location which has been previously registered}
}
\description{
Will create a new location composed of sub locations; if these sub locations have polygons, it will merge these polygons 
            into a unioned polygon.  We will need to register the type of this new location before this function is called.
}
