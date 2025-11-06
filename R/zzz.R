#' Package initialization
#'
#' This file contains the .onLoad hook that runs when the package is loaded.
#' It builds the LOCATION.MANAGER from the cached location data.
#'
#' @keywords internal

# Global variable that will hold the location manager
LOCATION.MANAGER <- NULL

.onLoad <- function(libname, pkgname) {
  # Load the cached location data (stored in sysdata.rda)
  # The build process saves .location_data (note the dot prefix)

  # Build the LOCATION.MANAGER from the data
  # Using <<- to assign to package namespace
  LOCATION.MANAGER <<- build_location_manager(.location_data)

  # Clean up the raw data from namespace (optional, but keeps namespace clean)
  # We don't actually need .location_data after initialization
  # Note: We can't use rm() here as it would try to remove from the function env
  # The data stays in sysdata.rda but that's fine
}
