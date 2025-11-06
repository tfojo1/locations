#' Package Lifecycle Hooks
#'
#' This file (zzz.R) contains package lifecycle functions following R convention.
#' Functions here run during package load/unload/attach events.
#'
#' @keywords internal

# Global variable that will hold the location manager
LOCATION.MANAGER <- NULL

.onLoad <- function(libname, pkgname) {
  # Check that cached location data exists
  if (!exists(".location_data", envir = parent.env(environment()), inherits = FALSE)) {
    stop(
      "Location data is missing from package. ",
      "The package may be corrupted. Please reinstall: ",
      "devtools::install_github('your-repo/locations')"
    )
  }

  # Build the LOCATION.MANAGER from the cached data
  # Using <<- to assign to package namespace
  tryCatch({
    LOCATION.MANAGER <<- build_location_manager(.location_data)
  }, error = function(e) {
    stop(
      "Failed to initialize location manager during package load.\n",
      "Error: ", e$message, "\n",
      "This may indicate corrupted package data. Please reinstall the package."
    )
  })

  # Optional: Add a startup message (only if needed for debugging)
  # packageStartupMessage("locations package loaded successfully")
}
