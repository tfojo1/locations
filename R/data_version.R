#' Location Data Version
#'
#' Return the version of the packaged location-data bundle. This version is
#' independent of the package version because data corrections and source
#' refreshes do not necessarily change the R API.
#'
#' The value identifies the complete bundled dataset; it is not a claim that
#' every source in the bundle has the same geographic vintage. Consult
#' `data-raw/SOURCES.csv` in the source repository for source-level vintages.
#'
#' @return A single character value containing the location-data version.
#' @export
locations.data.version <- function() { # nolint: object_name_linter.
  version <- utils::packageDescription(
    "locations",
    fields = "Config/locations/data-version"
  )

  if (length(version) != 1L || is.na(version) || !nzchar(version)) {
    stop("The installed locations package does not declare a data version")
  }

  unname(version)
}
