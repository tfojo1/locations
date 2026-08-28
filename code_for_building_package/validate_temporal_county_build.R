source("R/temporal_schema.R")
source("R/temporal_county_pipeline.R")

bundle <- build_temporal_county_data()
validate_temporal_location_data(bundle$data)

sysdata <- new.env(parent = emptyenv())
load(file.path("R", "sysdata.rda"), envir = sysdata)
if (!exists(".temporal_county_data", envir = sysdata, inherits = FALSE)) {
  stop("R/sysdata.rda does not contain .temporal_county_data")
}
packaged <- get(".temporal_county_data", envir = sysdata, inherits = FALSE)
if (!identical(bundle$data, packaged)) {
  stop(
    "Packaged temporal county data is stale; run ",
    "Rscript code_for_building_package/build_temporal_counties.R"
  )
}

report_path <- file.path(
  "code_for_building_package", "temporal_county_build_report.csv"
)
if (!file.exists(report_path)) stop("Temporal county build report is missing")
committed_report <- utils::read.csv(
  report_path,
  colClasses = "character",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
if (!identical(bundle$report, committed_report)) {
  stop(
    "Temporal county build report is stale; run ",
    "Rscript code_for_building_package/build_temporal_counties.R"
  )
}

message(
  "Validated deterministic temporal county build (",
  nrow(packaged$entities), " entities; default date ",
  packaged$metadata$default_reference_date, "; ",
  nrow(packaged$crosswalk_edges), " crosswalk edges)."
)
