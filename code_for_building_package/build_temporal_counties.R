arguments <- commandArgs(trailingOnly = TRUE)
update_registry <- "--update-registry" %in% arguments

source("R/temporal_schema.R")
source("R/temporal_county_pipeline.R")

bundle <- build_temporal_county_data(update_registry = update_registry)
.temporal_county_data <- bundle$data

sysdata_path <- file.path("R", "sysdata.rda")
sysdata <- new.env(parent = emptyenv())
load(sysdata_path, envir = sysdata)
if (!exists(".location_data", envir = sysdata, inherits = FALSE)) {
  stop("R/sysdata.rda does not contain the legacy .location_data object")
}
.location_data <- get(".location_data", envir = sysdata, inherits = FALSE)
save(
  .location_data,
  .temporal_county_data,
  file = sysdata_path,
  compress = "xz",
  version = 2
)

report_path <- file.path(
  "code_for_building_package", "temporal_county_build_report.csv"
)
utils::write.csv(bundle$report, report_path, row.names = FALSE, na = "")

message(
  "Built temporal county data: ",
  nrow(.temporal_county_data$entities), " entities, ",
  nrow(.temporal_county_data$versions), " versions, ",
  nrow(.temporal_county_data$codes), " code assignments, ",
  nrow(.temporal_county_data$successions), " succession edges, ",
  nrow(.temporal_county_data$crosswalk_edges), " crosswalk edges."
)
