baseline_environment <- new.env(parent = emptyenv())
load("R/sysdata.rda", envir = baseline_environment)
baseline <- baseline_environment$.location_data

source("R/temporal_schema.R")
source("R/temporal_county_pipeline.R")
temporal_bundle <- build_temporal_county_data()
.temporal_county_data <- temporal_bundle$data

source("R/location_manager.R")
source("R/location_validation.R")
source("R/location_init.R")
source("code_for_building_package/set_up_cached_location_manager.R")

generated <- extract_location_data(LOCATION.MANAGER)
validate_location_data(generated)
comparison <- all.equal(generated, baseline, check.attributes = TRUE)
if (!isTRUE(comparison)) {
  stop(
    "Generated manager changed the serialized legacy consumer boundary:\n",
    paste(comparison, collapse = "\n")
  )
}

county_rows <- generated$locations$type == "COUNTY"
if (sum(county_rows) != 3241L) {
  stop("Expected exactly 3,241 canonical legacy counties")
}

legacy_aliases <- utils::read.csv(
  "data-raw/legacy_code_aliases.csv",
  colClasses = "character",
  stringsAsFactors = FALSE
)
county_aliases <- generated$alias.codes$COUNTY
resolved <- unname(unlist(county_aliases[legacy_aliases$alias_code]))
if (!identical(resolved, legacy_aliases$canonical_code)) {
  stop("Connecticut legacy alias compatibility changed")
}
if (any(legacy_aliases$alias_code %in% generated$locations$code[county_rows])) {
  stop("Connecticut legacy aliases were reintroduced as canonical counties")
}

message(
  "Validated exact legacy manager parity (3,241 counties; codes, ordering, ",
  "names, aliases, relationships, coordinates, and polygons unchanged)."
)
