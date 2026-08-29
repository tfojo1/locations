temporal_location_columns <- c(
  "location_id", "location_version_id", "location_code_id", "type",
  "entity_kind", "status", "status_as_of", "preferred_name",
  "code_system_id", "code", "version_valid_from",
  "version_valid_from_precision", "version_valid_to",
  "version_valid_to_precision", "end_reason", "code_valid_from",
  "code_valid_from_precision", "code_valid_to", "code_valid_to_precision",
  "version_source_id", "version_source_vintage", "version_source_url",
  "code_source_id", "code_source_vintage", "code_source_url"
)

temporal_crosswalk_columns <- c(
  "crosswalk_id", "from_location_id", "from_location_version_id",
  "from_code", "from_name", "to_location_id", "to_location_version_id",
  "to_code", "to_name", "to_as_of", "relation_kind", "coverage",
  "measure_type", "numerator", "denominator", "fraction_of_from",
  "fraction_of_to", "reference_date", "population_universe", "method",
  "edge_source_id", "edge_source_url", "measure_source_id",
  "measure_source_url"
)

temporal_normalize_formals <- function(fun) {
  values <- formals(fun)
  vapply(seq_along(values), function(index) {
    name <- names(values)[index]
    entry <- values[index]
    names(entry) <- "argument"
    if (identical(entry, alist(argument = ))) return(name)
    paste0(
      name, "=",
      paste(deparse(values[[index]], width.cutoff = 500L), collapse = "")
    )
  }, character(1))
}

test_that("temporal API signatures and default date are stable", {
  expect_equal(
    temporal_normalize_formals(locations_default_date),
    character()
  )
  expect_equal(
    temporal_normalize_formals(get_locations),
    c(
      "type", "as_of=locations_default_date()", "status=\"current\""
    )
  )
  expect_equal(
    temporal_normalize_formals(resolve_location),
    c("code", "code_system=NULL", "as_of=NULL")
  )
  expect_equal(
    temporal_normalize_formals(get_location_history), "location_id"
  )
  expect_equal(
    temporal_normalize_formals(crosswalk_locations),
    c(
      "from", "to_as_of=locations_default_date()", "measure=\"none\""
    )
  )
  expect_s3_class(locations_default_date(), "Date")
  expect_equal(as.character(locations_default_date()), "2025-01-01")
})

test_that("get_locations selects type, vintage, and status explicitly", {
  current <- get_locations("county")
  expect_identical(names(current), temporal_location_columns)
  expect_equal(nrow(current), 3222L)
  expect_true(all(current$status == "current"))
  expect_true(all(current$status_as_of == "2025-01-01"))
  expect_equal(sum(substr(current$code, 1L, 2L) == "09"), 9L)

  ct_2021 <- get_locations("COUNTY", "2021-01-01", status = "all")
  old_ct <- ct_2021[ct_2021$code == "09003", , drop = FALSE]
  new_ct <- ct_2021[ct_2021$code == "09110", , drop = FALSE]
  expect_equal(old_ct$status, "current")
  expect_equal(new_ct$status, "future")

  historical <- get_locations("county", status = "historical")
  expect_true(all(historical$status == "historical"))
  expect_true(all(c("09001", "09003", "30113") %in% historical$code))
  expect_false(any(substr(historical$code, 1L, 5L) == "09110"))
})

test_that("temporal location inputs fail explicitly", {
  expect_error(get_locations("ZIPCODE"), "available types: COUNTY")
  expect_error(get_locations("COUNTY", status = "retired"), "status must")
  expect_error(get_locations("COUNTY", "2025-02-30"), "valid YYYY-MM-DD")
  expect_error(resolve_location("09003", "unknown"), "Unknown temporal")
  expect_error(resolve_location(character()), "code must")
})

test_that("resolve_location never silently canonicalizes historic codes", {
  hartford <- resolve_location("09003")
  expect_identical(names(hartford), temporal_location_columns)
  expect_equal(nrow(hartford), 1L)
  expect_equal(hartford$preferred_name, "Hartford County")
  expect_equal(hartford$code, "09003")
  expect_equal(hartford$status, "historical")
  expect_false(any(hartford$code == "09120"))

  expect_equal(nrow(resolve_location("09003", as_of = "2021-01-01")), 1L)
  expect_equal(nrow(resolve_location("09003", as_of = "2025-01-01")), 0L)

  unknown <- resolve_location("NOT-A-TEMPORAL-CODE")
  expect_identical(names(unknown), temporal_location_columns)
  expect_equal(nrow(unknown), 0L)
})

test_that("resolve_location uses date to disambiguate reused codes", {
  all_02230 <- resolve_location("02230")
  expect_equal(nrow(all_02230), 2L)
  expect_equal(length(unique(all_02230$location_id)), 2L)

  division <- resolve_location("02230", as_of = "1975-01-01")
  municipality <- resolve_location("02230", as_of = "2025-01-01")
  expect_equal(division$preferred_name, "Skagway-Yakutat Division")
  expect_equal(municipality$preferred_name, "Skagway Municipality")
  expect_false(identical(division$location_id, municipality$location_id))
})

test_that("get_location_history follows durable identity without succession", {
  kusilvak <- resolve_location("02158", as_of = "2025-01-01")
  history <- get_location_history(kusilvak$location_id)

  expect_identical(names(history), temporal_location_columns)
  expect_setequal(history$code, c("02270", "02158"))
  expect_setequal(history$preferred_name, c(
    "Wade Hampton Census Area", "Kusilvak Census Area"
  ))
  expect_equal(history$status, c("historical", "current"))
  expect_error(get_location_history("loc_not_real"), "Unknown temporal")
})

test_that("crosswalk_locations returns every target without legacy aliases", {
  edges <- crosswalk_locations("09003")
  expect_identical(names(edges), temporal_crosswalk_columns)
  expect_equal(nrow(edges), 3L)
  expect_setequal(edges$to_code, c("09110", "09140", "09160"))
  expect_false("09120" %in% edges$to_code)
  expect_true(all(edges$from_code == "09003"))
  expect_true(all(edges$measure_type == "none"))
  expect_true(all(is.na(edges$fraction_of_from)))
  expect_true(all(edges$relation_kind == "overlap"))
  expect_true(all(edges$coverage == "exhaustive"))

  no_legacy_reverse <- crosswalk_locations("09120")
  expect_equal(nrow(no_legacy_reverse), 0L)
  expect_identical(names(no_legacy_reverse), temporal_crosswalk_columns)
})

test_that("crosswalk measures are directional, typed, and never substituted", {
  land <- crosswalk_locations("09001", measure = "land_area")
  expect_equal(nrow(land), 3L)
  expect_true(all(land$measure_type == "land_area"))
  expect_equal(sum(land$fraction_of_from), 1, tolerance = 1e-12)
  expect_true(all(land$numerator >= 0))
  expect_true(all(land$denominator > 0))
  expect_true(all(land$reference_date == "2022-01-01"))
  expect_true(all(nzchar(land$measure_source_url)))

  water <- crosswalk_locations("09001", measure = "water_area")
  expect_equal(sum(water$fraction_of_from), 1, tolerance = 1e-12)
  expect_error(
    crosswalk_locations("09001", measure = "population"),
    "population.*unavailable.*land_area, water_area"
  )

  before_change <- crosswalk_locations("09001", to_as_of = "2021-01-01")
  expect_equal(nrow(before_change), 0L)
  expect_identical(names(before_change), temporal_crosswalk_columns)
})

test_that("crosswalk source can be an explicit durable or version ID", {
  resolved <- resolve_location("09001")
  by_location <- crosswalk_locations(resolved$location_id)
  by_version <- crosswalk_locations(resolved$location_version_id)
  expect_identical(by_location, by_version)
})
