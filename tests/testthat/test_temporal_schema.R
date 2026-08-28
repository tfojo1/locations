test_that("representative temporal county data satisfies the normalized schema", {
  data <- temporal_county_fixture()

  expect_true(locations:::validate_temporal_location_data(data))
  expect_setequal(names(data), names(locations:::temporal_location_schema()))
})

test_that("Connecticut historic counties and planning regions are distinct", {
  data <- temporal_county_fixture()
  ct_codes <- data$codes[data$codes$code %in% c("09003", "09110"), ]

  expect_equal(nrow(ct_codes), 2L)
  expect_false(identical(ct_codes$location_id[[1L]], ct_codes$location_id[[2L]]))
  expect_false(any(data$aliases$alias %in% c("09003", "09110")))
})

test_that("Alaska split and Montana replacement are explicit successions", {
  data <- temporal_county_fixture()

  ak <- data$successions[
    data$successions$from_location_id == "loc_ak_valdez_cordova",
  ]
  expect_setequal(
    ak$to_location_id,
    c("loc_ak_chugach", "loc_ak_copper_river")
  )
  expect_true(all(ak$succession_kind == "split"))

  mt <- data$successions[
    data$successions$from_location_id == "loc_mt_yellowstone_park",
  ]
  expect_setequal(mt$to_location_id, c("loc_mt_gallatin", "loc_mt_park"))
  expect_true(all(mt$succession_kind == "replaced_by"))
})

test_that("current status is derived from the pinned reference date", {
  data <- temporal_county_fixture()
  as_of <- as.Date(data$metadata$default_reference_date)
  versions <- data$versions
  starts_before <- versions$valid_from == "" |
    as.Date(versions$valid_from) <= as_of
  ends_after <- versions$valid_to == "" |
    as.Date(versions$valid_to) > as_of
  current <- versions$location_id[starts_before & ends_after]

  expect_true("loc_ct_capitol" %in% current)
  expect_false("loc_ct_hartford" %in% current)
  expect_true(all(c("loc_ak_chugach", "loc_ak_copper_river") %in% current))
  expect_false("loc_ak_valdez_cordova" %in% current)
  expect_true(all(c("loc_mt_gallatin", "loc_mt_park") %in% current))
  expect_false("loc_mt_yellowstone_park" %in% current)
})

test_that("overlapping entity versions and code assignments are rejected", {
  data <- temporal_county_fixture()
  data <- add_temporal_fixture_row(data, "versions", list(
    location_version_id = "ver_ct_capitol_duplicate",
    location_id = "loc_ct_capitol", type = "COUNTY",
    preferred_name = "Capitol Planning Region duplicate",
    valid_from = "2023-01-01", valid_from_precision = "day",
    valid_to = "", valid_to_precision = "unknown", end_reason = "",
    source_id = "src_ct"
  ))
  expect_error(
    locations:::validate_temporal_location_data(data),
    "versions contains overlapping validity intervals"
  )

  data <- temporal_county_fixture()
  data <- add_temporal_fixture_row(data, "codes", list(
    location_code_id = "code_ct_overlap", location_id = "loc_ct_capitol",
    code_system_id = "census_county_geoid", code = "09003",
    valid_from = "2021-01-01", valid_from_precision = "day",
    valid_to = "", valid_to_precision = "unknown", source_id = "src_ct"
  ))
  expect_error(
    locations:::validate_temporal_location_data(data),
    "code assignments contains overlapping validity intervals"
  )
})

test_that("historic official codes cannot be reintroduced as aliases", {
  data <- temporal_county_fixture()
  data <- add_temporal_fixture_row(data, "aliases", list(
    alias_id = "alias_bad_ct", alias = "09003", alias_kind = "synonym",
    location_id = "loc_ct_capitol", valid_from = "2022-01-01",
    valid_from_precision = "day", valid_to = "",
    valid_to_precision = "unknown", source_id = "src_ct",
    equivalence_evidence = "Deliberately invalid test record"
  ))

  expect_error(
    locations:::validate_temporal_location_data(data),
    "aliases may not duplicate official or package codes"
  )
})

test_that("crosswalk measure semantics are enforced", {
  data <- temporal_county_fixture()
  for (edge in c("cross_ak_chugach", "cross_ak_copper")) {
    data <- add_temporal_fixture_row(data, "crosswalk_measures", list(
      crosswalk_measure_id = paste0("measure_", edge),
      crosswalk_id = edge, measure_type = "land_area",
      reference_date = "2020-01-01", population_universe = "",
      method = "Deliberately incomplete fixture fractions", source_id = "src_ak",
      numerator = 40, denominator = 100,
      fraction_of_from = 0.4, fraction_of_to = NA_real_
    ))
  }
  expect_error(
    locations:::validate_temporal_location_data(data),
    "exhaustive fraction_of_from values must sum to one"
  )

  data <- temporal_county_fixture()
  data <- add_temporal_fixture_row(data, "crosswalk_measures", list(
    crosswalk_measure_id = "measure_population",
    crosswalk_id = "cross_ak_chugach", measure_type = "population",
    reference_date = "2020-01-01", population_universe = "",
    method = "Test population allocation", source_id = "src_ak",
    numerator = 50, denominator = 100,
    fraction_of_from = 0.5, fraction_of_to = NA_real_
  ))
  expect_error(
    locations:::validate_temporal_location_data(data),
    "population crosswalk measures require a population universe"
  )
})

test_that("temporal foreign keys and interval precision are enforced", {
  data <- temporal_county_fixture()
  data$codes$source_id[[1L]] <- "missing-source"
  expect_error(
    locations:::validate_temporal_location_data(data),
    "references missing values: missing-source"
  )

  data <- temporal_county_fixture()
  data$versions$valid_to_precision[[1L]] <- "unknown"
  expect_error(
    locations:::validate_temporal_location_data(data),
    "valid_to and its precision must agree"
  )
})

test_that("malformed dates fail with a validator diagnostic", {
  data <- temporal_county_fixture()
  data$versions$valid_to[[1L]] <- "2022-99-99"

  expect_error(
    locations:::validate_temporal_location_data(data),
    "versions\\$valid_to must use YYYY-MM-DD",
    fixed = FALSE
  )
})

test_that("relationship intervals must fit their endpoint versions", {
  data <- temporal_county_fixture()
  data <- add_temporal_fixture_row(data, "relationships", list(
    relationship_id = "rel_invalid_ct",
    child_version_id = "ver_ct_hartford_pre2022",
    parent_version_id = "ver_ct_capitol_2022",
    relation_kind = "overlaps", valid_from = "2021-01-01",
    valid_from_precision = "day", valid_to = "2023-01-01",
    valid_to_precision = "day", source_id = "src_ct"
  ))

  expect_error(
    locations:::validate_temporal_location_data(data),
    "relationship validity must fall within both endpoint versions"
  )
})

test_that("the normalized schema is strict", {
  data <- temporal_county_fixture()
  data$unexpected <- data.frame(value = character())
  expect_error(
    locations:::validate_temporal_location_data(data),
    "Unexpected tables: unexpected"
  )

  data <- temporal_county_fixture()
  data$entities$unexpected <- "value"
  expect_error(
    locations:::validate_temporal_location_data(data),
    "entities has unexpected columns: unexpected"
  )

  data <- temporal_county_fixture()
  data[[length(data) + 1L]] <- data$geometries
  names(data)[length(data)] <- "geometries"
  expect_error(
    locations:::validate_temporal_location_data(data),
    "data tables must have unique, non-blank names"
  )
})

test_that("crosswalk numeric values are finite and paired", {
  data <- temporal_county_fixture()
  data <- add_temporal_fixture_row(data, "crosswalk_measures", list(
    crosswalk_measure_id = "measure_infinite",
    crosswalk_id = "cross_ak_chugach", measure_type = "land_area",
    reference_date = "2020-01-01", population_universe = "",
    method = "Invalid numeric fixture", source_id = "src_ak",
    numerator = Inf, denominator = 100,
    fraction_of_from = NA_real_, fraction_of_to = NA_real_
  ))
  expect_error(
    locations:::validate_temporal_location_data(data),
    "crosswalk numeric values must be finite or NA"
  )

  data <- temporal_county_fixture()
  data <- add_temporal_fixture_row(data, "crosswalk_measures", list(
    crosswalk_measure_id = "measure_unpaired",
    crosswalk_id = "cross_ak_chugach", measure_type = "land_area",
    reference_date = "2020-01-01", population_universe = "",
    method = "Invalid numeric fixture", source_id = "src_ak",
    numerator = 40, denominator = NA_real_,
    fraction_of_from = NA_real_, fraction_of_to = NA_real_
  ))
  expect_error(
    locations:::validate_temporal_location_data(data),
    "crosswalk numerator and denominator must be supplied together"
  )
})
