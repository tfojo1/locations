temporal_county_codes_at <- function(code, as_of) {
  data <- locations:::.temporal_county_data
  data$codes[
    data$codes$code == code &
      locations:::temporal_county_active(
        data$codes$valid_from, data$codes$valid_to, as_of
      ),
    , drop = FALSE
  ]
}

test_that("packaged temporal county data satisfies its semantic schema", {
  expect_true(locations:::validate_temporal_location_data(
    locations:::.temporal_county_data
  ))
  expect_equal(
    locations:::.temporal_county_data$metadata$data_version,
    "census-counties-2025.2"
  )
  expect_equal(
    locations:::.temporal_county_data$metadata$default_reference_date,
    "2025-01-01"
  )
})

test_that("the pinned current county view has authoritative target counts", {
  current <- locations:::temporal_county_current_records(
    locations:::.temporal_county_data
  )
  counts <- table(substr(current$code, 1L, 2L))

  expect_equal(unname(counts[["02"]]), 30L)
  expect_equal(unname(counts[["09"]]), 9L)
  expect_equal(unname(counts[["30"]]), 56L)
  expect_equal(nrow(current), 3222L)
})

test_that("all legacy Alaska extras remain historical code records", {
  legacy_codes <- c(
    "02010", "02030", "02040", "02080", "02120", "02140",
    "02160", "02190", "02200", "02201", "02210", "02231",
    "02232", "02250", "02260", "02261", "02270", "02280"
  )
  data <- locations:::.temporal_county_data
  historic_codes <- data$codes$code[nzchar(data$codes$valid_to)]

  expect_true(all(legacy_codes %in% historic_codes))
  expect_equal(nrow(temporal_county_codes_at("02261", "2018-01-01")), 1L)
  expect_equal(nrow(temporal_county_codes_at("02261", "2025-01-01")), 0L)
})

test_that("a historic recode retains entity identity", {
  wade_hampton <- temporal_county_codes_at("02270", "2014-01-01")
  kusilvak <- temporal_county_codes_at("02158", "2025-01-01")

  expect_equal(nrow(wade_hampton), 1L)
  expect_equal(nrow(kusilvak), 1L)
  expect_identical(wade_hampton$location_id, kusilvak$location_id)
})

test_that("reused Alaska code 02230 resolves by date without an alias", {
  division <- temporal_county_codes_at("02230", "1975-01-01")
  municipality <- temporal_county_codes_at("02230", "2025-01-01")

  expect_equal(nrow(division), 1L)
  expect_equal(nrow(municipality), 1L)
  expect_false(identical(division$location_id, municipality$location_id))
  expect_equal(nrow(locations:::.temporal_county_data$aliases), 0L)
})

test_that("Connecticut old and new county equivalents stay distinct", {
  hartford <- temporal_county_codes_at("09003", "2021-01-01")
  capitol <- temporal_county_codes_at("09110", "2025-01-01")

  expect_equal(nrow(hartford), 1L)
  expect_equal(nrow(capitol), 1L)
  expect_false(identical(hartford$location_id, capitol$location_id))
  expect_equal(nrow(temporal_county_codes_at("09003", "2025-01-01")), 0L)
})

test_that("Connecticut has the complete directional county crosswalk", {
  data <- locations:::.temporal_county_data
  edges <- data$crosswalk_edges
  version_to_location <- stats::setNames(
    data$versions$location_id, data$versions$location_version_id
  )
  location_to_code <- stats::setNames(data$codes$code, data$codes$location_id)
  edge_codes <- paste0(
    unname(location_to_code[version_to_location[edges$from_version_id]]),
    "->",
    unname(location_to_code[version_to_location[edges$to_version_id]])
  )

  expect_setequal(edge_codes, c(
    "09001->09120", "09001->09140", "09001->09190",
    "09003->09110", "09003->09140", "09003->09160",
    "09005->09140", "09005->09160", "09005->09190",
    "09007->09130", "09009->09140", "09009->09170",
    "09011->09130", "09011->09150", "09011->09180",
    "09013->09110", "09013->09150",
    "09015->09150", "09015->09180"
  ))
  expect_true(all(edges$relation_kind == "overlap"))
  expect_true(all(edges$coverage == "exhaustive"))
  expect_true(all(edges$source_id == "src_ct_cousub_bg_2022"))
  expect_equal(nrow(data$aliases), 0L)
})

test_that("Connecticut crosswalk measures are explicit area fractions", {
  data <- locations:::.temporal_county_data
  measures <- merge(
    data$crosswalk_measures,
    data$crosswalk_edges[, c(
      "crosswalk_id", "from_version_id", "to_version_id"
    )],
    by = "crosswalk_id", sort = FALSE
  )

  expect_equal(nrow(measures), 38L)
  expect_setequal(unique(measures$measure_type), c("land_area", "water_area"))
  expect_false(any(measures$measure_type == "population"))
  expect_true(all(measures$population_universe == ""))
  expect_true(all(measures$reference_date == "2022-01-01"))
  expect_true(all(measures$numerator >= 0))
  expect_true(all(measures$denominator > 0))

  from_groups <- split(
    measures,
    paste(measures$from_version_id, measures$measure_type, sep = "\r")
  )
  expect_true(all(vapply(from_groups, function(group) {
    abs(sum(group$fraction_of_from) - 1) < 1e-12 &&
      all(group$denominator == sum(group$numerator))
  }, logical(1))))

  to_groups <- split(
    measures,
    paste(measures$to_version_id, measures$measure_type, sep = "\r")
  )
  expect_true(all(vapply(to_groups, function(group) {
    abs(sum(group$fraction_of_to) - 1) < 1e-12
  }, logical(1))))
})

test_that("Montana 30113 remains historical but not current", {
  expect_equal(nrow(temporal_county_codes_at("30113", "1990-01-01")), 1L)
  expect_equal(nrow(temporal_county_codes_at("30113", "2025-01-01")), 0L)

  successions <- locations:::.temporal_county_data$successions
  old_location <- temporal_county_codes_at("30113", "1990-01-01")$location_id
  successors <- successions$to_location_id[
    successions$from_location_id == old_location
  ]
  current_targets <- c(
    temporal_county_codes_at("30031", "2025-01-01")$location_id,
    temporal_county_codes_at("30067", "2025-01-01")$location_id
  )
  expect_setequal(successors, current_targets)
})

test_that("every normalized county record has pinned provenance", {
  data <- locations:::.temporal_county_data
  expect_equal(nrow(data$sources), 8L)
  expect_true(all(grepl("^md5:[0-9a-f]{32}$", data$sources$checksum)))
  expect_true(all(data$sources$retrieved_at == "2026-08-28"))
})
