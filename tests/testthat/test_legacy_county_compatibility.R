test_that("legacy county selection prefers the pinned-date record for reused codes", {
  records <- locations:::legacy_county_records(
    locations:::.temporal_county_data
  )
  skagway <- records[records$code == "02230", , drop = FALSE]
  selected <- locations:::legacy_county_select_record(
    skagway,
    locations:::.temporal_county_data$metadata$default_reference_date[[1L]]
  )

  expect_equal(nrow(selected), 1L)
  expect_equal(selected$preferred_name, "Skagway Municipality")
  expect_equal(selected$location_id, "loc_00000093")
})

test_that("legacy county selection retains the latest historical record", {
  records <- locations:::legacy_county_records(
    locations:::.temporal_county_data
  )
  historical <- records[records$code == "02010", , drop = FALSE]
  selected <- locations:::legacy_county_select_record(
    historical,
    locations:::.temporal_county_data$metadata$default_reference_date[[1L]]
  )

  expect_equal(nrow(selected), 1L)
  expect_equal(selected$preferred_name, "Aleutian Islands Census Area")
  expect_equal(selected$valid_to_version, "1987-10-23")
})

test_that("legacy county compatibility is isolated from temporal enumeration", {
  legacy <- unname(get.all.for.type("COUNTY"))
  ct_aliases <- sprintf("09%03d", seq(1L, 15L, by = 2L))

  expect_length(legacy, 3241L)
  expect_false(any(ct_aliases %in% legacy))
  expect_equal(
    unname(unlist(get.code.by.alias(ct_aliases, "COUNTY"))),
    sprintf("09%03d", seq(110L, 180L, by = 10L))
  )
})
