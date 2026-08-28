test_that("county registry updates preserve retired IDs and never reuse them", {
  registry_path <- tempfile(fileext = ".csv")
  registry <- data.frame(
    entity_key = "retired:test",
    location_id = "loc_00000001",
    initial_geoid = "99999",
    stringsAsFactors = FALSE
  )
  utils::write.csv(registry, registry_path, row.names = FALSE)

  current <- data.frame(
    GEOID = "01001", ANSICODE = "00161526", stringsAsFactors = FALSE
  )
  history <- data.frame(
    entity_key = character(), geoid = character(), stringsAsFactors = FALSE
  )
  updated <- locations:::update_temporal_county_registry(
    current, history, registry_path, write = TRUE
  )

  expect_equal(nrow(updated), 2L)
  expect_identical(updated$location_id, c("loc_00000001", "loc_00000002"))
  expect_true("retired:test" %in% updated$entity_key)

  repeated <- locations:::update_temporal_county_registry(
    current, history, registry_path, write = TRUE
  )
  expect_identical(repeated, updated)
})

test_that("county builds cannot silently invent unreviewed entity IDs", {
  registry_path <- tempfile(fileext = ".csv")
  current <- data.frame(
    GEOID = "01001", ANSICODE = "00161526", stringsAsFactors = FALSE
  )
  history <- data.frame(
    entity_key = character(), geoid = character(), stringsAsFactors = FALSE
  )

  expect_error(
    locations:::update_temporal_county_registry(
      current, history, registry_path, write = FALSE
    ),
    "registry is missing entity keys"
  )
})
