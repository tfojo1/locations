test_that("packaged location data passes integrity validation", {
  expect_true(locations:::validate_location_data(locations:::.location_data))
})

test_that("duplicate location codes are rejected", {
  data <- locations:::.location_data
  data$locations <- rbind(data$locations, data$locations[1, ])
  expect_error(
    locations:::validate_location_data(data),
    "Duplicate location codes"
  )
})

test_that("dangling and self relationships are rejected", {
  data <- locations:::.location_data
  data$relationships <- rbind(
    data$relationships,
    data.frame(sub = "MISSING", super = "AK", complete = TRUE),
    data.frame(sub = "AK", super = "AK", complete = TRUE)
  )
  expect_error(
    locations:::validate_location_data(data),
    "missing locations.*Self-relationships"
  )
})

test_that("relationship cycles are rejected", {
  data <- locations:::.location_data
  data$relationships <- rbind(
    data$relationships,
    data.frame(sub = "AK", super = "02013", complete = TRUE)
  )
  expect_error(
    locations:::validate_location_data(data),
    "Relationship graph contains a cycle"
  )
})

test_that("aliases must resolve uniquely to locations of the same type", {
  data <- locations:::.location_data
  data$alias.codes$COUNTY[["BAD-ALIAS"]] <- "MD"
  expect_error(
    locations:::validate_location_data(data),
    "different type"
  )

  data <- locations:::.location_data
  data$alias.codes$COUNTY[["BAD-ALIAS"]] <- "MISSING"
  expect_error(
    locations:::validate_location_data(data),
    "missing targets"
  )
})
