## -- get.contained.locations --

test_that("get.contained.locations returns counties for a state", {
  md_counties <- get.contained.locations("MD", "COUNTY")
  expect_true(length(md_counties) > 0)
  # Baltimore County
  expect_true("24005" %in% md_counties)
})

test_that("get.contained.locations returns states for US", {
  us_states <- get.contained.locations("US", "STATE")
  expect_true("MD" %in% us_states)
  expect_true("CA" %in% us_states)
})

test_that("get.contained.locations return.list mode works", {
  result <- get.contained.locations(c("MD", "CA"), "COUNTY", return.list = TRUE)
  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(length(result[["MD"]]) > 0)
  expect_true(length(result[["CA"]]) > 0)
})

test_that("get.contained.locations CT returns new codes", {
  ct_counties <- get.contained.locations("CT", "COUNTY")
  # Should return the new planning region codes, not old county codes
  expect_true("09110" %in% ct_counties || "09120" %in% ct_counties)
  # Old codes should NOT appear
  expect_false("09001" %in% ct_counties)
})

test_that("get.contained.locations can include partial county overlaps", {
  expected <- list(
    "IL.1" = "17031", "IL.2" = "17031", "IL.3" = "17031",
    "IL.4" = "17031", "IL.5" = "17031", "IL.6" = "17031",
    "IL.7" = "17031", "DC.1" = "11001", "DC.2" = "11001",
    "DC.3" = "11001", "DC.4" = "11001", "DC.5" = "11001",
    "DC.6" = "11001", "DC.7" = "11001", "DC.8" = "11001",
    "DE.2" = "10003", "DE.4" = "10003",
    "MA.2" = c("25013", "25017", "25021", "25027"),
    "MA.3" = c("25017", "25021", "25023", "25027")
  )

  result <- get.contained.locations(
    names(expected), "COUNTY", return.list = TRUE, include.partial = TRUE
  )

  for (code in names(expected)) {
    expect_equal(sort(unname(result[[code]])), sort(expected[[code]]))
  }
})

test_that("partial county membership is opt-in", {
  expect_length(get.contained.locations("IL.1", "COUNTY"), 0)
  expect_equal(
    unname(get.contained.locations("IL.1", "COUNTY", include.partial = TRUE)),
    "17031"
  )
})

test_that("get.contained.locations errors on invalid sub.type length", {
  expect_error(get.contained.locations("MD", c("COUNTY", "STATE")),
               "sub.type must be a single character")
})

## -- get.containing.locations --

test_that("get.containing.locations returns state for county", {
  result <- get.containing.locations("24005", "STATE")
  expect_true("MD" %in% result)
})

test_that("get.containing.locations can include partial parents", {
  result <- get.containing.locations("17031", "NSDUH", include.partial = TRUE)
  expect_true(all(paste0("IL.", 1:7) %in% result))
})

## -- location.type.comprises --

test_that("states comprise counties", {
  expect_true(location.type.comprises("STATE", "COUNTY"))
})

test_that("counties do not comprise states", {
  expect_false(location.type.comprises("COUNTY", "STATE"))
})

## -- get.overlapping.locations --

test_that("get.overlapping.locations finds states for CBSA", {
  # Baltimore CBSA spans MD
  result <- get.overlapping.locations("C.12580", "STATE")
  expect_true("MD" %in% result)
})

## Note: relationship functions (contained, containing, overlapping) return
## character(0) for unknown locations rather than NA. This is inconsistent with
## scalar getters (get.location.type, etc.) which return NA. Pre-existing
## behavior, not introduced by the Phase 1 refactor.

test_that("get.overlapping.locations returns empty for unknown location", {
  result <- get.overlapping.locations("NONEXISTENT_99999", "STATE")
  expect_length(result, 0)
})
