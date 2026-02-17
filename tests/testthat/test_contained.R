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

test_that("get.contained.locations errors on invalid sub.type length", {
  expect_error(get.contained.locations("MD", c("COUNTY", "STATE")),
               "sub.type must be a single character")
})

## -- get.containing.locations --

test_that("get.containing.locations returns state for county", {
  result <- get.containing.locations("24005", "STATE")
  expect_true("MD" %in% result)
})

## -- location.type.comprises --

test_that("states comprise counties", {
  expect_true(location.type.comprises("STATE", "COUNTY"))
})

test_that("counties do not comprise states", {
  expect_false(location.type.comprises("COUNTY", "STATE"))
})
