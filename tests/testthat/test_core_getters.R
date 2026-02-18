## -- get.location.type --

test_that("get.location.type returns correct types", {
  expect_equal(unname(get.location.type("MD")), "STATE")
  expect_equal(unname(get.location.type("24005")), "COUNTY")
  expect_equal(unname(get.location.type("C.12580")), "CBSA")
})

test_that("get.location.type returns NA for unknown codes", {
  result <- get.location.type("NONEXISTENT_99999")
  expect_true(is.na(unname(result)))
})

test_that("get.location.type works on vectors", {
  result <- get.location.type(c("MD", "24005"))
  expect_equal(unname(result), c("STATE", "COUNTY"))
})

## -- get.location.name --

test_that("get.location.name returns correct names", {
  result <- get.location.name("MD")
  expect_equal(unname(result), "Maryland")
})

test_that("get.location.name returns NA for unknown codes", {
  result <- get.location.name("NONEXISTENT_99999")
  expect_true(is.na(unname(result)))
})

test_that("get.location.name resolves aliases", {
  # Old CT code should resolve and return the new location's name
  old_name <- get.location.name("09001")
  new_name <- get.location.name("09110")
  expect_equal(unname(old_name), unname(new_name))
})

## -- get.location.types --

test_that("get.location.types returns registered types", {
  types <- get.location.types(simple = TRUE)
  expect_true("STATE" %in% types)
  expect_true("COUNTY" %in% types)
  expect_true("CBSA" %in% types)
  expect_true("COUNTRY" %in% types)
})

test_that("get.location.types detailed mode returns list", {
  types <- get.location.types(simple = FALSE)
  expect_type(types, "list")
  expect_true(length(types) > 0)
})

## -- get.all.for.type --

test_that("get.all.for.type returns codes for STATE", {
  states <- get.all.for.type("STATE")
  expect_true("MD" %in% states)
  expect_true("CA" %in% states)
  expect_true(length(states) >= 48)  # at least continental states
})

test_that("get.all.for.type returns NA for unregistered type", {
  result <- get.all.for.type("NONEXISTENT_TYPE")
  expect_true(is.na(result))
})

## -- get.location.coords --

test_that("get.location.coords returns coords for county", {
  coords <- get.location.coords("24005")
  expect_true(!is.na(coords))
  expect_match(coords, "^-?[0-9.]+,-?[0-9.]+$")
})

test_that("get.location.coords returns NA for location without coords", {
  coords <- get.location.coords("MD")
  expect_true(is.na(unname(coords)))
})

## -- polygons --

test_that("has.polygon works for state", {
  mgr <- locations:::LOCATION.MANAGER
  expect_true(mgr$has.polygon("MD"))
})

test_that("get.polys.for.type returns data.frame with expected columns", {
  mgr <- locations:::LOCATION.MANAGER
  polys <- mgr$get.polys.for.type("STATE")
  expect_true(is.data.frame(polys))
  expect_true(nrow(polys) > 0)
  expect_true(all(c("latitude", "longitude", "poly", "location.code") %in% names(polys)))
})
