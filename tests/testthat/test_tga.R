## -- TGA (Ryan White Transitional Grant Area) type, built into the package --
## Oakland TGA = Alameda County (06001) + Contra Costa County (06013)

test_that("TGA type is registered", {
  expect_true("TGA" %in% get.location.types())
})

test_that("Oakland TGA resolves with correct type and name", {
  expect_equal(unname(get.location.type("TGA.OAKLAND")), "TGA")
  expect_equal(unname(get.location.name("TGA.OAKLAND")), "Oakland")
})

test_that("TGA comprises COUNTY", {
  expect_true(location.type.comprises("TGA", "COUNTY"))
})

test_that("Oakland TGA contains Alameda and Contra Costa counties", {
  counties <- get.contained.locations("TGA.OAKLAND", "COUNTY")
  expect_setequal(unname(counties), c("06001", "06013"))
})

test_that("member counties report Oakland TGA as containing", {
  expect_equal(unname(get.containing.locations("06001", "TGA")), "TGA.OAKLAND")
  expect_equal(unname(get.containing.locations("06013", "TGA")), "TGA.OAKLAND")
})

test_that("a non-member county is not in the Oakland TGA", {
  # San Francisco County is in the SF-Oakland CBSA but NOT the Oakland TGA
  expect_length(get.containing.locations("06075", "TGA"), 0)
})
