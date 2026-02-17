test_that("sanitize uppercases codes", {
  result <- sanitize("md")
  expect_equal(unname(result), "MD")
})

test_that("sanitize preserves already-correct codes", {
  result <- sanitize("MD")
  expect_equal(unname(result), "MD")
})

test_that("sanitize handles CBSA prefix", {
  result <- sanitize("c.12580")
  expect_equal(unname(result), "C.12580")
})

test_that("sanitize resolves CT code aliases", {
  # Old CT FIPS 09001 should resolve to new code 09110
  result <- sanitize("09001")
  expect_equal(unname(result), "09110")
})

test_that("sanitize works on vectors", {
  result <- sanitize(c("md", "CA", "ny"))
  expect_equal(unname(result), c("MD", "CA", "NY"))
  expect_length(result, 3)
})

test_that("sanitize errors on non-character input", {
  expect_error(sanitize(12345), "character")
})

test_that("sanitize errors on unrecognized code", {
  expect_error(sanitize("ZZZZZ_FAKE"))
})
