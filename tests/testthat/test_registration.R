## -- Runtime registration (the bug we fixed) --

test_that("registering new types and locations at runtime works", {
  # This is the workflow that was broken before the lazy init fix
  register.types("TEST_REGION", "TR.", "Test regions for testthat")
  register.locations(
    type = "TEST_REGION",
    locations = c("01", "02"),
    location.names = c("test.region.1", "test.region.2")
  )

  # Verify they exist
  expect_equal(unname(get.location.type("TR.01")), "TEST_REGION")
  expect_equal(unname(get.location.name("TR.01")), "test.region.1")

  # Verify type is registered
  types <- get.location.types(simple = TRUE)
  expect_true("TEST_REGION" %in% types)
})

test_that("is.location.valid works for known and unknown codes", {
  expect_true(is.location.valid("MD"))
  expect_false(is.location.valid("ZZZZZ_FAKE_99"))
})
