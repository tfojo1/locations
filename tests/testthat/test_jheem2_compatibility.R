test_that("JHEEM2 model-location validation and canonicalization remain compatible", {
  # JHEEM_entity and compiled specifications validate first, then sanitize.
  expect_true(is.location.valid("md"))
  expect_identical(unname(sanitize("md")), "MD")

  expect_true(is.location.valid("09001"))
  expect_identical(unname(sanitize("09001")), "09110")
})

test_that("JHEEM2 location metadata lookups retain scalar-compatible values", {
  # Plotting and model metadata use these lookups directly in scalar contexts.
  expect_identical(unname(get.location.type("C.12580")), "CBSA")
  expect_identical(
    unname(get.location.name("C.12580")),
    "Baltimore-Columbia-Towson, MD"
  )
})

test_that("JHEEM2 nested-location discovery retains lookup round trips", {
  # Nested-proportion likelihoods pass containment results through
  # get.location.code() before comparing their minimal component sets.
  counties <- get.contained.locations("C.12580", "COUNTY")
  round_trip <- unlist(
    get.location.code(counties, "COUNTY"),
    use.names = FALSE
  )

  expect_length(counties, 7L)
  expect_setequal(round_trip, unname(counties))
  expect_identical(
    unname(get.overlapping.locations("C.12580", "STATE")),
    "MD"
  )
})
