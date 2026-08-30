normalize_formals <- function(fun) {
  values <- formals(fun)

  vapply(seq_along(values), function(index) {
    name <- names(values)[index]
    entry <- values[index]
    names(entry) <- "argument"

    if (identical(entry, alist(argument = ))) {
      return(name)
    }

    value <- values[[index]]
    default <- paste(deparse(value, width.cutoff = 500L), collapse = "")
    if (identical(default, "T")) default <- "TRUE"
    if (identical(default, "F")) default <- "FALSE"
    paste0(name, "=", default)
  }, character(1))
}

test_that("legacy public function signatures remain compatible", {
  expected <- list(
    get.all.for.type = "type",
    get.cbsa.for.msa.name = "names",
    get.code.by.alias = c("locations", "types"),
    get.contained.locations = c(
      "locations", "sub.type", "return.list=FALSE",
      "throw.error.if.unregistered.type=TRUE", "include.partial=FALSE"
    ),
    get.containing.locations = c(
      "locations", "super.type", "return.list=FALSE",
      "throw.error.if.unregistered.type=TRUE", "include.partial=FALSE"
    ),
    get.location.code = c("location.names", "types", "search.aliases=TRUE"),
    get.location.code.if.unique = c(
      "location.names", "types", "search.aliases=TRUE"
    ),
    get.location.coords = "locations",
    get.location.name = "locations",
    get.location.name.alias = c(
      "locations", "alias.name", "throw.error.if.unregistered.alias=TRUE"
    ),
    get.location.type = "locations",
    get.location.types = "simple=TRUE",
    get.overlapping.locations = c(
      "locations", "type", "return.list=FALSE",
      "throw.error.if.unregistered.type=TRUE"
    ),
    get.polygons.for.type = "type",
    get.prefix.for.type = "location.types",
    is.location.valid = c("locations", "suggest.options=FALSE"),
    location.plot = c(
      "data", "color", "fill", "size=NA", "title=NA",
      "bb=c(left = -125, bottom = 24, right = -66, top = 50)",
      "bb.edge=0.1", "size.range=c(1, 5)",
      "color.range=c(\"blue\", \"red\")", "pch=19", "size.label=\"\"",
      "color.label=\"\"", "alpha=1", "map_water_color=\"#C0C0C0\"",
      "stadia_api_key=Sys.getenv(\"STADIA_MAPS_API_KEY\")"
    ),
    location.type.comprises = c("super.location.type", "sub.location.type"),
    register.code.aliases = c("location=NA", "location.aliases=NA"),
    register.lat.and.long = c("location=NA", "lat=NA", "long=NA"),
    register.locations = c("type", "locations", "location.names"),
    register.name.aliases = c(
      "location=NA", "location.aliases=NA", "location.aliases.names=NA"
    ),
    register.relationship.between.types = c("super.type", "sub.type", "value"),
    register.sub.and.super.locations = c(
      "sub.locations", "super.locations", "super.completely.encloses.sub"
    ),
    register.types = c("type", "prefix", "prefix.longform"),
    sanitize = "codes"
  )

  exports <- getNamespaceExports("locations")
  expect_true(all(names(expected) %in% exports))

  for (function_name in names(expected)) {
    expect_equal(
      normalize_formals(getExportedValue("locations", function_name)),
      expected[[function_name]],
      info = function_name
    )
  }
})

test_that("legacy lookup return shapes remain compatible", {
  types <- get.location.type(c("MD", "24005", "NOT-A-LOCATION"))
  expect_type(types, "character")
  expect_named(types, c("MD", "24005", "NOT-A-LOCATION"))
  expect_true(is.na(types[[3]]))

  codes <- get.location.code(c("Maryland", "Baltimore County"), "STATE")
  expect_type(codes, "list")
  expect_named(codes, c("Maryland", "Baltimore County"))

  unique_code <- get.location.code.if.unique("Maryland", "STATE")
  expect_type(unique_code, "character")
  expect_named(unique_code, "Maryland")

  counties <- get.contained.locations("MD", "COUNTY")
  expect_type(counties, "character")
  expect_false(is.list(counties))

  county_list <- get.contained.locations("MD", "COUNTY", return.list = TRUE)
  expect_type(county_list, "list")
  expect_named(county_list, "MD")

  sanitized <- sanitize(c("md", "c.12580"))
  expect_type(sanitized, "character")
  expect_named(sanitized, c("md", "c.12580"))
})

test_that("code alias lookup supports its documented vector inputs", {
  one_type <- get.code.by.alias(c("24", "09"), "STATE")
  expect_type(one_type, "list")
  expect_named(one_type, c("24", "09"))
  expect_equal(unname(one_type), list("MD", "CT"))

  paired_types <- get.code.by.alias(c("24", "09003"), c("STATE", "COUNTY"))
  expect_equal(unname(paired_types), list("MD", "09120"))

  expect_error(
    get.code.by.alias(c("24", "09"), c("STATE", "STATE", "STATE")),
    "length either 1 or same as locations"
  )
})

test_that("legacy and temporal county enumeration diverge only as documented", {
  legacy <- unname(get.all.for.type("COUNTY"))
  current <- unique(get_locations("COUNTY")$code)
  expected_legacy_only <- c(
    "02010", "02030", "02040", "02080", "02120", "02140", "02160",
    "02190", "02200", "02201", "02210", "02231", "02232", "02250",
    "02260", "02261", "02270", "02280", "30113"
  )

  expect_setequal(setdiff(legacy, current), expected_legacy_only)
  expect_length(setdiff(current, legacy), 0L)
})

test_that("data bundle version is explicit and independent", {
  expect_type(locations.data.version(), "character")
  expect_length(locations.data.version(), 1L)
  expect_match(
    locations.data.version(),
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}\\.[0-9]+$"
  )
  expect_false(identical(
    locations.data.version(),
    as.character(packageVersion("locations"))
  ))
})
