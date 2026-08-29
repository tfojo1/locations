#!/usr/bin/env Rscript

# Run a small, real downstream contract against an installed JHEEM2 package.
# This intentionally lives outside tests/testthat: normal locations checks do
# not install or depend on JHEEM2.

fail <- function(message) {
  stop(message, call. = FALSE)
}

expect_identical <- function(actual, expected, label) {
  if (!identical(actual, expected)) {
    fail(paste0(
      label,
      "\nExpected: ", paste(deparse(expected), collapse = " "),
      "\nActual:   ", paste(deparse(actual), collapse = " ")
    ))
  }
}

expect_setequal <- function(actual, expected, label) {
  if (!setequal(actual, expected)) {
    fail(paste0(
      label,
      "\nExpected set: ", paste(sort(expected), collapse = ", "),
      "\nActual set:   ", paste(sort(actual), collapse = ", ")
    ))
  }
}

expect_error_matching <- function(expression, pattern, label) {
  message <- tryCatch(
    {
      force(expression)
      NA_character_
    },
    error = function(condition) conditionMessage(condition)
  )

  if (is.na(message) || !grepl(pattern, message, perl = TRUE)) {
    fail(paste0(
      label,
      "\nExpected an error matching: ", pattern,
      "\nActual: ", if (is.na(message)) "no error" else message
    ))
  }
}

if (!requireNamespace("locations", quietly = TRUE)) {
  fail("The candidate locations package is not installed")
}
if (!requireNamespace("jheem2", quietly = TRUE)) {
  fail("The pinned JHEEM2 package is not installed")
}

suppressPackageStartupMessages(library(locations))
suppressWarnings(suppressPackageStartupMessages(library(jheem2)))

integration_library <- Sys.getenv("R_LIBS_USER")
if (!nzchar(integration_library)) {
  fail("R_LIBS_USER must identify the clean integration library")
}
integration_library <- normalizePath(integration_library, mustWork = TRUE)
expect_identical(
  normalizePath(dirname(find.package("locations")), mustWork = TRUE),
  integration_library,
  "locations was not loaded from the clean integration library"
)
expect_identical(
  normalizePath(dirname(find.package("jheem2")), mustWork = TRUE),
  integration_library,
  "JHEEM2 was not loaded from the clean integration library"
)

cat("locations ", as.character(utils::packageVersion("locations")), "\n", sep = "")
cat("jheem2 ", as.character(utils::packageVersion("jheem2")), "\n", sep = "")

# JHEEM2 data-manager imports validate location dimension values through
# locations::is.location.valid(). Exercise both accepted aliases and rejection.
data_manager <- create.data.manager(
  "locations-integration",
  "locations downstream validation probe"
)
data_manager$register.outcome(
  "cases",
  metadata = create.outcome.metadata(
    scale = "non.negative.number",
    display.name = "Cases",
    axis.name = "Cases",
    units = "cases",
    description = "locations integration probe"
  )
)
data_manager$register.parent.source(
  "integration-parent",
  full.name = "Locations integration",
  short.name = "integration"
)
data_manager$register.source(
  "integration",
  parent.source = "integration-parent",
  full.name = "Locations integration",
  short.name = "integration"
)
data_manager$register.ontology(
  "by-location",
  ont = ontology(location = NULL)
)

valid_data <- array(
  c(1, 2),
  dim = 2,
  dimnames = list(location = c("md", "09001"))
)
put.data(
  data_manager,
  valid_data,
  "cases",
  metric = "estimate",
  source = "integration",
  ontology.name = "by-location",
  url = "https://example.invalid/locations-integration",
  details = "locations integration contract"
)
expect_setequal(
  data_manager$get.locations.with.data("cases"),
  c("md", "09001"),
  "JHEEM2 data-manager validation rejected valid locations aliases"
)

invalid_data <- array(
  1,
  dim = 1,
  dimnames = list(location = "NOT_A_LOCATION")
)
expect_error_matching(
  put.data(
    data_manager,
    invalid_data,
    "cases",
    metric = "estimate",
    source = "integration",
    ontology.name = "by-location",
    url = "https://example.invalid/locations-integration",
    details = "locations integration contract"
  ),
  "locations in data are invalid",
  "JHEEM2 data-manager validation accepted an invalid location"
)

# JHEEM entities sanitize their location while being initialized. A minimal
# subclass supplies only the abstract code-iteration method; the inherited
# initializer remains the real JHEEM2 implementation.
jheem_namespace <- asNamespace("jheem2")
entity_probe_class <- R6::R6Class(
  "locations.integration.entity.probe",
  inherit = get("JHEEM.ENTITY", jheem_namespace),
  portable = FALSE,
  public = list(
    initialize = function(location) {
      super$initialize(
        version = "locations-integration",
        sub.version = NULL,
        location = location,
        type = "locations-integration-probe",
        error.prefix = "locations integration: "
      )
    }
  ),
  private = list(
    get.current.code.iteration = function() "locations-integration-v1"
  )
)

expect_identical(
  unname(entity_probe_class$new("md")$location),
  "MD",
  "JHEEM2 entity initialization no longer sanitizes state aliases"
)
expect_identical(
  unname(entity_probe_class$new("09001")$location),
  "09110",
  "JHEEM2 entity initialization no longer sanitizes legacy county aliases"
)

# Outcome mappings inspect location metadata to decide whether a modeled CBSA
# needs expansion. Override only stateful construction; the inherited method is
# JHEEM2's implementation and calls locations::get.location.type().
outcome_mapping_probe_class <- R6::R6Class(
  "locations.integration.outcome.mapping.probe",
  inherit = get("OUTCOME.LOCATION.MAPPING", jheem_namespace),
  portable = FALSE,
  public = list(
    initialize = function(location = "MD") {
      private$i.location <- location
      private$i.location.mappings.for.outcomes <- list()
      private$i.version <- "locations-integration"
      private$i.sub.version <- NULL
      private$i.jheem.kernel <- NULL
    }
  )
)
outcome_mapping_probe <- outcome_mapping_probe_class$new()
expect_identical(
  outcome_mapping_probe$get.observed.locations(
    "incidence",
    "C.12580",
    data.manager = NULL
  ),
  "C.12580",
  "JHEEM2 outcome-location metadata classification changed"
)
expect_identical(
  unname(locations::get.location.name("C.12580")),
  "Baltimore-Columbia-Towson, MD",
  "JHEEM2 display metadata lookup changed"
)

# The nested-proportion likelihood discovers candidate data locations by
# running containment results through get.location.code() and combining them
# with overlapping locations. Override only its model-heavy initializer and
# expose the inherited private method through a test-only public wrapper.
nested_probe_class <- R6::R6Class(
  "locations.integration.nested.proportion.probe",
  inherit = get("JHEEM.NESTED.PROPORTION.LIKELIHOOD", jheem_namespace),
  portable = FALSE,
  public = list(
    initialize = function() {},
    discover.locations = function(location, location.types) {
      private$get.all.locations(
        location = location,
        location.types = location.types,
        maximum.locations.per.type = 100,
        minimum.geographic.resolution.type = "COUNTY",
        data.manager = NULL,
        years = NULL,
        error.prefix = "locations integration: "
      )
    }
  )
)

discovered <- nested_probe_class$new()$discover.locations(
  "C.12580",
  c("STATE", "COUNTY")
)
expect_setequal(
  unname(discovered),
  c("MD", "24003", "24005", "24013", "24025", "24027", "24035", "24510"),
  paste(
    "JHEEM2 nested location discovery changed across containment, overlap,",
    "or name-to-code lookup"
  )
)
expect_identical(
  unname(discovered[names(discovered) == "STATE"]),
  "MD",
  "JHEEM2 nested location discovery lost the overlapping state"
)

cat("JHEEM2 downstream locations contract passed\n")
