temporal_api_date <- function(value, argument) {
  valid_type <- inherits(value, "Date") || is.character(value)
  if (!valid_type || length(value) != 1L || is.na(value)) {
    stop(argument, " must be one Date or YYYY-MM-DD character value")
  }
  value <- as.character(value)
  parsed <- suppressWarnings(as.Date(value, format = "%Y-%m-%d"))
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value) || is.na(parsed) ||
      format(parsed, "%Y-%m-%d") != value) {
    stop(argument, " must use a valid YYYY-MM-DD date")
  }
  value
}

temporal_api_store <- function() {
  if (!exists(".temporal_county_data", inherits = TRUE)) {
    stop("The installed package does not contain temporal county data")
  }
  .temporal_county_data
}

temporal_api_status <- function(from, to, as_of) {
  current <- temporal_county_active(from, to, as_of)
  historical <- nzchar(to) & as.Date(to, format = "%Y-%m-%d") <= as.Date(as_of)
  future <- nzchar(from) & as.Date(from, format = "%Y-%m-%d") > as.Date(as_of)
  status <- rep("current", length(from))
  status[historical] <- "historical"
  status[future] <- "future"
  status[current] <- "current"
  status
}

temporal_api_intervals_overlap <- function(
    left_from, left_to, right_from, right_to) {
  interval_bound <- function(values, missing_value) {
    result <- rep(missing_value, length(values))
    present <- nzchar(values)
    result[present] <- as.numeric(as.Date(
      values[present], format = "%Y-%m-%d"
    ))
    result
  }
  left_start <- interval_bound(left_from, -Inf)
  left_end <- interval_bound(left_to, Inf)
  right_start <- interval_bound(right_from, -Inf)
  right_end <- interval_bound(right_to, Inf)
  left_start < right_end & right_start < left_end
}

temporal_location_result <- function() {
  columns <- c(
    "location_id", "location_version_id", "location_code_id", "type",
    "entity_kind", "status", "status_as_of", "preferred_name",
    "code_system_id", "code", "version_valid_from",
    "version_valid_from_precision", "version_valid_to",
    "version_valid_to_precision", "end_reason", "code_valid_from",
    "code_valid_from_precision", "code_valid_to", "code_valid_to_precision",
    "version_source_id", "version_source_vintage", "version_source_url",
    "code_source_id", "code_source_vintage", "code_source_url"
  )
  result <- as.data.frame(
    stats::setNames(rep(list(character()), length(columns)), columns),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  result
}

temporal_location_rows <- function(data, versions, codes, status_as_of) {
  if (nrow(versions) == 0L) return(temporal_location_result())

  joined <- merge(
    versions,
    codes,
    by = "location_id",
    all.x = TRUE,
    sort = FALSE,
    suffixes = c("_version", "_code")
  )
  has_code <- !is.na(joined$location_code_id)
  overlaps <- !has_code | temporal_api_intervals_overlap(
    joined$valid_from_version, joined$valid_to_version,
    ifelse(has_code, joined$valid_from_code, ""),
    ifelse(has_code, joined$valid_to_code, "")
  )
  joined <- joined[overlaps, , drop = FALSE]
  has_code <- !is.na(joined$location_code_id)

  entities <- data$entities[match(joined$location_id, data$entities$location_id), ]
  version_sources <- data$sources[match(
    joined$source_id_version, data$sources$source_id
  ), ]
  code_sources <- data$sources[match(
    joined$source_id_code, data$sources$source_id
  ), ]
  blank_if_na <- function(values) {
    values[is.na(values)] <- ""
    values
  }

  result <- data.frame(
    location_id = joined$location_id,
    location_version_id = joined$location_version_id,
    location_code_id = blank_if_na(joined$location_code_id),
    type = joined$type,
    entity_kind = entities$entity_kind,
    status = temporal_api_status(
      joined$valid_from_version, joined$valid_to_version, status_as_of
    ),
    status_as_of = rep(status_as_of, nrow(joined)),
    preferred_name = joined$preferred_name,
    code_system_id = blank_if_na(joined$code_system_id),
    code = blank_if_na(joined$code),
    version_valid_from = joined$valid_from_version,
    version_valid_from_precision = joined$valid_from_precision_version,
    version_valid_to = joined$valid_to_version,
    version_valid_to_precision = joined$valid_to_precision_version,
    end_reason = joined$end_reason,
    code_valid_from = blank_if_na(joined$valid_from_code),
    code_valid_from_precision = blank_if_na(joined$valid_from_precision_code),
    code_valid_to = blank_if_na(joined$valid_to_code),
    code_valid_to_precision = blank_if_na(joined$valid_to_precision_code),
    version_source_id = joined$source_id_version,
    version_source_vintage = version_sources$source_vintage,
    version_source_url = version_sources$url,
    code_source_id = blank_if_na(joined$source_id_code),
    code_source_vintage = blank_if_na(code_sources$source_vintage),
    code_source_url = blank_if_na(code_sources$url),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  rownames(result) <- NULL
  result
}

temporal_order_versions <- function(versions, location_order = NULL) {
  start <- rep(-Inf, nrow(versions))
  present <- nzchar(versions$valid_from)
  start[present] <- as.numeric(as.Date(
    versions$valid_from[present], format = "%Y-%m-%d"
  ))
  location <- if (is.null(location_order)) {
    versions$location_id
  } else {
    match(versions$location_id, location_order)
  }
  versions[order(location, start, versions$location_version_id), , drop = FALSE]
}

#' Default Temporal Location Date
#'
#' Return the pinned reference date used by the bundled temporal location
#' dataset. The value changes only with an explicit data release; it is not the
#' current system date.
#'
#' @return One [Date] value.
#' @family temporal location API
#' @export
locations_default_date <- function() {
  data <- temporal_api_store()
  as.Date(data$metadata$default_reference_date[[1L]], format = "%Y-%m-%d")
}

#' Query Temporal Locations
#'
#' Return location versions of one type classified relative to an explicit
#' date. This is an additive temporal API and does not use the mutable legacy
#' location manager.
#'
#' @param type One location type. Matching is case-insensitive. The first
#'   temporal slice supports `"COUNTY"`.
#' @param as_of One [Date] or `YYYY-MM-DD` character value.
#' @param status One of `"current"`, `"historical"`, `"future"`, or `"all"`.
#'
#' @return A data frame with one row per overlapping location-version and code
#'   assignment. Durable IDs, version IDs, code and version validity intervals,
#'   status reference date, and source metadata are always explicit.
#' @section Stable result columns:
#' `location_id`, `location_version_id`, `location_code_id`, `type`,
#' `entity_kind`, `status`, `status_as_of`, `preferred_name`, `code_system_id`,
#' `code`, `version_valid_from`, `version_valid_from_precision`,
#' `version_valid_to`, `version_valid_to_precision`, `end_reason`,
#' `code_valid_from`, `code_valid_from_precision`, `code_valid_to`,
#' `code_valid_to_precision`, `version_source_id`, `version_source_vintage`,
#' `version_source_url`, `code_source_id`, `code_source_vintage`, and
#' `code_source_url`. Validity intervals are half-open: the start is included
#' and the end is excluded. Blank bounds mean the source does not identify that
#' bound, not that the package inferred a date.
#' @family temporal location API
#' @export
get_locations <- function(
    type, as_of = locations_default_date(), status = "current") {
  if (!is.character(type) || length(type) != 1L || is.na(type) ||
      !nzchar(type)) {
    stop("type must be one non-blank character value")
  }
  type <- toupper(type)
  if (!is.character(status) || length(status) != 1L || is.na(status)) {
    stop("status must be one character value")
  }
  status <- tolower(status)
  allowed_status <- c("current", "historical", "future", "all")
  if (!status %in% allowed_status) {
    stop("status must be current, historical, future, or all")
  }
  as_of <- temporal_api_date(as_of, "as_of")
  data <- temporal_api_store()
  available_types <- unique(data$versions$type)
  if (!type %in% available_types) {
    stop(
      "type is not available in the temporal store; available types: ",
      paste(sort(available_types), collapse = ", ")
    )
  }

  versions <- data$versions[data$versions$type == type, , drop = FALSE]
  version_status <- temporal_api_status(
    versions$valid_from, versions$valid_to, as_of
  )
  if (status != "all") {
    versions <- versions[version_status == status, , drop = FALSE]
  }
  versions <- temporal_order_versions(versions)
  temporal_location_rows(data, versions, data$codes, as_of)
}

#' Resolve a Temporal Location Code
#'
#' Resolve official code-history rows without using legacy aliases or silently
#' canonicalizing a historic code to a current geography. With no `as_of`
#' value, all matching history is returned. With `as_of`, both the code and
#' location version must be valid on that date.
#'
#' @param code One or more official location codes.
#' @param code_system Optional code-system ID, such as
#'   `"census_county_geoid"`.
#' @param as_of Optional one [Date] or `YYYY-MM-DD` character value.
#'
#' @return A data frame with the same stable columns as [get_locations()]. An
#'   unmatched code returns a zero-row data frame with those columns. When
#'   `as_of` is omitted, `status` is classified relative to
#'   [locations_default_date()] while all matching history remains present.
#' @section Code resolution:
#' Matching uses the normalized official code table only. It does not consult
#' the legacy manager's compatibility aliases. More than one row is expected
#' when a code was reused by different entities or spanned multiple versions.
#' @family temporal location API
#' @export
resolve_location <- function(code, code_system = NULL, as_of = NULL) {
  if (!is.character(code) || length(code) == 0L || anyNA(code) ||
      any(!nzchar(code))) {
    stop("code must contain non-blank character values")
  }
  if (!is.null(code_system) &&
      (!is.character(code_system) || length(code_system) != 1L ||
        is.na(code_system) || !nzchar(code_system))) {
    stop("code_system must be NULL or one non-blank character value")
  }
  data <- temporal_api_store()
  if (!is.null(code_system) &&
      !code_system %in% data$code_systems$code_system_id) {
    stop("Unknown temporal code system: ", code_system)
  }

  input_codes <- unique(code)
  codes <- data$codes[data$codes$code %in% input_codes, , drop = FALSE]
  if (!is.null(code_system)) {
    codes <- codes[codes$code_system_id == code_system, , drop = FALSE]
  }
  status_as_of <- if (is.null(as_of)) {
    as.character(locations_default_date())
  } else {
    temporal_api_date(as_of, "as_of")
  }
  if (!is.null(as_of) && nrow(codes) > 0L) {
    codes <- codes[temporal_county_active(
      codes$valid_from, codes$valid_to, status_as_of
    ), , drop = FALSE]
  }
  if (nrow(codes) == 0L) return(temporal_location_result())

  versions <- data$versions[
    data$versions$location_id %in% codes$location_id,
    , drop = FALSE
  ]
  if (!is.null(as_of)) {
    versions <- versions[temporal_county_active(
      versions$valid_from, versions$valid_to, status_as_of
    ), , drop = FALSE]
  }
  versions <- temporal_order_versions(versions)
  result <- temporal_location_rows(data, versions, codes, status_as_of)
  result <- result[order(
    match(result$code, input_codes), result$version_valid_from,
    result$location_version_id
  ), , drop = FALSE]
  rownames(result) <- NULL
  result
}

#' Inspect a Location's Temporal History
#'
#' Return every version and overlapping code assignment for one or more durable
#' location IDs. Historic codes remain historic records; succession and
#' crosswalk edges are not traversed implicitly.
#'
#' @param location_id One or more durable IDs returned by [get_locations()] or
#'   [resolve_location()].
#'
#' @return A data frame with the same stable columns as [get_locations()],
#'   ordered chronologically within each requested durable ID. Status is
#'   classified relative to [locations_default_date()].
#' @family temporal location API
#' @export
get_location_history <- function(location_id) {
  if (!is.character(location_id) || length(location_id) == 0L ||
      anyNA(location_id) || any(!nzchar(location_id))) {
    stop("location_id must contain non-blank character values")
  }
  data <- temporal_api_store()
  input_ids <- unique(location_id)
  unknown <- setdiff(input_ids, data$entities$location_id)
  if (length(unknown) > 0L) {
    stop("Unknown temporal location_id: ", paste(unknown, collapse = ", "))
  }
  versions <- data$versions[
    data$versions$location_id %in% input_ids,
    , drop = FALSE
  ]
  versions <- temporal_order_versions(versions, input_ids)
  codes <- data$codes[data$codes$location_id %in% input_ids, , drop = FALSE]
  result <- temporal_location_rows(
    data, versions, codes, as.character(locations_default_date())
  )
  result <- result[order(
    match(result$location_id, input_ids), result$version_valid_from,
    result$location_version_id, result$code_valid_from
  ), , drop = FALSE]
  rownames(result) <- NULL
  result
}

temporal_crosswalk_result <- function() {
  character_columns <- c(
    "crosswalk_id", "from_location_id", "from_location_version_id",
    "from_code", "from_name", "to_location_id", "to_location_version_id",
    "to_code", "to_name", "to_as_of", "relation_kind", "coverage",
    "measure_type", "reference_date", "population_universe", "method",
    "edge_source_id", "edge_source_url", "measure_source_id",
    "measure_source_url"
  )
  numeric_columns <- c(
    "numerator", "denominator", "fraction_of_from", "fraction_of_to"
  )
  result <- c(
    stats::setNames(rep(list(character()), length(character_columns)),
                    character_columns),
    stats::setNames(rep(list(numeric()), length(numeric_columns)),
                    numeric_columns)
  )
  result <- result[c(
    character_columns[1:13], numeric_columns,
    character_columns[14:length(character_columns)]
  )]
  as.data.frame(result, stringsAsFactors = FALSE, check.names = FALSE)
}

temporal_crosswalk_from_version <- function(data, from) {
  edge_versions <- unique(data$crosswalk_edges$from_version_id)
  if (from %in% data$versions$location_version_id) {
    return(intersect(from, edge_versions))
  }
  if (from %in% data$entities$location_id) {
    candidates <- data$versions$location_version_id[
      data$versions$location_id == from
    ]
  } else {
    resolved <- resolve_location(from)
    candidates <- unique(resolved$location_version_id)
  }
  candidates <- intersect(candidates, edge_versions)
  if (length(candidates) > 1L) {
    stop(
      "from identifies multiple crosswalk source versions; pass an explicit ",
      "location_version_id"
    )
  }
  candidates
}

temporal_crosswalk_endpoint_codes <- function(data, version_ids) {
  vapply(version_ids, function(version_id) {
    version <- data$versions[
      data$versions$location_version_id == version_id,
      , drop = FALSE
    ]
    codes <- data$codes[
      data$codes$location_id == version$location_id &
        data$codes$code_system_id == "census_county_geoid",
      , drop = FALSE
    ]
    codes <- codes[temporal_api_intervals_overlap(
      rep(version$valid_from, nrow(codes)), rep(version$valid_to, nrow(codes)),
      codes$valid_from, codes$valid_to
    ), , drop = FALSE]
    if (nrow(codes) != 1L) {
      stop(
        "Crosswalk endpoint ", version_id,
        " does not have exactly one overlapping Census county GEOID"
      )
    }
    codes$code[[1L]]
  }, character(1))
}

#' Crosswalk Temporal Locations
#'
#' Return explicit, directional crosswalk edges from one historic location to
#' target versions valid at a requested date. A code, durable location ID, or
#' location-version ID may identify `from`, but it must resolve to at most one
#' source version with crosswalk edges.
#'
#' @param from One official code, durable `location_id`, or explicit
#'   `location_version_id`.
#' @param to_as_of One target [Date] or `YYYY-MM-DD` character value.
#' @param measure One of `"none"`, `"land_area"`, `"water_area"`, or
#'   `"population"`. Unavailable measures produce an error; area is never
#'   substituted for population.
#'
#' @return A data frame with one row per target edge. `fraction_of_from` is the
#'   directional allocation fraction for the requested measure;
#'   `fraction_of_to` describes the fraction of the target footprint represented
#'   by the same overlap. With `measure = "none"`, measurement columns are `NA`
#'   or blank. No succession or reverse crosswalk is inferred.
#' @section Stable result columns:
#' `crosswalk_id`, `from_location_id`, `from_location_version_id`, `from_code`,
#' `from_name`, `to_location_id`, `to_location_version_id`, `to_code`,
#' `to_name`, `to_as_of`, `relation_kind`, `coverage`, `measure_type`,
#' `numerator`, `denominator`, `fraction_of_from`, `fraction_of_to`,
#' `reference_date`, `population_universe`, `method`, `edge_source_id`,
#' `edge_source_url`, `measure_source_id`, and `measure_source_url`.
#' `denominator` is the source-footprint total for the requested measure.
#' @family temporal location API
#' @export
crosswalk_locations <- function(
    from, to_as_of = locations_default_date(), measure = "none") {
  if (!is.character(from) || length(from) != 1L || is.na(from) ||
      !nzchar(from)) {
    stop("from must be one non-blank code or temporal ID")
  }
  if (!is.character(measure) || length(measure) != 1L || is.na(measure)) {
    stop("measure must be one character value")
  }
  measure <- tolower(measure)
  allowed_measures <- c("none", "land_area", "water_area", "population")
  if (!measure %in% allowed_measures) {
    stop(
      "measure must be none, land_area, water_area, or population"
    )
  }
  to_as_of <- temporal_api_date(to_as_of, "to_as_of")
  data <- temporal_api_store()
  from_version <- temporal_crosswalk_from_version(data, from)
  if (length(from_version) == 0L) return(temporal_crosswalk_result())

  edges <- data$crosswalk_edges[
    data$crosswalk_edges$from_version_id == from_version,
    , drop = FALSE
  ]
  to_versions <- data$versions[match(
    edges$to_version_id, data$versions$location_version_id
  ), ]
  active_target <- temporal_county_active(
    to_versions$valid_from, to_versions$valid_to, to_as_of
  )
  edges <- edges[active_target, , drop = FALSE]
  to_versions <- to_versions[active_target, , drop = FALSE]
  if (nrow(edges) == 0L) return(temporal_crosswalk_result())

  if (measure == "none") {
    measures <- data.frame(
      measure_type = rep("none", nrow(edges)), numerator = NA_real_,
      denominator = NA_real_, fraction_of_from = NA_real_,
      fraction_of_to = NA_real_, reference_date = "",
      population_universe = "", method = "", source_id = "",
      stringsAsFactors = FALSE
    )
  } else {
    candidates <- data$crosswalk_measures[
      data$crosswalk_measures$measure_type == measure,
      , drop = FALSE
    ]
    measure_rows <- match(edges$crosswalk_id, candidates$crosswalk_id)
    if (anyNA(measure_rows)) {
      available <- sort(unique(data$crosswalk_measures$measure_type[
        data$crosswalk_measures$crosswalk_id %in% edges$crosswalk_id
      ]))
      stop(
        "Crosswalk measure '", measure, "' is unavailable for this source; ",
        "available measures: ",
        if (length(available) == 0L) "none" else paste(available, collapse = ", ")
      )
    }
    measures <- candidates[measure_rows, , drop = FALSE]
  }

  from_versions <- data$versions[match(
    edges$from_version_id, data$versions$location_version_id
  ), ]
  edge_sources <- data$sources[match(edges$source_id, data$sources$source_id), ]
  measure_sources <- data$sources[match(
    measures$source_id, data$sources$source_id
  ), ]
  blank_if_na <- function(values) {
    values[is.na(values)] <- ""
    values
  }
  result <- data.frame(
    crosswalk_id = edges$crosswalk_id,
    from_location_id = from_versions$location_id,
    from_location_version_id = edges$from_version_id,
    from_code = temporal_crosswalk_endpoint_codes(
      data, edges$from_version_id
    ),
    from_name = from_versions$preferred_name,
    to_location_id = to_versions$location_id,
    to_location_version_id = edges$to_version_id,
    to_code = temporal_crosswalk_endpoint_codes(data, edges$to_version_id),
    to_name = to_versions$preferred_name,
    to_as_of = rep(to_as_of, nrow(edges)),
    relation_kind = edges$relation_kind,
    coverage = edges$coverage,
    measure_type = measures$measure_type,
    numerator = measures$numerator,
    denominator = measures$denominator,
    fraction_of_from = measures$fraction_of_from,
    fraction_of_to = measures$fraction_of_to,
    reference_date = measures$reference_date,
    population_universe = measures$population_universe,
    method = measures$method,
    edge_source_id = edges$source_id,
    edge_source_url = edge_sources$url,
    measure_source_id = blank_if_na(measures$source_id),
    measure_source_url = blank_if_na(measure_sources$url),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  result <- result[order(result$to_code, result$crosswalk_id), , drop = FALSE]
  rownames(result) <- NULL
  result
}
