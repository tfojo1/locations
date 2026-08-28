temporal_location_schema <- function() {
  character_columns <- function(...) {
    stats::setNames(rep("character", length(list(...))), c(...))
  }

  list(
    metadata = character_columns(
      "data_version", "default_reference_date"
    ),
    sources = character_columns(
      "source_id", "publisher", "title", "source_vintage",
      "reference_date", "retrieved_at", "url", "license_status", "checksum"
    ),
    code_systems = character_columns(
      "code_system_id", "publisher", "geography_level", "description",
      "source_id"
    ),
    entities = character_columns(
      "location_id", "entity_kind", "created_from_source_id"
    ),
    versions = character_columns(
      "location_version_id", "location_id", "type", "preferred_name",
      "valid_from", "valid_from_precision", "valid_to", "valid_to_precision",
      "end_reason", "source_id"
    ),
    codes = character_columns(
      "location_code_id", "location_id", "code_system_id", "code",
      "valid_from", "valid_from_precision", "valid_to", "valid_to_precision",
      "source_id"
    ),
    names = character_columns(
      "location_name_id", "location_id", "name", "name_kind",
      "valid_from", "valid_from_precision", "valid_to", "valid_to_precision",
      "source_id"
    ),
    aliases = character_columns(
      "alias_id", "alias", "alias_kind", "location_id",
      "valid_from", "valid_from_precision", "valid_to", "valid_to_precision",
      "source_id", "equivalence_evidence"
    ),
    relationships = character_columns(
      "relationship_id", "child_version_id", "parent_version_id",
      "relation_kind", "valid_from", "valid_from_precision", "valid_to",
      "valid_to_precision", "source_id"
    ),
    successions = character_columns(
      "succession_id", "from_location_id", "to_location_id",
      "succession_kind", "effective_date", "source_id"
    ),
    crosswalk_edges = character_columns(
      "crosswalk_id", "from_version_id", "to_version_id", "relation_kind",
      "coverage", "source_id"
    ),
    crosswalk_measures = c(
      character_columns(
        "crosswalk_measure_id", "crosswalk_id", "measure_type",
        "reference_date", "population_universe", "method", "source_id"
      ),
      stats::setNames(
        rep("numeric", 4L),
        c("numerator", "denominator", "fraction_of_from", "fraction_of_to")
      )
    ),
    geometries = character_columns(
      "geometry_id", "location_version_id", "geometry_format",
      "geometry_ref", "reference_date", "source_id"
    )
  )
}

new_temporal_location_data <- function() {
  lapply(temporal_location_schema(), function(specification) {
    columns <- lapply(unname(specification), function(type) {
      if (identical(type, "numeric")) numeric() else character()
    })
    names(columns) <- names(specification)
    as.data.frame(columns, stringsAsFactors = FALSE, check.names = FALSE)
  })
}

stop_temporal_location_validation <- function(errors) {
  stop(
    paste(
      "Temporal location data validation failed:",
      paste0("- ", unique(errors), collapse = "\n"),
      sep = "\n"
    ),
    call. = FALSE
  )
}

validate_temporal_location_data <- function(data, tolerance = 1e-8) {
  errors <- character()
  add_error <- function(message) errors <<- c(errors, message)
  schema <- temporal_location_schema()

  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      is.na(tolerance) || !is.finite(tolerance) || tolerance < 0) {
    stop_temporal_location_validation(
      "tolerance must be one finite, non-negative number"
    )
  }

  if (!is.list(data)) {
    stop_temporal_location_validation("data must be a named list")
  }
  if (is.null(names(data)) || any(!nzchar(names(data))) ||
      anyDuplicated(names(data))) {
    add_error("data tables must have unique, non-blank names")
  }

  missing_tables <- setdiff(names(schema), names(data))
  if (length(missing_tables) > 0L) {
    add_error(paste("Missing tables:", paste(missing_tables, collapse = ", ")))
  }
  unexpected_tables <- setdiff(names(data), names(schema))
  if (length(unexpected_tables) > 0L) {
    add_error(paste(
      "Unexpected tables:", paste(unexpected_tables, collapse = ", ")
    ))
  }

  for (table_name in intersect(names(schema), names(data))) {
    table <- data[[table_name]]
    if (!is.data.frame(table)) {
      add_error(paste0(table_name, " must be a data.frame"))
      next
    }
    if (is.null(names(table)) || any(!nzchar(names(table))) ||
        anyDuplicated(names(table))) {
      add_error(paste0(
        table_name, " columns must have unique, non-blank names"
      ))
      next
    }

    missing_columns <- setdiff(names(schema[[table_name]]), names(table))
    if (length(missing_columns) > 0L) {
      add_error(paste0(
        table_name, " is missing columns: ",
        paste(missing_columns, collapse = ", ")
      ))
      next
    }
    unexpected_columns <- setdiff(names(table), names(schema[[table_name]]))
    if (length(unexpected_columns) > 0L) {
      add_error(paste0(
        table_name, " has unexpected columns: ",
        paste(unexpected_columns, collapse = ", ")
      ))
    }

    for (column_name in names(schema[[table_name]])) {
      expected_type <- schema[[table_name]][[column_name]]
      column <- table[[column_name]]
      valid_type <- if (identical(expected_type, "numeric")) {
        is.numeric(column)
      } else {
        is.character(column)
      }
      if (!valid_type) {
        add_error(paste0(
          table_name, "$", column_name, " must be ", expected_type
        ))
      }
    }
  }

  if (length(errors) > 0L) stop_temporal_location_validation(errors)

  blank <- function(x) is.na(x) | !nzchar(x)
  require_values <- function(table_name, columns) {
    table <- data[[table_name]]
    for (column_name in columns) {
      if (any(blank(table[[column_name]]))) {
        add_error(paste0(table_name, "$", column_name, " must not be blank"))
      }
    }
  }

  primary_keys <- c(
    sources = "source_id",
    code_systems = "code_system_id",
    entities = "location_id",
    versions = "location_version_id",
    codes = "location_code_id",
    names = "location_name_id",
    aliases = "alias_id",
    relationships = "relationship_id",
    successions = "succession_id",
    crosswalk_edges = "crosswalk_id",
    crosswalk_measures = "crosswalk_measure_id",
    geometries = "geometry_id"
  )

  for (table_name in names(primary_keys)) {
    key <- primary_keys[[table_name]]
    require_values(table_name, key)
    if (anyDuplicated(data[[table_name]][[key]])) {
      add_error(paste0(table_name, "$", key, " must be unique"))
    }
  }

  if (nrow(data$metadata) != 1L) {
    add_error("metadata must contain exactly one row")
  } else {
    require_values("metadata", c("data_version", "default_reference_date"))
  }

  require_values(
    "sources",
    c(
      "publisher", "title", "source_vintage", "reference_date",
      "retrieved_at", "url", "license_status", "checksum"
    )
  )
  require_values(
    "code_systems",
    c("publisher", "geography_level", "description", "source_id")
  )
  require_values("entities", c("entity_kind", "created_from_source_id"))
  require_values(
    "versions",
    c("location_id", "type", "preferred_name", "source_id")
  )
  require_values(
    "codes",
    c("location_id", "code_system_id", "code", "source_id")
  )
  require_values("names", c("location_id", "name", "name_kind", "source_id"))
  require_values(
    "aliases",
    c(
      "alias", "alias_kind", "location_id", "source_id",
      "equivalence_evidence"
    )
  )
  require_values(
    "relationships",
    c("child_version_id", "parent_version_id", "relation_kind", "source_id")
  )
  require_values(
    "successions",
    c(
      "from_location_id", "to_location_id", "succession_kind",
      "effective_date", "source_id"
    )
  )
  require_values(
    "crosswalk_edges",
    c(
      "from_version_id", "to_version_id", "relation_kind", "coverage",
      "source_id"
    )
  )
  require_values(
    "crosswalk_measures",
    c("crosswalk_id", "measure_type", "reference_date", "method", "source_id")
  )
  require_values(
    "geometries",
    c(
      "location_version_id", "geometry_format", "geometry_ref",
      "reference_date", "source_id"
    )
  )

  parse_dates <- function(table_name, columns, required = FALSE) {
    table <- data[[table_name]]
    for (column_name in columns) {
      values <- table[[column_name]]
      present <- !blank(values)
      if (required && any(!present)) {
        add_error(paste0(table_name, "$", column_name, " must not be blank"))
      }
      invalid <- present & (
        !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", values) |
          is.na(suppressWarnings(as.Date(values, format = "%Y-%m-%d")))
      )
      if (any(invalid)) {
        add_error(paste0(table_name, "$", column_name, " must use YYYY-MM-DD"))
      }
    }
  }

  parse_dates("metadata", "default_reference_date", required = TRUE)
  parse_dates("sources", c("reference_date", "retrieved_at"), required = TRUE)
  parse_dates("successions", "effective_date", required = TRUE)
  parse_dates("crosswalk_measures", "reference_date", required = TRUE)
  parse_dates("geometries", "reference_date", required = TRUE)

  interval_tables <- c("versions", "codes", "names", "aliases", "relationships")
  allowed_precision <- c("day", "year", "unknown")
  for (table_name in interval_tables) {
    table <- data[[table_name]]
    parse_dates(table_name, c("valid_from", "valid_to"))

    for (bound in c("valid_from", "valid_to")) {
      precision_column <- paste0(bound, "_precision")
      precision <- table[[precision_column]]
      if (any(!precision %in% allowed_precision)) {
        add_error(paste0(
          table_name, "$", precision_column,
          " must be day, year, or unknown"
        ))
      }
      has_date <- !blank(table[[bound]])
      if (any(has_date & precision == "unknown") ||
          any(!has_date & precision != "unknown")) {
        add_error(paste0(
          table_name, "$", bound,
          " and its precision must agree"
        ))
      }
    }

    known_interval <- !blank(table$valid_from) & !blank(table$valid_to)
    if (any(
      as.Date(table$valid_from[known_interval], format = "%Y-%m-%d") >=
        as.Date(table$valid_to[known_interval], format = "%Y-%m-%d")
    )) {
      add_error(paste0(table_name, " validity intervals must be non-empty"))
    }
  }

  allowed_values <- list(
    entities = list(entity_kind = c("legal", "administrative", "statistical", "program", "hybrid")),
    versions = list(
      end_reason = c(
        "", "retired", "superseded", "split", "merged", "redefined",
        "renamed", "recoded", "unknown"
      )
    ),
    names = list(name_kind = c("official", "former_official", "synonym", "display")),
    aliases = list(alias_kind = c("case", "punctuation", "spelling", "prefix", "synonym")),
    relationships = list(relation_kind = c("contains", "overlaps", "member_of")),
    successions = list(
      succession_kind = c("renamed", "recoded", "split", "merged", "replaced_by")
    ),
    crosswalk_edges = list(
      relation_kind = c("overlap", "replacement"),
      coverage = c("exhaustive", "partial")
    ),
    crosswalk_measures = list(
      measure_type = c("land_area", "water_area", "population")
    )
  )

  for (table_name in names(allowed_values)) {
    for (column_name in names(allowed_values[[table_name]])) {
      invalid <- setdiff(
        unique(data[[table_name]][[column_name]]),
        allowed_values[[table_name]][[column_name]]
      )
      if (length(invalid) > 0L) {
        add_error(paste0(
          table_name, "$", column_name, " contains invalid values: ",
          paste(invalid, collapse = ", ")
        ))
      }
    }
  }

  # Date conversion and interval arithmetic below assume the structural and
  # lexical date checks have succeeded. Stop here so malformed input receives
  # a validator diagnostic instead of an incidental base-R error.
  if (length(errors) > 0L) stop_temporal_location_validation(errors)

  require_foreign_key <- function(table_name, column_name, target_values) {
    missing <- setdiff(data[[table_name]][[column_name]], target_values)
    if (length(missing) > 0L) {
      add_error(paste0(
        table_name, "$", column_name, " references missing values: ",
        paste(missing, collapse = ", ")
      ))
    }
  }

  source_ids <- data$sources$source_id
  require_foreign_key("code_systems", "source_id", source_ids)
  require_foreign_key("entities", "created_from_source_id", source_ids)
  for (table_name in c(
    "versions", "codes", "names", "aliases", "relationships", "successions",
    "crosswalk_edges", "crosswalk_measures", "geometries"
  )) {
    require_foreign_key(table_name, "source_id", source_ids)
  }

  entity_ids <- data$entities$location_id
  require_foreign_key("versions", "location_id", entity_ids)
  require_foreign_key("codes", "location_id", entity_ids)
  require_foreign_key("names", "location_id", entity_ids)
  require_foreign_key("aliases", "location_id", entity_ids)
  require_foreign_key("successions", "from_location_id", entity_ids)
  require_foreign_key("successions", "to_location_id", entity_ids)
  require_foreign_key(
    "codes", "code_system_id", data$code_systems$code_system_id
  )

  version_ids <- data$versions$location_version_id
  require_foreign_key("relationships", "child_version_id", version_ids)
  require_foreign_key("relationships", "parent_version_id", version_ids)
  require_foreign_key("crosswalk_edges", "from_version_id", version_ids)
  require_foreign_key("crosswalk_edges", "to_version_id", version_ids)
  require_foreign_key("geometries", "location_version_id", version_ids)
  require_foreign_key(
    "crosswalk_measures", "crosswalk_id", data$crosswalk_edges$crosswalk_id
  )

  if (any(
    data$relationships$child_version_id == data$relationships$parent_version_id
  )) {
    add_error("relationships may not reference the same version at both endpoints")
  }
  if (any(data$successions$from_location_id == data$successions$to_location_id)) {
    add_error("successions may not reference the same entity at both endpoints")
  }
  if (any(
    data$crosswalk_edges$from_version_id == data$crosswalk_edges$to_version_id
  )) {
    add_error("crosswalk edges may not reference the same version at both endpoints")
  }

  interval_bounds <- function(from, to) {
    start <- rep(-Inf, length(from))
    end <- rep(Inf, length(to))
    has_from <- !blank(from)
    has_to <- !blank(to)
    start[has_from] <- as.numeric(as.Date(
      from[has_from], format = "%Y-%m-%d"
    ))
    end[has_to] <- as.numeric(as.Date(
      to[has_to], format = "%Y-%m-%d"
    ))
    list(start = start, end = end)
  }
  intervals_overlap <- function(bounds, left, right) {
    max(bounds$start[c(left, right)]) < min(bounds$end[c(left, right)])
  }
  reject_overlaps <- function(table, group, label) {
    bounds <- interval_bounds(table$valid_from, table$valid_to)
    for (indices in split(seq_len(nrow(table)), group)) {
      if (length(indices) < 2L) next
      pairs <- utils::combn(indices, 2L)
      if (any(apply(pairs, 2L, function(pair) {
        intervals_overlap(bounds, pair[[1L]], pair[[2L]])
      }))) {
        add_error(paste0(label, " contains overlapping validity intervals"))
      }
    }
  }

  reject_overlaps(data$versions, data$versions$location_id, "versions")
  reject_overlaps(
    data$codes,
    paste(data$codes$code_system_id, data$codes$code, sep = "\r"),
    "code assignments"
  )
  reject_overlaps(
    data$relationships,
    paste(
      data$relationships$child_version_id,
      data$relationships$parent_version_id,
      data$relationships$relation_kind,
      sep = "\r"
    ),
    "relationships"
  )

  interval_is_covered <- function(record_start, record_end, version_rows) {
    if (length(version_rows) == 0L) return(FALSE)
    version_bounds <- interval_bounds(
      data$versions$valid_from[version_rows],
      data$versions$valid_to[version_rows]
    )
    order_index <- order(version_bounds$start, version_bounds$end)
    coverage_end <- record_start
    for (index in order_index) {
      if (version_bounds$end[[index]] <= coverage_end) next
      if (version_bounds$start[[index]] > coverage_end) return(FALSE)
      coverage_end <- max(coverage_end, version_bounds$end[[index]])
      if (coverage_end >= record_end) return(TRUE)
    }
    FALSE
  }
  require_version_coverage <- function(table_name) {
    table <- data[[table_name]]
    if (nrow(table) == 0L) return(invisible(NULL))
    record_bounds <- interval_bounds(table$valid_from, table$valid_to)
    covered <- vapply(seq_len(nrow(table)), function(index) {
      version_rows <- which(
        data$versions$location_id == table$location_id[[index]]
      )
      interval_is_covered(
        record_bounds$start[[index]], record_bounds$end[[index]], version_rows
      )
    }, logical(1))
    if (any(!covered)) {
      add_error(paste0(
        table_name, " validity must be covered by entity versions"
      ))
    }
  }
  for (table_name in c("codes", "names", "aliases")) {
    require_version_coverage(table_name)
  }

  if (nrow(data$relationships) > 0L) {
    version_rows <- match(
      c(
        data$relationships$child_version_id,
        data$relationships$parent_version_id
      ),
      data$versions$location_version_id
    )
    endpoint_bounds <- interval_bounds(
      data$versions$valid_from[version_rows],
      data$versions$valid_to[version_rows]
    )
    relationship_bounds <- interval_bounds(
      data$relationships$valid_from,
      data$relationships$valid_to
    )
    number_of_relationships <- nrow(data$relationships)
    child_rows <- seq_len(number_of_relationships)
    parent_rows <- number_of_relationships + child_rows
    compatible <-
      relationship_bounds$start >= pmax(
        endpoint_bounds$start[child_rows],
        endpoint_bounds$start[parent_rows]
      ) &
      relationship_bounds$end <= pmin(
        endpoint_bounds$end[child_rows],
        endpoint_bounds$end[parent_rows]
      )
    if (any(!compatible)) {
      add_error(
        "relationship validity must fall within both endpoint versions"
      )
    }
  }

  if (nrow(data$aliases) > 0L &&
      any(data$aliases$alias %in% data$codes$code)) {
    add_error("aliases may not duplicate official or package codes")
  }

  measures <- data$crosswalk_measures
  if (nrow(measures) > 0L) {
    numeric_values <- unlist(
      measures[, c(
        "numerator", "denominator", "fraction_of_from", "fraction_of_to"
      )],
      use.names = FALSE
    )
    if (any(!is.na(numeric_values) & !is.finite(numeric_values))) {
      add_error("crosswalk numeric values must be finite or NA")
    }

    fractions <- c(measures$fraction_of_from, measures$fraction_of_to)
    if (any(!is.na(fractions) & (fractions < 0 | fractions > 1))) {
      add_error("crosswalk fractions must be between zero and one")
    }
    if (any(!is.na(measures$numerator) & measures$numerator < 0) ||
        any(!is.na(measures$denominator) & measures$denominator <= 0)) {
      add_error("crosswalk numerators must be non-negative and denominators positive")
    }
    if (anyDuplicated(paste(measures$crosswalk_id, measures$measure_type))) {
      add_error("crosswalk edges may have at most one row per measure type")
    }

    numerator_present <- !is.na(measures$numerator)
    denominator_present <- !is.na(measures$denominator)
    if (any(xor(numerator_present, denominator_present))) {
      add_error("crosswalk numerator and denominator must be supplied together")
    }

    population <- measures$measure_type == "population"
    if (any(population & blank(measures$population_universe))) {
      add_error("population crosswalk measures require a population universe")
    }

    measured_edges <- merge(
      measures,
      data$crosswalk_edges[, c("crosswalk_id", "from_version_id", "coverage")],
      by = "crosswalk_id",
      all.x = TRUE,
      sort = FALSE
    )
    exhaustive <- measured_edges$coverage == "exhaustive" &
      !is.na(measured_edges$fraction_of_from)
    groups <- split(
      measured_edges[exhaustive, , drop = FALSE],
      paste(
        measured_edges$from_version_id[exhaustive],
        measured_edges$measure_type[exhaustive],
        sep = "\r"
      )
    )
    bad_sums <- vapply(groups, function(group) {
      abs(sum(group$fraction_of_from) - 1) > tolerance
    }, logical(1))
    if (any(bad_sums)) {
      add_error("exhaustive fraction_of_from values must sum to one")
    }
  }

  if (length(errors) > 0L) stop_temporal_location_validation(errors)
  invisible(TRUE)
}
