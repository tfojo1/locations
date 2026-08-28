read_temporal_county_input <- function(path, required_columns, separator = ",") {
  if (!file.exists(path)) stop("Temporal county input is missing: ", path)

  data <- utils::read.table(
    path,
    header = TRUE,
    sep = separator,
    quote = '"',
    comment.char = "",
    colClasses = "character",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      basename(path), " is missing columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  data
}

append_temporal_county_row <- function(data, table_name, values) {
  row <- data[[table_name]][0, , drop = FALSE]
  for (column_name in names(row)) {
    row[1L, column_name] <- if (is.numeric(row[[column_name]])) {
      NA_real_
    } else {
      ""
    }
  }
  for (column_name in names(values)) {
    row[1L, column_name] <- values[[column_name]]
  }
  data[[table_name]] <- rbind(data[[table_name]], row)
  data
}

read_temporal_county_sources <- function(path, raw_directory) {
  required <- c(
    "source_id", "publisher", "title", "source_vintage",
    "reference_date", "retrieved_at", "url", "license_status",
    "checksum", "local_file"
  )
  sources <- read_temporal_county_input(path, required)
  if (anyDuplicated(sources$source_id)) {
    stop("Temporal county source IDs must be unique")
  }

  local_paths <- file.path(raw_directory, sources$local_file)
  if (any(!file.exists(local_paths))) {
    stop(
      "Temporal county source snapshots are missing: ",
      paste(sources$local_file[!file.exists(local_paths)], collapse = ", ")
    )
  }
  if (any(!grepl("^md5:[0-9a-f]{32}$", sources$checksum))) {
    stop("Temporal county source checksums must use md5:<32 hex digits>")
  }
  expected <- sub("^md5:", "", sources$checksum)
  actual <- unname(tools::md5sum(local_paths))
  changed <- sources$local_file[tolower(expected) != tolower(actual)]
  if (length(changed) > 0L) {
    stop(
      "Temporal county source snapshots changed: ",
      paste(changed, collapse = ", ")
    )
  }
  sources
}

read_temporal_county_current <- function(path) {
  required <- c(
    "USPS", "GEOID", "GEOIDFQ", "ANSICODE", "NAME", "ALAND",
    "AWATER", "ALAND_SQMI", "AWATER_SQMI", "INTPTLAT", "INTPTLONG"
  )
  current <- read_temporal_county_input(path, required, separator = "|")
  if (anyDuplicated(current$GEOID) || anyDuplicated(current$ANSICODE)) {
    stop("Current Census county GEOIDs and ANSI codes must be unique")
  }
  if (any(!grepl("^[0-9]{5}$", current$GEOID))) {
    stop("Current county GEOIDs must contain exactly five digits")
  }

  expected_counts <- c(AK = 30L, CT = 9L, MT = 56L)
  actual_counts <- table(current$USPS)[names(expected_counts)]
  if (any(is.na(actual_counts)) ||
      !identical(unname(as.integer(actual_counts)), unname(expected_counts))) {
    stop(
      "Pinned current county counts changed for AK, CT, or MT; ",
      "review the authoritative source before updating"
    )
  }
  current[order(current$GEOID), , drop = FALSE]
}

read_temporal_county_ct_crosswalk <- function(path) {
  required <- c(
    "GEOID_COUSUB_22", "AREALAND_COUSUB_22", "AREAWATER_COUSUB_22",
    "GEOID_BLKGRP_20", "AREALAND_BLKGRP_20", "AREAWATER_BLKGRP_20",
    "AREALAND_PART", "AREAWATER_PART"
  )
  relationship <- read_temporal_county_input(
    path, required, separator = "|"
  )

  if (any(!grepl("^09[0-9]{8}$", relationship$GEOID_COUSUB_22)) ||
      any(!grepl("^09[0-9]{10}$", relationship$GEOID_BLKGRP_20))) {
    stop("Connecticut relationship GEOIDs have an unexpected format")
  }
  endpoint_key <- paste(
    relationship$GEOID_COUSUB_22, relationship$GEOID_BLKGRP_20, sep = "\r"
  )
  if (anyDuplicated(endpoint_key)) {
    stop("Connecticut relationship endpoints must be unique")
  }

  area_columns <- c(
    "AREALAND_COUSUB_22", "AREAWATER_COUSUB_22",
    "AREALAND_BLKGRP_20", "AREAWATER_BLKGRP_20",
    "AREALAND_PART", "AREAWATER_PART"
  )
  area <- lapply(relationship[area_columns], function(values) {
    suppressWarnings(as.numeric(values))
  })
  invalid_area <- vapply(area, function(values) {
    any(is.na(values) | !is.finite(values) | values < 0)
  }, logical(1))
  if (any(invalid_area)) {
    stop(
      "Connecticut relationship area fields must be finite, non-negative ",
      "square-meter values: ",
      paste(area_columns[invalid_area], collapse = ", ")
    )
  }

  relationship$old_geoid <- substr(relationship$GEOID_BLKGRP_20, 1L, 5L)
  relationship$new_geoid <- substr(relationship$GEOID_COUSUB_22, 1L, 5L)
  relationship$land_area <- area$AREALAND_PART
  relationship$water_area <- area$AREAWATER_PART

  expected_old <- sprintf("09%03d", seq.int(1L, 15L, by = 2L))
  expected_new <- sprintf("09%03d", seq.int(110L, 190L, by = 10L))
  if (!setequal(unique(relationship$old_geoid), expected_old) ||
      !setequal(unique(relationship$new_geoid), expected_new)) {
    stop(
      "Pinned Connecticut relationship coverage changed; review the ",
      "authoritative source before updating"
    )
  }

  verify_endpoint_totals <- function(
      geoid_column, total_column, part_values, label) {
    geoids <- relationship[[geoid_column]]
    totals <- as.numeric(relationship[[total_column]])
    distinct_total <- vapply(split(totals, geoids), function(values) {
      unique_values <- unique(values)
      if (length(unique_values) != 1L) return(NA_real_)
      unique_values[[1L]]
    }, numeric(1))
    part_total <- vapply(split(part_values, geoids), sum, numeric(1))
    if (anyNA(distinct_total) ||
        !identical(unname(part_total), unname(distinct_total))) {
      stop(
        "Connecticut relationship ", label,
        " parts do not reproduce published endpoint totals"
      )
    }
  }
  verify_endpoint_totals(
    "GEOID_COUSUB_22", "AREALAND_COUSUB_22",
    relationship$land_area, "county-subdivision land-area"
  )
  verify_endpoint_totals(
    "GEOID_COUSUB_22", "AREAWATER_COUSUB_22",
    relationship$water_area, "county-subdivision water-area"
  )
  verify_endpoint_totals(
    "GEOID_BLKGRP_20", "AREALAND_BLKGRP_20",
    relationship$land_area, "block-group land-area"
  )
  verify_endpoint_totals(
    "GEOID_BLKGRP_20", "AREAWATER_BLKGRP_20",
    relationship$water_area, "block-group water-area"
  )

  crosswalk <- stats::aggregate(
    relationship[, c("land_area", "water_area")],
    relationship[, c("old_geoid", "new_geoid")],
    sum
  )
  crosswalk <- crosswalk[
    crosswalk$land_area > 0 | crosswalk$water_area > 0,
    , drop = FALSE
  ]
  crosswalk <- crosswalk[order(
    crosswalk$old_geoid, crosswalk$new_geoid
  ), , drop = FALSE]
  rownames(crosswalk) <- NULL
  if (nrow(crosswalk) != 19L) {
    stop("Pinned Connecticut county crosswalk must contain 19 overlap edges")
  }
  crosswalk
}

temporal_county_entity_keys <- function(current, history) {
  current_keys <- paste0("ansi:", current$ANSICODE)
  history_only <- setdiff(history$entity_key, current_keys)
  c(current_keys, sort(unique(history_only)))
}

update_temporal_county_registry <- function(
    current, history, registry_path, write = FALSE) {
  required <- c("entity_key", "location_id", "initial_geoid")
  if (file.exists(registry_path)) {
    registry <- read_temporal_county_input(registry_path, required)
  } else {
    registry <- data.frame(
      entity_key = character(), location_id = character(),
      initial_geoid = character(), stringsAsFactors = FALSE
    )
  }
  if (anyDuplicated(registry$entity_key) || anyDuplicated(registry$location_id)) {
    stop("Temporal county registry keys and location IDs must be unique")
  }
  if (nrow(registry) > 0L &&
      any(!grepl("^loc_[0-9]{8}$", registry$location_id))) {
    stop("Temporal county registry IDs must use loc_<8 digits>")
  }

  keys <- temporal_county_entity_keys(current, history)
  missing_keys <- setdiff(keys, registry$entity_key)
  if (length(missing_keys) > 0L && !write) {
    stop(
      "Temporal county registry is missing entity keys: ",
      paste(utils::head(missing_keys, 10L), collapse = ", "),
      if (length(missing_keys) > 10L) " ..." else ""
    )
  }

  if (length(missing_keys) > 0L) {
    existing_numbers <- suppressWarnings(as.integer(sub(
      "^loc_", "", registry$location_id
    )))
    next_number <- if (length(existing_numbers) == 0L) {
      1L
    } else {
      max(existing_numbers) + 1L
    }
    current_key_to_geoid <- stats::setNames(
      current$GEOID, paste0("ansi:", current$ANSICODE)
    )
    history_key_to_geoid <- stats::setNames(
      history$geoid, history$entity_key
    )
    initial_geoid <- c(current_key_to_geoid, history_key_to_geoid)
    additions <- data.frame(
      entity_key = missing_keys,
      location_id = sprintf(
        "loc_%08d", seq.int(next_number, length.out = length(missing_keys))
      ),
      initial_geoid = unname(initial_geoid[missing_keys]),
      stringsAsFactors = FALSE
    )
    registry <- rbind(registry, additions)
  }

  # Registry rows are append-only. A later source may retire an entity before
  # its history input is curated; dropping that row would permit accidental ID
  # reuse and break durable identity.
  rownames(registry) <- NULL
  if (write) {
    utils::write.csv(registry, registry_path, row.names = FALSE, na = "")
  }
  registry
}

temporal_county_entity_kind <- function(name) {
  ifelse(
    grepl("Census Area$|Planning Region$", name),
    "statistical",
    "legal"
  )
}

temporal_county_active <- function(from, to, as_of) {
  as_of <- as.Date(as_of)
  starts <- !nzchar(from) |
    as.Date(from, format = "%Y-%m-%d") <= as_of
  ends <- !nzchar(to) |
    as.Date(to, format = "%Y-%m-%d") > as_of
  starts & ends
}

temporal_county_current_records <- function(data, as_of = NULL) {
  if (is.null(as_of)) as_of <- data$metadata$default_reference_date[[1L]]
  versions <- data$versions[
    temporal_county_active(
      data$versions$valid_from, data$versions$valid_to, as_of
    ),
    , drop = FALSE
  ]
  codes <- data$codes[
    temporal_county_active(data$codes$valid_from, data$codes$valid_to, as_of),
    , drop = FALSE
  ]
  merged <- merge(
    versions,
    codes[, c("location_id", "code_system_id", "code")],
    by = "location_id",
    all = FALSE,
    sort = FALSE
  )
  merged[merged$code_system_id == "census_county_geoid", , drop = FALSE]
}

resolve_temporal_county_successor <- function(data, geoid, effective_date) {
  candidates <- data$codes[
    data$codes$code_system_id == "census_county_geoid" &
      data$codes$code == geoid &
      temporal_county_active(
        data$codes$valid_from, data$codes$valid_to, effective_date
      ),
    , drop = FALSE
  ]
  if (nrow(candidates) != 1L) {
    stop(
      "Expected one successor for county GEOID ", geoid, " at ",
      effective_date, "; found ", nrow(candidates)
    )
  }
  candidates$location_id[[1L]]
}

resolve_temporal_county_version <- function(data, geoid, as_of) {
  codes <- data$codes[
    data$codes$code_system_id == "census_county_geoid" &
      data$codes$code == geoid &
      temporal_county_active(
        data$codes$valid_from, data$codes$valid_to, as_of
      ),
    , drop = FALSE
  ]
  versions <- data$versions[
    data$versions$location_id %in% codes$location_id &
      temporal_county_active(
        data$versions$valid_from, data$versions$valid_to, as_of
      ),
    , drop = FALSE
  ]
  if (nrow(versions) != 1L) {
    stop(
      "Expected one version for county GEOID ", geoid, " at ", as_of,
      "; found ", nrow(versions)
    )
  }
  versions$location_version_id[[1L]]
}

append_temporal_county_ct_crosswalk <- function(data, crosswalk) {
  reference_date <- "2022-01-01"
  source_id <- "src_ct_cousub_bg_2022"
  from_versions <- vapply(crosswalk$old_geoid, function(geoid) {
    resolve_temporal_county_version(data, geoid, "2021-12-31")
  }, character(1))
  to_versions <- vapply(crosswalk$new_geoid, function(geoid) {
    resolve_temporal_county_version(data, geoid, reference_date)
  }, character(1))

  for (index in seq_len(nrow(crosswalk))) {
    row <- crosswalk[index, ]
    crosswalk_id <- paste0(
      "cross_ct_", row$old_geoid, "_", row$new_geoid
    )
    data <- append_temporal_county_row(data, "crosswalk_edges", list(
      crosswalk_id = crosswalk_id,
      from_version_id = from_versions[[index]],
      to_version_id = to_versions[[index]],
      relation_kind = "overlap", coverage = "exhaustive",
      source_id = source_id
    ))

    for (measure_type in c("land_area", "water_area")) {
      numerator <- row[[measure_type]]
      from_denominator <- sum(
        crosswalk[[measure_type]][
          crosswalk$old_geoid == row$old_geoid
        ]
      )
      to_denominator <- sum(
        crosswalk[[measure_type]][
          crosswalk$new_geoid == row$new_geoid
        ]
      )
      method <- paste0(
        "Sum Census ",
        if (measure_type == "land_area") "AREALAND_PART" else "AREAWATER_PART",
        " square meters by 2020 county GEOID and 2022 county GEOID; ",
        "denominator is the former-county total"
      )
      data <- append_temporal_county_row(data, "crosswalk_measures", list(
        crosswalk_measure_id = paste0(
          "measure_ct_", row$old_geoid, "_", row$new_geoid, "_",
          measure_type
        ),
        crosswalk_id = crosswalk_id, measure_type = measure_type,
        numerator = numerator, denominator = from_denominator,
        fraction_of_from = numerator / from_denominator,
        fraction_of_to = numerator / to_denominator,
        reference_date = reference_date, population_universe = "",
        method = method, source_id = source_id
      ))
    }
  }
  data
}

build_temporal_county_data <- function(
    raw_directory = "data-raw",
    registry_path = file.path(raw_directory, "temporal_county_registry.csv"),
    update_registry = FALSE) {
  source_path <- file.path(raw_directory, "temporal_county_sources.csv")
  current_path <- file.path(raw_directory, "2025_Gaz_counties_national.txt")
  history_path <- file.path(raw_directory, "temporal_county_history.csv")
  override_path <- file.path(
    raw_directory, "temporal_county_current_overrides.csv"
  )
  prior_version_path <- file.path(
    raw_directory, "temporal_county_prior_versions.csv"
  )
  ct_crosswalk_path <- file.path(
    raw_directory, "acs22_cousub22_blkgrp20_st09.txt"
  )

  sources <- read_temporal_county_sources(source_path, raw_directory)
  current <- read_temporal_county_current(current_path)
  history <- read_temporal_county_input(
    history_path,
    c(
      "entity_key", "entity_kind", "preferred_name", "geoid",
      "valid_from", "valid_from_precision", "valid_to",
      "valid_to_precision", "end_reason", "source_id",
      "successor_geoids", "succession_kind"
    )
  )
  overrides <- read_temporal_county_input(
    override_path,
    c(
      "geoid", "version_valid_from", "version_valid_from_precision",
      "code_valid_from", "code_valid_from_precision", "source_id"
    )
  )
  prior_versions <- read_temporal_county_input(
    prior_version_path,
    c(
      "version_record_id", "entity_key", "preferred_name", "valid_from",
      "valid_from_precision", "valid_to", "valid_to_precision",
      "end_reason", "source_id"
    )
  )
  ct_crosswalk <- read_temporal_county_ct_crosswalk(ct_crosswalk_path)
  registry <- update_temporal_county_registry(
    current, history, registry_path, write = update_registry
  )

  if (anyDuplicated(history$entity_key) || anyDuplicated(history$geoid)) {
    stop("Temporal county history entity keys and GEOIDs must be unique")
  }
  if (anyDuplicated(overrides$geoid)) {
    stop("Temporal county current overrides must have unique GEOIDs")
  }
  if (anyDuplicated(prior_versions$version_record_id)) {
    stop("Temporal county prior-version record IDs must be unique")
  }
  if (length(setdiff(overrides$geoid, current$GEOID)) > 0L) {
    stop("Temporal county overrides reference non-current GEOIDs")
  }
  known_sources <- sources$source_id
  if (length(setdiff(
    c(history$source_id, overrides$source_id, prior_versions$source_id),
    known_sources
  ))) {
    stop("Temporal county inputs reference unknown source IDs")
  }

  key_to_id <- stats::setNames(registry$location_id, registry$entity_key)
  current$entity_key <- paste0("ansi:", current$ANSICODE)
  current$location_id <- unname(key_to_id[current$entity_key])
  history$location_id <- unname(key_to_id[history$entity_key])
  prior_versions$location_id <- unname(key_to_id[prior_versions$entity_key])
  if (any(is.na(prior_versions$location_id))) {
    stop("Temporal county prior versions reference unknown entity keys")
  }
  override_index <- match(current$GEOID, overrides$geoid)
  has_override <- !is.na(override_index)

  data <- new_temporal_location_data()
  data <- append_temporal_county_row(data, "metadata", list(
    data_version = "census-counties-2025.2",
    default_reference_date = "2025-01-01"
  ))
  for (index in seq_len(nrow(sources))) {
    source <- sources[index, ]
    data <- append_temporal_county_row(data, "sources", as.list(
      source[, names(data$sources), drop = FALSE]
    ))
  }
  data <- append_temporal_county_row(data, "code_systems", list(
    code_system_id = "census_county_geoid",
    publisher = "U.S. Census Bureau",
    geography_level = "county_or_equivalent",
    description = "Two-digit state ANSI plus three-digit county ANSI code",
    source_id = "src_gazetteer_2025"
  ))

  for (index in seq_len(nrow(current))) {
    row <- current[index, ]
    data <- append_temporal_county_row(data, "entities", list(
      location_id = row$location_id,
      entity_kind = temporal_county_entity_kind(row$NAME),
      created_from_source_id = "src_gazetteer_2025"
    ))
    override <- if (has_override[[index]]) {
      overrides[override_index[[index]], , drop = FALSE]
    } else {
      NULL
    }
    version_from <- if (is.null(override)) "" else override$version_valid_from
    version_precision <- if (is.null(override)) {
      "unknown"
    } else {
      override$version_valid_from_precision
    }
    code_from <- if (is.null(override)) "" else override$code_valid_from
    code_precision <- if (is.null(override)) {
      "unknown"
    } else {
      override$code_valid_from_precision
    }
    row_source <- if (is.null(override)) {
      "src_gazetteer_2025"
    } else {
      override$source_id
    }
    data <- append_temporal_county_row(data, "versions", list(
      location_version_id = paste0("ver_current_", row$location_id),
      location_id = row$location_id, type = "COUNTY",
      preferred_name = row$NAME, valid_from = version_from,
      valid_from_precision = version_precision, valid_to = "",
      valid_to_precision = "unknown", end_reason = "",
      source_id = row_source
    ))
    data <- append_temporal_county_row(data, "codes", list(
      location_code_id = paste0("code_current_", row$location_id),
      location_id = row$location_id,
      code_system_id = "census_county_geoid", code = row$GEOID,
      valid_from = code_from, valid_from_precision = code_precision,
      valid_to = "", valid_to_precision = "unknown", source_id = row_source
    ))
    data <- append_temporal_county_row(data, "names", list(
      location_name_id = paste0("name_current_", row$location_id),
      location_id = row$location_id, name = row$NAME,
      name_kind = "official", valid_from = version_from,
      valid_from_precision = version_precision, valid_to = "",
      valid_to_precision = "unknown", source_id = row_source
    ))
  }

  existing_entities <- data$entities$location_id
  for (index in seq_len(nrow(history))) {
    row <- history[index, ]
    if (!row$location_id %in% existing_entities) {
      data <- append_temporal_county_row(data, "entities", list(
        location_id = row$location_id, entity_kind = row$entity_kind,
        created_from_source_id = row$source_id
      ))
      existing_entities <- c(existing_entities, row$location_id)
    }
    record_suffix <- paste0(row$geoid, "_", sprintf("%02d", index))
    data <- append_temporal_county_row(data, "versions", list(
      location_version_id = paste0("ver_history_", record_suffix),
      location_id = row$location_id, type = "COUNTY",
      preferred_name = row$preferred_name, valid_from = row$valid_from,
      valid_from_precision = row$valid_from_precision,
      valid_to = row$valid_to, valid_to_precision = row$valid_to_precision,
      end_reason = row$end_reason, source_id = row$source_id
    ))
    data <- append_temporal_county_row(data, "codes", list(
      location_code_id = paste0("code_history_", record_suffix),
      location_id = row$location_id,
      code_system_id = "census_county_geoid", code = row$geoid,
      valid_from = row$valid_from,
      valid_from_precision = row$valid_from_precision,
      valid_to = row$valid_to, valid_to_precision = row$valid_to_precision,
      source_id = row$source_id
    ))
    data <- append_temporal_county_row(data, "names", list(
      location_name_id = paste0("name_history_", record_suffix),
      location_id = row$location_id, name = row$preferred_name,
      name_kind = "former_official", valid_from = row$valid_from,
      valid_from_precision = row$valid_from_precision,
      valid_to = row$valid_to, valid_to_precision = row$valid_to_precision,
      source_id = row$source_id
    ))
  }

  for (index in seq_len(nrow(prior_versions))) {
    row <- prior_versions[index, ]
    data <- append_temporal_county_row(data, "versions", list(
      location_version_id = paste0("ver_prior_", row$version_record_id),
      location_id = row$location_id, type = "COUNTY",
      preferred_name = row$preferred_name, valid_from = row$valid_from,
      valid_from_precision = row$valid_from_precision,
      valid_to = row$valid_to, valid_to_precision = row$valid_to_precision,
      end_reason = row$end_reason, source_id = row$source_id
    ))
    data <- append_temporal_county_row(data, "names", list(
      location_name_id = paste0("name_prior_", row$version_record_id),
      location_id = row$location_id, name = row$preferred_name,
      name_kind = "former_official", valid_from = row$valid_from,
      valid_from_precision = row$valid_from_precision,
      valid_to = row$valid_to, valid_to_precision = row$valid_to_precision,
      source_id = row$source_id
    ))
  }

  for (index in which(nzchar(history$successor_geoids))) {
    row <- history[index, ]
    successors <- strsplit(row$successor_geoids, ";", fixed = TRUE)[[1L]]
    for (successor_index in seq_along(successors)) {
      to_location_id <- resolve_temporal_county_successor(
        data, successors[[successor_index]], row$valid_to
      )
      data <- append_temporal_county_row(data, "successions", list(
        succession_id = paste0(
          "succ_", row$geoid, "_", successors[[successor_index]]
        ),
        from_location_id = row$location_id,
        to_location_id = to_location_id,
        succession_kind = row$succession_kind,
        effective_date = row$valid_to,
        source_id = row$source_id
      ))
    }
  }

  data <- append_temporal_county_ct_crosswalk(data, ct_crosswalk)

  validate_temporal_location_data(data)
  current_records <- temporal_county_current_records(data)
  current_records$state <- current$USPS[
    match(current_records$code, current$GEOID)
  ]
  expected_counts <- c(AK = 30L, CT = 9L, MT = 56L)
  actual_counts <- table(current_records$state)[names(expected_counts)]
  if (!identical(unname(as.integer(actual_counts)), unname(expected_counts))) {
    stop("Normalized current county counts do not match the pinned contracts")
  }

  report <- data.frame(
    category = c(
      rep("metadata", 3L), rep("records", length(data)),
      rep("current_count", 3L), rep("source", nrow(sources))
    ),
    metric = c(
      "data_version", "default_reference_date", "rejected_rows",
      names(data), names(expected_counts), sources$source_id
    ),
    value = c(
      data$metadata$data_version,
      data$metadata$default_reference_date,
      "0",
      as.character(vapply(data, nrow, integer(1))),
      as.character(unname(actual_counts)),
      paste(sources$source_vintage, sources$checksum, sep = "|")
    ),
    stringsAsFactors = FALSE
  )
  list(data = data, report = report)
}
