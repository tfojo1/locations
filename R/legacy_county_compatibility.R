legacy_county_compatibility_input <- function(path) {
  compatibility <- utils::read.csv(
    path,
    colClasses = "character",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
  required <- c("code", "append_order", "legacy_name", "reason")
  missing <- setdiff(required, names(compatibility))
  if (length(missing) > 0L) {
    stop(
      "Legacy county compatibility input is missing columns: ",
      paste(missing, collapse = ", ")
    )
  }
  compatibility <- compatibility[, required, drop = FALSE]
  if (anyNA(compatibility) || any(!nzchar(compatibility$code)) ||
      any(!nzchar(compatibility$reason))) {
    stop("Legacy county compatibility rows must be explicit and non-missing")
  }
  if (any(!grepl("^[0-9]{5}$", compatibility$code))) {
    stop("Legacy county compatibility codes must be five digits")
  }
  if (anyDuplicated(compatibility$code)) {
    stop("Legacy county compatibility codes must be unique")
  }

  append_rows <- nzchar(compatibility$append_order)
  append_order <- suppressWarnings(as.integer(
    compatibility$append_order[append_rows]
  ))
  if (anyNA(append_order) ||
      !identical(sort(append_order), seq_along(append_order))) {
    stop("Legacy county append_order must be a complete sequence from one")
  }
  compatibility$append_order[append_rows] <- as.character(append_order)
  compatibility
}

legacy_county_interval_active <- function(from, to, as_of) {
  as_of <- as.Date(as_of, format = "%Y-%m-%d")
  (!nzchar(from) | as.Date(from, format = "%Y-%m-%d") <= as_of) &
    (!nzchar(to) | as.Date(to, format = "%Y-%m-%d") > as_of)
}

legacy_county_records <- function(data) {
  code_rows <- data$codes[
    data$codes$code_system_id == "census_county_geoid",
    , drop = FALSE
  ]
  records <- merge(
    code_rows,
    data$versions,
    by = "location_id",
    all = FALSE,
    sort = FALSE,
    suffixes = c("_code", "_version")
  )

  code_start <- ifelse(
    nzchar(records$valid_from_code), records$valid_from_code, "0001-01-01"
  )
  version_start <- ifelse(
    nzchar(records$valid_from_version),
    records$valid_from_version,
    "0001-01-01"
  )
  code_end <- ifelse(
    nzchar(records$valid_to_code), records$valid_to_code, "9999-12-31"
  )
  version_end <- ifelse(
    nzchar(records$valid_to_version),
    records$valid_to_version,
    "9999-12-31"
  )
  records <- records[
    pmax(code_start, version_start) < pmin(code_end, version_end),
    , drop = FALSE
  ]
  records
}

legacy_county_select_record <- function(records, as_of) {
  active <- legacy_county_interval_active(
    records$valid_from_code, records$valid_to_code, as_of
  ) & legacy_county_interval_active(
    records$valid_from_version, records$valid_to_version, as_of
  )
  if (sum(active) == 1L) return(records[which(active), , drop = FALSE])
  if (sum(active) > 1L) {
    stop("Multiple county records are active for code ", records$code[[1L]])
  }

  ended <- nzchar(records$valid_to_code) | nzchar(records$valid_to_version)
  if (!all(ended)) {
    stop("County code has no active or unambiguously historical record: ",
         records$code[[1L]])
  }
  ends <- pmin(
    ifelse(nzchar(records$valid_to_code),
           records$valid_to_code, "9999-12-31"),
    ifelse(nzchar(records$valid_to_version),
           records$valid_to_version, "9999-12-31")
  )
  latest <- which(ends == max(ends))
  if (length(latest) != 1L) {
    stop("County code has multiple equally recent historical records: ",
         records$code[[1L]])
  }
  records[latest, , drop = FALSE]
}

legacy_county_compatibility_view <- function(
    data, compatibility_path, legacy_aliases_path) {
  compatibility <- legacy_county_compatibility_input(compatibility_path)
  legacy_aliases <- utils::read.csv(
    legacy_aliases_path,
    colClasses = "character",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (!all(c("canonical_code", "alias_code") %in% names(legacy_aliases))) {
    stop("Legacy alias input must contain canonical_code and alias_code")
  }

  records <- legacy_county_records(data)
  codes <- sort(setdiff(unique(records$code), legacy_aliases$alias_code))
  if (length(codes) != 3241L) {
    stop("Expected 3,241 legacy canonical county codes; found ", length(codes))
  }
  if (!all(compatibility$code %in% codes)) {
    stop(
      "Compatibility input contains unknown canonical county codes: ",
      paste(setdiff(compatibility$code, codes), collapse = ", ")
    )
  }

  as_of <- data$metadata$default_reference_date[[1L]]
  selected <- lapply(split(records, records$code), function(rows) {
    legacy_county_select_record(rows, as_of)
  })
  selected <- do.call(rbind, selected)
  selected <- selected[match(codes, selected$code), , drop = FALSE]
  if (anyNA(selected$code) || anyDuplicated(selected$code)) {
    stop("Temporal county projection did not select exactly one record per code")
  }

  append_rows <- nzchar(compatibility$append_order)
  append_codes <- compatibility$code[append_rows][order(
    as.integer(compatibility$append_order[append_rows])
  )]
  ordered_codes <- c(sort(setdiff(codes, append_codes)), append_codes)
  selected <- selected[match(ordered_codes, selected$code), , drop = FALSE]

  override <- match(selected$code, compatibility$code)
  legacy_name <- compatibility$legacy_name[override]
  use_override <- !is.na(legacy_name) & nzchar(legacy_name)
  names <- selected$preferred_name
  names[use_override] <- legacy_name[use_override]

  data.frame(
    code = selected$code,
    name = names,
    state_code = substr(selected$code, 1L, 2L),
    location_id = selected$location_id,
    location_version_id = selected$location_version_id,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

register.legacy.county.compatibility.view <- function(
    LM, data, compatibility_path, legacy_aliases_path,
    fips.typename = "county") {
  view <- legacy_county_compatibility_view(
    data, compatibility_path, legacy_aliases_path
  )
  fips.typename <- toupper(fips.typename)
  LM$register(rep(fips.typename, nrow(view)), view$name, view$code)

  states <- unname(LM$get.by.alias(view$state_code, "STATE"))
  if (anyNA(states)) {
    stop(
      "Legacy county projection contains unregistered state FIPS codes: ",
      paste(unique(view$state_code[is.na(states)]), collapse = ", ")
    )
  }
  LM$register.hierarchy(view$code, states, rep(TRUE, nrow(view)), TRUE)
  LM
}
