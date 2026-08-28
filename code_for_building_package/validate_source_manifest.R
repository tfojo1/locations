manifest_path <- file.path("data-raw", "SOURCES.csv")
data_directory <- "data-raw"

if (!file.exists(manifest_path)) {
  stop("Raw source manifest is missing: ", manifest_path)
}

manifest <- read.csv(
  manifest_path,
  colClasses = "character",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_columns <- c(
  "file", "publisher", "title", "vintage", "source_url", "license_status",
  "provenance_status", "usage_status", "md5", "notes"
)
missing_columns <- setdiff(required_columns, names(manifest))
if (length(missing_columns) > 0) {
  stop(
    "Source manifest is missing columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

if (anyNA(manifest[, required_columns]) ||
      any(manifest[, required_columns] == "")) {
  stop(
    "Source manifest fields must be explicit; use UNKNOWN rather than blanks"
  )
}

if (anyDuplicated(manifest$file)) {
  stop("Source manifest contains duplicate file entries")
}

allowed_provenance <- c("verified", "partial", "curated", "unknown")
invalid_provenance <- setdiff(
  unique(manifest$provenance_status), allowed_provenance
)
if (length(invalid_provenance) > 0) {
  stop(
    "Invalid provenance_status values: ",
    paste(invalid_provenance, collapse = ", ")
  )
}

allowed_usage <- c("active", "dormant")
invalid_usage <- setdiff(unique(manifest$usage_status), allowed_usage)
if (length(invalid_usage) > 0) {
  stop(
    "Invalid usage_status values: ",
    paste(invalid_usage, collapse = ", ")
  )
}

raw_files <- setdiff(
  basename(list.files(data_directory, full.names = FALSE, all.files = FALSE)),
  basename(manifest_path)
)
missing_entries <- setdiff(raw_files, manifest$file)
stale_entries <- setdiff(manifest$file, raw_files)
if (length(missing_entries) > 0) {
  stop(
    "Raw files missing from source manifest: ",
    paste(missing_entries, collapse = ", ")
  )
}
if (length(stale_entries) > 0) {
  stop(
    "Source manifest entries have no raw file: ",
    paste(stale_entries, collapse = ", ")
  )
}

paths <- file.path(data_directory, manifest$file)
actual_md5 <- unname(tools::md5sum(paths))
mismatched <- manifest$file[tolower(actual_md5) != tolower(manifest$md5)]
if (length(mismatched) > 0) {
  stop(
    "Raw source checksums changed; review the data and update SOURCES.csv: ",
    paste(mismatched, collapse = ", ")
  )
}

unverified <- manifest$provenance_status %in% c("partial", "unknown")
active_unverified <- sum(unverified & manifest$usage_status == "active")
dormant_unverified <- sum(unverified & manifest$usage_status == "dormant")
metadata_fields <- c("publisher", "vintage", "source_url", "license_status")
metadata_followup <- apply(
  manifest[, metadata_fields, drop = FALSE],
  1,
  function(values) {
    any(values %in% c("UNKNOWN", "DEFERRED")) ||
      any(grepl("unverified", values, ignore.case = TRUE))
  }
)
active_metadata_followup <- sum(
  metadata_followup & manifest$usage_status == "active"
)
message(
  "Validated ", nrow(manifest), " raw source files (", active_unverified,
  " active provenance gaps; ", active_metadata_followup,
  " active metadata follow-ups; ", dormant_unverified,
  " dormant provenance gaps)."
)
