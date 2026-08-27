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
  "provenance_status", "md5", "notes"
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

unverified <- sum(manifest$provenance_status %in% c("partial", "unknown"))
message(
  "Validated ", nrow(manifest), " raw source files (", unverified,
  " still require provenance follow-up)."
)
