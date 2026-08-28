issues_path <- file.path("data-raw", "KNOWN_DATA_ISSUES.csv")
aliases_path <- file.path("data-raw", "code_aliases.csv")
legacy_aliases_path <- file.path("data-raw", "legacy_code_aliases.csv")

for (path in c(issues_path, aliases_path, legacy_aliases_path)) {
  if (!file.exists(path)) {
    stop("Required data-governance file is missing: ", path)
  }
}

issues <- read.csv(
  issues_path,
  colClasses = "character",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
aliases <- read.csv(
  aliases_path,
  colClasses = "character",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
legacy_aliases <- read.csv(
  legacy_aliases_path,
  colClasses = "character",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_columns <- c(
  "issue_id", "severity", "status", "blocker_scope", "alias_code", "canonical_code",
  "summary", "required_resolution", "source_url"
)
missing_columns <- setdiff(required_columns, names(issues))
if (length(missing_columns) > 0L) {
  stop(
    "Known-data-issues registry is missing columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

if (anyNA(issues[, required_columns]) ||
      any(issues[, required_columns] == "")) {
  stop("Known-data-issues registry fields must not be blank")
}

allowed_severities <- c("release-blocking", "high", "moderate", "low")
invalid_severities <- setdiff(unique(issues$severity), allowed_severities)
if (length(invalid_severities) > 0L) {
  stop(
    "Invalid known-issue severities: ",
    paste(invalid_severities, collapse = ", ")
  )
}

allowed_statuses <- c("open", "resolved", "accepted")
invalid_statuses <- setdiff(unique(issues$status), allowed_statuses)
if (length(invalid_statuses) > 0L) {
  stop(
    "Invalid known-issue statuses: ",
    paste(invalid_statuses, collapse = ", ")
  )
}

pair_key <- function(alias_code, canonical_code) {
  paste(alias_code, canonical_code, sep = "->")
}

issue_keys <- pair_key(issues$alias_code, issues$canonical_code)
if (anyDuplicated(issue_keys)) {
  stop("Known-data-issues registry contains duplicate alias pairs")
}

alias_keys <- pair_key(aliases$alias_code, aliases$canonical_code)
legacy_alias_keys <- pair_key(
  legacy_aliases$alias_code, legacy_aliases$canonical_code
)
if (length(intersect(alias_keys, legacy_alias_keys)) > 0L) {
  stop(
    "Code aliases may not be both semantic and legacy compatibility aliases"
  )
}

ct_aliases <- aliases[aliases$state == "CT", , drop = FALSE]
if (nrow(ct_aliases) > 0L) {
  stop(
    "Connecticut county changes are crosswalks and may not remain in the ",
    "semantic code-alias inventory"
  )
}

expected_ct_aliases <- sprintf("09%03d", seq.int(1L, 15L, by = 2L))
expected_ct_targets <- sprintf("09%03d", seq.int(110L, 180L, by = 10L))
ct_legacy <- legacy_aliases[legacy_aliases$state == "CT", , drop = FALSE]
if (!setequal(ct_legacy$alias_code, expected_ct_aliases) ||
    !setequal(ct_legacy$canonical_code, expected_ct_targets) ||
    nrow(ct_legacy) != 8L) {
  stop(
    "The legacy Connecticut compatibility view must preserve exactly the ",
    "eight pre-crosswalk resolver mappings"
  )
}
required_legacy_columns <- c("compatibility_scope", "removal_schedule")
if (length(setdiff(required_legacy_columns, names(legacy_aliases))) > 0L ||
    any(ct_legacy$compatibility_scope != "legacy-dot-api") ||
    any(ct_legacy$removal_schedule !=
      "1.0.0-or-later-after-warning-release")) {
  stop("Legacy Connecticut aliases must declare the ADR 0001 migration scope")
}

if (!setequal(issue_keys, legacy_alias_keys)) {
  stop(
    "Resolved Connecticut issue records must match the isolated legacy ",
    "compatibility mappings"
  )
}
ct_issues <- issues[match(legacy_alias_keys, issue_keys), , drop = FALSE]
if (any(ct_issues$severity != "high") ||
    any(ct_issues$status != "resolved") ||
    any(ct_issues$blocker_scope != "none")) {
  stop(
    "Connecticut crosswalk issues must be resolved and must not block the ",
    "temporal county release once the replacement is packaged"
  )
}

crosswalk_path <- file.path(
  "data-raw", "acs22_cousub22_blkgrp20_st09.txt"
)
if (!file.exists(crosswalk_path)) {
  stop("The resolved Connecticut issues require the pinned Census crosswalk")
}

message(
  "Validated ", nrow(issues), " known issue records across ",
  length(unique(issues$issue_id)), " resolved issue."
)
