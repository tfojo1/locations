issues_path <- file.path("data-raw", "KNOWN_DATA_ISSUES.csv")
aliases_path <- file.path("data-raw", "code_aliases.csv")

for (path in c(issues_path, aliases_path)) {
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
stale_issues <- setdiff(issue_keys, alias_keys)
if (length(stale_issues) > 0L) {
  stop(
    "Known data issues refer to aliases that are not registered: ",
    paste(stale_issues, collapse = ", ")
  )
}

ct_aliases <- aliases[aliases$state == "CT", , drop = FALSE]
ct_keys <- pair_key(ct_aliases$alias_code, ct_aliases$canonical_code)
missing_ct_issues <- setdiff(ct_keys, issue_keys)
if (length(missing_ct_issues) > 0L) {
  stop(
    "Connecticut compatibility aliases must be registered as known issues: ",
    paste(missing_ct_issues, collapse = ", ")
  )
}

ct_issues <- issues[match(ct_keys, issue_keys), , drop = FALSE]
if (any(ct_issues$severity != "high") ||
      any(ct_issues$status != "open") ||
      any(ct_issues$blocker_scope != "temporal-county-release")) {
  stop(
    "Connecticut compatibility aliases must remain open high-severity ",
    "blockers for the temporal county release until the crosswalk ",
    "implementation removes them"
  )
}

message(
  "Validated ", nrow(issues), " known issue records across ",
  length(unique(issues$issue_id)), " open issue."
)
