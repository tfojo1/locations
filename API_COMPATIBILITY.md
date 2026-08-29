# API Compatibility Policy

The package will modernize behind a compatibility facade. Existing exported
dot-named functions, their positional argument order, defaults, and established
return shapes remain supported throughout the current modernization roadmap.
New temporal, current-vintage, and crosswalk behavior will first be introduced
through additive APIs.

## What compatibility covers

- Existing calls continue to run without source changes.
- New optional arguments are appended after existing arguments and preserve the
  old behavior by default.
- Internal storage, indexing, validation, and build-pipeline changes are not
  observable through the public API.
- Deprecations require a documented replacement and warning period. Removal, if
  ever necessary, is reserved for a major package release.
- Contract tests protect the legacy exports, formal arguments, defaults, and
  representative return shapes.

## Data corrections are different from API changes

API compatibility does not promise that a lookup will preserve a result known
to be geographically false. Authoritative source updates, corrected labels,
and explicit vintage selection can change returned data while leaving the call
signature and return type intact.

Potentially disruptive data corrections will be identified in release notes.
Where practical, the package will provide an opt-in corrected API, an explicit
vintage or status parameter, and a migration window before changing legacy
defaults. Ambiguous geographic changes such as splits, merges, and boundary
changes will be represented as crosswalks rather than silently forced through
one-to-one aliases.

Use `packageVersion("locations")` for the code/API release and
`locations.data.version()` for the independently versioned bundled dataset.

## Additive temporal API

The snake-case functions are a separate, immutable temporal interface:

- `locations_default_date()` returns the dataset's pinned reference date;
- `get_locations()` selects versions by type, date, and status;
- `resolve_location()` returns official code history without legacy aliases;
- `get_location_history()` returns versions for one durable identity; and
- `crosswalk_locations()` returns directional, potentially many-to-many edges
  with a requested and explicitly named measure.

These functions always return data frames with documented stable columns.
Empty lookups return zero-row data frames with the same columns. Runtime
registration functions continue to affect only the legacy manager.

Connecticut demonstrates the intentional compatibility boundary:

```r
# Legacy compatibility behavior remains unchanged during 0.5.x
sanitize("09003")
#> "09120"

# Temporal resolution keeps Hartford County historic
resolve_location("09003")

# Geographic conversion returns Hartford's actual multiple overlaps
crosswalk_locations("09003", measure = "land_area")
```

No new warning is added to the legacy call in the first additive release. The
warning and removal schedule is documented in
`docs/connecticut-crosswalk-migration.md` and ADR 0001.
