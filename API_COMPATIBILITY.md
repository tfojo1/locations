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
