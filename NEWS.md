# locations (development version)

## Maintenance

- Added ADR 0001 defining stable location identity, versioned geographic
  definitions, time-bounded codes, typed crosswalk measures, and the legacy API
  migration boundary for the temporal-county work.
- Added executable normalized temporal-data schemas and semantic validators,
  with representative Connecticut, Alaska, and Montana contract fixtures.
- Added a deterministic internal temporal-county data slice built from the
  pinned 2025 Census Gazetteer and county-change sources. Its current view has
  30 Alaska county-equivalents, 9 Connecticut planning regions, and 56 Montana
  counties; legacy historical codes remain time-bounded records, not aliases.
- Replaced Connecticut's invalid one-to-one modeling in the normalized store
  with 19 directional former-county-to-planning-region overlap edges derived
  from the pinned Census ACS22 relationship file. Land and water fractions are
  explicit; no population weights are inferred.
- Isolated the eight Connecticut one-to-one mappings in a legacy-only build
  input. Existing dot-named APIs retain their current behavior through the
  ADR 0001 migration schedule.
- Updated package maintainer metadata while preserving original authorship.
- Resolved the Connecticut crosswalk blocker for the temporal-county release.
- Added compatibility contracts derived from location workflows used by
  JHEEM2.
- Updated GitHub Actions checkout to its Node.js 24-based release.

# locations 0.4.1

## User-facing changes

- Added `include.partial = TRUE` to `get.contained.locations()` and
  `get.containing.locations()`. This exposes partial county footprints for
  tract-defined NSDUH regions while preserving the existing complete-only
  default.
- Added exact regression coverage for the 19 Illinois, District of Columbia,
  Delaware, and Massachusetts substate regions reported as returning no
  counties.
- Corrected the label for historical Montana county-equivalent `30113` and
  restored three county names that had been truncated during an earlier raw
  data rewrite.
- Added `locations.data.version()` so consumers can track bundled-data updates
  separately from package-code releases.

## Maintenance and data governance

- Added compatibility contracts for existing exports, argument order,
  defaults, and representative return shapes.
- Added build-time integrity checks for locations, relationships, aliases,
  polygons, and relationship cycles.
- Added a checksum-validated source manifest and provenance audit for every
  raw input used by the package build.
- Marked the existing Connecticut county-to-planning-region aliases as a known
  high-severity blocker for the temporal-county release pending a many-to-many
  crosswalk.
- Classified the unused legacy ZIP and ZIP-like polygon inputs as dormant. No
  ZIP data or public functionality was removed.

## Known issues

- The package still mixes current and historical county-equivalent records in
  legacy county listings. A temporal county layer is planned for the next
  feature release.
- The package license and maintainer metadata remain pending owner decisions.
