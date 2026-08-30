# locations (development version)

## User-facing changes

- Fixed `get.code.by.alias()` so one type can be applied to multiple aliases
  and a same-length type vector can be paired with the aliases, as documented.
  Scalar behavior and the named-list return shape are unchanged.
- Documented the exact differences between legacy all-vintage county results
  and the pinned current temporal view.

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
- Added explicit temporal read APIs for querying locations by date and status,
  resolving complete official code history, inspecting durable identity, and
  returning typed many-to-many crosswalk results. Existing dot-named APIs and
  runtime registration behavior remain unchanged.
- Added compatibility contracts derived from location workflows used by
  JHEEM2.
- Added a separately pinned downstream integration workflow that installs the
  candidate package with JHEEM2 and exercises its real validation,
  sanitization, metadata, containment, overlap, and name-to-code paths.
- Generated the legacy county manager from the normalized temporal store plus
  an explicit order-and-label compatibility overlay. A full-manager CI parity
  gate protects existing codes, names, aliases, relationships, coordinates,
  and polygon behavior.
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

- Legacy county enumeration intentionally remains all-vintage for compatibility;
  use `get_locations("COUNTY")` for the pinned current view.
- Package and active curated-data reuse terms remain pending owner decisions in
  issue #3. Maintainer metadata has been updated but still requires final
  confirmation for the release checklist.
