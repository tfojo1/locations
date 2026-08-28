# Maintainer Assessment and Modernization Roadmap

Date: 2026-08-27

## Executive assessment

The package is in substantially better engineering shape than its history might suggest. The recent refactor removed serialized R6 location objects, introduced indexed data-frame storage, added CI and tests, made aliases data-driven, and documented the high-level architecture. The current runtime graph also passes basic integrity checks: 4,640 unique location codes, no duplicate relationships, no dangling relationship endpoints, no self-relationships, and no aliases pointing at missing codes.

The main risk is now the data model, not the graph implementation. The package treats a changing geographic entity, a code, and an alias as if they were interchangeable. That works for case variants and exact renames, but it cannot correctly represent splits, merges, boundary changes, or mixed data vintages. Connecticut, Alaska, and Montana make that limitation visible today.

The recommended direction is evolutionary: preserve the public compatibility layer, make location identity and time explicit underneath it, and replace the global build script with a validated data pipeline in stages. A ground-up rewrite would add migration risk without addressing the highest-priority data questions first.

## Findings from the reported issues

### NSDUH substate regions

The reported regions are present and their county relationships are registered. They return no counties from `get.contained.locations()` because that function previously exposed only complete containment:

- `IL.1` through `IL.7` each contain part of Cook County (`17031`).
- `DC.1` through `DC.8` each contain part of the District of Columbia county-equivalent (`11001`).
- `DE.2` and `DE.4` partition New Castle County (`10003`).
- `MA.2` and `MA.3` each use portions of multiple counties.

These are tract-defined regions, so marking the affected counties as completely contained would be incorrect. The package already stores them as partial edges. The public API now exposes those edges with `include.partial = TRUE`, while retaining complete-only behavior by default for compatibility.

The supplied 2021-2023 NSDUH definitions visually confirm these relationships. The existing raw files use `SBST18` field names and should nevertheless be replaced or accompanied by an explicit source-vintage manifest; matching selected definitions is not a substitute for a reproducible provenance record.

### Alias semantics

Aliases should be reserved for codes that denote the same geographic entity and footprint. They should not be used as a generic missing-data repair.

- South Dakota `46113` to `46102` is a defensible alias: Shannon County was renamed Oglala Lakota County and received a new code.
- The eight Connecticut mappings in `data-raw/code_aliases.csv` are not defensible one-to-one aliases. The nine planning regions replaced the eight former counties as Census county-equivalents in 2022, but the boundaries cross the former county boundaries. Sanitizing `09001` to `09110`, for example, silently changes geography.
- Alaska returns 48 `COUNTY` records because the package combines 29 entities from its older base file, two current post-2019 entities, and 17 additional historical codes. One base entity, Valdez-Cordova (`02261`), is also now historical. The current count is 30; the package count is an all-vintages count presented as if it were current.
- Montana returns 57 because it includes the 56 current counties plus historical Yellowstone National Park county-equivalent (`30113`). That historical row was mislabeled as Yellowstone County; the package data now labels it correctly and distinguishes it from current Yellowstone County (`30111`).

The earlier Zoe-related commits explain the downstream concern. Historical FIPS values were added so old inputs would not be classified as missing, and `is.location.valid()` was later changed to accept aliases. That solved validation symptoms but did not establish whether resolution preserves geographic meaning.

## Current engineering baseline

### Strengths

- The installed data is reconstructed from plain serializable structures rather than a stale serialized R6 object.
- Hash indexes make code and type lookup inexpensive.
- Complete and partial relationships are represented separately in both directions.
- Public functions mostly form a compatibility facade over one manager.
- The test suite covers core lookup, registration, containment, sanitization, and the data-driven TGA path.
- `R CMD check` completes with zero errors and zero notes.

### Risks and debt

1. **No temporal geography model.** There is no `valid_from`, `valid_to`, vintage, active status, successor/predecessor relation, or source on a location record.
2. **Alias and crosswalk concepts are conflated.** The model assumes an alias resolves to exactly one current canonical code. Real geographic changes are often one-to-many or many-to-many.
3. **Mixed, weakly documented source vintages.** County, CBSA, NSDUH, gazetteer, and polygon inputs come from different years, and the build does not emit a manifest or enforce compatibility.
4. **An implicit 844-line build script.** It relies on order, mutates a global manager, has build-time dependencies not declared in a reproducible environment, and mixes ingestion, correction, derivation, and serialization.
5. **Global mutable runtime state.** The singleton is convenient for compatibility but makes isolation, concurrency, and test fixtures harder.
6. **Validation is mostly structural, not semantic.** There are no assertions for expected active counts by vintage, state coverage, source uniqueness, temporal overlap, alias footprint equivalence, or graph cycles.
7. **Package metadata is unfinished.** `R CMD check` reports one warning because `License: What license is it under?`; the maintainer still names the former maintainer, and the description contains a typo. These require owner decisions, not an automated guess.
8. **CI is a minimal single-platform check.** It checks Ubuntu with one R version and fails only on errors, so the current license warning is accepted.
9. **Documentation and API consistency remain uneven.** There are no checked examples or vignette, unknown-location return shapes vary, and the registered `ZIPCODE` and `PHD` types currently have no runtime records.
10. **The manager remains a large class.** `R/location_manager.R` is 873 lines and combines storage, resolution, graph traversal, names, geometry, registration, and validation.

## Recommended target model

Use stable internal entity IDs and treat codes as time-bounded identifiers:

```text
locations
  location_id, type, preferred_name

location_codes
  location_id, code_system, code, valid_from, valid_to, status, source_id

relationships
  child_location_id, parent_location_id, relation, valid_from, valid_to, source_id

aliases
  alias, location_id, alias_kind, valid_from, valid_to

crosswalks
  from_location_id, to_location_id, relation, weight_type, weight, vintage, source_id

sources
  source_id, publisher, title, vintage, url, retrieved_at, checksum
```

An alias must resolve without changing footprint. A crosswalk may return multiple targets and must make its weighting and vintage explicit. Historical codes remain valid identifiers without being included in a current-vintage county listing.

The existing `LOCATION.MANAGER` can remain as the default compatibility facade. Internally, construct an immutable store and allow advanced callers and tests to create independent manager instances.

## Roadmap

### Phase 0 implementation status (2026-08-28)

The structural integrity suite, source manifest, API compatibility contracts,
independent data-bundle version, and machine-readable Connecticut release
blocker are implemented on the Phase 0 branch. The provenance audit recovered
authoritative lineage for every active input: the polygon files are exact
derivatives of 2018 Census cartographic boundaries, the main geographic-code
inventory descends from the Census Vintage 2016 workbook with documented
maintainer edits, and the NSDUH inputs are curated extracts of the 2016-2018
definitions.

The audit also corrected three county names that had been truncated during a
historical CSV rewrite. Two disabled ZIP inputs remain explicitly dormant and
must not be enabled without replacement. Four active rows still need data-reuse
metadata decisions: the maintainer-curated aliases and historical-code table,
plus the two SAMHSA extracts. The package license and maintainer metadata remain
deferred owner decisions, so Phase 0 has not reached its zero-warning exit
criterion.

### Phase 0 - Correctness guardrails

- Release the partial-containment API and exact NSDUH regression tests.
- Decide package license and update maintainer metadata; make CI fail on warnings.
- Add a build-time integrity suite for duplicate codes, missing endpoints, cycles, alias target validity, and expected active counts by state and vintage.
- Add a machine-readable source manifest with URLs, vintages, retrieval dates, checksums, and licenses.
- Mark the Connecticut one-to-one mappings as a release-blocking data defect; replace them only alongside an explicit historical-location/crosswalk representation.

Exit criterion: every shipped location and relationship has a source and vintage, and `R CMD check` has zero errors, warnings, and notes.

### Phase 1 - Temporal county layer

- Introduce location-code history and active-vintage filtering without changing legacy function defaults in the same release.
- Import a current authoritative county/county-equivalent list and classify older codes as historical.
- Model Connecticut old counties and new planning regions as distinct entities connected by a many-to-many crosswalk.
- Model Alaska splits and Montana `30113` as historical geography rather than current counties.
- Add explicit APIs such as `get_locations(type, vintage, status)` and `crosswalk_locations(from, to_vintage, weights)`; keep dot-named APIs as compatibility wrappers.

Exit criterion: current queries return 30 Alaska county-equivalents, 9 Connecticut planning regions, and 56 Montana counties, while historical codes still validate at the appropriate vintage.

### Phase 2 - Reproducible data pipeline

- Split ingestion into small source-specific modules that produce normalized tables.
- Separate vendored raw inputs, hand-maintained corrections, generated intermediates, and packaged outputs.
- Validate schemas and referential/temporal integrity before serialization.
- Make the build deterministic and runnable from one documented command in a clean checkout.
- Generate a build report containing source versions, record counts, rejected rows, and checksums.

Exit criterion: rebuilding in CI produces the expected normalized data and a reviewable manifest without relying on an existing `sysdata.rda`.

### Phase 3 - Runtime decomposition

- Extract code resolution, graph queries, names, and geometry into focused modules.
- Keep the singleton only as a backward-compatible default instance.
- Standardize scalar/vector/list behavior and unknown-location handling in the new API.
- Move plotting behind optional dependencies or a companion package if core users do not need it.

Exit criterion: core lookup and graph logic can be tested against small in-memory fixtures without package-global state or full national geometry data.

### Phase 4 - Release and maintenance discipline

- Add R release/oldrel/devel and macOS/Windows coverage as appropriate.
- Publish lifecycle and deprecation policy, changelog, contributor guide, and data-update runbook.
- Add contract tests against known downstream workflows before changing legacy behavior.
- Adopt semantic versioning and release additive changes separately from data-vintage changes.

## Suggested next release boundary

Treat the current additive API change and Montana label correction as a small maintenance release. Do not silently change Connecticut resolution or default county counts in that release. The following feature release should introduce temporal records and crosswalks, migrate Connecticut/Alaska/Montana together, and give downstream users an explicit compatibility window.

## Authoritative references consulted

- SAMHSA, *2021-2023 National Surveys on Drug Use and Health: Substate Region Definitions* (the PDF supplied for this investigation).
- U.S. Census Bureau, [2022 Geographic Levels](https://www.census.gov/programs-surveys/economic-census/geographies/levels/2022-levels.html), documenting Connecticut and Alaska county-equivalent changes.
- U.S. Census Bureau, [Special 2022 Connecticut Relationship Files](https://www.census.gov/geographies/reference-files/2022/geo/relationship-files.html).
- U.S. Census Bureau, [Current Alaska Counties and County-Equivalents](https://tigerweb.geo.census.gov/tigerwebmain/Files/acs25/tigerweb_acs25_county_ak.html).
- U.S. Census Bureau, [1990s County and County-Equivalent Changes](https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.1990.html), documenting deletion of Montana `30-113` in 1997.
