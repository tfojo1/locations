# Data Provenance Audit

Date: 2026-08-28

`data-raw/SOURCES.csv` is the machine-readable inventory for every raw input in
the repository. `provenance_status` describes how confidently the input can be
traced, while `usage_status` distinguishes inputs used to build the shipped
package from disabled legacy material. A curated input has explicit maintainer
lineage but may not be a byte-for-byte upstream artifact.

## Recovered authoritative lineage

The three active polygon CSVs are flattened forms of the U.S. Census Bureau's
2018 cartographic-boundary shapefiles at 1:20,000,000 resolution. The state,
county, and CBSA identifiers match their respective shapefiles exactly, and
every packaged CSV coordinate occurs in the corresponding official file at
six-decimal precision. Comparison against 2019 through 2023 releases produced
lower coordinate matches and different entity sets.

`fips_codes.csv` descends from the Census Population Division's *Estimates
Geography File: Vintage 2016*. The repository's original import has the same
43,934 records and differs from the workbook only in the text encoding of 20
accented names. Later commits document Alaska additions, movement of additions
to `new_fips_codes.csv`, and removal of former Connecticut counties. Because it
has been edited, the manifest classifies it as curated rather than as an
untouched upstream file.

The `SBST18` schema and region definitions in `nsduh-county.csv` and
`nsduh-tract.csv` identify them as curated extracts of SAMHSA's 2016-2018 NSDUH
substate definitions. The source appendix is identified, but the original
extraction script is not present.

The Census state-code reference verifies `fips_state_aliases.csv` and supports
the codes in `us_state_abbreviations.csv`. The latter remains curated because
two territory display names were shortened locally. HRSA's FY 2025 Part A
notice verifies that the Oakland TGA comprises Alameda and Contra Costa
counties.

## Temporal county slice

The internal temporal county store is generated from the pinned 2025 Census
National Counties Gazetteer. Its default reference date is January 1, 2025,
and the source file and six Census county-change pages are vendored with
verified checksums. Curated temporal inputs transcribe effective dates and
successions for the legacy Alaska, Connecticut, and Montana records; the
original source snapshots remain alongside those inputs for review.

Opaque `location_id` values live in `temporal_county_registry.csv` and are
assigned once rather than recalculated from a current code. The deterministic
build validates its normalized schema, source checksums, referential and
temporal integrity, target-state current counts, and committed build report.
This store is additive: the legacy manager and its public API continue to use
their existing compatibility data until the temporal read APIs are introduced.

## Dormant ZIP material

ZIP loading and ZIP polygon loading are disabled in the build. The coordinate
columns in `zip_codes.csv` match a subset of the archived CRAN `zipcode` 1.0
dataset, but the source of its county-FIPS join is unknown. The polygon file
appears to contain simplified Census ZIP Code Tabulation Areas, which are not
the same geography as USPS ZIP Codes, and its derivation script is absent.

Both files remain inventoried as dormant and unresolved. They must not be
enabled until they are replaced by a sourced, explicitly modeled ZIP or ZCTA
dataset with a reproducible derivation and compatible license.

## Remaining follow-up

Active provenance is now explicit. Remaining metadata follow-up concerns reuse
terms for the curated project data and SAMHSA extracts; it is intentionally not
resolved by assuming the package's deferred software license decision also
covers its data inputs.
