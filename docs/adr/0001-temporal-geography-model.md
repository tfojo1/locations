# ADR 0001: Temporal Geography Identity and Crosswalk Model

- Status: Proposed
- Date: 2026-08-28
- Decision owners: locations maintainers
- Tracking issue: [#4](https://github.com/tfojo1/locations/issues/4)
- Target release: 0.5.0

## Context

The package currently uses one character code as all of the following:

- a location's identity;
- its current external identifier;
- a lookup key;
- an alias target; and
- the endpoint of containment relationships.

That model cannot distinguish a rename or recode from a split, merge,
replacement, or boundary change. It also combines records from multiple source
vintages in apparently current lists. The practical failures are already
visible:

- eight historic Connecticut counties are mapped one-to-one to eight of the
  nine replacement planning regions even though their footprints cross;
- Alaska current and historical county-equivalents are returned together;
- Montana's historical `30113` is returned alongside its 56 current counties;
- an NSDUH region code can describe a survey-vintage-specific tract footprint;
  and
- CBSA and TGA membership can change while a displayed code remains stable.

A code is not a durable geographic identity, and a durable entity is not a
single immutable footprint. Census guidance notes that a GEOID may remain the
same while a boundary changes. Census also distinguishes a change's legal
effective date from the reference date on which it appears in a data product.
The new model therefore needs three separate concepts: stable entities,
versioned geographic definitions, and time-bounded identifiers.

## Decision drivers

1. Prevent silent geographic transformation through aliases.
2. Answer current and historical queries explicitly and reproducibly.
3. Preserve existing exported functions and defaults during the 0.x migration.
4. Support one-to-many and many-to-many change relationships.
5. Distinguish spatial overlap from a defensible statistical allocation.
6. Attach source and vintage information to every shipped record.
7. Keep rebuilds deterministic and independent of mutable runtime state.

## Decision

### 1. Separate identity, definition, and code

The normalized model will use these layers:

```text
location_entities
       |
       +----< location_versions >---- geometries
       |              |
       |              +----< relationships
       |              +----< crosswalk_edges >---- location_versions
       |
       +----< location_codes
       +----< location_names
       +----< successions >---- location_entities

sources ---- every sourced or derived row
```

#### `location_entities`

One row represents a durable legal, administrative, statistical, or program
concept. Its primary key, `location_id`, is an opaque immutable identifier. It
is not derived at runtime from a name or current external code.

Minimum fields:

```text
location_id, entity_kind, created_from_source_id
```

Identity continues through a rename, recode, or ordinary boundary adjustment
when the authoritative source treats the geography as the continuing entity.
A split, merge, or explicit replacement creates new entities connected through
`successions`.

#### `location_versions`

One row represents an entity's authoritative definition for a half-open period
`[valid_from, valid_to)`. A new version is required when its footprint,
preferred official name, type, or legal/statistical definition changes.

Minimum fields:

```text
location_version_id, location_id, type, preferred_name,
valid_from, valid_to, end_reason, source_id
```

Unknown interval bounds are allowed only when accompanied by explicit date
precision and a documented source limitation. A source product's vintage or
reference date is stored separately and must not be substituted for an unknown
legal effective date.

#### `location_codes`

Official and package identifiers are time-bounded attributes, not primary keys.
Codes are always stored as character data so leading zeroes are preserved.

Minimum fields:

```text
location_id, code_system, code, valid_from, valid_to, source_id
```

The same entity may have several simultaneous code systems. The same code may
identify different entities only in non-overlapping intervals. Historic
official codes are code-history records, not aliases.

Each `code_system` names both the issuing authority and geographic level, such
as a Census state-county GEOID namespace. This prevents equal-looking codes at
different geographic levels from colliding.

#### `location_names`

Official former names, current names, and supported synonyms are time-bounded
and typed rather than embedded in lookup code.

Minimum fields:

```text
location_id, name, name_kind, valid_from, valid_to, source_id
```

### 2. Derive temporal status instead of storing a mutable current flag

`current` and `historical` are query classifications relative to an `as_of`
date:

- **current**: the requested date falls within the record's validity interval;
- **historical**: the record's validity interval ended on or before the
  requested date; and
- **future**: the record begins after the requested date.

`retired` and `superseded` describe why an entity or version ended:

- **retired**: it ended without an authoritative replacement relationship;
- **superseded**: one or more successor entities or definitions replaced it;
- **split**: it was superseded by multiple successor entities;
- **merged**: it and at least one other entity were superseded by one successor;
- **redefined**: the durable entity continued but a new version changed its
  footprint or definition; and
- **renamed/recoded**: the durable entity continued with a new name or code.

The default dataset reference date is packaged metadata. It is pinned during a
data release and never means "whatever is latest when the package is loaded."

### 3. Restrict aliases to non-geographic equivalence

An alias may normalize spelling, capitalization, punctuation, a documented
synonymous name, or a package-specific prefix. It must resolve to the same
entity and the same geographic definition for its declared validity interval.

The following are not aliases in the temporal model:

- an old official code;
- a predecessor or successor;
- a split or merge;
- a boundary-changing replacement; or
- a convenient target chosen merely to prevent missing values.

Historic official codes belong in `location_codes`. Predecessor/successor
semantics belong in `successions`. Spatial transformations belong in
crosswalks. A curated alias requires a source or maintainer decision record
that establishes identity and footprint equivalence.

The existing alias inventory migrates by meaning:

- state ANSI codes and USPS abbreviations become simultaneous entries in
  distinct code systems for the same entity;
- raw CBSA codes and package-prefixed `C.` codes become official and package
  code-system entries rather than aliases;
- former county codes `12025`, `29193`, and `46113` become time-bounded code
  history after their continuity and effective dates are sourced; and
- the eight Connecticut pairs become distinct entities and crosswalk inputs,
  never temporal aliases.

This inventory must be completed for every generated or runtime alias before
the compatibility view is generated from normalized data.

### 4. Separate four kinds of relationship

The current `complete` Boolean collapses several meanings. Normalized data will
use explicit relationship kinds:

1. **spatial containment**: `contains` or `overlaps`, between versioned
   footprints at a compatible reference date;
2. **membership**: a component belongs to a program or statistical aggregate,
   such as a CBSA, TGA, or NSDUH definition;
3. **succession**: entities are renamed, recoded, split, merged, replaced, or
   retired over time; and
4. **crosswalk**: source and target versions intersect for transformation or
   comparison.

Minimum containment/membership fields:

```text
child_version_id, parent_version_id, relation_kind,
valid_from, valid_to, source_id
```

Minimum succession fields:

```text
from_location_id, to_location_id, succession_kind,
effective_date, source_id
```

Graph traversal never follows succession or crosswalk edges implicitly.

### 5. Make crosswalk measures directional and typed

`crosswalk_edges` records that two versioned footprints intersect. It does not
imply that observations can be allocated between them.

Minimum edge fields:

```text
crosswalk_id, from_version_id, to_version_id,
relation_kind, source_id
```

Optional measurements are stored separately:

```text
crosswalk_id, measure_type, numerator, denominator,
fraction_of_from, fraction_of_to, reference_date,
population_universe, method, source_id
```

Supported measure types are explicit, initially:

- `land_area`;
- `water_area`; and
- `population` only when an identified population dataset, universe, vintage,
  and derivation method are available.

There is no generic unqualified `weight`. Area fractions must not be presented
as population weights. Population allocation must not silently fall back to
area. When a requested measure is unavailable, the API returns an explicit
unavailable result or error rather than substituting another measure.

For an exhaustive directional crosswalk, `fraction_of_from` must sum to one
within a declared numeric tolerance for each source version. Partial-coverage
crosswalks must be marked as such and are not subject to that invariant.

### 6. Require row-level provenance and distinguish dates

Every entity version, code assignment, name, relationship, succession,
crosswalk edge, crosswalk measure, and geometry has a `source_id`. Derived rows
also identify their derivation method or build step.

The source table includes at least:

```text
source_id, publisher, title, source_vintage, reference_date,
retrieved_at, url, license_status, checksum
```

The model distinguishes:

- **effective date**: when a legal or statistical change took effect;
- **reference date**: the boundary date represented by a source product;
- **source vintage**: the publisher's named data release; and
- **retrieval date**: when the project acquired the artifact.

Curated corrections live in a separate reviewable input with the affected row,
rationale, source, issue reference, and reviewer. A later input file does not
silently override an earlier source.

### 7. Add explicit APIs while retaining a compatibility view

The temporal APIs will be additive. Proposed names are illustrative but their
semantics are decided here:

```r
get_locations(type, as_of = locations_default_date(), status = "current")
resolve_location(code, code_system = NULL, as_of = NULL)
get_location_history(location_id)
crosswalk_locations(from, to_as_of, measure = "none")
```

New APIs return explicit identifiers, codes, validity intervals, and source
metadata in stable data-frame shapes. They do not silently normalize a historic
official code to a current geography.

`resolve_location()` with an `as_of` date selects records valid at that date.
Without `as_of`, it returns all matching code-history rows in a data frame; it
never selects one ambiguous vintage by row order.

For the 0.5.x series, existing dot-named APIs continue to use a separately
constructed compatibility view with their current signatures, defaults, and
return shapes. In particular:

- existing complete-only containment remains the default;
- existing all-vintage county enumeration remains available through the legacy
  view even when it differs from a current temporal query;
- the Connecticut mappings remain confined to legacy resolution during the
  first additive release and are never exposed as valid crosswalks; and
- runtime registration continues to affect the legacy manager only until a
  separate temporal-store extension contract is designed.

Migration schedule:

1. **0.5.0**: add temporal APIs and document divergences; do not change legacy
   defaults or introduce new legacy warnings.
2. **A later 0.x release**: introduce a classed, once-per-session warning for
   geographically unsafe legacy resolutions, with a documented opt-out for
   controlled batch migrations.
3. **1.0.0 or later**: remove invalid one-to-one geographic aliases after at
   least one documented warning release. Historic codes remain queryable
   through temporal APIs.

This schedule preserves current consumers while providing a path away from
known false geography.

## Required invariants

The build fails when any of these are violated:

- entity, version, source, and relationship primary keys are unique;
- validity intervals are non-empty and use half-open semantics;
- versions for one entity do not overlap unless an explicit source-conflict
  record exists;
- one code system/code pair does not identify multiple entities at the same
  time;
- relationship endpoints exist and have temporally compatible versions;
- containment and membership graphs are acyclic where the relation requires it;
- historic official codes are not duplicated as temporal aliases;
- aliases do not shadow canonical codes and have equivalence evidence;
- crosswalk fractions are in `[0, 1]` and exhaustive directional fractions sum
  within tolerance;
- every active row has source and reference-vintage metadata; and
- the pinned default county view has expected authoritative counts by state.

## Application to known cases

### Connecticut

The eight former counties and nine planning regions are distinct entities. The
2022 planning-region versions become current at the package's selected Census
reference date. Former counties remain historically queryable. Census
county/county-subdivision crosswalks and relationship files supply the inputs
for reproducibly deriving spatial edges and area measurements; they do not by
themselves supply population weights. The derivation and its source chain must
be recorded. The eight existing one-to-one aliases are legacy compatibility
behavior only and are removed after the migration period.

### Alaska

Valdez-Cordova is a historical entity superseded by Chugach and Copper River.
Earlier Alaska entities remain historical records rather than members of the
current county view. A same-entity name/code change such as Wade Hampton to
Kusilvak is represented by versioned names and codes, not a geographic
crosswalk.

### Montana

Yellowstone National Park county-equivalent `30113` is a historical entity. It
is absent from current county enumeration but remains resolvable at an
appropriate historical date. Any allocation to successor counties requires an
authoritative crosswalk rather than an alias.

### Other county recodes and renames

Miami-Dade/Dade, Ste. Genevieve, and Oglala Lakota/Shannon require sourced
code/name intervals. When the authoritative change record establishes entity
continuity, the old and new codes resolve to one durable entity at their
respective dates. Explicit conversion to the current code is a normalization
request, not ordinary alias resolution.

### NSDUH, CBSA, and TGA

These are source-vintage-specific statistical or program definitions. Their
membership and partial spatial relationships are versioned independently of
the component county entities. Reuse of the same display code across source
vintages does not imply an unchanged footprint.

### ZIP and ZCTA

USPS ZIP Codes and Census ZCTAs are separate concepts and code systems. This
ADR does not enable either dormant dataset or treat them as aliases. Their
product scope remains tracked in
[#9](https://github.com/tfojo1/locations/issues/9).

## Implementation sequence

1. Add normalized schemas, validators, and small committed fixtures without
   changing runtime behavior.
2. Build the reproducible county vertical slice and pinned default date.
3. Import Connecticut crosswalk edges and available area measures.
4. Add temporal read APIs over an immutable store.
5. Generate the existing manager from a deliberate compatibility view.
6. Run package contracts and real JHEEM2 integration coverage.
7. Publish 0.5.0 with the legacy/new-result differences documented.

The implementation must not extend the monolithic build script with a second
implicit temporal model. County ingestion should be the first source-specific
pipeline module.

## Consequences

### Benefits

- Current and historical results become explicit and auditable.
- Code changes no longer imply entity replacement.
- Boundary changes no longer disappear behind a stable code.
- Crosswalk consumers must choose a defensible measure.
- Existing consumers receive an additive migration path.
- The normalized model supports later CBSA, NSDUH, TGA, geometry, and possible
  ZCTA work without redesigning identity again.

### Costs

- The data pipeline and packaged data become larger and more relational.
- Some historic effective dates or footprints will require documented unknowns.
- Compatibility and temporal views must coexist during the 0.x series.
- Population-weighted crosswalks require additional datasets and methodology;
  they cannot be inferred from Census area relationships.
- Runtime registration cannot automatically mutate authoritative temporal data.

## Rejected alternatives

### Keep code as the primary identity and add `valid_from`/`valid_to`

Rejected because a code can remain stable while a boundary changes, and a code
can change while an entity continues.

### Treat every vintage footprint as a completely new location

Rejected because it loses durable entity continuity across ordinary renames,
recodes, and boundary adjustments.

### Resolve every historical code to one current code

Rejected because splits, merges, and Connecticut's replacement are not
one-to-one transformations.

### Store one generic crosswalk weight

Rejected because area overlap and population allocation answer different
questions and can yield materially different results.

### Change legacy defaults in 0.5.0

Rejected because downstream JHEEM2 workflows depend on current signatures and
lookup shapes. The additive API provides corrected behavior without forcing an
unannounced migration.

## Decisions intentionally deferred

- the exact stable-ID encoding, provided IDs are opaque, committed, immutable,
  and deterministic across rebuilds;
- the authoritative county product and reference date to pin for 0.5.0;
- whether a reproducible population-weighted Connecticut crosswalk belongs in
  0.5.0 or a later data release;
- temporal mutation/registration APIs;
- ZIP or ZCTA product scope; and
- the package and curated-data license decisions tracked in issue #3.

## Authoritative references

- U.S. Census Bureau, [Substantial Changes to Counties and County Equivalent
  Entities: 1970-Present](https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2000.html).
- U.S. Census Bureau, [Changes to Counties or County Equivalent Entities:
  2010s](https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes/2010.html).
- U.S. Census Bureau, [2022 Geography
  Changes](https://www.census.gov/programs-surveys/acs/technical-documentation/table-and-geography-changes/2022/geography-changes.html).
- U.S. Census Bureau, [Special 2022 Relationship Files for the New County
  Equivalents in Connecticut](https://www.census.gov/geographies/reference-files/2022/geo/relationship-files.html).
- U.S. Census Bureau, [2020 Comparability Relationship File Record
  Layouts](https://www.census.gov/programs-surveys/geography/technical-documentation/records-layout/2020-comp-record-layout.html).
- U.S. Census Bureau, [Geographic Boundaries, Vintages, and Frequency of
  Updates](https://www.census.gov/content/dam/Census/library/publications/2020/acs/acs_geography_handbook_2020_ch02.pdf).
