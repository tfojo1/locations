# Connecticut County Crosswalk and Legacy Migration

Connecticut's eight former counties and nine planning regions are different
geographic footprints. A former county code therefore cannot be treated as an
alias for one planning-region code.

The normalized temporal county store models them as distinct entities and
contains 19 overlap edges directed from the former county versions to the 2022
planning-region versions. The build derives those edges reproducibly from the
U.S. Census Bureau's
[2022 County Subdivision to 2020 Block Groups for Connecticut relationship file](https://www2.census.gov/geo/docs/maps-data/data/rel2022/acs22_cousub22_blkgrp20_st09.txt).
The first five digits of the 2020 block-group GEOID identify the former county;
the first five digits of the 2022 county-subdivision GEOID identify the new
planning region. The pipeline sums the published overlap-part areas for each
pair and verifies that the parts reproduce both endpoint totals.

## Direction and measures

Every edge is `former county -> planning region`, has `relation_kind =
"overlap"`, and is marked `coverage = "exhaustive"`.

Each edge has two independently typed measurements:

- `land_area`, derived from `AREALAND_PART`; and
- `water_area`, derived from `AREAWATER_PART`.

Areas and denominators are square meters. `numerator` is the overlap area and
`denominator` is the total area of the source former county for that measure.
Consequently `fraction_of_from` allocates a former-county footprint across
planning regions and sums to one per former county and measure.
`fraction_of_to` is the same overlap divided by the planning-region total and
sums to one per planning region and measure.

The Census relationship product contains no population or housing counts, so
the package does not create a population measure and never substitutes an area
fraction for one. A later population crosswalk would require a separately
identified dataset, universe, vintage, and derivation method.

## Compatibility schedule

The additive temporal API does not alter an existing exported function,
default, or return shape. In particular, the legacy dot-named API continues to
resolve the eight former county codes one-to-one during the 0.5.x migration
period. Those mappings are isolated in `data-raw/legacy_code_aliases.csv`; they
are not aliases in the normalized temporal store and must not be used as
geographic weights.

Use temporal resolution to inspect the historic record and the crosswalk API
to transform its geographic footprint:

```r
hartford <- resolve_location("09003")
stopifnot(hartford$preferred_name == "Hartford County")

targets <- crosswalk_locations("09003", measure = "land_area")
targets[, c("to_code", "to_name", "fraction_of_from")]
stopifnot(abs(sum(targets$fraction_of_from) - 1) < 1e-8)
```

`crosswalk_locations()` is directional and never follows the legacy mapping.
For example, former Hartford County overlaps Capitol, Naugatuck Valley, and
Northwest Hills planning regions; it does not resolve to the legacy
one-to-one target Greater Bridgeport.

The migration schedule follows
[ADR 0001](adr/0001-temporal-geography-model.md):

1. Version 0.5.0 publishes the additive temporal read and crosswalk APIs
   without changing legacy defaults or adding legacy warnings.
2. A later 0.x release adds one classed warning per session for unsafe legacy
   Connecticut resolution, with a documented batch-migration opt-out.
3. Version 1.0.0 or later may remove the one-to-one compatibility mappings only
   after at least one warning release. Former codes remain available as
   historical records through the temporal APIs.
