# Legacy County Compatibility View

The legacy `COUNTY` manager is generated from the normalized temporal county
store. It no longer ingests `fips_codes.csv` or `new_fips_codes.csv` as a
second county inventory.

The projection deliberately preserves the installed package's consumer
boundary:

1. Begin with every official Census county GEOID in the temporal store.
2. Exclude the eight former Connecticut codes that ADR 0001 retains only as
   legacy dot-API aliases.
3. For each code, select the record active on the store's pinned default date.
   If no record is active, select its most recent historical record. This rule
   disambiguates reused code `02230` in favor of current Skagway while retaining
   the 19 legacy-only Alaska and Montana records.
4. Sort the baseline codes and then apply the 29-entry legacy tail order.
5. Apply the 33 legacy display-label exceptions.
6. Derive state containment from the first two GEOID digits, then join the
   existing coordinate and polygon sources by the generated code set.

The only county-specific compatibility metadata is
`data-raw/legacy_county_compatibility.csv`: 29 tail-order entries and 33 label
exceptions, with overlap between those groups. Official spellings and
diacritics remain available through the temporal API; the legacy labels remain
unchanged for existing consumers.

The former full inventories remain in `data-raw` as dormant provenance and
migration-audit artifacts. They are not manager-build inputs.

`code_for_building_package/validate_legacy_county_compatibility.R` rebuilds the
entire manager and requires exact equality with the packaged legacy data. The
comparison decompresses polygon objects first because bzip2 storage bytes are
not portable across platforms; the polygon data returned to consumers remains
exact. CI therefore protects codes, registration order, names, aliases,
containment and other relationships, coordinates, and polygon behavior—not
just record counts.
