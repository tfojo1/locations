# Architecture Guide

This document explains the internal design of the `locations` package for maintainers and advanced users.

## Overview

The package manages US geographic location data using a singleton R6 object (`LOCATION.MANAGER`) that holds all locations, hierarchical relationships, aliases, and polygon data in memory. The public API (exported functions in `location_api.R`) delegates to this singleton. Internally, locations are stored in a flat data.frame with environment-based hash maps for fast lookup.

## Source Files

| File | Purpose |
|------|---------|
| `R/location_api.R` | Public API functions (thin wrappers around LOCATION.MANAGER methods) |
| `R/location_manager.R` | `Location.Manager` R6 class with all data storage and query logic |
| `R/location_init.R` | `build_location_manager()` and `extract_location_data()` for serialization |
| `R/location_plot.R` | `location.plot()` for geospatial visualization (requires ggmap) |
| `R/zzz.R` | `.onLoad()` hook that rebuilds LOCATION.MANAGER on package load |
| `code_for_building_package/set_up_cached_location_manager.R` | Build script that reads raw data and constructs the location manager |
| `code_for_building_package/run_before_pushing_installable_package_code_to_github.R` | Extracts data to `sysdata.rda` and generates docs |
| `data-raw/code_aliases.csv` | CT and SD FIPS code alias mappings |

## Data Flow

### Build Time

The build script runs once to prepare the cached data that ships with the package:

```
Raw CSV files (data-raw/)
  |
  v
Build script (set_up_cached_location_manager.R)
  - Reads FIPS codes, gazetteer data, CBSA mappings, polygon data
  - Calls register.*() methods to populate LOCATION.MANAGER
  - Calls register.code.aliases.from.csv() for CT/SD aliases
  |
  v
extract_location_data(LOCATION.MANAGER)  -->  .location_data (plain lists + data.frames)
  |
  v
usethis::use_data(.location_data, internal = TRUE)  -->  R/sysdata.rda
```

**Important**: The build script must save `sysdata.rda` BEFORE running `devtools::document()`, because `document()` triggers `.onLoad()` which needs the data to exist.

### Load Time

When a user loads the package, the data is rebuilt into a live R6 object:

```
R/sysdata.rda  -->  .location_data (loaded automatically by R)
  |
  v
.onLoad() in zzz.R
  |
  v
build_location_manager(.location_data)
  - Creates fresh Location.Manager instance
  - Populates locations_df, hash indexes, relationship maps
  - Restores aliases, polygon data, type matrix
  |
  v
LOCATION.MANAGER (live R6 object, assigned to package namespace)
```

This lazy initialization pattern avoids serializing R6 objects directly. R6 objects capture closures with environment references that become stale when deserialized in a new R session.

## Internal Data Structures

All data lives inside `Location.Manager`'s private fields:

### Location Storage

```r
private$locations_df    # data.frame with columns: code, name, type, lat, long, has_poly
                        # One row per location (~6,000 rows)
```

### Hash Map Indexes

R environments with `hash = TRUE, parent = emptyenv()` provide O(1) key lookup:

```r
private$code_index      # code -> integer row index in locations_df
                        # e.g., code_index[["MD"]] returns 42

private$type_index      # type -> character vector of all codes of that type
                        # e.g., type_index[["STATE"]] returns c("AL", "AK", ...)
```

### Relationship Maps

Four hash maps store the hierarchical graph edges:

```r
private$contains_complete       # parent -> children (fully enclosed)
private$contains_partial        # parent -> children (partially overlapping)
private$contained_by_complete   # child -> parents (fully enclosed by)
private$contained_by_partial    # child -> parents (partially overlapping with)
```

For example, `contains_complete[["MD"]]` returns the vector of all county codes completely within Maryland.

### Other Structures

```r
private$types           # named list: type name -> c(prefix, prefix.longform)
private$type.matrix     # boolean matrix: type.matrix["STATE","COUNTY"] == TRUE means states contain counties
private$alias.codes     # nested list: alias.codes[["COUNTY"]][["09001"]] == "09110"
private$alias.names     # nested list: alias.names[["MD"]][["short"]] == "MD"
private$compressed.poly.data  # list: type -> memCompress'd serialized polygon data.frame
private$poly.index      # integer: next polygon ID to assign
```

## Key Algorithms

### Code Resolution (`resolve.code`)

When a user passes a location code, it goes through this resolution chain:

1. Uppercase the input
2. Check `code_index` for direct match
3. Check `alias.codes` across all types for alias match
4. If still not found, try adding each registered type prefix and re-check
5. Return the canonical code or NA

The `sanitize()` function uses this same resolution to normalize user input.

### Hierarchical Queries (BFS)

`get.contained()` and `get.containing()` use breadth-first search over the relationship hash maps:

```
get.contained("MD", "COUNTY"):
  1. Start with queue = [MD]
  2. Pop MD, look up contains_complete[["MD"]] -> [24001, 24003, 24005, ...]
  3. For each child not yet visited, add to queue
  4. Continue until queue is empty
  5. Filter visited set by requested type (COUNTY)
  6. Return matching codes
```

If `limit.to.completely.enclosing = FALSE`, the search also follows partial containment edges after exhausting complete containment.

### Overlapping Queries

`get.overlapping()` combines contained and containing results, plus finds locations that share sub-locations (counties) with the query location. This handles cases like CBSAs that cross state boundaries.

## Adding a New Location Type

To add a new location type to the package data:

1. **Prepare data**: Create or identify a CSV with codes and names
2. **Edit the build script** (`set_up_cached_location_manager.R`):
   ```r
   # Register the type
   LOCATION.MANAGER$register.types("MYTYPE", "MT.", "MyType.")

   # Register type relationships
   LOCATION.MANAGER$register.type.relationship("STATE", "MYTYPE", TRUE)  # if states contain this type

   # Register locations
   LOCATION.MANAGER$register(rep("MYTYPE", n), names_vector, codes_vector)

   # Register hierarchical relationships
   LOCATION.MANAGER$register.hierarchy(sub_codes, super_codes, completely_enclosed)

   # Optionally add coordinates, aliases, polygon data
   ```
3. **Rebuild**: Run the build script, then the pre-push script
4. **Add tests**: Add test cases in `tests/testthat/`

For runtime registration (user-side, not baked into the package), use the public API functions (`register.types()`, `register.locations()`, etc.) as shown in the README.

## Known Limitations

- **Global mutable singleton**: All code shares one `LOCATION.MANAGER`. No isolation for testing or parallel use. Accepted tradeoff for API simplicity.
- **No data validation**: Relationships aren't checked for cycles, alias uniqueness isn't enforced across types (known bug in comments), polygon data can be registered for nonexistent locations.
- **Inconsistent NA handling**: Scalar getters (`get.location.type`, `get.location.name`) return `NA` for unknown locations. Relationship functions (`get.contained.locations`, etc.) return `character(0)`. Pre-existing behavior.
- **Name normalization hardcoded**: Special cases like "Saint Louis" -> "St. Louis" and dash normalization are in R code, not data-driven.
- **Build script ordering matters**: Some registration calls depend on others having run first (e.g., aliases must be registered after the locations they point to). The ordering is correct but not enforced programmatically.

## History

| When | What |
|------|------|
| Pre-2025 | Original development by Jeff Pennington (JHU) |
| Dec 2025 | Ownership transfer. Registration bug found and fixed (lazy initialization pattern) |
| Feb 2026 | Incremental modernization: testthat suite, aliases to CSV, ggmap to Suggests |
| Feb 2026 | Phase 1 refactor: eliminated Location R6 class, replaced with data.frames + hash maps, BFS traversal |
