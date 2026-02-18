# locations

Hierarchical US geographic location manager for the [jheem2](https://github.com/tfojo1/jheem2) project. Provides fast lookups, hierarchical queries, code aliasing, and geospatial plotting across multiple geographic resolutions.

## Installation

```r
devtools::install_github("tfojo1/locations")
```

## Location Types

| Type | Description | Prefix | Example Code | Example Name |
|------|-------------|--------|--------------|--------------|
| COUNTRY | United States | | `US` | United States |
| STATE | US states (continental + territories) | | `MD` | Maryland |
| COUNTY | Counties (FIPS codes) | | `24005` | Baltimore County |
| CBSA | Core-based statistical areas | `C.` | `C.12580` | Baltimore-Columbia-Towson, MD |
| NSDUH | National Survey on Drug Use regions | | | |
| PHD | Public health districts (AL, LA) | | | |
| ZIPCODE | ZIP codes (registered but not loaded) | `Z.` | | |

## Quick Start

```r
library(locations)

# Look up location info
get.location.type(c("MD", "24005", "C.12580"))
#>       MD    24005  C.12580
#>  "STATE" "COUNTY"   "CBSA"

get.location.name(c("MD", "24005"))
#>                   MD              24005
#>         "Maryland"   "Baltimore County"

# Get all locations of a type
states <- get.all.for.type("STATE")  # 59 states/territories

# Get counties within a state
md_counties <- get.contained.locations("MD", "COUNTY")  # 24 counties

# Get counties for multiple states at once
county_list <- get.contained.locations(c("MD", "CA"), "COUNTY", return.list = TRUE)
# Returns a named list: county_list[["MD"]], county_list[["CA"]]

# Find which state a county belongs to
get.containing.locations("24005", "STATE")
#> Maryland
#>     "MD"

# Find locations that overlap (e.g., which states does a CBSA span?)
get.overlapping.locations("C.12580", "STATE")

# Check if a type fully contains another
location.type.comprises("STATE", "COUNTY")   # TRUE
location.type.comprises("STATE", "CBSA")     # FALSE (CBSAs cross state lines)
```

### Code Validation

```r
# Sanitize converts acceptable codes to canonical form (uppercases, resolves aliases)
sanitize(c("md", "c.12580"))
#>        md   c.12580
#>      "MD" "C.12580"

# Check if codes are valid
is.location.valid(c("MD", "FAKE123"))
#>      MD FAKE123
#>    TRUE   FALSE

# Look up a location code by name
get.location.code("Baltimore County", "COUNTY")
```

## Code Aliases

Some FIPS codes have changed over time. The package handles this transparently:

- **Connecticut** (2022): 8 historic counties reorganized into 9 planning regions. Old codes (09001, 09003, ...) automatically resolve to new codes (09110, 09120, ...).
- **South Dakota**: County 46113 (Shannon) renamed to 46102 (Oglala Lakota) in 2015.

```r
# Old CT code resolves to new code automatically
sanitize("09001")
#> 09001
#> "09110"
```

## Plotting

The `location.plot()` function creates maps with points or filled polygons. Requires the `ggmap` package and a [Stadia Maps API key](https://stadiamaps.com/).

```r
# install.packages("ggmap")  # if not already installed
# Sys.setenv(STADIA_MAPS_API_KEY = "your-key-here")

# Create a data frame with a 'locations' column and value columns
plot_data <- data.frame(
  locations = get.contained.locations("MD", "COUNTY"),
  value = runif(24)
)

# Plot as filled polygons
location.plot(plot_data, color = "value", fill = "value", bb = "AUTO")

# Plot as sized points (requires locations with lat/lon data)
location.plot(plot_data, color = "value", fill = "value", size = "value", bb = "AUTO")
```

## Runtime Registration

You can register custom location types and locations at runtime:

```r
# Register a new type
register.types("REGION", "R.", "Region.")

# Register locations of that type
register.locations("REGION", c("R.NORTHEAST", "R.SOUTH"), c("Northeast", "South"))

# Register hierarchical relationships
register.sub.and.super.locations(
  sub.locations = c("MD", "VA"),
  super.locations = c("R.SOUTH", "R.SOUTH"),
  super.completely.encloses.sub = TRUE
)

# Query works immediately
get.contained.locations("R.SOUTH", "STATE")
```

## Recent Changes

**v0.3.0** (2026-02) - Internal Modernization
- Replaced internal Location R6 objects with data.frame + hash map storage
- Hierarchical queries now use BFS traversal (was recursive)
- Removed dead code, cleaned up dependencies
- Added 54 tests (testthat edition 3)
- Moved ggmap to Suggests (faster package load)
- Source files renamed for clarity
- See [ARCHITECTURE.md](ARCHITECTURE.md) for technical details

**v0.2.0** (2025-12) - Location Registration Fix
- Fixed bug preventing dynamic registration of new location types
- Internal: refactored to use lazy initialization pattern
