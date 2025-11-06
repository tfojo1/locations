
##--------------------------------------------------------------------------------------------##
##--                                     OVERVIEW                                           --##
##--------------------------------------------------------------------------------------------##
##
## Source this file prior to pushing an 'installable' package up to GitHub
## It will take care of updating documentation and internally-cached data structures
##
## This depends on packages
## - usethis
## - devtools
## Which do NOT have to be imported by the package itself (just used to set up the package)
##
##--------------------------------------------------------------------------------------------##


##---------------------------------------------------------##
##-- STEP 1: Build and Store the Cached Location Manager --##
##---------------------------------------------------------##
## NOTE: This must happen BEFORE documentation, because devtools::document()
##       loads the package, which triggers .onLoad(), which needs the data.

cat("\n------------------------------\nBUILDING LOCATION MANAGER...\n")

# Source the initialization functions (needed for extract_location_data)
source('R/LOCATIONS_impl.R')
source('R/LOCATIONS_init.R')

# Build the location manager (creates LOCATION.MANAGER)
source('code_for_building_package/set_up_cached_location_manager.R')

# Extract the data from LOCATION.MANAGER into simple structures
cat("Extracting location data from LOCATION.MANAGER...\n")
.location_data <- extract_location_data(LOCATION.MANAGER)

# Store the data structures (not the R6 object) to internal file
# Note: using .location_data (with dot prefix) to indicate it's internal
usethis::use_data(.location_data, internal = T, overwrite = T)

cat("DONE BUILDING LOCATION MANAGER\n------------------------------\n")


##-----------------------------------------------------------##
##-- STEP 2: Update Documentation (updates NAMESPACE file) --##
##-----------------------------------------------------------##

cat("\n-----------------------------\nSETTING UP DOCUMENTATION...\n")
devtools::document(quiet=T)
cat("DONE SETTING UP DOCUMENTATION\n-----------------------------\n")


##-----------------------------------------------------------------##
##-- STEP 3: Indicate Which Files the Package should NOT include --##
##-----------------------------------------------------------------##

cat("\n------------------------------------\nSETTING FILES TO IGNORE IN BUILD...\n")
usethis::use_build_ignore(files=c("^R\\tests$",
                                  "^data-raw$",
                                  "^\\.github$",
                                  "^.*\\.Rproj$"),
                          escape=F)
cat("DONE SETTING FILES TO IGNORE IN BUILD\n------------------------------------\n")


cat("\n\n ALL DONE PREPARING PACKAGE TO BE INSTALLED\n")