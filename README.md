# locations 

Location manager for the **jheem2** project.  Adds in locations referenced by the
model, and their super and sub locations.  Types of locations currently in the
module are:

- COUNTRY : United States as a whole
- STATE : All individual state in the continental US
- COUNTY : US State counties
- CBSA : Core-based statistical areas
- NSDUH : National survey on Drug Use
- PHD : State-level public health divisions (currently only AL and LA)
- ZIPCODE : Support exists in the model for zipcodes but currently isn't loaded into the module.

Also includes support for plotting the locations; polygon data exists for all states, counties,
cbsas and phd level types.  We also have polygon data for zipcodes but it isn't loaded into
the module.

Please see the Roxygen2 documentation for public API information.

## Recent Changes

**v0.2.0** (2025-01) - Location Registration Fix
- Fixed bug preventing dynamic registration of new location types
- Users can now register custom location types and locations at runtime
- Backward compatible - all existing code works unchanged
- Internal: Refactored to use lazy initialization pattern
