
##-----------------------------##
##-----------------------------##
##-- PUBLIC-FACING INTERFACE --##
##-----------------------------##
##-----------------------------##

##-------------##
##-- Getters --##
##-------------##

#'@title get.location.types
#'
#'@description Get a vector of type names, or a list of registered location types, prefixes and longform prefix names.
#'
#'@param simple A boolean; TRUE if asking for simple list of types, FALSE if you want a list of types and prefixes.  Defaults to TRUE
#'
#'@return If simple=TRUE, a simple vector of named types; else a list the same length as the registered number of types, with each list item being the name, the prefix and the longform prefix name.  If there are no registered types it will return an empty list
#'
#'@export
get.location.types <- function(simple=TRUE)
{
  LOCATION.MANAGER$get.registered.types(simple)
}

#'@title get.all.for.type
#'
#'@description Returns a vector of all valid location codes of a certain type
#'
#'@param type A string type name
#'
#'@return if 'type' is a registered type, it will return a vector of all location codes for that type.  If 'type' is not registered, it will return NA
#'
#'@export
get.all.for.type <- function(type)
{
  LOCATION.MANAGER$get.all.type(type)
}

#'@title get.location.name
#'
#'@description Get the Name of a Location
#'
#'@param locations A character vector of location codes
#'
#'@return A character vector of location names, with length(locations) and names=locations. If location codes are not registered (or if they were NA), the corresponding returned name is NA
#'
#'@export
get.location.name <- function(locations)
{
  # How do we handle NAs?
  # we could return NA value
  LOCATION.MANAGER$get.names(locations)
}

#'@title get.location.coords
#'
#'@description Get the latitude and longitude of a location, provided it is known
#'
#'@param locations A character vector of location codes
#'
#'@return A character vector of string location coordinates, separated by commas (latitude first, longitude second), with length(locations) and names=locations. If location codes are not registered (or if they were NA), the corresponding returned name is NA
#'
#'@export
get.location.coords <- function(locations)
{
  # How do we handle NAs?
  # we could return NA value
  LOCATION.MANAGER$get.coords(locations)
}

#'@title get.location.code
#'
#'@description Get the location code for a name and a type
#'
#'@param location.names A list of names to get the location code for
#'@param types A corresponding list of types
#'@param search.aliases A boolean flag indicating whether we want to check the aliases if the name check fails. Default = TRUE
#'
#'@return A list of location.names, with values corresponding to the identified location code.  Multiple results per location.names is possible. If if the location.name is not registered or NA, NA is returned
#'
#'@export
get.location.code <- function(location.names, types, search.aliases = TRUE)
{
  if (length(types) != 1 && length(types) != length(location.names)) {
    stop("get.location.code: Length of types can either be 1 or it must be the length of the location.names")
  }
  if (length(types) == 1) {
    types = rep(types, length(location.names))
  }
  LOCATION.MANAGER$get.codes.from.names(location.names, types, search.aliases)
}

#'@title get.location.code.if.unique
#'
#'@description Get the location code for a name and a type.  The difference between this function and get.location.code
#'             is that get.location.code returns a list with multiple entries, and this function returns a named vector
#'             of results only if the location name matches one location uniquely.  This function uses get.location.code
#'             and converts the output.
#'
#'@param location.names A list of names to get the location code for
#'@param types A corresponding list of types (or one type to be applied to each)
#'@param search.aliases A boolean flag indicating whether we want to check the aliases if the name check fails. Default = TRUE
#'
#'@return A named vector of location.codes, with names corresponding to the location names.  If there are multiple results or no results,
#'        the result in the vector will be NA
#'
#'@export
get.location.code.if.unique <- function(location.names, types, search.aliases = TRUE)
{
  if (length(types) != 1 && length(types) != length(location.names)) {
    stop("get.location.code.if.unique: Length of types can either be 1 or it must be the length of the location.names")
  }
  if (length(types) == 1) {
    types = rep(types, length(location.names))
  }
  list.results <- LOCATION.MANAGER$get.codes.from.names(location.names, types, search.aliases)
  
  #We have a list; convert to a named vector, but only if the list values are unique.
  sapply(list.results, function(x) {
    if (length(x) == 1) {
      return (x)
    } else {
      return (NA)
    }
  })
}

#'@title get.code.by.alias
#'
#'@description Get a location code by its code alias and type
#'
#'@param locations A character vector of location code aliases
#'@param types A single type or a vector of types the same length as locations
#'
#'@return A character vector of true location codes, or NA
#'
#'@export
get.code.by.alias <- function(locations, types)
{
  if (length(types) != 1 || length(types) != length(locations)) {
    stop("get.code.by.alias: types must have a length either 1 or same as locations")
  }
  LOCATION.MANAGER$get.by.alias(locations, types)
}

#'@title get.location.name.alias
#'
#'@description Get an Name alias Associated with a Location
#'
#'@param locations A character vector of location codes
#'@param alias.name A single character value representing a previously registered name alias
#'@param throw.error.if.unregistered.alias A single logical value indicating whether the function should throw an error if no alias with name alias.name has been registered
#'
#'@return A character vector of aliases, with length(locations) and names=locations. If location codes are not registered (or if they were NA), or if no value for the alias.name has been registered to a location, the corresponding returned alias is NA
#'
#'@export
get.location.name.alias <- function(locations, alias.name,
                               throw.error.if.unregistered.alias=T)
{
  if (length(alias.name) != 1) {
    stop("get.location.name.alias: alias.name must be a single name")
  }
  LOCATION.MANAGER$get.name.aliases(locations, alias.name, throw.error.if.unregistered.alias)
}

#'@title get.polygons.for.type
#'
#'@description Return the polygon data for a given location type
#'
#'@param type A single location type
#'
#'@return A data.frame containing all the polygon data for a given type, NA if it doesn't exist
#'
#'@export
get.polygons.for.type <- function(type)
{
  #Type will be checked a level down
  LOCATION.MANAGER$get.polys.for.type(type)
}

#'@title get.location.type
#'
#'@description Get the Type (Geographic Resolution) of a Location
#'
#'@param locations A character vector of location codes
#'
#'@return A character vector of location types, with length(locations) and names=locations. If location codes are not registered (or if they were NA), the corresponding returned type is NA
#'
#'@export
get.location.type <- function(locations)
{
  #No need to check lengths
  LOCATION.MANAGER$get.types(locations)
}

#'@title get.prefix.for.type
#'
#'@description Get the prefix for a given type
#'
#'@param location.types A character vector of location types
#'
#'@return A character vector of prefixes, with length(location.types) and names=prefixes. If the types are not registered (or if they were NA), the corresponding returned type is NA
#'
#'@export
get.prefix.for.type <- function(location.types)
{
  #No need to check lengths
  LOCATION.MANAGER$get.prefix(location.types)
}

#'@title get.overlapping.locations
#'
#'@description Get all locations that overlap a Location
#'
#'@param locations A character vector of location codes
#'@param type The type (geographic resolution) of locations requested for the sub-locations
#'@param return.list A single logical value indicating whether the return value should be a list with one element for each location, or whether all sub-locations should be 'unlisted' into a vector
#'@param throw.error.if.unregistered.type A single logical value indicating whether the function should throw an error if sub.type has not been registered as a location type
#'
#'@return If return.list==T, a list with length(locations) and names=locations. Each element is itself a character vector with zero or more locations corresponding to sub-locations. If return.list=F, returns a character vector (arbitrary length) containing all sub-locations that fall within ANY of the given locations
#'
#'@export
get.overlapping.locations <- function(locations, type,
                                      return.list=F,
                                      throw.error.if.unregistered.type=T)
{
   if (length(type) != 1) {
     stop("get.overlapping.locations: type must be a single character type")
   } 
   if (!is.logical(c(return.list,throw.error.if.unregistered.type))
       || length(c(return.list,throw.error.if.unregistered.type)) != 2) {
     stop("get.overlapping.locations: error in one of the logical types return.list or throw.error.if.unregistered.type")
   }
   LOCATION.MANAGER$get.overlapping(locations, type, return.list, throw.error.if.unregistered.type)
}

#'@title get.contained.locations
#'
#'@description Get Locations that Fall Completely Within a Location
#'
#'@param locations A character vector of location codes
#'@param sub.type The type (geographic resolution) of locations requested for the sub-locations
#'@param return.list A single logical value indicating whether the return value should be a list with one element for each location, or whether all sub-locations should be 'unlisted' into a vector
#'@param throw.error.if.unregistered.type A single logical value indicating whether the function should throw an error if sub.type has not been registered as a location type
#'
#'@return If return.list==T, a list with length(locations) and names=locations. Each element is itself a character vector with zero or more locations corresponding to sub-locations. If return.list=F, returns a character vector (arbitrary length) containing all sub-locations that fall within ANY of the given locations
#'
#'@export
get.contained.locations <- function(locations, sub.type,
                                    return.list=F,
                                    throw.error.if.unregistered.type=T)
{
   if (length(sub.type) != 1) {
     stop("get.contained.locations: sub.type must be a single character type")
   } 
   if (!is.logical(c(return.list,throw.error.if.unregistered.type))
       || length(c(return.list,throw.error.if.unregistered.type)) != 2) {
     stop("get.contained.locations: error in one of the logical types return.list or throw.error.if.unregistered.type")
   }
   LOCATION.MANAGER$get.contained(locations, sub.type, TRUE, return.list, throw.error.if.unregistered.type)
}


#'@title get.containing.locations
#'
#'@description Get Locations that Enclose a Location
#'
#'@param locations A character vector of location codes
#'@param super.type The type (geographic resolution) of locations requested for the super-locations
#'@param return.list A single logical value indicating whether the return value should be a list with one element for each location, or whether all super-locations should be 'unlisted' into a vector
#'@param throw.error.if.unregistered.type A single logical value indicating whether the function should throw an error if super.type has not been registered as a location type
#'
#'@return If return.list==T, a list with length(locations) and names=locations. Each element is itself a character vector with zero or more locations corresponding to super-locations. If return.list=F, returns a character vector (arbitrary length) containing all super-locations that contain ANY of the given locations
#'
#'@export
get.containing.locations <- function(locations, super.type,
                                    return.list=F,
                                    throw.error.if.unregistered.type=T)
{
  if (length(super.type) != 1) {
    stop("get.containing.locations: sub.type must be a single character type")
  } 
  if (!is.logical(c(return.list,throw.error.if.unregistered.type))
      || length(c(return.list,throw.error.if.unregistered.type)) != 2) {
    stop("get.containing.locations: error in one of the logical types return.list or throw.error.if.unregistered.type")
  }
  LOCATION.MANAGER$get.containing(locations, super.type, TRUE, return.list, throw.error.if.unregistered.type)
}

##--------------##
##-- Checkers --##
##--------------##

#'@title is.location.valid
#'
#'@description Check to see if the passed-in value matches any location code or alias
#'
#'@param location A character vector representing potential locations
#'@param suggest.options A boolean indicating whether to check the aliases for potential matches
#'
#'@return A vector of boolean values whether the passed-in value is a location codes.  If false, this function will display a list of possibilities.
#'
#'@export
is.location.valid <- function(locations, suggest.options = F)
{
  # How do we handle NAs?
  # we could return FALSE
  LOCATION.MANAGER$check.many.locations(locations, suggest.options)
}

# We need to map location names to cbsa data

#'@title get.cbsa.for.msa.name
#'
#'@description We are reading data in from .csv files, and these files do not have proper cbsa codes; instead the are indexed by cbsa name.  We need
#'             to take a vector of msa names and return a named vector of cbsa location codes.
#'
#'@param names A vector of msa names to convert into cbsa location codes
#'
#'@return A named vector of string location codes, with NAs where no result was found
#'
#'@export
get.cbsa.for.msa.name <- function(names)
{
  # We want to make the actual LOCATION.MANAGER functionality generic, so 
  # include the known type at this stage (CBSA)
  LOCATION.MANAGER$get.type.by.name(names,"CBSA")
}

#'@title sanitize
#'
#'@description There are values that we will accept as location codes (eg. 'c.12580' will work for Baltimore, but Baltimore's actual code is 'C.12580').
#'             This function converts codes we will accept into codes that are correct.  The function should fail on unrecognized location code.
#'             
#'@param codes A character vector of location codes, or acceptable location codes
#'
#'@return A named character vector of location codes
#'
#'@export
sanitize <- function(codes) 
{
  if (typeof(codes) != "character") {
    stop("sanitize: Codes must be character arrays")
  }
  LOCATION.MANAGER$sanitize.codes(codes)
}

#'@title location.type.comprises
#'
#'@description This function allows us to understand the relationship between two different location types.  Some location types contain other
#'             location types completely (as states contain counties, for instance), while some are not comparable or disjoint in other ways (states and CBSAs).
#'             This function takes two location types (super.location.type and sub.location.type), and returns TRUE or FALSE depending on whether the types overlap 
#'             perfectly or not.
#'
#'@param super.location.type The location type into which we are attempting to arrange another sub-type
#'@param sub.location.type The location type we are trying to fit inside the super-type             
#'
#'@return A Boolean TRUE or FALSE, representing whether members of the sub.location.type fit perfectly in the super.location.type
#'@export
location.type.comprises <- function(super.location.type, sub.location.type) 
{
  #Both super.location.type and sub.location.type must be single length character vectors
  if (typeof(super.location.type) != "character" || length(super.location.type) != 1) {
    stop(paste0("location.type.comprises: super.location.type ", super.location.type, " must be a single length character vector"))
  }
  if (typeof(sub.location.type) != "character" || length(sub.location.type) != 1) {
    stop(paste0("location.type.comprises: sub.location.type ", sub.location.type, " must be a single length character vector"))
  }
  LOCATION.MANAGER$type.composition(super.location.type, sub.location.type)
}

##-------------##
##-- Setters --##
##-------------##

#'@title register.types
#'
#'@description Register location type, prefix, and prefix.longform
#'
#'@param type A character vector representing types to be added
#'@param prefix A character vector of unique prefixes for the location codes for types of this kind
#'@param prefix.longform A character vector of longform names for that particular unique prefixes
#'
#'@details The prefix is restricted to letters, numbers, period and '-'.
#'@export
register.types <- function(type,
                          prefix,
                          prefix.longform)
{
  if (length(type) != length(prefix) || length(prefix) != length(prefix.longform)) {
    stop("register.types: Lengths of the 3 parameters must be equal")
  }
  if (any(c(typeof(type),typeof(prefix),typeof(prefix.longform)) != "character")) {
    stop("register.types: All parameters must be characters/strings")
  }
  LOCATION.MANAGER$register.types(type, prefix, prefix.longform)
}

#'@title register.relationships.between.types
#'
#'@description Register a relationship between location types; by default the types are independent; setting
#'             the value to TRUE indicates that the super.type contains the sub.type completely
#'
#'@param super.type A character vector of super types
#'@param sub.type A character vector of sub types of the same length
#'@param value A vector of boolean values indicating the whether the sub.type is contained completely in the super type, or not
#'
#'@export
register.relationship.between.types <- function(super.type, sub.type, value) 
{
  if (length(super.type) != length(sub.type) || length(sub.type) != length(value)) {
    stop("register.relationship.between.types: Lengths of the 3 parameters must be equal")
  }
  if (any(c(typeof(super.type),typeof(sub.type)) != "character")) {
    stop("register.relationship.between.types: All types must be characters/strings")
  }
  if (typeof(value) != "logical") {
    stop("register.relationship.between.types: value vector must be bool")
  }
  LOCATION.MANAGER$register.type.relationship(super.type, sub.type, value)
}

#'@title register.locations
#'
#'@description Register information about locations.  
#'
#'@param type The geographic resolution at which to register the locations. Can be be either a single character value, or a vector of the same length as locations
#'@param locations A character vector of location codes 
#'@param location.names A character vector of the same length as locations with corresponding names
#'
#'@details There is no error checking here; we assume that multiple locations with the same name are possible at different resolutions/types.
#'@export
register.locations <- function(type,
                               locations,
                               location.names)
{
  if (length(type) != 1 && length(type) != length(location.names)) {
    stop("register.locations: Length can either be 1 or it must be the length of the locations")
  }
  if (length(locations) != length(location.names)) {
    stop("register.locations: The length of the codes and the names must be equal")
  }
  
  # Repeat the type as many times as needed
  if (length(type) == 1) {
    type = rep(type, length(locations))
  }
  
  LOCATION.MANAGER$register(type, location.names, locations)
}

#'@title register.name.aliases
#'
#'@description Register name aliases for specific location  
#'
#'@param location A single, previously registered location code or a registered location code alias.
#'@param location.aliases A character vector of aliases for this location name
#'@param location.alias.names A character vector names for the particular alias 'short','no spaces', 'full', etc
#'
#'@details There is no error checking here; we assume that multiple locations with the same name are possible at different resolutions/types.
#'@export
register.name.aliases <- function(location = NA,
                             location.aliases = NA,
                             location.aliases.names = NA)
{
  if (anyNA(c(location,location.aliases))) {
    stop("register.name.aliases: NA values not allowed for location or location.aliases")
  }
  if (length(location.aliases) != length(location.aliases.names)) {
    stop("register.name.aliases: You must provide a name for each alias")
  }
  if (length(location) > 1) {
    stop("register.name.aliases: You can only provide one location code at a time")
  }
  
  LOCATION.MANAGER$register.name.aliases(location, location.aliases, location.aliases.names)
}

#'@title register.code.aliases
#'
#'@description Register location code aliases for specific location  
#'
#'@param location A single, previously registered location code or a registered location code alias.
#'@param location.aliases A character vector of location code aliases for this location name.  Code aliases are unique by type
#'
#'@export
register.code.aliases <- function(location = NA,
                                  location.aliases = NA)
{
  if (anyNA(c(location,location.aliases))) {
    stop("register.code.aliases: NA values not allowed for location or location.aliases")
  }
  if (length(location) > 1) {
    stop("register.code.aliases: You can only provide one location code at a time")
  }
  
  LOCATION.MANAGER$register.code.aliases(location, location.aliases)
}

#'@title register.lat.and.long
#'
#'@description Register a valid latitude and longitude for a location code
#'
#'@param location A previously registered location code, or a registered location code alias.
#'@param lat A single numeric value between -90 and 90
#'@param long A single numeric value between -180 and 180
#'
#'@export
register.lat.and.long <- function(location = NA, lat = NA, long = NA)
{
  if (anyNA(c(location,location.aliases))) {
    stop("register.code.aliases: NA values not allowed for location or location.aliases")
  }
  if (length(location) > 1) {
    stop("register.code.aliases: You can only provide one location code at a time")
  }
  #Check the lat and long
  # The latitude must be a number between -90 and 90 and the longitude between -180 and 180.
  # The first condition checks if they are any non numeric, and will short circuit.  The subsequent
  # conditions assume that the first condition is false; ie that lat and long are numbers
  if (!all(is.numeric(c(lat,long))) || (lat < -90 || lat > 90 ) || (long < -180 || long > 180)) {
    stop(paste0("Invalid lat (", lat, "), or long (", long, ") for ", location, ", not set"))
  }
  
  LOCATION.MANAGER$register.lat.long(location, lat, long)
}

#'@title register.sub.and.super.locations
#'
#'@description Register hierarchical sub-super relationships
#'
#'@param sub.locations A character vector of locations codes/location code aliases
#'@param super.locations A character vector of location codes/location code aliases of the same length as sub.locations, with corresponding super.locations
#'@param super.completely.encloses.sub Either a single logical value or a vector the same length as sub.locations and super.locations, indicating whether the location is completely enclosed
#'
#'@details Where super.completely.encloses.sub==T, the function will automatically recognize that locations completely enclosed within the given sub.locations are also completely enclosed within the corresponding super.locations, and, conversely that locations which completely enclose the given super.locations also completely enclose the corresponding sub.locations
#'
#'@export
register.sub.and.super.locations <- function(sub.locations,
                                             super.locations,
                                             super.completely.encloses.sub)
{
  if (length(sub.locations) != length(super.locations)) {
    stop("register.sub.and.super.locations: We must have the same number of sub locations and super locations")
  }
  if (length(super.completely.encloses.sub) > 1 && length(super.completely.encloses.sub) != length(sub.locations)) {
    stop("register.sub.and.super.locations: The length of super.completely.encloses.sub must be either 1 or the same length as sub.locations")
  }
  if (length(super.completely.encloses.sub) == 1) {
    super.completely.encloses.sub = rep(super.completely.encloses.sub,length(sub.locations))
  }
  LOCATION.MANAGER$register.hierarchy(sub.locations, super.locations, super.completely.encloses.sub) 
}

# #'@title combine.locations
# #'
# #'@description Will create a new location composed of sub locations; if these sub locations have polygons, it will merge these polygons 
# #'             into a unioned polygon.  We will need to register the type of this new location before this function is called.
# #'
# #'@param sub.locations A character vector of location codes we wish to combine into a new location.  Will fail on unknown codes.
# #'@param new.location.code The location code for the new location we are creating, without the type prefix
# #'@param new.location.name The location name for the new location we are creating.
# #'@param type The type of the new location which has been previously registered
# #'
# #'@export
# combine.locations <- function(sub.locations, new.location.code, new.location.name, type)
# {
#   if (length(new.location.code) != 1) {
#     stop("combine.locations: new location code must be a single value")
#   }
#   if (length(new.location.name) != 1) {
#     stop("combine.locations: new location name must be a single value")
#   }
#   LOCATION.MANAGER$combine.locations.into.new.location(sub.locations, new.location.code, new.location.name, type)
# }