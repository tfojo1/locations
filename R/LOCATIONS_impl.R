library(R6)
library(purrr)

Location <- R6Class("Location",
  class = FALSE,
  clone = FALSE,
  public = list(
    initialize = function(args) {
      private$name <- args[1]
      private$type <- args[2]
      private$contains <- list()
      private$contained_by <- list()
      private$lat <- NA
      private$long <- NA
      private$poly <- NA
    },
    set.lat.and.long = function(lat, long) {
      private$lat <- lat
      private$long <- long
    },
    set.poly.data = function(poly.data) {
      private$poly <- poly.data
    },
    register.sub.location = function (sub.code, enclose.completely) {
      # Register one sub location at a time
      # we will already have the vector size due to error checking higher up
      private$contains <- append (private$contains, list(c(sub.code,enclose.completely)))
    },
    register.super.location = function (super.code, enclose.completely) {
      # Register one sub location at a time
      # we will already have the vector size due to error checking higher up
      private$contained_by <- append (private$contained_by, list(c(super.code,enclose.completely)))
    },
    return.sub.locations = function( contain.completely ) {
      # returns a character vector of contained location codes
      # if contain.completely is true, we only return those entries are completely
      # if it is false, we only return those that are partially contained.
      # contained (second value is true)
      if (contain.completely) {
        sapply(private$contains[as.logical(sapply(private$contains,"[[", 2))], "[[", 1)
      } else {
        sapply(private$contains[!as.logical(sapply(private$contains,"[[", 2))], "[[", 1)
      }
    },
    return.super.locations = function( contain.completely ) {
      # returns a character vector of locations containing this location code
      # if contain.completely is true, we only return those entries are completely
      # if it is false, we only return those that are partially contained.
      # contained (second value is true)
      if (contain.completely) {
        sapply(private$contained_by[as.logical(sapply(private$contained_by,"[[", 2))], "[[", 1)
      } else {
        sapply(private$contained_by[!as.logical(sapply(private$contained_by,"[[", 2))], "[[", 1)
      }
    }
  ),
  active = list(
    return.type = function() {
      private$type
    },
    contains.list = function() {
      private$contains
    },
    contained_by.list = function() {
      private$contained_by
    },
    return.name = function() {
      private$name
    },
    return.lat = function() {
      private$lat
    },
    return.long = function() {
      private$long
    },
    has.poly.data = function() {
      return (!is.na(private$poly))
    },
    return.poly.data = function() {
      private$poly
    }
  ),
  private = list(  type = NULL, # vector of characters
                   name = NULL, # string
                   contains = NULL, # list of vector pairs c("token","BOOL"),
                   contained_by = NULL, # list of vector pairs c("token","BOOL"),
                   lat = NULL, # Valid Latitude data for mapping if known
                   long = NULL, # Valid Longitude data for mapping if known
                   poly = NULL # Polygon data if known
                   # where BOOL is a textual repr. of boolean
                   # values, where the value is TRUE if the
                   # current location completely encases
                   # the contained region.  The token field
                   # is the token of the contained region
                 )
)

Location.Manager = R6Class("LocationManager",
  class = FALSE,
  clone = FALSE,
  active = list (
    get.type.matrix = function() { #FIXME
      return(private$type.matrix)
    },
    read.location.list = function() {
      return(private$location.list)
    }
  ),
  private = list (
    location.list = list(),
    alias.names = list(),
    alias.codes = list(),
    types = list(),
    type.matrix = matrix(nrow = 0, ncol = 0),
    check.is.type = function (type) {
      if (type %in% names(private$types)) {
        return (TRUE)
      }
      return (FALSE)
    },
    check.code.validity = function(code) {
      #We allow only characters, numbers, periods or dashes
      grepl("^[A-Za-z0-9.-]*$", code)
    },
    check.location = function (location, suggest.options) {
      location <- toupper(location)
      
      #If it's a location code, return.
      if (location %in% names(private$location.list)) {
        return (TRUE)
      }
      
      if (suggest.options) {
        # Is it an alias?
        # Get the current list of types:
        types = names(private$alias.codes)
        
        #For each type:
        for (type in types) {
          # browser()
          if (location %in% names(private$alias.codes[[type]])) {
            #Found as alias to type 'type'
            #Display results
            location.code <- private$alias.codes[[type]][[location]]
            cat(sprintf("Possible Result : %s alias of location %s, %s\n", type, location.code, self$get.names(location.code)))
          }
        }
      }
      return (FALSE)
    },
    resolve.code = function(code,fail.on.unknown=T) {
      
      #Resolves a single location code from potential alias to actual code
      #If fail.on.unknown is FALSE, the result is NA
      
      #Capitalize
      code <- toupper(code)
      
      #could this function be vectorized?
      
      if (!code %in% names(private$location.list)) {
        #The code is not in the list, check the code alias list.
        if (fail.on.unknown) {
          stop(paste0("LOCATION.MANAGER: The location code used (",code,") cannot be recognized, stopping"))
        } else {
          code <- NA
        }
      }
      code
    },
    trim.white.space = function(x) {
      gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
    },
    normalize.dashes = function(x) {
      x <- gsub("\u2013", "-", x)  # Replace En Dash
      x <- gsub("\u2014", "-", x)  # Replace Em Dash
      x <- gsub("\u2010", "-", x)  # Replace Hyphen
      x <- gsub("\u2011", "-", x)  # Replace Non-Breaking Hyphen
      return(x)
    },
    normalize.names = function(val) {
      vals = private$normalize.dashes(val)
      state.map= list("OHIO" = "OH", 
                      "N MEX" = "NM", 
                      "NMEX" = "NM",
                      "TEX" = "TX", 
                      "CALIF" = "CA", 
                      "ALA" = "AL", 
                      "IDAHO" = "ID",
                      "MASS" = "MA",
                      "NHA" = "NH",
                      "CONN" = "CT",
                      "FLA" = "FL",
                      "ILL" = "IL",
                      "IND" = "IN",
                      "WIS" = "WI",
                      "COLO" = "CO",
                      "IOWA" = "IA",
                      "MICH" = "MI",
                      "HAWAII" = "HI",
                      "MISS" = "MS",
                      "KANS" = "KS",
                      "TENN" = "TN",
                      "NEV" = "NV",
                      "ARK" = "AR",
                      "MINN" = "MN",
                      "OKLA" = "OK",
                      "NEB" = "NE",
                      "NEBR" = "NE",
                      "DEL" = "DE",
                      "ARIZ" = "AZ",
                      "MAINE" = "ME",
                      "ORE" = "OR",
                      "OREG" = "OR",
                      "WASH" = "WA",
                      "UTAH" = "UT")
      sapply(strsplit(vals,","), function(name) {
        # Name, State(s),
        #check the length, should be 2
        if (length(name) == 2) {
          #Keep the name the same (we have already normalized dashes),
          #except when we encounter Saint Louis, which is swapped to St. Louis
          name[1] <- sub("(Saint)(.*Louis)", "St.\\2", name[1])
          #special case for strange New Haven, CT name
          name[1] <- sub("N Havn","New Haven", name[1])

          #Check if we need to replace states
          #Convert the state to uppercase, remove all periods and extra space, split on '-'
          state.split = trimws(toupper(gsub("\\.","",strsplit(name[2], "-")[[1]])))
          #Replace any non-standard state with the standard 2 digit code
          state.replace = sapply(state.split, function(res) {
            if (res %in% names(state.map)) {
              return (state.map[res])
            }
            return (res)
          })
          #Collapse it all back together with the '-' as separator
          state.final = paste(state.replace, collapse = "-")
          #Return the full name
          return (paste0(name[1], ", ", state.final))
        } else if (length(name) != 1)  {
          warning(paste0("An additional split on ", as.vector(name)))
        } else {
          # Length of split was 1, no comma
          # Sometimes the state is the last element of the spaces
          name = unlist(strsplit(name, " (?=[^ ]*$)", perl = TRUE))
        }
        #If there weren't two splits, join everything back up and send it back
        return (paste(name, collapse=","))
      })
    }
  ),
  public = list (
    initialize = function () {
      #Already initialized
    },
    sanitize.codes = function(codes) {
      # Function should error if we get an unrecognized code
      clean.codes = unlist(lapply(codes,private$resolve.code))
      setNames(clean.codes,codes)
    },
    type.composition = function(super.location.type, sub.location.type) {
      #Return a true or false, checking the composition of types
      if (super.location.type %in% rownames(private$type.matrix)) {
        if (sub.location.type %in% rownames(private$type.matrix)) {
          return(private$type.matrix[super.location.type, sub.location.type])
        } else {
          stop(paste0("LOCATION.MANAGER$type.composition: Couldn't find type ", sub.location.type, ", aborting"))
        }
      } else {
        stop(paste0("LOCATION.MANAGER$type.composition: Couldn't find type ", super.location.type, ", aborting"))
      }
    },
    get.coords = function(locations) {
      # return A character vector of string location coordinates, separated by commas, with length(locations) and names=locations. 
      # If location codes are not registered (or if they were NA), the corresponding returned name is NA
      returned.coords = unlist(lapply(locations, function(x){private$resolve.code(x,F)}))
      returned.coords[!is.na(returned.coords)] = unlist(lapply(returned.coords[!is.na(returned.coords)], function(x) {
        lat = private$location.list[[x]]$return.lat
        long = private$location.list[[x]]$return.long
        if (any(is.na(c(lat,long)))) {
          return (NA)
        } else {
          return(paste(lat,long,sep=","))
        }
      }))
      names(returned.coords) = locations
      returned.coords
    },
    get.polygon = function(location) {
      #Return the polygon data for a valid location
      clean.code = private$resolve.code(location,F)
      if (is.na(clean.code)) {
        return (NA)
      }
      #Get the poly data 
      poly.data = private$location.list[[clean.code]]$return.poly.data
      if (length(poly.data) == 1 && is.na(poly.data)) {
        return (NA)
      }
      return (poly.data)
    },
    get.names = function(locations) {
      # return A character vector of location names, with length(locations) and names=locations. If location codes are not registered (or if they were NA), 
      # the corresponding returned name is NA
      
      #Resolve the location codes, preserving NAs and missing codes as NAs
      returned.names = unlist(lapply(locations,function(x){private$resolve.code(x,F)})) #Now contains the fully resolved location codes or NAs
      
      #Set those non-na results to the appropriate name
      returned.names[!is.na(returned.names)] = unlist(lapply(returned.names[!is.na(returned.names)], function(x) { private$location.list[[x]]$return.name }))
      
      #We set the names to the original locations
      names(returned.names) = locations
    
      returned.names
      
    },
    get.codes.from.names = function(location.names, types, search.aliases) {
      mapply(function(location, type) {
        if (is.na(location)) {
          return (NA)
        }
        if (location %in% names(private$location.list) && private$location.list[[location]]$return.type == type) {
          return (location)
        }
        #Build a list of all names of 'type'
        all.of.type = sapply(private$location.list, function(li) {ifelse(li$return.type == type,li$return.name,"")})
        
        #Find the indexes that match the location
        indexes = which (all.of.type == location)
        if (length(indexes) == 0) {
          indexes = grep(location, all.of.type)
        }
        
        rv = names(private$location.list)[indexes]
        
        if (length(rv) == 0) {
          if (search.aliases) {
            # If not found in location list, check the alias codes
            if (location %in% names(private$alias.codes[[type]])) {
              return (private$alias.codes[[type]][[location]])
            }
          }
          return (NA)
        } else {
          return (rv)
        }
      }, location.names, toupper(types), SIMPLIFY = FALSE)
    },
    get.by.alias = function(aliases, types) {
      #Sizes are checked a level up; either they match or types has a length of 1.
      types = if (length(types) == 1) rep(types,length(aliases)) else types
      
      setNames(mapply(function(alias, type) {
        if (alias %in% names(private$alias.codes[[type]])) {
          return (private$alias.codes[[type]][alias])
        } else {
          return (NA)
        }
      }, toupper(aliases), toupper(types), SIMPLIFY=T), aliases)
    },
    get.types = function(locations) {
      #return A character vector of location types, with length(locations) and names=locations. If location codes are not registered 
      #(or if they were NA), the corresponding returned type is NA
      
      #Resolve the location codes, preserving NAs and missing codes as NAs
      returned.types = unlist(lapply(locations,function(x){private$resolve.code(x,F)})) #Now contains the fully resolved location codes or NAs
      
      #Set those non-na results to the appropriate type
      returned.types[!is.na(returned.types)] = unlist(lapply(returned.types[!is.na(returned.types)], function(x) { private$location.list[[x]]$return.type }))
      
      #We set the names to the original locations
      names(returned.types) = locations
      
      returned.types
    },
    get.type.by.name = function(names, chosen.type, warnings = T) {
      chosen.type = toupper(chosen.type)
      #Verify that the type is registered
      if (!private$check.is.type(chosen.type)) {
        stop(paste0("Calling get.type.from.name with invalid type ", chosen.type))
      }
      sapply(private$normalize.names(names), function(name) {
        #Split the strings on commas, remove everything in brackets, trim
        split = private$trim.white.space(gsub("\\(.*?\\)", "", strsplit(name,",")[[1]]))
        #Stage1: Assume that the first component of the split is the name
        #Check the name against get.codes.from.names; search aliases too
        name.check = self$get.codes.from.names(split[1], chosen.type, T)[[1]]
        if (is.na(name.check[1])) {
          # No such luck from get.codes.from.names; try and split again on the
          # dash and slash and take the first result, see if that helps
          
          second.split = private$trim.white.space(strsplit(split[1], "[-/]+")[[1]])
          name.check = self$get.codes.from.names(second.split[1], chosen.type, T)[[1]]
          if (is.na (name.check[1])) {          
            #Skip everything with the string 'division' in it
            if (!grepl("division",name,ignore.case=T)) {
              msg = paste0("Couldn't find anything for ", name, ", even after '-' split")
              if (warnings) {
                warning(msg)
              } else {
                print(msg)
              }
            }
            return (NA)
          }
        }
        if (length(name.check) == 1) {
          #We have only one result, return it
          return (name.check)
        } else {
          #We have multiple results, check the state
          additional.splits = length(split) - 1
          if (additional.splits == 0) {
            #There was no comma, we don't know the state
            msg = paste0("Muiltiple results for ", name, " without additional data")
            if (warnings) {
              warning(msg)
            } else {
              print (msg)
            }
            return (NA)
          } else if (additional.splits == 1) {
            #There is one additional part of the string after the name, suppose it's the state
            state = split[2]
            #For each result, check to see if its containing state matches the desired state
            index = which(sapply(name.check, function(possibility) {
              self$get.containing(possibility, "STATE", T)
            }) == state)
            return (name.check[index])
          } else {
            msg = paste0("More than 2 divisions for ", name)
            if (warnings) {
              warning(msg)
            } else {
              print (msg)
            }
            return (NA)
          }
        }
      })
    },
    get.prefix = function(location.types) {
  
      location.types <- toupper(location.types)
      #A character vector of prefixes, with length(location.types) and names=prefixes. 
      #If the types are not registered (or if they were NA), the corresponding returned type is NA
      
      rv = lapply(seq_along(location.types), function(index) {
        if (location.types[index] %in% names(private$types)) {
          return (private$types[[location.types[index]]][1])
        } else {
          return (NA)
        }
      })
      
      names(rv) = location.types
      rv
    },
    check.many.locations = function(locations, suggest.options) {
      rv = sapply(locations, function(l) {
        return (private$check.location(l,suggest.options))
      })
      names(rv) = locations
      return (rv)
    },
    get.contained = function(locations, sub.type, limit.to.completely.enclosing, return.list = F, throw.error.if.unregistered.type = T) {
      #return If return.list==T, a list with length(locations) and names=locations. Each element is itself a character vector
      #with zero or more locations corresponding to sub-locations. If return.list=F, returns a character vector
      #(arbitrary length) containing all sub-locations that fall within ANY of the given locations
    
      #Capitalize the type
      sub.type = toupper(sub.type)
    
      if (throw.error.if.unregistered.type) {
        #Check the type against the type list;
        if (!sub.type %in% names(private$types)) {
          stop(paste0("LOCATION.MANAGER$get.contained: Type ", sub.type," not registered, aborting"))
        }
      }
    
      #Resolve the location codes, preserving NAs and missing codes as NAs
      codes = unlist(lapply(locations,function(x){private$resolve.code(x,F)})) #Now contains the fully resolved location codes or NAs
    
      #Here I need create sub locations with the following logic:
    
      #We need to do the fully enclosed search in both cases
      #Here we ask each location code for a list of the regions it contains completely.  Then we ask each of those location codes for a list of the regions it contains
      #completely, and so on until the list we had previously is equal to our current list.
    
      #Start with collecting all fully enclosed from each location
      location.contained.collector = function(location, fully.contained) {
        if (is.na(location)) {
          return(NA)
        }
        #TRUE in this case set the boolean contain.completely
        rv = private$location.list[[location]]$return.sub.locations(fully.contained)
        if (length(rv) == 0 || identical(rv,character(0))) {
          return(NA)
        }
        rv
      }
    
      all.sub.locations = lapply(codes,function(x) {location.contained.collector(x, TRUE)})
    
      #For each code in each vector, get their list of fully contained regions
      fully.contained.children = function(locations) {
        #locations is a vector of location codes
        unlist(lapply( locations, function(x) {location.contained.collector(x,TRUE)} ))
      }
    
      #TWO LOOPS
      repeat {
        next.sub.locations = lapply(all.sub.locations, fully.contained.children)
        try.again = FALSE
    
        #LOOP FIXME
        for (i in seq_along(all.sub.locations)) {
          #If the length of the union of the current sub locations and the new sub locations are of different sizes,
          #then the new sub locations has something to add to all.sub.locations
    
          if (length(union(all.sub.locations[[i]], next.sub.locations[[i]])) != length(all.sub.locations[[i]])) {
            diff = setdiff(next.sub.locations[[i]], all.sub.locations[[i]])
            if (length(na.omit(diff)) != 0) {  #There are NA's sneaking in here and I don't know how as of yet. FIXME
              all.sub.locations[[i]] = c(all.sub.locations[[i]],
                                         na.omit(diff))
              #We still have locations returning
              try.again= TRUE
            }
          }
        }
        if (!try.again) {
          break;
        }
      }
    
      #print("Before adding")
      #print(all.sub.locations)
      if (!limit.to.completely.enclosing) {
        #We want fully and partially enclosed lists
        #We ask each location for a list of those places it partially includes, add them on to the list
        partially.contained.children = function(locations) {
          unlist(lapply(locations, function(x) {location.contained.collector(x,FALSE)}))
        }
        # Add the location itself to the locations to check for partially.contained.children
        all.sub.locations = mapply(function(x, code) c(code, x), all.sub.locations, codes, SIMPLIFY = FALSE)
        
        partially.contained = lapply(all.sub.locations, partially.contained.children)
        
        #Now we have to add these lists into the main lists and then unique the whole thing:
        
        #LOOP FIXME
        for (i in seq_along(all.sub.locations)) {
          all.sub.locations[[i]] = unique(c(all.sub.locations[[i]], na.omit(partially.contained[[i]])))
        }
      } 
      #all.sub.locations is a list, each entry contains a list of things this location contains, from top to bottom
      #this includes locations of all types.  The types are later filtered with the mask.collector algorithm.
      #print(all.sub.locations)
    
      mask.collector = function (locations) {
        if (anyNA(locations)) {
          return (NA)
        }
        unlist(lapply(locations, function(location) { private$location.list[[location]]$return.type == sub.type} ))
      }
    
      #for each list of contained locations, check to make sure they correspond to the correct type
      sub.types = lapply(all.sub.locations, mask.collector)
    
      count = length(all.sub.locations)
    
      #LOOP FIXME
      for (i in 1:count) {
        #apply the mask
        if (length(sub.types[[i]]) != 0) {
          all.sub.locations[[i]] = all.sub.locations[[i]][sub.types[[i]]]
          #I confess I don't quite understand why I need this here below, but I do
          if (identical(all.sub.locations[[i]],character(0))) {
            all.sub.locations[[i]] = NA
          }
        }
      }
    
      for (i in 1:length(all.sub.locations)) {
        names(all.sub.locations[[i]]) =  sapply(all.sub.locations[[i]], function(loc_id) get.location.name(loc_id))
      }
    
      #Now sub.locations is a proper list; if we want a list returned, return it now
      if (return.list) {
        return (all.sub.locations)
      }
    
      #Return a collapsed vector of valid entries for all locations
      rv = unname(unlist(lapply(all.sub.locations, function (l) {
        l[!is.na(l)]
      })))
    
      if (length(rv) == 0) {
        return (character())
      }
    
      setNames(rv, sapply(rv, function(loc_id) get.location.name(loc_id)))
    },
    get.overlapping = function(locations, type, return.list = F, throw.error.if.unregistered.type = T) {
      #return If return.list==T, a list with length(locations) and names=locations. Each element is itself a character vector
      #with zero or more locations corresponding to sub-locations. If return.list=F, returns a character vector
      #(arbitrary length) containing all sub-locations that fall within ANY of the given locations
    
      #Capitalize the type
      type = toupper(type)
    
      if (throw.error.if.unregistered.type) {
        #Check the type against the type list;
        if (!type %in% names(private$types)) {
          stop(paste0("LOCATION.MANAGER$get.overlapping: Type ", super.type," not registered, aborting"))
        }
      }
    
      #Resolve the location codes, preserving NAs and missing codes as NAs
      codes = unlist(lapply(locations,function(x){private$resolve.code(x,F)})) #Now contains the fully resolved location codes or NAs
    
      # Now we can call both the contained and containing functions 
      contained.results = self$get.contained(codes, type, FALSE, return.list, throw.error.if.unregistered.type)
      containing.results = self$get.containing(codes, type, FALSE, return.list, throw.error.if.unregistered.type)
    
      # Will work for both lists and vectors  
      return (c(contained.results, containing.results))
    },
    get.containing = function(locations, super.type, limit.to.completely.enclosing, return.list = F, throw.error.if.unregistered.type = T) {
      #return If return.list==T, a list with length(locations) and names=locations. Each element is itself a character vector
      #with zero or more locations corresponding to sub-locations. If return.list=F, returns a character vector
      #(arbitrary length) containing all sub-locations that fall within ANY of the given locations
    
      #Capitalize the type
      super.type = toupper(super.type)
    
      if (throw.error.if.unregistered.type) {
        #Check the type against the type list;
        if (!super.type %in% names(private$types)) {
          stop(paste0("LOCATION.MANAGER$get.containing: Type ", super.type," not registered, aborting"))
        }
      }
    
      #Resolve the location codes, preserving NAs and missing codes as NAs
      codes = unlist(lapply(locations,function(x){private$resolve.code(x,F)})) #Now contains the fully resolved location codes or NAs
    
      #We need to do the fully enclosed search in both cases
      #Here we ask each location code for a list of the regions that contain it completely.
      # Then we ask each of those location codes for a list of the regions that contain them
      #completely, and so on until the list we had previously is equal to our current list.
    
      #Start with collecting all fully enclosed from each location
      location.contained_by.collector = function(location, fully.contained) {
        if (is.na(location)) {
          return(list())
        }
        #TRUE in this case set the boolean contain.completely
        rv = private$location.list[[location]]$return.super.locations(fully.contained)
    
        if (length(rv) == 0 || identical(rv,character(0))) {
          return(list())
        }
        rv
      }
    
      all.super.locations = lapply(codes,function(x) {location.contained_by.collector(x, TRUE)})
    
      #For each code in each vector, get their list of parents who fully contain them
      fully.contained.parents = function(locations) {
        #locations is a vector of location codes
        unlist(lapply( locations, function(x) {location.contained_by.collector(x,TRUE)} ))
      }
    
      #TWO LOOPS
      repeat {
        next.super.locations = lapply(all.super.locations, fully.contained.parents)
        try.again = FALSE
    
        #LOOP FIXME
        for (i in seq_along(all.super.locations)) {
          #If the length of the union of the current super locations and the new super locations are of different sizes,
          #then the new super locations has something to add to all.super.locations
    
          if (length(union(all.super.locations[[i]], next.super.locations[[i]])) != length(all.super.locations[[i]])) {
            diff = setdiff(next.super.locations[[i]], all.super.locations[[i]])
            if (length(na.omit(diff)) != 0) {  #There are NA's sneaking in here and I don't know how as of yet. FIXME
              all.super.locations[[i]] = c(all.super.locations[[i]],
                                         na.omit(diff))
              #We still have locations returning
              try.again= TRUE
            }
          }
        }
        if (!try.again) {
          break;
        }
      }
      
      if (!limit.to.completely.enclosing) {
        #We want fully and partially enclosed lists
        #We ask each location for a list of those places that partially include it, add them on to the list
        partially.contained.parents = function(locations) {
          unlist(lapply(locations, function(x) {location.contained_by.collector(x,FALSE)}))
        }
        # Add the location itself to the locations to check for partially.contained.parents
        all.super.locations = mapply(function(x, code) c(code, x), all.super.locations, codes, SIMPLIFY = FALSE)
        
        partially.contained = lapply(all.super.locations, partially.contained.parents)
        
        #Now we have to add these lists into the main lists and then unique the whole thing:
        
        #LOOP FIXME
        for (i in seq_along(all.super.locations)) {
          all.super.locations[[i]] = unique(c(all.super.locations[[i]], na.omit(partially.contained[[i]])))
        }
      }
      
      mask.collector = function (locations) {
        if (anyNA(locations)) {
          return (NA)
        }
        unlist(lapply(locations, function(location) { private$location.list[[location]]$return.type == super.type} ))
      }
    
      #for each list of contained locations, check to make sure they correspond to the correct type
      super.types = lapply(all.super.locations, mask.collector)
    
      count = length(all.super.locations)
    
      #LOOP FIXME
      for (i in 1:count) {
        #apply the mask
        if (length(super.types[[i]]) != 0) {
          all.super.locations[[i]] = all.super.locations[[i]][super.types[[i]]]
          #I confess I don't quite understand why I need this here below, but I do
          if (identical(all.super.locations[[i]],character(0))) {
            all.super.locations[[i]] = NA
          }
        }
      }
    
      for (i in 1:length(all.super.locations)) {
        names(all.super.locations[[i]]) =  sapply(all.super.locations[[i]], function(loc_id) get.location.name(loc_id))
      }
    
      #Now sub.locations is a proper list; if we want a list returned, return it now
      if (return.list) {
        return (all.super.locations)
      }
    
      #Return a collapsed vector of valid entries for all locations
      rv = unname(unlist(lapply(all.super.locations, function (l) {
        l[!is.na(l)]
      })))
    
      if (length(rv) == 0) {
        return (character())
      }
    
      setNames(rv, sapply(rv, function(loc_id) get.location.name(loc_id)))
    },
    get.name.aliases = function(locations, alias.name, throw.error.if.unregistered.alias) {
      # return A character vector of aliases, with length(locations) and names=locations. If location codes are not registered
      # (or if they were NA), or if no value for the alias.name has been registered to a location, the corresponding returned alias is NA
    
      #Resolve the location codes, preserving NAs and missing codes as NAs
      codes = unlist(lapply(locations,function(x){private$resolve.code(x,F)})) #Now contains the fully resolved location codes or NAs
    
      type.collector = function(location) {
        if (is.na(location)) {
          return (NA)
        }
        private$location.list[[location]]$return.type
      }
    
      #We need to get the type for the location codes
      types = unlist(lapply(codes,type.collector))
      #print(LOCATION.MANAGER$alias.names[types])
      result = unlist(lapply(mapply(c,codes, types,SIMPLIFY=F), function(vars) {
        l = vars[[1]]
        t = vars[[2]]
        n = names(private$alias.names[[t]])
        #n is a list of all aliases of that type
        #LOOP FIXME
        for (x in seq_along(n)) {
          #For every n
          #If the alias.name matches and the location matches
          if (private$alias.names[[t]][[n[x]]][[2]] == alias.name
              && l == private$alias.names[[t]][[n[x]]][[1]]) {
            return(n[x])
          }
        }
        #We didn't find a match for this alias.name, type and location combination
        if (throw.error.if.unregistered.alias) {
          stop(paste0("LOCATION.MANAGER$get.name.aliases: Unrecognized alias combination :", alias.name, ", ", l, ", ", t))
        } else {
          return(NA)
        }
      }))
      names(result) = locations
    
      result
    },
    register.type.relationship = function (super.type, sub.type, b) {
      #Lengths have been checked a level up, they all match
      iterations = length(super.type)
      for (i in 1:iterations) {
        if (super.type[i] %in% rownames(private$type.matrix)) {
          if (sub.type[i] %in% rownames(private$type.matrix)) {
            private$type.matrix[super.type[i], sub.type[i]] = b
          } else {
            stop(paste0("LOCATION.MANAGER$register.type.relationship: Couldn't find type ", sub.type[i], ", aborting"))
          }
        } else {
          stop(paste0("LOCATION.MANAGER$register.type.relationship: Couldn't find type ", super.type[i], ", aborting"))
        }
      }
    },
    register.types = function (type, prefix, prefix.longform) {
      #Sizes have been checked a step up
      type <- toupper(type)
      prefix <- toupper(prefix)
    
      invisible(Map(function(t, p, p.l) {
        #Check validity of the prefix
        if (!private$check.code.validity(p)) {
          stop("LOCATION.MANAGER$register.types: We are not allowed characters outside of the letters, numbers, . and -")
        }
        #Add the type into the types list as a list item type = c(prefix,prefix.longform)
        if (!t %in% names(private$types)) {
          #append(LOCATION.MANAGER$types, list(t = c(p, p.l)), 1)
          private$types[[t]] = c(p, p.l)
          #Add entry for the aliases to the code alias object
          private$alias.codes[[t]] = list()
          #Add a column and a row for the type.matrix relationship object.  Default to FALSE
          if (nrow(private$type.matrix) == 0) {
            private$type.matrix = matrix(FALSE,nrow = 1, ncol = 1)
            rownames(private$type.matrix) = t
            colnames(private$type.matrix) = t
          } else {
            matrix.values <- rep(FALSE, nrow(private$type.matrix) + 1)
            # Add the new row and column
            suppressWarnings ({
              #I'm getting miss-sized binding here, but it sets the value as I want it
              #And I'd rather not copy the matrix every time
              private$type.matrix <- rbind(private$type.matrix, matrix.values)
              private$type.matrix <- cbind(private$type.matrix, matrix.values)
            })
            
            # Update the row and column names
            new.names <- c(rownames(private$type.matrix)[-length(rownames(private$type.matrix))], t)
            rownames(private$type.matrix) <- new.names
            colnames(private$type.matrix) <- new.names
            
          }
          
        } else {
          # the type name already exists
          stop(paste0("LOCATION.MANAGER$register.types: Type ",t, " already exists in the system, aborting"))
        }
      }, type, prefix, prefix.longform))
    },
    get.registered.types = function (simple) {
      #No parameters; return a list of currently registered types, their longform prefix name and their prefix
      if (!simple) {
        lapply(names(private$types), function(name) {
          list("TypeName" = name, "TypePrefix" = private$types[[name]][1], "TypeLongform" = private$types[[name]][2])
        })
      } else {
        return(names(private$types))
      }
    },
    get.all.type = function(type) {
      type = toupper(type)
      if (!type %in% names(private$types)) {
        # This type isn't valid, return
        return (NA)
      }
      # Find all those location codes that have this type
      match_type = sapply(private$location.list, function(obj) { return (obj$return.type == type) })
      # Return a vector of all of their location.codes
      return (names(private$location.list)[match_type])
    },
    get.aliases.for.type = function(type) {
      return (private$alias.codes[[toupper(type)]])
    },
    register = function (types, location.names, codes) {
      #codes and types are all uppercase; case insensitive
      codes <- toupper(codes)
      types <- toupper(types)
    
      #First we need to check if the type is registered
      if (!all(types %in% names(private$types))) {
        stop("LOCATION.MANAGER$register: Type not previously registered - ",
             types[which(is.na(match(types,names(private$types))))[1]])
      }
    
      #Add Prefixes depending on type
      #TODO Check if the prefix is already added to the location code
      codes = sprintf("%s%s", sapply(private$types[types], function(x) x[1]), codes)
    
      #Check that this code doesn't already exist
      if (any( codes %in% names(private$location.list) )) {
        stop("LOCATION.MANAGER: Attempting to add a code that already exists in the manager")
      }
    
      #Check that this code doesn't contain undesirable characters
      if (!all(private$check.code.validity(codes) )) {
        stop("LOCATION.MANAGER: Attempting to enter a code with invalid characters (only letters, numbers, . and - allowed")
      }
    
      private$location.list [ codes ]<- lapply(mapply(c,location.names,types,SIMPLIFY=F), Location$new)
    
    },
    register.name.aliases = function(code, name.aliases, names.alias.labels) {
    
      #Sizes have already been checked up one level
      code <- private$resolve.code(code)  
      
      #Get the type of the location
      location.type = private$location.list[[code]]$return.type
      
      # If there isn't an existing list, create it
      if (!location.type %in% names(private$alias.names)) {
        private$alias.names[[location.type]] = list()
      } else {
        # If the list already exists, if any of the to-assign name aliases are already included in the type, abort
        if ( any (name.aliases %in% names(private$alias.names[[location.type]]))) {
          stop(paste0("LOCATION.MANAGER: Attempting to add an alias for type ", location.type, " that already exists."))
        }
      }
      private$alias.names[[location.type]][name.aliases] = mapply(c,rep(code,length(names.alias.labels)),names.alias.labels,SIMPLIFY=F)
    },
    register.code.aliases = function(code, code.aliases) {
      
      # We have ONE code and one or more code.aliases
      #Sizes have already been checked up one level
      code <- private$resolve.code(code)
      code.aliases <- toupper(code.aliases)
      
      #Get the name of the type to be aliased
      location.type <- private$location.list[[code]]$return.type
      
      #Verify that none of the location code aliases are currently in use
      if ( any( code.aliases %in% names(private$alias.codes[[location.type]]))) {
        stop("LOCATION.MANAGER: One of the location code aliases are already registered")
      }
      
      #Assign the aliases
      private$alias.codes[[location.type]][code.aliases] = code
    },
    register.lat.long = function(code, lat, long) {
      # We have one location value, and valid lat and long
      valid.code <- private$resolve.code(code, F)
      if (is.na(valid.code)) {
        warning(paste0("Code ", code, " not found, lat and long not set"))
      } else {
        private$location.list[[valid.code]]$set.lat.and.long(lat,long)
      }
    },
    register.polygons = function(code, poly.data) {
      valid.code <- private$resolve.code(code, F)
      if (is.na(valid.code)) {
        warning(paste0("Code ", code, " not found, polygon data not set"))
      } else {
        private$location.list[[valid.code]]$set.poly.data(poly.data)
      }
    },
    register.hierarchy = function(sub, super, fully.contains, fail.on.unknown = T) {
    
      #Sizes have already been checked up one level
      #We now have three vectors of equal length
    
      if (fail.on.unknown) {
      #Check the location codes/aliases for both sub and super
        sub = unlist(lapply(sub,private$resolve.code))
        super = unlist(lapply(super,private$resolve.code))
        #LOOP FIXME
        #mapply and function { ?
        for (i in seq_along(sub)) {
          # Register the sub location
          private$location.list[[super[i]]]$register.sub.location(sub[i],fully.contains[i])
          # Register the super location
          private$location.list[[sub[i]]]$register.super.location(super[i],fully.contains[i])
        }
      } else {
        sub = unlist(lapply(sub, function(x) { private$resolve.code(x,F)} ))
        super = unlist(lapply(super,function(x) { private$resolve.code(x,F)} ))
        #LOOP FIXME
        #mapply and function { ?
        for (i in seq_along(sub)) {
          if (!any(is.na(c(super[i],sub[i])))) {
            #Verify that neither address is NA before we attempt to register the sub location
            #Skip those that are not registered yet.
            # Register the sub location
            private$location.list[[super[i]]]$register.sub.location(sub[i],fully.contains[i])
            # Register the super location
            private$location.list[[sub[i]]]$register.super.location(super[i],fully.contains[i])
          }
        }
      }
    }
  )
)