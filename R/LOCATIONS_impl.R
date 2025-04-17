library(R6)
library(sf) #used for merging the polygons in the case of groups value

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
      private$poly <- FALSE
    },
    set.lat.and.long = function(lat, long) {
      private$lat <- lat
      private$long <- long
    },
    set.poly.data = function() {
      private$poly <- TRUE
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
      return (private$poly)
    },
    has.lat.lon.data = function() {
      return(!is.null(private$long))
    }
  ),
  private = list(  type = NULL, # vector of characters
                   name = NULL, # string
                   contains = NULL, # list of vector pairs c("token","BOOL"),
                   contained_by = NULL, # list of vector pairs c("token","BOOL"),
                   # where BOOL is a textual repr. of boolean
                   # values, where the value is TRUE if the
                   # current location completely encases
                   # the contained region.  The token field
                   # is the token of the contained region
                   lat = NULL, # Valid Latitude data for mapping if known
                   long = NULL, # Valid Longitude data for mapping if known
                   poly = NULL # Boolean; TRUE if poly data is contained in LOCATION.MANAGER
                 )
)

Location.Manager = R6Class("LocationManager",
  class = FALSE,
  clone = FALSE,
  active = list (
    get.type.matrix = function() {
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
    compressed.poly.data = list(),
    poly.index = 0,
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
      
      #If it's an alias
      location.codes = c()
      
      for (type in names(private$alias.codes)) {
        if (location %in% names(private$alias.codes[[type]])) {
          location.codes = c(location.codes, private$alias.codes[[type]][[location]])
        }
      }
      
      if (length(location.codes) == 1) {
        # This code is an alias for a single location value, return TRUE
        return (TRUE)
      }
      
      
      if (suggest.options) {
        # Is it an alias?
        # Get the current list of types:
        types = names(private$alias.codes)
        
        #For each type:
        for (type in types) {
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
      
      if (!code %in% names(private$location.list)) {
        #The code is not in the list, check the code alias list.
        #Collect all the known type names
        types = names(private$alias.codes)
        alias.target = character() # A vector list of potential aliases
        
        #For each type, check if an alias for this code exists
        for (type in types) {
          if (code %in% names(private$alias.codes[[type]])) {
            #Store results in alias.target, for later comparison
            alias.target <- c(alias.target,private$alias.codes[[type]][[code]])
            
            #Break; assuming one alias per code.  This fails the
            #unique within types for alias requirement, BUG 
            #TODO
          }
        }
        
        result.length = length(alias.target)
        
        if (result.length != 0) {
          # We found at least something
          if (result.length == 1) {
            # We found exactly one thing
            code <- alias.target
          } else {
            # We found more than one entry for an alias across types
            # This currently doesn't work, as it would require us to
            # have the potential type of the code beforehand.  Will
            # have to consider workarounds.  Code aliases are across all types
            stop(paste0("LOCATION.MANAGER: BUG - Currently code aliases are unique across all types ",alias.target))
          }
        } else {
          # Our alias search came up empty
          if (fail.on.unknown) {
            stop(paste0("LOCATION.MANAGER: The location code used (",code,") cannot be recognized, stopping"))
          } else {
            code <- NA
          }
        }
        
      }
      code
    },
    trim.white.space = function(x) {
      gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
    },
    normalize.dashes = function(x) {
      # Replace En Dash, Replace Em Dash
      # Replace Hyphen,  Replace Non-Breaking Hyphen
      return(gsub("[\u2010\u2011\u2013\u2014]", "-", x, perl=TRUE))
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
    number.polygons = function(df) {
      # Initialize the "poly" column
      df$poly <- rep(private$poly.index, nrow(df))
      
      # Variables to store the first point of the current polygon
      current_lat <-  df$latitude[1]
      current_long <- df$longitude[1]
      
      poly.reset = FALSE
      
      # Iterate through rows to increment "poly" when a new polygon starts
      # Here we are counting polygons in the set; some states have multiple polygons
      # and geom_polygon has a group= feature that allows you to group by polygon.
      # So we are numbering the polygons here
      for (i in 2:nrow(df)) {
        df$poly[i] = private$poly.index
        
        if (poly.reset) {
          current_lat = df$latitude[i]
          current_long = df$longitude[i]
        }
        
        if (df$latitude[i] == current_lat && df$longitude[i] == current_long && !poly.reset) {
          #This is the end of a polygon
          if (i != nrow(df)) {
            # This isn't the last polygon in the data.frame, increment the poly counter
            # If it is the last one, the polygon count will be increased at the end of the function
            private$poly.index = private$poly.index + 1
          }
          poly.reset = TRUE
        } else {
          poly.reset = FALSE
        }
      }
      
      # Increase poly.index by one for the next polygon
      private$poly.index = private$poly.index + 1
      
      return (df)
    },
    add.poly.data = function(type, dataframe) {
      private$compressed.poly.data[[type]] = memCompress(serialize(dataframe, NULL), type = "bzip2")
    },
    add.to.poly.data = function(type, dataframe) {
      if (is.null(private$compressed.poly.data[[type]])) {
        # There is no data here yet:
        self$add.poly.data(type, dataframe)
      } else {
        # Uncompress what is there
        current_poly_data_type = unserialize(memDecompress(private$compressed.poly.data[[type]], type = "bzip2"))
        # Add the new data to what is already there
        current_poly_data_type = rbind(current_poly_data_type, dataframe)
        # Compress the full data and assign it back to the variable
        private$compressed.poly.data[[type]] = memCompress(serialize(current_poly_data_type, NULL), type = "bzip2")
      }
    },
    get.polys.for.type = function(type) {
      # Will return NA if there is no poly data for this type, or if the type doesn't exist
      if (!is.null(private$compressed.poly.data[[type]])) {
        return (unserialize(memDecompress(private$compressed.poly.data[[type]], type = "bzip2")))
      }
      return (NA) 
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
    has.polygon = function(location) {
      clean.code = private$resolve.code(location,F)
      if (is.na(clean.code)) {
        return (NA)
      }
      return (private$location.list[[clean.code]]$has.poly.data)
    },
    has.lat.lon = function(location) {
      clean.code = private$resolve.code(location,F)
      if (is.na(clean.code)) {
        return (NA)
      }
      return (private$location.list[[clean.code]]$has.lat.lon.data)
    },
    has.location = function(location) {
      clean.code = private$resolve.code(location,F)
      return (!is.na(clean.code))
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
      types = toupper(types)
      #Sizes are checked a level up; either they match or types has a length of 1.
      types = if (length(types) == 1) rep(types,length(aliases)) else types
      
      setNames(mapply(function(alias, type) {
        if (alias %in% names(private$alias.codes[[type]])) {
          return (private$alias.codes[[type]][alias])
        } else {
          return (NA)
        }
      }, toupper(aliases), types, SIMPLIFY=T), aliases)
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
              returned.states = self$get.containing(possibility, "STATE", F)
              any(returned.states %in% state) 
            }) == TRUE)
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
    
      # For get.contained, make sure that the location itself is added
      all.sub.locations = mapply(function(x, code) c(code, x), all.sub.locations, codes, SIMPLIFY = FALSE)
      
      if (!limit.to.completely.enclosing) {
        #We want fully and partially enclosed lists
        #We ask each location for a list of those places it partially includes, add them on to the list
        partially.contained.children = function(locations) {
          unlist(lapply(locations, function(x) {location.contained.collector(x,FALSE)}))
        }
        
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
        unlist(lapply(locations, function(location) { 
          if (is.na(location)) {
            return (NA)
          } else {
            return (private$location.list[[location]]$return.type == sub.type)
          }
        }))
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
        names(all.sub.locations[[i]]) =  sapply(all.sub.locations[[i]], function(loc_id) self$get.names(loc_id))
      }
    
      #Now sub.locations is a proper list; if we want a list returned, return it now
      if (return.list) {
        return (setNames(all.sub.locations, codes))
      }
    
      #Return a collapsed vector of valid entries for all locations
      rv = unname(unlist(lapply(all.sub.locations, function (l) {
        l[!is.na(l)]
      })))
    
      if (length(rv) == 0) {
        return (character())
      }
    
      setNames(rv, sapply(rv, function(loc_id) self$get.names(loc_id)))
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
          stop(paste0("LOCATION.MANAGER$get.overlapping: Type ", type," not registered, aborting"))
        }
      }
    
      #Resolve the location codes, preserving NAs and missing codes as NAs
      codes = unlist(lapply(locations,function(x){private$resolve.code(x,F)})) #Now contains the fully resolved location codes or NAs
    
      result = lapply (codes, function(code) { 
        # Now we can call both the contained and containing functions 
        # For containing, we are looking for all entries that overlap at all with the source location, as they will
        # overlap for sure.  For contained, we are looking for exactly contained, as overlapping can be over
        # another section.
        contained.results = self$get.contained(code, type, TRUE, FALSE, throw.error.if.unregistered.type)
        containing.results = self$get.containing(code, type, FALSE, FALSE, throw.error.if.unregistered.type)
      
        combined.cont = c(contained.results, containing.results)
        
        # We have some situations where they share underlying locations (counties)
        
        # Get all counties that are partially contained by the location
        list.of.counties.in.locations = self$get.contained(code, "COUNTY", FALSE, FALSE, throw.error.if.unregistered.type)
        
        # Get a list() of all locations of type 'type', each list item containing all counties that they contain
        list.of.all.by.type = self$get.all.type(type)
        all.counties.for.types = setNames(
                                   lapply(
                                     list.of.all.by.type, 
                                     function(id) self$get.contained(id, "COUNTY", FALSE, FALSE, throw.error.if.unregistered.type)
                                   ), 
                                 list.of.all.by.type)
        
        matches = sapply(all.counties.for.types, function(ids) any(ids %in% list.of.counties.in.locations))
        
        matching.locations = names(matches[matches==TRUE])
        
        if (length(matching.locations) > 0) {
          names(matching.locations) = self$get.names(matching.locations)
        }
        
        combined = c(matching.locations,combined.cont)
        
        combined = combined [!duplicated(combined)]
        
        return (combined)
      })
      
      if (return.list) {
        return (setNames(result, codes))
      }
      
      #Return a collapsed vector of valid entries for all locations
      rv = unname(unlist(lapply(result, function (l) {
        l[!is.na(l)]
      })))
    
      if (length(rv) == 0) {
        return (character())
      }
      
      rv = rv [!duplicated(rv)]
    
      setNames(rv, sapply(rv, function(id) self$get.names(id)))
      
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
        unlist(lapply(locations, function(location) { 
          if (is.na(location)) {
            return (NA)
          } else {
            return (private$location.list[[location]]$return.type == super.type)
          }
        }))
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
        names(all.super.locations[[i]]) =  sapply(all.super.locations[[i]], function(loc_id) self$get.names(loc_id))
      }
    
      #Now sub.locations is a proper list; if we want a list returned, return it now
      if (return.list) {
        return (setNames(all.super.locations, codes))
      }
    
      #Return a collapsed vector of valid entries for all locations
      rv = unname(unlist(lapply(all.super.locations, function (l) {
        l[!is.na(l)]
      })))
    
      if (length(rv) == 0) {
        return (character())
      }
    
      setNames(rv, sapply(rv, function(loc_id) self$get.names(loc_id)))
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
      #Check if the prefix is already added to the location code
      #do not add if so
      codes = unname(mapply(function(pre, name) {
        if (substr(name, 1, nchar(pre)) == pre) {
          return (name)
        } else {
          return (paste0(pre,name))
        }
      }, self$get.prefix(types), codes, SIMPLIFY = TRUE))
      
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
    # debug.resolve = function(code) { #Used the check the resolution externally
    #   print(private$resolve.code(code, F))
    # },
    register.lat.long = function(code, lat, long) {
      # We have one location value, and valid lat and long
      valid.code <- private$resolve.code(code, F)
      if (is.na(valid.code)) {
        warning(paste0("Code ", code, " not found, lat and long not set"))
      } else {
        private$location.list[[valid.code]]$set.lat.and.long(lat,long)
      }
    },
    register.polygons = function(code) {
      valid.code <- private$resolve.code(code, F)
      if (is.na(valid.code)) {
        warning(paste0("Code ", code, " not found, polygon data not set"))
      } else {
        private$location.list[[valid.code]]$set.poly.data()
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
    },
    merge.polygons = function(locations, new.location.code) {
      
      # determine if there are any locations without polygon data
      indexes.with.polygon.data = sapply(locations, self$has.polygon)
      if (any(!indexes.with.polygon.data)) {
        bad.locations = paste(locations[!indexes.with.polygon.data], collapse=",")
        stop(paste0("Locations ", bad.locations, " do not have polygon data"))
      }
      
      # All locations have polygon data, collect it
    
      location.types = unname(self$get.types(locations))
      unique.location.types = unique(location.types)
      polygon.data = setNames(lapply (unique.location.types, self$get.polys.for.type), unique.location.types)
      
      poly.data.list = lapply (seq_along(locations), function (idx) {
        df = polygon.data[[location.types[idx]]]
        return(df[ df$location.code == locations[idx], ])
      })
      
      names(poly.data.list) = locations
      # poly.data.list is a list() of data.frames, indexed by the location code.
      
      final.poly.df = do.call(rbind, poly.data.list)
      
      polys.sf = st_as_sf(final.poly.df, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
      
      # Get unique locations
      unique_locations = unique(polys.sf$location.code)
      location_sf_list = list()
    
      # Initialize a list to store sf objects for each location
    
      for(code in unique_locations) {
        # Filter points for the current location
        points_for_location = polys.sf[polys.sf$location.code == code, ]
        
        
        # Get unique polygons (poly) within this location
        unique_polys = unique(points_for_location$poly)
        
        # Initialize a list to store polygons for the current location
        polygons = list()
    
        for (poly_id in unique_polys) {
          # Filter points for the current polygon ID within the location
          points_for_poly = points_for_location[points_for_location$poly == poly_id, ]
    
          # Extract coordinates for the polygon
          # Ensuring extraction of coordinates directly from the sf object
          coords = st_coordinates(points_for_poly)
    
          # Construct a polygon from the coordinates
          polygon = st_polygon(list(coords))
          polygons[[as.character(poly_id)]] = polygon
        }
    
        # Combine polygons into a single MULTI-POLYGON if multiple, else keep as single POLYGON
        if (length(polygons) > 1) {
          geometry = st_sfc(polygons, crs = st_crs(points_for_location))
        } else {
          geometry = st_sfc(polygons[[1]], crs = st_crs(points_for_location))
        }
    
        # Create an sf object for the location with the combined geometry and color attribute
        location_sf = st_sf(location_id = as.factor(code), geometry = geometry)
    
        # Store the sf object in the list using location as the key
        location_sf_list[[as.character(code)]] = location_sf
      }
    
      # Merge them:
    
      # S2 for the merging process is creating errors that are irrelevant to the merge
      # Disable it
      suppressMessages(sf_use_s2(FALSE))
      
      # Initialize an object to store merged sf objects
      merged_sf = NULL
      
      #  Retrieve sf objects
      sf_objects_for_group = lapply(locations, function(location_code) location_sf_list[[location_code]])
      
      if (length(sf_objects_for_group) > 0) {
        # Merge geometries of the sf objects
        # we are getting a message here saying that st_union assumes planar coordinates.
        # In this case we are dealing with a small enough scale that I believe this can be ignored
        # suppress the message
      
        merged_geometry = suppressMessages(st_union( st_geometry(sf_objects_for_group[[1]]), st_geometry(sf_objects_for_group[[2]])))
      
        if (length(sf_objects_for_group) > 2) {
          for (j in 3:length(sf_objects_for_group)) {
            merged_geometry = suppressMessages(st_union(merged_geometry, st_geometry(sf_objects_for_group[[j]])))
          }
        }
      
        if (length(merged_geometry) > 1) {
          for (j in 1:(length(merged_geometry) - 1)) {
            merged_geometry = suppressMessages(st_union(merged_geometry[[j]], merged_geometry[[j+1]]))
          }
        }
      
        # Create a new sf object for the merged geometry
        merged_sf = st_sf(geometry = st_sfc(merged_geometry), crs="4326")
      }
      
      # Type can either be POLYGON or MULTIPOLYGON
      poly.type = st_geometry_type(merged_sf)
      
      if (poly.type == "POLYGON") {
        
        coordinates = st_coordinates(merged_sf)
        final.poly = data.frame(longitude=coordinates[,1], latitude=coordinates[,2], location.code=rep(new.location.code, nrow(coordinates)))
        # Uniquely number the polygons
        return (self$number.polygons(final.poly))
        
      } else if (poly.type == "MULTIPOLYGON") {
        
        geometry = st_geometry(merged_sf)
        
        final.poly = data.frame(longitude = numeric(), latitude = numeric(), location.code=character())
        # Loop through each MULTIPOLYGON
        for (poly.index in seq_len(length(geometry))) {
          coordinates = st_coordinates(geometry[poly.index])
          temp.df = data.frame(longitude = coordinates[,1], latitude = coordinates[,2], location.code=rep(new.location.code, nrow(coordinates)))
          final.poly = rbind(final.poly, temp.df)
        }
        # Uniquely number the polygons
        return (self$number.polygons(final.poly))
        
      } else {
        stop(paste0("ST Geometry type ", poly.type, " not recognized, stopping"))
      }
    },
    combine.locations.into.new.location = function(sub.locations, new.location.code, new.location.name, type) {
      type = toupper(type)
      
      if (!private$check.is.type(type)) {
        stop(paste0("combine.locations.into.new.location: Type ", type, " is not currently registered"))
      }
      
      codes = unlist(lapply(sub.locations,function(x){private$resolve.code(x)})) #Now contains the fully resolved location codes or it will fail
      
      # Register the new location
      self$register(type, new.location.name, new.location.code)
      
      # Get the type prefix
      type.prefix = unname(self$get.prefix(type))
      
      # Create the full location code
      location.code.with.prefix = paste0(type.prefix, new.location.code)
      
      # Register the sub.locations as sub locations
      self$register.hierarchy(codes, rep(location.code.with.prefix, length(codes)), TRUE)
      
      if (all(sapply(codes,self$has.polygon) == TRUE)) {
        # All sub locations have polygon data, merge the polygons as well
        
        poly.data = self$merge.polygons(codes, location.code.with.prefix)
        
        self$add.to.poly.data(type, poly.data)
      }
    }
  )
)