

# File to load and location data so it doesn't have to be parsed each
# time the model is executed.
# Needs to finish with LOCATION.MANAGER in the global namespace

#We are currently registering 4 types:

# State; "STATE",  No Prefix
# County: "COUNTY", No prefix (location code is 2 digit state + 3 digit local fips code)
# CBSA: "CBSA", 'C.' prefix (location code is 5 digit cbsa code)
# zipcode : "ZIPCODE", 'Z.' prefix (location code is zip code)

source("R/LOCATIONS_location_manager.R") 
source("R/LOCATIONS_impl.R")
source("R/LOCATIONS_plot.R")

library("stringi")

remove.non.locale = function(string_list) {
  # Go through each string, removing non locale strings.  Convert to UTF-8
  # return (iconv(gsub("[^\x01-\x7F]+", "-", string_list), from = "ISO-8859-1", to = "UTF-8"))
  sapply(string_list, function(s) {
    s <- stri_trans_nfc(s)
    s <- stri_replace_all_regex(s, "[^\x01-\x7F]+", "-")
    s <- stri_enc_toascii(s)
    return (s)
  })
}

modify.south.dakota.codes = function(LM) {
  # Zoe asked me to remove 46131, which was in the new_fips_codes.csv file
  # 46131 was combined with another county in 1979. I have removed it.
  # We also wish to create an alias for county 46113, which changed its code
  # to 46102 in 2015.  46113 is also in the new_fips_codes.csv file, which
  # I have also removed.  46102 is in the fips_codes.csv file, so all I need
  # here is to create the alias to make 46113 equivalent to 46102
  
  LM$register.code.aliases("46102", "46113")
  LM
}

modify.connecticut.codes = function(LM) {
  # In 2020 Connecticut changed their county codes
  # We want to make aliases for the previous codes
  
  # In the following list, the first entry ("09110")
  # is the new, correct fips code, and the second ("09001") is the
  # old code that will become the alias for the new code.  
  # The new codes are properly created from the
  # new_fips_codes.csv file, so this function needs to be
  # called after the one that operates on that file 
  # (register.additional.fips())
  # The old codes have been removed from the fips_codes.csv data file
  
  ct.code.map = list (  "09110" = "09001",
                        "09120" = "09003",
                        "09130" = "09005",
                        "09140" = "09007",
                        "09150" = "09009",
                        "09160" = "09011",
                        "09170" = "09013",
                        "09180" = "09015")
  
  for (i in seq_along(ct.code.map)) {
    correct.code = names(ct.code.map)[i]
    alias.for.code = ct.code.map[i]
    LM$register.code.aliases(correct.code, alias.for.code)
  }
  
  LM
}


register.united.states = function(LM) {
  # We register the country type; empty prefix.
  LM$register.types("COUNTRY", "", "Type for top level; the United States")
  
  # We register the "US" as the code.
  LM$register("COUNTRY", "United States", "US")
  
  LM
}

register.state.abbrev = function(LM, filename) {
  #Check if the file exists
  if (!file.exists(filename)) {
    stop(paste0("LOCATION.MANAGER: Cannot find the zipcode file with filename ", filename))
  } 
  
  abbrev.data = read.csv(file= filename, header=FALSE)
  
  types = rep("state", nrow(abbrev.data))
  # Name is in column 1, abbreviation is in column 2
  LM$register(types, abbrev.data[[1]], abbrev.data[[2]])
  
  # Register each state as a sub state to the US
  LM$register.hierarchy(abbrev.data[[2]], rep("US", times=length(abbrev.data[[2]])), 
                        rep(T, times=length(abbrev.data[[2]])))
  
  #We need to do this first to register the states with their abbreviations as their 
  #location codes
  LM
}

register.state.fips.aliases <- function(LM, filename, fips.typename = "county") {
  
  fips.typename <- toupper(fips.typename)
  
  #Check if the file exists
  if (!file.exists(filename)) {
    stop(paste0("LOCATION.MANAGER: Cannot find the fips state alias file with filename ", filename))
  }
  fips.state.alias.data = read.csv(file=filename,header=FALSE)
  
  #Column one is state name, mostly for debug purposes; column 2 is the fips code (0padded, 2 chars)
  #Column 3 is the state abbreviation/location code

  # No need to add the prefix as the code alias is based on the type of location
  fips.codes = sprintf("%02d",as.numeric(fips.state.alias.data[[2]]))
  
  #LOOP FIXME
  for ( i in 1:nrow(fips.state.alias.data) ) {
    LM$register.code.aliases (fips.state.alias.data[[3]][i], fips.codes[i])  
  }
  LM
}

register.fips <- function(LM, filename, fips.typename = "county") {
  
  fips.typename <- toupper(fips.typename)
  #Check if the file exists
  if (!file.exists(filename)) {
    stop(paste0("LOCATION.MANAGER: Cannot find the fips file with filename ", filename))
  }
  
  fips.data = read.csv(file = filename)
  
  #States
  states = fips.data[ fips.data[1] == 040, ] #Get only the state data from the fips info
  
  #Column 2 is the state code
  state.codes = sprintf("%02d",states[[2]])
  
  #Counties
  counties = fips.data[ fips.data[1] == 050, ] #Get only the county data from the fips info.
  #Column 3 is the county code
  county.codes = counties[[2]] * 1000 + counties[[3]]
  
  types = rep(fips.typename,length(county.codes))
  
  #Convert the county.codes to 0 padded 5 char
  county.codes = sprintf("%05d", county.codes)
  
  #Column 7 is only one name; it's actually columns 7-10 that count.
  #get a list of all the proper names
  #Paste collapses them into a string, but if there are trailing NAs there are
  #extra spaces at the end; the sub gets rid of them, removing all spaces but anchored at the back
  proper.county.names = sub("\\s+$", "", apply(counties[7:10], 1, function(row) { paste(row[!is.na(row)],collapse=" ") }))
  LM$register(types, remove.non.locale(proper.county.names), county.codes)
  
  #There appear to be entries in the county code that don't have a corresponding
  #registered state.  Refrain from trying to create a connection to the non-existent
  #state
  #This list is checked against the state.codes above to make sure the state
  #is registered before we create a hierarchy.
  possible.state.codes = sprintf("%02d",counties[[2]])
  #Get only the counties with proper states
  counties.of.states = county.codes [ possible.state.codes %in% state.codes ]
  corresponding.states = possible.state.codes [ possible.state.codes %in% state.codes ] 
  
  counties.of.states.with.fips.prefix = sprintf("%s%s",LM$get.prefix(fips.typename), counties.of.states)
  # corresponding.states.with.fips.prefix = sprintf("%s%s",LM$get.prefix(fips.typename), corresponding.states)
  #Where previously I could use the code alias here, I can instead use the fips state number and the type
  corresponding.states.location.code = LM$get.by.alias (corresponding.states, "STATE")
  #Register the counties as completely contained by the states
  LM$register.hierarchy(counties.of.states.with.fips.prefix, corresponding.states.location.code, rep(TRUE,length(counties.of.states)))
  LM
}

register.additional.fips = function(LM, filename, fips.typename) {
  fips.typename = toupper(fips.typename)
  
  #Registers additional fips locations that were not previously registered
  #We have moved them to the csv file
  
  new_fips = read.csv(file=filename, stringsAsFactors=FALSE)
  
  codes = sprintf("%05d",new_fips[['fipscode']])
  names = new_fips[['name']]
  states = new_fips[['state']]
  
  # Register the fips counties
  LM$register(rep(fips.typename,length(codes)), names, codes)
  
  # Register them to their states
  LM$register.hierarchy(codes,states,rep(TRUE,length(codes)),T)
  
  # We have two cases of fips codes that were renumbered.  We wish to add
  # code aliases for both of these.  They are:
  
  # - Dade County, FL (12025) renamed/renumbered to Miami-Dade County (12086)
  # - Ste. Genevieve, MO (29193) renumbered to 29186
  
  LM$register.code.aliases("12086","12025")
  LM$register.code.aliases("29186","29193")
  
  LM
}

register.fips.lat.and.long = function(LM, filename) {
  #Set the latitude and longitude for locations, provided we have them
  
  lat.long.data = read.csv(file=filename, stringsAsFactors=FALSE, sep ="\t")
  
  fips.codes = sprintf("%05d",as.numeric(lat.long.data[, "GEOID"]))
  latitude = as.numeric(lat.long.data[,"INTPTLAT"])
  longitude = as.numeric(lat.long.data[,"INTPTLONG"])
  
  for (i in 1:nrow(lat.long.data)) {
    LM$register.lat.long(fips.codes[i], latitude[i], longitude[i])
  }
  
  LM
} 

register.zipcodes = function(LM, filename, fips.typename = "county", zip.typename = "zipcode", 
                                           zipcode.name.format.string = "ZIP_N_%s") { #Format for Zip name (unique not required)
  
  zip.typename <- toupper(zip.typename)
  #Check if the file exists
  if (!file.exists(filename)) {
    stop(paste0("LOCATION.MANAGER: Cannot find the zipcode file with filename ", filename))
  } 
  
  zip.data = read.csv(file= filename)
  
  zip.codes = zip.data[['zip']]
  #Add proper prefix for register.hierarchy
  unique.zip.codes = sprintf("%s%s",LM$get.prefix(zip.typename),zip.codes)
  fips.codes = sprintf("%05d",zip.data[['fips']])
  state.codes = sprintf("%02g",floor(zip.data[['fips']] / 1000))
  zip.names = sprintf(zipcode.name.format.string,zip.codes)
  
  #Register all the zip codes
  #No prefix
  LM$register(rep(zip.typename, length(zip.codes)), zip.names, zip.codes)
  #Now register the raw zipcodes as aliases:
  # Maybe hold off on this
  # for ( i in seq_along(zip.codes) ) {
  #   LM$register.code.aliases(unique.zip.codes[i], zip.codes[i])
  # }
  
  #Register the zip code as completely contained by the fips code. If any result is NA, skip
  #With Prefix
  LM$register.hierarchy(unique.zip.codes,paste0(LM$get.prefix(fips.typename),fips.codes),
                        rep(TRUE,length(fips.codes)),F)
  #Register the zip code as completely contained by the state.  If any result is NA, skip
  LM$register.hierarchy(unique.zip.codes,paste0(LM$get.by.alias(state.codes, "state"), 
                                                state.codes),
                        rep(TRUE,length(state.codes)),F)
  LM
}

register.nsduh = function(LM, county.filename, tract.filename, nsduh.typename = "nsduh") {
  nsduh.typename = toupper(nsduh.typename)
  
  if (!file.exists(county.filename)) {
    stop(paste0("LOCATION.MANAGER: Cannot find the nsduh county file with filename ", county.filename))
  }
  if (!file.exists(tract.filename)) {
    stop(paste0("LOCATION.MANAGER: Cannot find the nsduh census tract file with filename ", tract.filename))
  }
  nsduh.county.data = read.csv(file = county.filename)
  
  # At this point, we are only going to add the nsduh as a sub-member
  # of the state; will work on MSAs later
  # We can add counties 
  # Get a list of viable state numerical codes
  known.states = LM$get.aliases.for.type("state")
  all.known.state.codes = as.numeric(names(known.states))
  #For each of our known state codes
  for (code in all.known.state.codes) {
    state.data = nsduh.county.data[nsduh.county.data$state == code, ]
    if (nrow(state.data) > 0) {
      #If there are entries with this state code:
      #Get the two letter for this state
      code.ch = sprintf("%02g",code)
      state = known.states[[code.ch]]
      
      # Get all the unique nsduh region names for this state
      region.names = unique(state.data$SBST18N)
      # for each unique region name
      for (name in region.names) {
        # We can guarantee that the [1] element exists through the previous
        # code
        numeric.code = state.data[state.data$SBST18N == name, "SBST18"][1]
        
        # Add this location to the location manager
        # Create a name for this location
        region.name = sprintf("%s %s", LM$get.names(state), name)
        # Create a code for this location
        region.code = sprintf("%s.%g", state, numeric.code)
        LM$register("nsduh", region.name, region.code)
        
        #Register this region as contained by their states
        LM$register.hierarchy(region.code, state, TRUE, TRUE)
        
        #Register the contained counties
        contained.counties = state.data[state.data$SBST18N == name, "county"]
        full.county.fips.codes = sprintf("%s%03g",code.ch, contained.counties)
        for (county.code in full.county.fips.codes) {
          LM$register.hierarchy(county.code, region.code, TRUE, TRUE)
        }
      }
    }
  }
  # Some regions are not divided by county, but by census tract.  Most regions
  # in California, for instance, have a single county or multiple counties contained
  # within, where LA county has multiple regions for only one county.  Relationship
  # must be checked to differentiate between contained and overlapping relationships
  nsduh.tract.data = read.csv(file = tract.filename)
  
  # Remove 'tract' column
  nsduh.tract.data <- nsduh.tract.data[, !(names(nsduh.tract.data) %in% "tract")]
  
  # Remove duplicate rows
  nsduh.tract.data <- nsduh.tract.data[!duplicated(nsduh.tract.data), ]
    
  for (code in all.known.state.codes) {
    state.data = nsduh.tract.data[nsduh.tract.data$state == code, ]
    if (nrow(state.data) > 0) {
      #If there are entries with this state code:
      #Get the two letter for this state
      code.ch = sprintf("%02g",code)
      state = known.states[[code.ch]]
      
      if (state == "CA") {
        region.code = "CA.11"
        region.name = "California Region 11 (Los Angeles)"
        # Adding in CA.11, Region 11 for the whole of LA county
        LM$register("nsduh",region.name, region.code)
        # Register it as being completely contained by the state of California
        LM$register.hierarchy(region.code,"CA", TRUE, TRUE)
        # Register it as containing LA County (06037) completely
        LM$register.hierarchy("06037", region.code, TRUE, TRUE)
      }
      
      # Get all the unique nsduh region names for this state
      region.names = unique(state.data$SBST18N)
      # for each unique region name
      counties.per.region = sapply(unique(state.data$SBST18N), function(reg.name) { state.data[state.data$SBST18N == reg.name, "county"] })
      unlist.counties.per.region = unlist(counties.per.region)
      for (name in region.names) {
        
        # California Exceptions
        # California's codes are based on their NSDUH region names, where all other
        # state codes are based on the SBST18 column value in the csv file
        if (state == "CA") {
          if (substr(name,1,6) == "LA SPA") { #Skip the LA SPA NSDUH locations
                                              #as they are subsumed into Region 11
            next
          }
          numeric.code = strsplit(name," ")[[1]][2] # Pull the region code from the name for California NSDUH regions
                                                    # The format is "Region <code>".  There are often values following the
                                                    # code.  These are ignored.
        } else {
          # We can guarantee that the [1] element exists through the previous
          # code
          numeric.code = as.character(state.data[state.data$SBST18N == name, "SBST18"][1])
        }
        
        # Add this location to the location manager
        # Create a name for this location
        region.name = sprintf("%s %s", LM$get.names(state), name)
        # Create a code for this location
        region.code = sprintf("%s.%s", state, numeric.code)
        LM$register("nsduh", region.name, region.code)
        
        #Register this region as contained by their states
        LM$register.hierarchy(region.code, state, TRUE, TRUE)
        
        #Register the contained counties
        
        #Get a list of claimed counties
        claimed.counties = counties.per.region[[name]]
        full.county.fips.codes = sprintf("%s%03g",code.ch, claimed.counties)
        
        for (county.code.index in seq_along(claimed.counties)) {
          # Check if there is more than one instance of this county.code
          # across all the regions in this state
          if ( sum(unlist.counties.per.region == claimed.counties[county.code.index]) <= 1) {
            #This is the only region to contain this county, we can add as a contains relationship
            LM$register.hierarchy(full.county.fips.codes[county.code.index], region.code, TRUE, TRUE)
          } else {
            # There are multiple regions claiming this county, we must add as overlapping
            LM$register.hierarchy(full.county.fips.codes[county.code.index], region.code, FALSE, TRUE)
          }
        }
      }
    }
  }
  LM
}

register.cbsa = function(LM, filename, cbsa.typename = "cbsa", fips.typename = "county") {
  
  cbsa.typename <- toupper(cbsa.typename)
  fips.typename <- toupper(fips.typename)
  
  if (!file.exists(filename)) {
    stop(paste0("LOCATION.MANAGER: Cannot find the cbsa file with filename ", filename))
  } 
  cbsa.data = read.csv(file = filename)
  #
  # Type will be CBSA
  #
  # Important Columns : 
  #   'CBSA Code', column 1 - Should be primary key
  #   'CBSA Title', column 4 - Should be name
  #   'FIPS State Code', column 10 - State fips code
  #   'FIPS County Code', column 11 - County fips code
  #
  # So we want to add all the cbsa's, then say that they are contained by
  # the state (entirely in the case where there is only one FIPS State Code for
  # all the entries for the particular CBSA code, partially for the rest) and
  # that they contain their FIPS County Code entirely.
  #
  # Get all the unique CBSA Codes with proper prefixes:
  unique.codes = head(unique(cbsa.data[[1]]), -4) #Remove the last four lines as they contain sources.
  location.codes = sprintf("%s%s",LM$get.prefix(cbsa.typename),unique.codes) 
  #LOOP FIXME
  for (i in seq_along(unique.codes)) {
    # Get cbsa code data:
    code.data = cbsa.data[ cbsa.data$CBSA.Code == unique.codes[i], ]
    
    # register the cbsa:
    # There will always be minimum one entry in the CBSA.Title column
    # Register the unique codes here as the prefixes are added in $register()
    LM$register(cbsa.typename, remove.non.locale(code.data$CBSA.Title[1]), unique.codes[i])
    #Now register the raw cbsa codes as aliases:
    LM$register.code.aliases( location.codes[i], unique.codes[i] )
    
    # Is it fully or only partially contained by the state?
    unique.states <- sprintf("%02d",unique(code.data$FIPS.State.Code))
    #Use the fully prefixed for the hierarchy registration
    if (length(unique.states) == 1) {
      #The majority of cases:
      # Register the location.code value as being a sub location of the prefixed state FIPS code, marking as fully.contains.
      # There will always be at least one value in unique.states
      LM$register.hierarchy ( location.codes[i], LM$get.by.alias(unique.states[1],"STATE"), TRUE)
    } else {
      #Register as being sub of each state in the list
      #LOOP FIXME
      for ( j in seq_along(unique.states) ) {
        #Do the same but mark fully.contains as FALSE
        LM$register.hierarchy ( location.codes[i], LM$get.by.alias(unique.states[j],"STATE"), FALSE)
      }
    }
    # It contains the FIPS.County.Code entirely
    fips.county.data = sprintf("%s%02d%03d",LM$get.prefix(fips.typename),
                                            code.data$FIPS.State.Code, 
                                            code.data$FIPS.County.Code)
    for (county.code in fips.county.data) {
      #We are marking the counties as fully contained by the cbsas
      LM$register.hierarchy ( county.code, location.codes[i], TRUE) 
    }
  }
  
  LM
}

register.cbsa.lat.and.long = function(LM, filename) {
  #Set the known longitude and latitude for cbsa locations
  lat.long.data = read.csv(file=filename, stringsAsFactors=FALSE, sep ="\t")
  
  cbsa.codes = sprintf("C.%05d",as.numeric(lat.long.data[, "GEOID"]))
  latitude = as.numeric(lat.long.data[,"INTPTLAT"])
  longitude = as.numeric(lat.long.data[,"INTPTLONG"])
  
  #Unregistered CBSA C.14160 Bluffton, IN Micro Area	2	953341275	5596377	368.087	2.161	40.735273	-85.212974 
  #Leaving the warning in as we may get information later; ask Todd
  
  for (i in 1:nrow(lat.long.data)) {
    if (LM$has.location(cbsa.codes[i])) {
      LM$register.lat.long(cbsa.codes[i], latitude[i], longitude[i])
    }
  }
  
  LM
}

register.cbsa.poly.data = function(LM, filename, cbsa.type) {
  cbsa.type = toupper(cbsa.type)
  
  cbsa.prefix = LM$get.prefix(cbsa.type)
  #For those locations that have polygon data (cbsa), set the polygon data
  # Load the cbsa polygon data
  # US Census Data, low-vertex count
  poly.data = read.csv(filename, stringsAsFactors = FALSE)
  
  poly.df = as.data.frame(LM$number.polygons(poly.data))
  
  #Get all the codes available from the poly data.
  poly.codes = sprintf("%05g",unique(poly.data$CBSAFP))
  
  full.df = data.frame()
  
  #Iterate over all the polygon data and assign to the proper location
  for (i in seq_along(poly.codes)) {
    # the location code is the poly.code for counties with the cbsa prefix
    location.code = paste0(cbsa.prefix, poly.codes[i])
    if (LM$has.location(location.code)) {
      # Get the relevant data
      cbsa.poly.data = poly.df [ poly.df$CBSAFP == as.numeric(poly.codes[i]), ]
      # Strip out CBSAFP
      cbsa.poly.data$CBSAFP = NULL
      # Register the polygon data
      LM$register.polygons(location.code)
      
      cbsa.poly.data$location.code = rep(location.code, nrow(cbsa.poly.data))
      
      full.df <- rbind(full.df, cbsa.poly.data)
    }
  }
  
  LM$add.poly.data(cbsa.type, full.df)
  LM
}

register.county.poly.data = function(LM, filename, county.type) {
  county.type = toupper(county.type)
  #For those locations that have polygon data (Counties), set the polygon data
  # Load the county polygon data
  # US Census Data, low-vertex count
  poly.data = read.csv(filename, stringsAsFactors = FALSE)
  
  poly.df = as.data.frame(LM$number.polygons(poly.data))
  
  #Get all the codes available from the poly data.
  poly.codes = sprintf("%05g",unique(poly.data$COUNTYFP))
  
  full.df = data.frame()
  
  #Iterate over all the polygon data and assign to the proper location
  for (i in seq_along(poly.codes)) {
    # the location code is the poly.code for counties 
    if (LM$has.location(poly.codes[i])) {
      # Get the relevant data
      county.poly.data = poly.df [ poly.df$COUNTYFP == as.numeric(poly.codes[i]), ]
      # Strip out COUNTYFP
      county.poly.data$COUNTYFP = NULL
      # Register the polygon data
      LM$register.polygons(poly.codes[i])
      
      county.poly.data$location.code = rep(poly.codes[i], nrow(county.poly.data))
      
      full.df <- rbind(full.df, county.poly.data)
    }
  }
  
  LM$add.poly.data(county.type, full.df)
  LM
}

register.zip.poly.data = function(LM, filename, zipcode.type) {
  zipcode.type = toupper(zipcode.type)
  
  zipcode.prefix = LM$get.prefix(zipcode.type)
  #For those locations that have polygon data (Counties), set the polygon data
  # Load the zipcode polygon data
  # US Census Data, low-vertex count
  poly.data = read.csv(filename, stringsAsFactors = FALSE)
  
  poly.df = as.data.frame(LM$number.polygons(poly.data))
  
  #Get all the codes available from the poly data.
  poly.codes = sprintf("%05g",unique(poly.data$ZIPCODE))
  
  full.df = data.frame()
  
  #Iterate over all the polygon data and assign to the proper location
  for (i in seq_along(poly.codes)) {
    # the location code is the poly.code with the zipcode prefix 
    location.code = paste0(zipcode.prefix, poly.codes[i])
    if (LM$has.location(location.code)) {
      # Get the relevant data
      zip.poly.data = poly.df [ poly.df$ZIPCODE == as.numeric(poly.codes[i]), ]
      # Strip out ZIPCODE
      zip.poly.data$ZIPCODE = NULL
      # Register the polygon data
      LM$register.polygons(location.code)
      
      zip.poly.data$location.code = rep(location.code, nrow(zip.poly.data))
      
      full.df <- rbind(full.df, zip.poly.data)
    }
  }
  
  LM$add.poly.data(zipcode.type, full.df)
  LM
}

register.state.poly.data = function(LM, filename, state.type) {
  state.type = toupper(state.type)
  #For those locations that have polygon data (States), set the polygon data
  # Load the state polygon data
  # US Census Data, low-vertex count
  poly.data = read.csv(filename, stringsAsFactors = FALSE)
  
  poly.df = as.data.frame(LM$number.polygons(poly.data))
  
  #Get all the codes available from the poly data.
  poly.codes = sprintf("%02g",unique(poly.data$STATEFP))
  
  full.df = data.frame()
  
  #Iterate over all the polygon data and assign to the proper location
  for (i in seq_along(poly.codes)) {
    # Get the relevant location code for the location
    location.code = LM$get.by.alias(poly.codes[i], state.type)
    if (LM$has.location(location.code)) {
      # Get the relevant data
      state.poly.data = poly.df [ poly.df$STATEFP == as.numeric(poly.codes[i]), ]
      state.poly.data = state.poly.data [, -which(names(state.poly.data) %in% c("STATEFP", "NAME"))]
      # Register the polygon data
      LM$register.polygons(location.code)
      
      state.poly.data$location.code = rep(location.code, nrow(state.poly.data))
      full.df <- rbind(full.df, state.poly.data)
    }
  }
  
  LM$add.poly.data(state.type, full.df)
  LM
}

merge.state.phd <- function(LM, state_groups, state_code, all.county.polygons, phd.type) {
  
  phd.prefix = LM$get.prefix(phd.type)
  
  state.groups.poly.data = data.frame()
  
  # No groups should be empty
  if (any(sapply(state_groups,length) == 0)) {
    stop("No merging groups can be empty")
  }
  state.name = unname(LM$get.names(state_code))
  
  districts = names(state_groups)
  
  for (d in districts) {
    location.code = toupper(paste0(state_code,".",gsub(" ","",d)))
    location.name = paste0(state.name, " ", d," Public Health District")
    
    location.code.with.prefix = paste0(phd.prefix, location.code)
    
    # Register this district as a location
    LM$register(phd.type, location.name, location.code)
    # Register this district as being contained within its state
    LM$register.hierarchy(location.code.with.prefix, state_code, TRUE, TRUE)
    # Register that this location has polygons
    LM$register.polygons(location.code.with.prefix)
    
    ph.polygons = data.frame()
    
    if (length(state_groups[[d]]) == 1) {
      # Region made up of only one county, just assign the region the value of that polygon
      ph.polygons = all.county.polygons[all.county.polygons$location.code == state_groups[[d]], ]
      
      # Set the new location code for this district
      ph.polygons$location.code = rep(location.code.with.prefix, nrow(ph.polygons))
      
      # Register this county as being fully contained by the district
      LM$register.hierarchy(state_groups[[d]], location.code.with.prefix, TRUE, TRUE)
      
    } else {
      # Merge the polygons into one group
      ph.polygons = LM$merge.polygons(state_groups[[d]], location.code.with.prefix)
      
      # Register these counties as being fully contained by the public health district
      for (i in seq_along(state_groups[[d]])) {
        LM$register.hierarchy(state_groups[[d]][i], location.code.with.prefix, TRUE, TRUE)
      }
    }
    state.groups.poly.data = rbind(state.groups.poly.data, ph.polygons)
  }
  return (state.groups.poly.data)
}

register.public.health.districts <- function(LM, phd.type, county.type) {
  # Public health districts are made up of counties
  
  phd.type = toupper(phd.type)
  county.type = toupper(county.type)
  
  # Retrieve all county polygons
  all.county.polygons = LM$get.polys.for.type(county.type)
  
  all.ph.poly.data = data.frame()
  
  ph.data.list = list (
    AL = list(
           "Northern"=c("01077","01083","01089","01071","01033","01059","01079","01103","01095","01093","01133","01043"),
           "Jefferson"=c("01073"),
           "West Central"=c("01075","01057","01127","01107","01125","01119","01063","01065","01105","01007","01021"),
           "Northeastern"=c("01049","01009","01055","01019","01115","01015","01029","01117","01121","01027","01111"),
           "Southwestern"=c("01023","01091","01047","01129","01025","01131","01099","01035","01003","01053"),
           "Mobile"=c("01097"),
           "East Central"=c("01037","01123","01017","01001","01051","01081","01085","01101","01087","01011","01113"),
           "Southeastern"=c("01013","01041","01109","01005","01039","01031","01045","01067","01069","01061")
         ),
    LA = list(
           "I"=c("22109","22057","22093","22095","22089","22051","22071","22087","22075"),
           "II"=c("22077","22125","22037","22091","22121","22033","22063","22047","22005","22007"),
           "IV"=c("22023","22053","22039","22097","22001","22055","22099","22113","22045","22101"),
           "V"=c("22115","22011","22003","22019"),
           "VI"=c("22043","22059","22025","22029","22079","22009"),
           "VII"=c("22017","22015","22119","22027","22013","22031","22081","22085","22069"),
           "VIII"=c("22111","22067","22123","22035","22061","22073","22083","22065","22049","22127","22021","22041","22107"),
           "IX"=c("22105","22117","22103")
         ),
    # Older Mississippi
    MS = list(
          "I"=c("28033","28143","28137","28027","28119","28107","28135","28161","28043"),
          "II"=c("28093","28009","28139","28003","28117","28141","28071","28145","28115","28081","28057"),
          "III"=c("28011","28151","28133","28053","28083","28051","28015","28097","28007"),
          "IV"=c("28013","28155","28017","28095","28025","28019","28105","28087","28159","28103"),
          "V"=c("28055","28125","28163","28089","28149","28049","28021","28029","28127","28121"),
          "VI"=c("28079","28099","28069","28123","28101","28075","28129","28061","28023"),
          "VII"=c("28063","28001","28157","28037","28005","28085","28113","28077","28147"),
          "VIII"=c("28065","28091","28031","28067","28153","28073","28035","28111","28041"),
          "IX"=c("28109","28131","28039","28045","28047","28059")
         )
    # Modern Mississippi
    # MS = list(
    #        "Northern"=c("28033","28143","28137","28027","28119","28107","28135","28161","28043",
    #                     "28093","28009","28139","28003","28117","28141","28071","28145","28115","28081","28057",
    #                     "28011","28133","28083","28015","28097","28013","28155","28017","28095","28025"),
    #        "Central"=c("28151","28053","28051","28007","28105","28087","28159","28103","28019",
    #                    "28055","28125","28163","28089","28149","28049","28021","28029","28127","28121",
    #                    "28079","28099","28069","28123","28101","28075","28129","28061","28023"),
    #        "Southern"=c("28063","28001","28157","28037","28005","28085","28113","28077","28147",
    #                     "28065","28091","28031","28067","28153","28073","28035","28111","28041",
    #                     "28109","28131","28039","28045","28047","28059")
    #      )
  )
  
  for (state_code in names(ph.data.list)) {
    ph.poly.data = merge.state.phd(LM, ph.data.list[[state_code]], state_code, all.county.polygons, phd.type)
    all.ph.poly.data = rbind(all.ph.poly.data, ph.poly.data)
  }
  
  LM$add.poly.data(phd.type, all.ph.poly.data)
  
  LM
}

register.type.relationships = function(LM) {
  
  #Set the defaults
  LM$register.type.relationship("STATE","COUNTY",TRUE)
  LM$register.type.relationship("CBSA", "COUNTY", TRUE)
  LM$register.type.relationship("NSDUH", "COUNTY", TRUE)
  LM$register.type.relationship("COUNTRY", "STATE", TRUE)
  LM$register.type.relationship("COUNTRY", "COUNTY", TRUE)
  LM$register.type.relationship("COUNTRY", "NSDUH", TRUE)
  LM$register.type.relationship("COUNTRY", "ZIPCODE", TRUE)
  LM$register.type.relationship("COUNTRY", "CBSA", TRUE)
  
  LM
}

#Prefix and type are auto capitalized

state.type = "state"
state.prefix = ""
state.prefix.longform = "State"

county.type = "county"
county.prefix = ""
county.prefix.longform = "FIPS code"

cbsa.type = "cbsa"
cbsa.prefix = "c."
cbsa.prefix.longform = "Community Based Statistical Area"

zipcode.type = "zipcode"
zipcode.prefix = "z."
zipcode.prefix.longform = "Zip code"

nsduh.type = "nsduh"
nsduh.prefix = ""
nsduh.prefix.longform = "National Surveys on Drug Use and Health"

phd.type = "phd"
phd.prefix = "ph."
phd.prefix.longform = "State level public health districts"

# Create the initial LOCATION.MANAGER object
LOCATION.MANAGER = Location.Manager$new()

LOCATION.MANAGER$register.types(c(county.type,            zipcode.type,            cbsa.type,            state.type,            nsduh.type,            phd.type), #Typename
                                c(county.prefix,          zipcode.prefix,          cbsa.prefix,          state.prefix,          nsduh.prefix,          phd.prefix), #Prefix
                                c(county.prefix.longform, zipcode.prefix.longform, cbsa.prefix.longform, state.prefix.longform, nsduh.prefix.longform, phd.prefix.longform)) #Longform Name

LOCATION.MANAGER = register.united.states(LOCATION.MANAGER)

#Define relationships between types
LOCATION.MANAGER = register.type.relationships(LOCATION.MANAGER)

DATA.DIR = 'data-raw'

LOCATION.MANAGER = register.state.abbrev(LOCATION.MANAGER, file.path(DATA.DIR, "us_state_abbreviations.csv"))
LOCATION.MANAGER = register.state.fips.aliases(LOCATION.MANAGER, file.path(DATA.DIR, "fips_state_aliases.csv"), fips.typename= county.type) #Set the fips typename
LOCATION.MANAGER = register.fips(LOCATION.MANAGER, file.path(DATA.DIR, "fips_codes.csv"), fips.typename = county.type) #Set the fips typename
LOCATION.MANAGER = register.additional.fips(LOCATION.MANAGER, file.path(DATA.DIR,"new_fips_codes.csv"), fips.typename = county.type) #Set the fips typename
LOCATION.MANAGER = modify.connecticut.codes(LOCATION.MANAGER)
LOCATION.MANAGER = modify.south.dakota.codes(LOCATION.MANAGER)
LOCATION.MANAGER = register.cbsa(LOCATION.MANAGER, file.path(DATA.DIR, "cbsas.csv"), cbsa.typename = cbsa.type, fips.typename = county.type) #Sets the fips and cbsa typename
LOCATION.MANAGER = register.cbsa.lat.and.long(LOCATION.MANAGER, file.path(DATA.DIR,"2021_Gaz_cbsa_national.txt")) #Set the known longitude and latitude for cbsa locations
LOCATION.MANAGER = register.nsduh(LOCATION.MANAGER, file.path(DATA.DIR, "nsduh-county.csv"), file.path(DATA.DIR, "nsduh-tract.csv"), nsduh.typename = nsduh.type) #Sets only the NSDUH typename
# LOCATION.MANAGER = register.zipcodes(LOCATION.MANAGER, file.path(DATA.DIR, "zip_codes.csv"), fips.typename = county.type, zip.typename = zipcode.type)
LOCATION.MANAGER = register.fips.lat.and.long(LOCATION.MANAGER, file.path(DATA.DIR,"2021_Gaz_counties_national.txt")) #Set the latitude and longitude for locations, provided we have them
LOCATION.MANAGER = register.state.poly.data(LOCATION.MANAGER, file.path(DATA.DIR, "state_geom_data.csv"), state.type) #Give each location the proper polygon data (states)
LOCATION.MANAGER = register.county.poly.data(LOCATION.MANAGER, file.path(DATA.DIR, "county_geom_data.csv"), county.type) #Give each location the proper polygon data (counties)
LOCATION.MANAGER = register.cbsa.poly.data(LOCATION.MANAGER, file.path(DATA.DIR, "cbsa_geom_data.csv"), cbsa.type) #Give each location the proper polygon data (cbsa)
# LOCATION.MANAGER = register.zip.poly.data(LOCATION.MANAGER, file.path(DATA.DIR, "zip_geom_data_0_1.csv"), zipcode.type) #Give each location the proper polygon data (zip)
LOCATION.MANAGER = register.public.health.districts(LOCATION.MANAGER, phd.type, county.type)

rm(DATA.DIR)
