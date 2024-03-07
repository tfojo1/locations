library(ggplot2)
library(ggmap) #register_stadiamaps
library(sf)

#'@title location.plot
#'@description Create a plot of various points in the US from a data frame
#'
#'@param data A data frame with a column labeled 'location', containing a location code, and columns to be mapped
#'            by color or size.
#'
#'@param color The name of a column in the data frame to be used in the aesthetic as the color of the location.
#'             If used with polygons, this is the outline color.
#'
#'@param fill The name of a column in the data frame to be used as the fill color for points or polygons
#'
#'@param size If left blank, the data will be plotted as polygons, and those locations without polygons will be skipped.
#'            If filled in, it represents the name of a column in the data frame to be used to scale the size of the point,
#'            and only those locations with latitude and longitude values will be plotted.
#'
#'@param title Defaults to blank; a character string representing the title on the graph.
#'
#'@param groups A list of groupings of locations to merge into larger groups. Ex: list(c("44001","44007"),c("44003","44005","44009")).
#'              Only applicable when dealing with polygons.
#'
#'@param bb The bounding box value; The default is the bounding box of the contiguous US.  Otherwise, can take 
#'          one of two values: either the character string "AUTO" which will use the dimensions of the locations to 
#'          automatically bound the graph (using the bb.edge parameter, see below, as an edge buffer), or a named 
#'          character vector of the format c(left=-125,bottom=24,right=-66, top=50), outlining the dimensions of 
#'          the bounding box in latitude and longitude.  Defaults to NULL.
#'         
#'@param bb.edge If param 'bb' is 'AUTO', this will be used to create a buffer space around the outside of the 
#'               automatically generated bounding box, as a proportion of the size of the plotted locations.  Defaults
#'               to 0.1, or 10% of the bounding box. 
#'              
#'@param size.range If 'size' is used, this will scale the values in the size column between the two size values 
#'                  specified.  Defaults to 1-5
#'                  
#'@param color.range The color ranges to span for the 'color' and 'fill' parameters. Defaults to c('blue','red'),
#'                   so lower values will be more blue, and higher values will be more red.
#'                   
#'@param pch The shape of the point when plotting by points; see https://sape.inf.usi.ch/quick-reference/ggplot2/shape
#'           for a detailed list. Defaults to 19, a circle.
#'
#'@param size.label A label to give to the size in the legend.  Defaults to blank.
#'
#'@param color.label A label to give to the color/fill in the legend.  Defaults to blank.
#'
#'@param alpha An alpha blending/transparency value for the 'color'/'fill' parameter. Defaults to 1, or no transparency.
#'
#'@param map_water_color A hex color value for the color of any water on the map. Defaults to a light gray.
#'
#'@param stadia_api_key The api key to access the stamen/stadia toner map tiles.  Defaults to looking for the
#'                      key in the STADIA_MAPS_API_KEY environment variable
#'            
#'@return Returns the ggplot2 object/plot
#'
#'@export
location.plot <- function(data,
                          color,
                          fill,
                          size=NA,
                          title=NA,
                          groups=NULL,
                          bb=c(left=-125,bottom=24,right=-66,top=50),
                          bb.edge=0.1,
                          size.range=c(1,5),
                          color.range=c('blue', 'red'),
                          pch=19,
                          size.label='',
                          color.label='',
                          alpha=1,
                          map_water_color = "#C0C0C0",
                          stadia_api_key=Sys.getenv("STADIA_MAPS_API_KEY"))
{
  # Check to make sure that 'data' is a data.frame
  if (!is.data.frame(data)) {
    stop("Parameter 'data' is not a data.frame")
  }
  
  # Sanity checks
  
  # make sure that 'data' has a column named 'locations'
  if (!("locations" %in% names(data))) {
    stop("No 'locations' column in the data.frame")
  }
  # make sure that 'data' has a column name corresponding to the
  # values in 'color' and 'fill'
  if (!(color %in% names(data))) {
    stop(paste0("No column named ", color, "found in the data.frame (color)"))
  }
  if (!(fill %in% names(data))) {
    stop(paste0("No column named ", fill, "found in the data.frame (fill)"))
  }
  
  point.df = data.frame()
  poly.df = data.frame()
  
  no.data = c() # reserved for those locations without plotting data.
  
  # if 'size' is specified (Not NA), then we don't want polygon data we want
  # point data.  Else we want the polygon data where possible
  if (is.na(size)) {
    # Determine if the locations have polygon data
    indexes.with.polygon.data = sapply(data[['locations']], LOCATION.MANAGER$has.polygon)
    poly.df = data [ indexes.with.polygon.data, ]
    no.data = data [ !indexes.with.polygon.data, ][['locations']]
  } else {
    indexes.with.point.data = sapply(data[['locations']], LOCATION.MANAGER$has.lat.lon)
    point.df = data [ indexes.with.point.data, ]
    no.data = data [ !indexes.with.point.data, ][['locations']]
  }
  # If there are any locations without the relevant data, display a list
  
  if (length(no.data) > 0) {
    cat(paste0("Unable to plot ", length(no.data)," locations, no location data :\n"))
    for (i in seq_along(no.data)) {
      cat(paste0(i, ". ", no.data[i],"\n"))
    }
  }
  # First, point.locations
  
  # Get the coordinates for all the locations included in the point data.frame; they
  # are returned as a character value with two values separated by a comma.
  # The first value is latitude, the second value is longitude
  if (nrow(point.df) > 0) {
    coordinates = LOCATION.MANAGER$get.coords(point.df$locations)
    
    lat.lon = strsplit(coordinates, ",")
    point.df$latitude = as.numeric(sapply(lat.lon, function(x) {return(x[1])}))
    point.df$longitude = as.numeric(sapply(lat.lon, function(x) {return(x[2])}))
  }
  # Now, polygon locations
  final.poly.df = data.frame()
  all_merged_sf <- NULL
  
  if (nrow(poly.df) > 0) {
    # Get all the polygon data for the location codes
    location.types = unname(LOCATION.MANAGER$get.types(poly.df$locations))
    unique.location.types = unique(location.types)
    polygon.data = setNames(lapply (unique.location.types, LOCATION.MANAGER$get.polys.for.type), unique.location.types)
    
    poly.data.list = lapply (seq_along(poly.df$locations), function (idx) {
      df = polygon.data[[location.types[idx]]]
      return(df[ df$location.code == poly.df$locations[idx], ])
    })
    names(poly.data.list) <- poly.df$locations
    # poly.data.list is a list() of data.frames, indexed by the location code.
    
    merged.poly.df = lapply (seq_len(nrow(poly.df)), function(i) {
      original.row = poly.df[i, , drop = FALSE]
      row.location.code = poly.df$locations[i]

      location.poly.data = poly.data.list[[row.location.code]]
      original.replicated = original.row[rep(1, nrow(location.poly.data)), ]
      location.poly.data$location.code = NULL
      merged.poly.data = cbind(original.replicated, location.poly.data)
      return (merged.poly.data)
    })
    
    final.poly.df = do.call(rbind, merged.poly.df)
    if (!is.null(groups)) {
      # final.poly.df$locations = as.numeric(final.poly.df$locations)
      
      polys.sf <- st_as_sf(final.poly.df, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
      # polys.sf$longitude = as.numeric(polys.sf$longitude)
      # polys.sf$latitude = as.numeric(polys.sf$latitude)
      
      
      # Get unique locations
      unique_locations <- unique(polys.sf$locations)
      location_sf_list = list()
      
      # Initialize a list to store sf objects for each location
      
      for(location in unique_locations) {
        # Filter points for the current location
        points_for_location <- polys.sf[polys.sf$locations == location, ]
        
        # Get unique polygons (poly) within this location
        unique_polys <- unique(points_for_location$poly)
        
        # Initialize a list to store polygons for the current location
        polygons <- list()
        
        for (poly_id in unique_polys) {
          # Filter points for the current polygon ID within the location
          points_for_poly <- points_for_location[points_for_location$poly == poly_id, ]
          
          # Extract coordinates for the polygon
          # Ensuring extraction of coordinates directly from the sf object
          coords <- st_coordinates(points_for_poly)
          
          # Construct a polygon from the coordinates
          polygon <- st_polygon(list(coords))
          polygons[[as.character(poly_id)]] <- polygon
        }
        
        # Combine polygons into a single MULTI-POLYGON if multiple, else keep as single POLYGON
        if (length(polygons) > 1) {
          geometry <- st_sfc(polygons, crs = st_crs(points_for_location))
        } else {
          geometry <- st_sfc(polygons[[1]], crs = st_crs(points_for_location))
        }
        
        # Extract the color attribute, assuming it's consistent within each location
        col <- unique(points_for_location$color)[1]
        
        # Create an sf object for the location with the combined geometry and color attribute
        location_sf <- st_sf(location_id = as.factor(location), color = as.factor(col), geometry = geometry)
        
        # Store the sf object in the list using location as the key
        location_sf_list[[as.character(location)]] <- location_sf
      }
      
      # Merge them by groups:
      
      # Initialize a list to store merged sf objects
      merged_sf_objects <- list()
      
      for (i in seq_along(groups)) {
        group <- groups[[i]]
        
        # Retrieve sf objects for the current group
        sf_objects_for_group <- lapply(group, function(location_code) location_sf_list[[location_code]])
        
        #Here we assume all group members have the same color
        color_for_group = data[[color]][data$locations == group[[1]]] 
        
        # Remove NULL entries in case some locations were not found
        sf_objects_for_group <- Filter(NROW, sf_objects_for_group)
        
        if (length(sf_objects_for_group) > 0) {
          # Merge geometries of the sf objects
          # we are getting a message here saying that st_union assumes planar coordinates.
          # In this case we are dealing with a small enough scale that I believe this can be ignored
          # suppress the message
          
          # merged_geometry <- suppressMessages(do.call(st_union, lapply(sf_objects_for_group, st_geometry)))NULL
          merged_geometry <- suppressMessages(st_union( st_geometry(sf_objects_for_group[[1]]), st_geometry(sf_objects_for_group[[2]])))
          
          if (length(sf_objects_for_group) > 2) {
            for (j in 3:length(sf_objects_for_group)) {
              merged_geometry <- suppressMessages(st_union(merged_geometry, st_geometry(sf_objects_for_group[[j]])))
            }
          }  
          
          if (length(merged_geometry) > 1) {
            for (j in 1:(length(merged_geometry) - 1)) {
              merged_geometry = suppressMessages(st_union(merged_geometry[[j]], merged_geometry[[j+1]]))
            }
          }
          
          # browser()
          col <- as.numeric(color_for_group)
          
          # Create a new sf object for the merged geometry
          merged_sf <- st_sf(geometry = st_sfc(merged_geometry), color = col, crs="4326")
          
          # Store the merged sf object in the list
          merged_sf_objects[[i]] <- merged_sf
        }
      }
      
      all_merged_sf <- do.call(rbind, merged_sf_objects)
    }
  }
  
  #Plot
  
  # There are three formats acceptable for bb (the bounding box):
  #
  #  - The default, the contiguous US
  #  - character(1) value of "AUTO" (bb="AUTO", eg.), calculates the bounding box
  #    from the polygons/points of the locations
  #  - numeric(4) named vector with names "bottom","left","right","top"
  #     (bb=c(left=-125,bottom=24,right=-66, top=50), eg.)
  #
  
  if (all(is.character(bb), length(bb) == 1, bb == "AUTO") || 
           (all(class(bb) == 'numeric', length(bb) == 4, !is.null(names(bb)), 
                sort(names(bb)) == c("bottom","left","right","top")))) {
    # Change the bounding box
    if (all(is.character(bb), length(bb) == 1, bb == "AUTO")) {
      # Determine what the bounding box should be, given the locations we are plotting
      
      updated.bb = c(left=min(c(final.poly.df$longitude,point.df$longitude)), 
                     bottom=min(c(final.poly.df$latitude,point.df$latitude)), 
                     right=max(c(final.poly.df$longitude,point.df$longitude)), 
                     top=max(c(final.poly.df$latitude,point.df$latitude)))
      height.outeredge = (updated.bb[['top']] - updated.bb[['bottom']]) * bb.edge
      width.outeredge = (updated.bb[['right']] - updated.bb[['left']]) * bb.edge
      updated.bb[['top']] = updated.bb[['top']] + height.outeredge
      updated.bb[['bottom']] = updated.bb[['bottom']] - height.outeredge
      updated.bb[['right']] = updated.bb[['right']] + width.outeredge
      updated.bb[['left']] = updated.bb[['left']] - width.outeredge
      bb = updated.bb
    } 
  } else {
    # If the bb format is unknown
    bb.names.values <- paste(names(bb), bb, sep=": ", collapse=", ")
    wrn.msg = paste("Unknown value for bounding box (", bb.names.values, "), proceeding with default")
    warning(wrn.msg)
    bb = c(left=-125,bottom=24,right=-66, top=50)
  }
  # Loading the stamen/stadia tiles
  register_stadiamaps(stadia_api_key)
  MAP = get_stadiamap(bbox=bb, zoom = calc_zoom(bb), maptype = "stamen_toner_background")
  
  attr_map <- attr(MAP, "bb")    # save attributes from original
  # 
  # ## change color in raster; change the black water color to map_water_color
  MAP[MAP == "#000000"] <- map_water_color
  # Some background is colored with almost-black, change it as well
  MAP[MAP == "#010101"] <- map_water_color
  # 
  # ## correct class, attributes
  class(MAP) <- c("ggmap", "raster")
  attr(MAP, "bb") <- attr_map
  
  plot = ggmap(MAP)
  
  if (nrow(point.df) > 0) {
    plot = plot + geom_point(data=point.df, 
                             aes(x=longitude, y=latitude, size=!!sym(size), color=!!sym(color), fill=!!sym(fill)), 
                             shape = pch, alpha = alpha)
  }
  
  if (nrow(poly.df) > 0 && is.null(all_merged_sf)) {
    plot = plot + geom_polygon(data = final.poly.df, 
                               aes(x=longitude, y=latitude, color=.data[[color]], fill=.data[[fill]], group=poly), 
                               alpha = alpha)
  }
  
  if (nrow(poly.df) > 0 && !is.null(all_merged_sf)) {
    plot = plot + geom_sf(data=all_merged_sf, aes(fill = .data[["color"]]), inherit.aes = FALSE, color = "black", alpha = alpha)
  }
  
  plot = plot + 
    theme(panel.background = element_rect(fill='white'), 
          axis.ticks = element_blank(),
          axis.text = element_blank(), 
          axis.title = element_blank(), 
          plot.title = element_text(hjust = 0.5))
  
  
  # Add a title if asked for
  if ( !is.na(title) ) {
    plot = plot + ggtitle(title)
  }
  
  if (!is.null(size.range)){
    plot = plot + scale_size_continuous(name=size.label, range=size.range)
  }
  
  if (!is.null(color.range)) {
    plot = plot + scale_fill_gradient(name=color.label, low=color.range[1], high=color.range[2])
    plot = plot + scale_color_gradient(name=color.label, low=color.range[1], high=color.range[2])
  } 
  
  plot
}