library(ggplot2)

#'@title location.plot
#'@description Create a plot of various points in the US located in a data frame with
#'             an accompanying ggplot2 aesthetic (aes()).
#'
#'@param data A data frame with a column labeled 'location', containing a location code 
#'
#'@param mapping Aesthetics to pass to the ggplot object, created by aes()
#'
#'@param title A title for the plotted area
#'
#'@export
location.plot <- function(data,
                          size=NA,
                          color='blue',
                          fill='blue',
                          title=NA,
                          bb=NULL,
                          bb.edge=0.1,
                          size.range=c(1,5),
                          color.range=c('blue', 'red'),
                          pch=19,
                          size.label='',
                          color.label='',
                          alpha=1)
{
  # Check to make sure that 'data' is a data.frame
  if (!is.data.frame(data)) {
    stop("Parameter 'data' is not a data.frame")
  }
  # Sanity checks: make sure that 'data' has a column named 'locations'
  if (!("locations" %in% names(data))) {
    stop("No 'locations' column in the data.frame")
  }
  
  point.df = data.frame()
  poly.df = data.frame()
  
  # if 'size' is specified (Not NA), then we don't want polygon data we want
  # point data.  Else we want the polygon data where possible
  if (is.na(size)) {
    # Determine if the locations have polygon data
    poly.df = data [ sapply(data[['locations']], LOCATION.MANAGER$has.polygon), ]
  } else {
    point.df = data [ sapply(data[['locations']], LOCATION.MANAGER$has.lat.lon), ]
  }
  # First, point.locations
  
  # Get the coordinates for all the locations included in the point data.frame; they
  # are returned as a character value with two values separated by a comma.
  # The first value is latitude, the second value is longitude
  if (nrow(point.df) > 0) {
    coordinates = get.location.coords(point.df$locations)
    
    lat.lon = strsplit(coordinates, ",")
    point.df$latitude = as.numeric(sapply(lat.lon, function(x) {return(x[1])}))
    point.df$longitude = as.numeric(sapply(lat.lon, function(x) {return(x[2])}))
    
    # Are any of the locations NA?  This is a result of missing location data.
    na.indexes = which(is.na(point.df$latitude))
    
    # If there are any NA locations, display a list
    if (length(na.indexes) > 0) {
      cat(paste0("Unable to plot ", length(na.indexes)," locations, no location data :\n"))
      for (i in seq_along(na.indexes)) {
        # We could print the name in those dataframes that contain it, but all we know for 
        # sure is that this dataframe contains a locations column
        cat(paste0(i, ". ", point.df$locations[na.indexes[i]],"\n"))
      }
    }
  }
  # Now, polygon locations
  final.poly.df = data.frame()
  
  if (nrow(poly.df) > 0) {
    # Get all the polygon data for the location codes
    location.types = unname(get.location.type(poly.df$locations))
    unique.location.types = unique(location.types)
    polygon.data = setNames(lapply (unique.location.types, get.polygons.for.type), unique.location.types)
    
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
      merged.poly.data = cbind(original.replicated, location.poly.data)
      return (merged.poly.data)
    })
    
    final.poly.df = do.call(rbind, merged.poly.df)
  }
  
  #Plot
  
  # Decompress the US.MAP.BZIP2
  US.MAP.UNCOMPRESSED = NULL 
  
  # There are three formats acceptable for bb:
  #
  #  - NULL, defaults to entire map
  #  - character(1) value of "AUTO" (bb="AUTO", eg.), calculates the bounding box
  #    from the polygons/points of the locations
  #  - numeric(4) named vector with names "bottom","left","right","top"
  #     (bb=c(left=-125,bottom=24,right=-66, top=50), eg.)
  #
  
  if (!is.null(bb) && 
      ((any(all(is.character(bb), length(bb) == 1, bb == "AUTO"), 
           (all(class(bb) == 'numeric', length(bb) == 4, !is.null(names(bb)), 
                sort(names(bb)) == c("bottom","left","right","top"))))))) {
    # Change the bounding box; this seems to require reloading the tile map from stadia
    updated.bb = NULL
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
      
    } else if (all(class(bb) == 'numeric', length(bb) == 4, !is.null(names(bb)), 
                   sort(names(bb)) == c("bottom","left","right","top"))) {
      updated.bb = bb
    } 
    
    # Reloading the stadia tiles
    US.MAP.UNCOMPRESSED = get_stadiamap(bbox=updated.bb, zoom = calc_zoom(updated.bb), 
                                        maptype = "stamen_toner_background")
    
    attr_map <- attr(US.MAP.UNCOMPRESSED, "bb")    # save attributes from original
    # 
    # ## change color in raster; change the black background to a nicer gray
    US.MAP.UNCOMPRESSED[US.MAP.UNCOMPRESSED == "#000000"] <- "#C0C0C0"
    # Some background is colored with almost-black, change it as well
    US.MAP.UNCOMPRESSED[US.MAP.UNCOMPRESSED == "#010101"] <- "#C0C0C0"
    # 
    # ## correct class, attributes
    class(US.MAP.UNCOMPRESSED) <- c("ggmap", "raster")
    attr(US.MAP.UNCOMPRESSED, "bb") <- attr_map
  
  } else {
    if (!is.null(bb)) {
      # If the bb format is unknown
      bb.names.values <- paste(names(bb), bb, sep=": ", collapse=", ")
      wrn.msg = paste("Unknown value for bounding box (", bb.names.values, "), proceeding with default")
      warning(wrn.msg)
    }
    US.MAP.UNCOMPRESSED = unserialize(memDecompress(LOCATION.MANAGER$US.MAP.BZIP2, type = "bzip2"))
  }
    
  plot = ggmap(US.MAP.UNCOMPRESSED)
  
  if (nrow(point.df) > 0) {
    plot = plot + geom_point(data=point.df, 
                             aes(x=longitude, y=latitude, size=!!sym(size), color=!!sym(color), fill=!!sym(fill)), 
                             shape = pch, alpha = alpha)
  }
  
  if (nrow(poly.df) > 0) {
    plot = plot + geom_polygon(data = final.poly.df, 
                               aes(x=longitude, y=latitude, color=!!sym(color), fill=!!sym(fill), group=poly), 
                               alpha = alpha)
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