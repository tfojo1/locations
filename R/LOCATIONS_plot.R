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
                          mapping,
                          title=NA,
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
  
  # Determine if the locations have polygon data
  location.has.poly = sapply(data[['locations']], LOCATION.MANAGER$has.polygon)
  
  point.df = data[ !location.has.poly, ]
  poly.df = data [ location.has.poly, ]
  
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
  if (nrow(poly.df) > 0) {
    poly.data.list = lapply (poly.df$locations, get.location.polygon)
    names(poly.data.list) <- poly.df$locations
    
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
  US.MAP.UNCOMPRESSED = unserialize(memDecompress(LOCATION.MANAGER$US.MAP.BZIP2, type = "bzip2"))
  
  plot = ggmap(US.MAP.UNCOMPRESSED)
  
  if (nrow(point.df) > 0) {
    plot = plot + geom_point(data=point.df, mapping, shape = pch, alpha = alpha)
  }
  
  if (nrow(poly.df) > 0) {
    mapping$size = NULL #Polygons don't have a size mapping
    mapping = modifyList(mapping, aes(group = poly)) #We have to add the group
    plot = plot + geom_polygon(data = final.poly.df, mapping, alpha = alpha)
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