library(ggplot2)
library(ggmap)

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
                          poly.data=NA,
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
  
  # Get the coordinates for all the locations included in the data.frame; they
  # are returned as a character value with two values separated by a comma.
  # The first value is latitude, the second value is longitude
  coordinates = get.location.coords(data[['locations']])
  
  lat.lon = strsplit(coordinates, ",")
  data["latitude"] = as.numeric(sapply(lat.lon, function(x) {return(x[1])}))
  data["longitude"] = as.numeric(sapply(lat.lon, function(x) {return(x[2])}))
  
  # Are any of the locations NA?  This is a result of missing location data.
  na.indexes = which(is.na(data["latitude"]))
  
  # If there are any NA locations, display a list
  if (length(na.indexes) > 0) {
    cat(paste0("Unable to plot ", length(na.indexes)," locations, no location data :\n"))
    for (i in seq_along(na.indexes)) {
      # We could print the name in those dataframes that contain it, but all we know for 
      # sure is that this dataframe contains a locations column
      cat(paste0(i, ". ", data[["locations"]][na.indexes[i]],"\n"))
    }
  }
  
  
  state.data = poly.data [ poly.data$NAME %in% c("Washington","Florida", "Wyoming"), ]
  # state.data = poly.data
  
  plot = ggmap(US.MAP) +
    geom_point(data=data, mapping, shape = pch, alpha = alpha) +
    geom_polygon(data = state.data, aes(x = longitude, y = latitude, group=poly), fill = "cyan", color = "black", alpha = 1.0) +
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
  } 
  
  plot
}

##--------------------------------------------------##
##-- THE GENERAL PLOT FUNCTION AND NECESSARY DATA --##
##--------------------------------------------------##
register_stadiamaps(api_key)
#US.MAP = get_stamenmap(bbox=c(left=-125,bottom=24, right=-66, top=50),zoom=4, maptype='toner-background')
US.MAP = get_stadiamap(bbox=c(left=-125,bottom=24,right=-66, top=50), zoom = 5, maptype = "stamen_toner_background")

# ALL ON BEFORE JP
attr_map <- attr(US.MAP, "bb")    # save attributes from original
# 
# ## change color in raster; change the black background to a nicer gray
US.MAP[US.MAP == "#000000"] <- "#C0C0C0"
# Some background is colored with almost-black, change it as well
US.MAP[US.MAP == "#010101"] <- "#C0C0C0"
# 
# ## correct class, attributes
class(US.MAP) <- c("ggmap", "raster")
attr(US.MAP, "bb") <- attr_map

# Load the state polygon data
poly.data = read.csv("data-raw/geom_data.csv", stringsAsFactors = FALSE)

# Initialize the "poly" column
poly.data$poly <- rep(1, nrow(poly.data))

# Variables to store the first point of the first polygon in each state
first_lat <-  poly.data$latitude[1]
first_long <- poly.data$longitude[1]
poly.index <- 1
poly.reset = FALSE

# Iterate through rows to increment "poly" when a new polygon starts
# Here we are counting polygons in the set; some states have multiple polygons
# and geom_polygon has a group= feature that allows you to group by polygon.
# So we are numbering the polygons here
for (i in 2:nrow(poly.data)) {
  poly.data$poly[i] = poly.index
  
  if (poly.reset) {
    first_lat = poly.data$latitude[i]
    first_long = poly.data$longitude[i]
  }
  
  if (poly.data$latitude[i] == first_lat && poly.data$longitude[i] == first_long && !poly.reset) {
    #This is the end of a polygon
    poly.index = poly.index + 1
    poly.reset = TRUE
  } else {
    poly.reset = FALSE
  }
}

# Test plot: State CBSAs
state.cbsa = get.contained.locations("TX","CBSA")
state2.cbsa = get.contained.locations("OH", "CBSA")
name_data = c(names(state.cbsa),names(state2.cbsa))
code_data = c(unname(state.cbsa),unname(state2.cbsa))

state.df = data.frame(locations=code_data, size=rep(1,length(code_data)), color=seq(1,length(code_data)))

location.plot(state.df,aes(x=longitude, y=latitude,size=size,color=color), "State CBSAs", poly.data)
