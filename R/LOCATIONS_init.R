#' Build Location Manager from Data
#'
#' Internal function to construct a Location.Manager instance from
#' serialized data structures. This is called during package load.
#'
#' @param location_data A list containing the serialized location data
#' @return A fully initialized Location.Manager object
#' @keywords internal
build_location_manager <- function(location_data) {

  # Create a fresh Location.Manager instance
  manager <- Location.Manager$new()

  # Restore types
  if (!is.null(location_data$types) && length(location_data$types) > 0) {
    for (type_name in names(location_data$types)) {
      # types is a named list where each element is c(prefix, prefix.longform)
      type_info <- location_data$types[[type_name]]
      manager$register.types(type_name, type_info[1], type_info[2])
    }
  }

  # Restore type relationships matrix
  if (!is.null(location_data$type.matrix)) {
    # Directly assign the matrix (it's already been built)
    manager_env <- environment(manager$initialize)$private
    manager_env$type.matrix <- location_data$type.matrix
  }

  # Restore locations
  # location_data$locations is a data.frame with columns: code, name, type
  if (!is.null(location_data$locations) && nrow(location_data$locations) > 0) {
    for (i in seq_len(nrow(location_data$locations))) {
      code <- location_data$locations$code[i]
      name <- location_data$locations$name[i]
      type <- location_data$locations$type[i]

      # Create a Location object and add it to the manager
      loc <- Location$new(c(name, type))
      manager_env <- environment(manager$initialize)$private
      manager_env$location.list[[code]] <- loc
    }
  }

  # Restore location coordinates (lat/long)
  if (!is.null(location_data$coordinates) && nrow(location_data$coordinates) > 0) {
    for (i in seq_len(nrow(location_data$coordinates))) {
      code <- location_data$coordinates$code[i]
      lat <- location_data$coordinates$lat[i]
      long <- location_data$coordinates$long[i]

      if (!is.na(lat) && !is.na(long)) {
        manager_env <- environment(manager$initialize)$private
        if (!is.null(manager_env$location.list[[code]])) {
          manager_env$location.list[[code]]$set.lat.and.long(lat, long)
        }
      }
    }
  }

  # Restore hierarchical relationships
  # relationships is a data.frame with columns: sub, super, complete
  if (!is.null(location_data$relationships) && nrow(location_data$relationships) > 0) {
    manager_env <- environment(manager$initialize)$private

    for (i in seq_len(nrow(location_data$relationships))) {
      sub_code <- location_data$relationships$sub[i]
      super_code <- location_data$relationships$super[i]
      complete <- location_data$relationships$complete[i]

      # Add relationship to both locations
      if (!is.null(manager_env$location.list[[sub_code]]) &&
          !is.null(manager_env$location.list[[super_code]])) {
        manager_env$location.list[[super_code]]$register.sub.location(sub_code, complete)
        manager_env$location.list[[sub_code]]$register.super.location(super_code, complete)
      }
    }
  }

  # Restore code aliases
  if (!is.null(location_data$alias.codes)) {
    manager_env <- environment(manager$initialize)$private
    manager_env$alias.codes <- location_data$alias.codes
  }

  # Restore name aliases
  if (!is.null(location_data$alias.names)) {
    manager_env <- environment(manager$initialize)$private
    manager_env$alias.names <- location_data$alias.names
  }

  # Restore compressed polygon data
  if (!is.null(location_data$compressed.poly.data)) {
    manager_env <- environment(manager$initialize)$private
    manager_env$compressed.poly.data <- location_data$compressed.poly.data
  }

  # Restore polygon index
  if (!is.null(location_data$poly.index)) {
    manager_env <- environment(manager$initialize)$private
    manager_env$poly.index <- location_data$poly.index
  }

  # Mark which locations have polygon data
  if (!is.null(location_data$locations.with.polygons)) {
    manager_env <- environment(manager$initialize)$private
    for (code in location_data$locations.with.polygons) {
      if (!is.null(manager_env$location.list[[code]])) {
        manager_env$location.list[[code]]$set.poly.data()
      }
    }
  }

  return(manager)
}


#' Extract Location Manager Data
#'
#' Internal function to extract all data from a Location.Manager instance
#' into serializable data structures (data.frames and lists).
#'
#' @param manager A Location.Manager object
#' @return A list containing all location data in simple structures
#' @keywords internal
extract_location_data <- function(manager) {

  manager_env <- environment(manager$initialize)$private

  # Extract location list into a data.frame
  location_codes <- names(manager_env$location.list)

  if (length(location_codes) > 0) {
    locations_df <- data.frame(
      code = location_codes,
      name = sapply(location_codes, function(code) {
        manager_env$location.list[[code]]$return.name
      }),
      type = sapply(location_codes, function(code) {
        manager_env$location.list[[code]]$return.type
      }),
      stringsAsFactors = FALSE
    )

    # Extract coordinates into a separate data.frame
    coordinates_df <- data.frame(
      code = location_codes,
      lat = sapply(location_codes, function(code) {
        manager_env$location.list[[code]]$return.lat
      }),
      long = sapply(location_codes, function(code) {
        manager_env$location.list[[code]]$return.long
      }),
      stringsAsFactors = FALSE
    )

    # Extract relationships into a data.frame
    relationships_list <- list()
    for (code in location_codes) {
      loc <- manager_env$location.list[[code]]
      contains_list <- loc$contains.list

      if (length(contains_list) > 0) {
        for (relationship in contains_list) {
          relationships_list[[length(relationships_list) + 1]] <- list(
            sub = relationship[1],
            super = code,
            complete = as.logical(relationship[2])
          )
        }
      }
    }

    relationships_df <- if (length(relationships_list) > 0) {
      do.call(rbind, lapply(relationships_list, as.data.frame, stringsAsFactors = FALSE))
    } else {
      data.frame(sub = character(), super = character(), complete = logical(),
                 stringsAsFactors = FALSE)
    }

    # Find which locations have polygon data
    locations_with_polygons <- location_codes[sapply(location_codes, function(code) {
      manager_env$location.list[[code]]$has.poly.data
    })]

  } else {
    locations_df <- data.frame(code = character(), name = character(),
                               type = character(), stringsAsFactors = FALSE)
    coordinates_df <- data.frame(code = character(), lat = numeric(),
                                 long = numeric(), stringsAsFactors = FALSE)
    relationships_df <- data.frame(sub = character(), super = character(),
                                   complete = logical(), stringsAsFactors = FALSE)
    locations_with_polygons <- character()
  }

  # Return all data in simple structures
  list(
    types = manager_env$types,
    type.matrix = manager_env$type.matrix,
    locations = locations_df,
    coordinates = coordinates_df,
    relationships = relationships_df,
    alias.codes = manager_env$alias.codes,
    alias.names = manager_env$alias.names,
    compressed.poly.data = manager_env$compressed.poly.data,
    poly.index = manager_env$poly.index,
    locations.with.polygons = locations_with_polygons
  )
}
