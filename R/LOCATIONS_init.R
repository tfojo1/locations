#' Build Location Manager from Data
#'
#' Internal function to construct a Location.Manager instance from
#' serialized data structures. This is called during package load.
#'
#' @param location_data A list containing the serialized location data
#' @return A fully initialized Location.Manager object
#' @keywords internal
build_location_manager <- function(location_data) {

  # Validate input data structure
  required_fields <- c("types", "type.matrix", "locations", "coordinates",
                       "relationships", "alias.codes", "alias.names",
                       "compressed.poly.data", "poly.index", "locations.with.polygons")

  missing_fields <- setdiff(required_fields, names(location_data))
  if (length(missing_fields) > 0) {
    stop("Invalid location_data structure. Missing fields: ",
         paste(missing_fields, collapse = ", "))
  }

  # Create a fresh Location.Manager instance
  manager <- Location.Manager$new()

  # Get private environment once
  mgr_private <- environment(manager$initialize)$private

  # Restore types
  if (!is.null(location_data$types) && length(location_data$types) > 0) {
    for (type_name in names(location_data$types)) {
      type_info <- location_data$types[[type_name]]
      manager$register.types(type_name, type_info[1], type_info[2])
    }
  }

  # Restore type relationships matrix
  if (!is.null(location_data$type.matrix) && length(location_data$type.matrix) > 0) {
    mgr_private$type.matrix <- location_data$type.matrix
  }

  # Restore locations directly into locations_df and indexes
  locs <- location_data$locations
  coords <- location_data$coordinates
  polys_set <- location_data$locations.with.polygons

  if (!is.null(locs) && nrow(locs) > 0) {
    # Build locations_df with all columns
    locations_df <- data.frame(
      code = locs$code,
      name = locs$name,
      type = locs$type,
      lat = NA_real_,
      long = NA_real_,
      has_poly = FALSE,
      stringsAsFactors = FALSE
    )

    # Merge in coordinates
    if (!is.null(coords) && nrow(coords) > 0) {
      coord_match <- match(locations_df$code, coords$code)
      has_coord <- !is.na(coord_match)
      locations_df$lat[has_coord] <- coords$lat[coord_match[has_coord]]
      locations_df$long[has_coord] <- coords$long[coord_match[has_coord]]
    }

    # Mark polygon data
    if (!is.null(polys_set) && length(polys_set) > 0) {
      locations_df$has_poly[locations_df$code %in% polys_set] <- TRUE
    }

    # Set the data.frame
    mgr_private$locations_df <- locations_df

    # Build code index
    for (i in seq_len(nrow(locations_df))) {
      mgr_private$code_index[[locations_df$code[i]]] <- i
    }

    # Build type index
    for (type in unique(locations_df$type)) {
      mgr_private$type_index[[type]] <- locations_df$code[locations_df$type == type]
    }
  }

  # Restore hierarchical relationships into hash maps
  rels <- location_data$relationships
  if (!is.null(rels) && nrow(rels) > 0) {
    for (i in seq_len(nrow(rels))) {
      sub_code <- rels$sub[i]
      super_code <- rels$super[i]
      is_complete <- rels$complete[i]

      if (is_complete) {
        existing <- mgr_private$contains_complete[[super_code]]
        mgr_private$contains_complete[[super_code]] <- c(existing, sub_code)

        existing <- mgr_private$contained_by_complete[[sub_code]]
        mgr_private$contained_by_complete[[sub_code]] <- c(existing, super_code)
      } else {
        existing <- mgr_private$contains_partial[[super_code]]
        mgr_private$contains_partial[[super_code]] <- c(existing, sub_code)

        existing <- mgr_private$contained_by_partial[[sub_code]]
        mgr_private$contained_by_partial[[sub_code]] <- c(existing, super_code)
      }
    }
  }

  # Restore code aliases
  if (!is.null(location_data$alias.codes) && length(location_data$alias.codes) > 0) {
    mgr_private$alias.codes <- location_data$alias.codes
  }

  # Restore name aliases
  if (!is.null(location_data$alias.names) && length(location_data$alias.names) > 0) {
    mgr_private$alias.names <- location_data$alias.names
  }

  # Restore compressed polygon data
  if (!is.null(location_data$compressed.poly.data) && length(location_data$compressed.poly.data) > 0) {
    mgr_private$compressed.poly.data <- location_data$compressed.poly.data
  }

  # Restore polygon index
  if (!is.null(location_data$poly.index)) {
    mgr_private$poly.index <- location_data$poly.index
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

  # Get private environment once
  mgr_private <- environment(manager$initialize)$private

  locations_df <- mgr_private$locations_df

  if (!is.null(locations_df) && nrow(locations_df) > 0) {
    # Extract locations (code, name, type)
    locs_out <- data.frame(
      code = locations_df$code,
      name = locations_df$name,
      type = locations_df$type,
      stringsAsFactors = FALSE
    )

    # Extract coordinates
    coords_out <- data.frame(
      code = locations_df$code,
      lat = locations_df$lat,
      long = locations_df$long,
      stringsAsFactors = FALSE
    )

    # Extract relationships from the hash maps
    relationships_list <- list()

    # Walk contains_complete to build relationship rows
    for (super_code in ls(mgr_private$contains_complete)) {
      sub_codes <- mgr_private$contains_complete[[super_code]]
      for (sub_code in sub_codes) {
        relationships_list[[length(relationships_list) + 1]] <- list(
          sub = sub_code, super = super_code, complete = TRUE
        )
      }
    }
    for (super_code in ls(mgr_private$contains_partial)) {
      sub_codes <- mgr_private$contains_partial[[super_code]]
      for (sub_code in sub_codes) {
        relationships_list[[length(relationships_list) + 1]] <- list(
          sub = sub_code, super = super_code, complete = FALSE
        )
      }
    }

    relationships_df <- if (length(relationships_list) > 0) {
      do.call(rbind, lapply(relationships_list, as.data.frame, stringsAsFactors = FALSE))
    } else {
      data.frame(sub = character(), super = character(), complete = logical(),
                 stringsAsFactors = FALSE)
    }

    # Find locations with polygon data
    locations_with_polygons <- locations_df$code[locations_df$has_poly]

  } else {
    locs_out <- data.frame(code = character(), name = character(),
                            type = character(), stringsAsFactors = FALSE)
    coords_out <- data.frame(code = character(), lat = numeric(),
                              long = numeric(), stringsAsFactors = FALSE)
    relationships_df <- data.frame(sub = character(), super = character(),
                                    complete = logical(), stringsAsFactors = FALSE)
    locations_with_polygons <- character()
  }

  list(
    types = mgr_private$types,
    type.matrix = mgr_private$type.matrix,
    locations = locs_out,
    coordinates = coords_out,
    relationships = relationships_df,
    alias.codes = mgr_private$alias.codes,
    alias.names = mgr_private$alias.names,
    compressed.poly.data = mgr_private$compressed.poly.data,
    poly.index = mgr_private$poly.index,
    locations.with.polygons = locations_with_polygons
  )
}
