#' Validate Serialized Location Data
#'
#' Checks structural invariants that must hold before serialized location data
#' are packaged or used to construct a location manager.
#'
#' @param location_data A serialized location data list as returned by
#'   extract_location_data().
#' @return `TRUE`, invisibly. Throws an error containing every detected
#'   integrity violation.
#' @keywords internal
validate_location_data <- function(location_data) {
  errors <- character()
  add_error <- function(message) {
    errors <<- c(errors, message)
  }

  required_fields <- c(
    "types", "type.matrix", "locations", "coordinates", "relationships",
    "alias.codes", "alias.names", "compressed.poly.data", "poly.index",
    "locations.with.polygons"
  )
  missing_fields <- setdiff(required_fields, names(location_data))
  if (length(missing_fields) > 0) {
    add_error(paste("Missing fields:", paste(missing_fields, collapse = ", ")))
  }

  if (length(errors) > 0) {
    stop_location_data_validation(errors)
  }

  locations <- location_data$locations
  location_columns <- c("code", "name", "type")
  if (!is.data.frame(locations)) {
    add_error("locations must be a data.frame")
  } else {
    missing_columns <- setdiff(location_columns, names(locations))
    if (length(missing_columns) > 0) {
      add_error(paste(
        "locations is missing columns:",
        paste(missing_columns, collapse = ", ")
      ))
    } else {
      invalid_codes <- is.na(locations$code) | locations$code == ""
      if (any(invalid_codes)) {
        add_error("Location codes must be non-missing and non-empty")
      }
      duplicate_codes <- unique(locations$code[duplicated(locations$code)])
      if (length(duplicate_codes) > 0) {
        add_error(paste(
          "Duplicate location codes:",
          paste(duplicate_codes, collapse = ", ")
        ))
      }
      invalid_names <- is.na(locations$name) | locations$name == ""
      if (any(invalid_names)) {
        add_error("Location names must be non-missing and non-empty")
      }
      unknown_types <- setdiff(
        unique(locations$type), names(location_data$types)
      )
      if (length(unknown_types) > 0) {
        add_error(paste(
          "Locations use unregistered types:",
          paste(unknown_types, collapse = ", ")
        ))
      }
    }
  }

  registered_types <- names(location_data$types)
  type_matrix <- location_data$type.matrix
  if (!is.matrix(type_matrix)) {
    add_error("type.matrix must be a matrix")
  } else if (!setequal(rownames(type_matrix), registered_types) ||
               !setequal(colnames(type_matrix), registered_types)) {
    add_error("type.matrix row and column names must match registered types")
  }

  location_codes <- if (
    is.data.frame(locations) && "code" %in% names(locations)
  ) locations$code else character()

  coordinates <- location_data$coordinates
  if (!is.data.frame(coordinates) ||
        !all(c("code", "lat", "long") %in% names(coordinates))) {
    add_error("coordinates must have code, lat, and long columns")
  } else {
    duplicate_coordinates <- unique(
      coordinates$code[duplicated(coordinates$code)]
    )
    if (length(duplicate_coordinates) > 0) {
      add_error(paste(
        "Duplicate coordinate codes:",
        paste(duplicate_coordinates, collapse = ", ")
      ))
    }
    unknown_coordinates <- setdiff(coordinates$code, location_codes)
    if (length(unknown_coordinates) > 0) {
      add_error(paste(
        "Coordinates reference missing locations:",
        paste(unknown_coordinates, collapse = ", ")
      ))
    }
  }

  relationships <- location_data$relationships
  relationship_columns <- c("sub", "super", "complete")
  if (!is.data.frame(relationships) ||
        !all(relationship_columns %in% names(relationships))) {
    add_error("relationships must have sub, super, and complete columns")
  } else {
    invalid_relationship_codes <-
      is.na(relationships$sub) | relationships$sub == "" |
      is.na(relationships$super) | relationships$super == ""
    if (any(invalid_relationship_codes)) {
      add_error("Relationship codes must be non-missing and non-empty")
    }
    duplicate_relationships <- duplicated(relationships[, relationship_columns])
    if (any(duplicate_relationships)) {
      add_error("Duplicate relationships are not allowed")
    }
    if (!is.logical(relationships$complete) || anyNA(relationships$complete)) {
      add_error("Relationship completeness must be non-missing logical values")
    }
    missing_sub <- setdiff(relationships$sub, location_codes)
    if (length(missing_sub) > 0) {
      add_error(paste(
        "Relationship children reference missing locations:",
        paste(missing_sub, collapse = ", ")
      ))
    }
    missing_super <- setdiff(relationships$super, location_codes)
    if (length(missing_super) > 0) {
      add_error(paste(
        "Relationship parents reference missing locations:",
        paste(missing_super, collapse = ", ")
      ))
    }
    if (any(relationships$sub == relationships$super, na.rm = TRUE)) {
      add_error("Self-relationships are not allowed")
    }
    if (!any(invalid_relationship_codes) &&
          length(missing_sub) == 0 && length(missing_super) == 0 &&
          relationship_graph_has_cycle(relationships)) {
      add_error("Relationship graph contains a cycle")
    }
  }

  alias_rows <- collect_code_aliases(location_data$alias.codes)
  if (nrow(alias_rows) > 0) {
    unknown_alias_types <- setdiff(unique(alias_rows$type), registered_types)
    if (length(unknown_alias_types) > 0) {
      add_error(paste(
        "Aliases use unregistered types:",
        paste(unknown_alias_types, collapse = ", ")
      ))
    }
    duplicate_aliases <- unique(alias_rows$alias[duplicated(alias_rows$alias)])
    if (length(duplicate_aliases) > 0) {
      add_error(paste(
        "Alias codes must be globally unique:",
        paste(duplicate_aliases, collapse = ", ")
      ))
    }
    shadowed_aliases <- intersect(alias_rows$alias, location_codes)
    if (length(shadowed_aliases) > 0) {
      add_error(paste(
        "Alias codes shadow canonical location codes:",
        paste(shadowed_aliases, collapse = ", ")
      ))
    }
    missing_targets <- setdiff(alias_rows$target, location_codes)
    if (length(missing_targets) > 0) {
      add_error(paste(
        "Aliases reference missing targets:",
        paste(missing_targets, collapse = ", ")
      ))
    }

    if (length(missing_targets) == 0 && is.data.frame(locations) &&
          all(location_columns %in% names(locations))) {
      target_types <- locations$type[match(alias_rows$target, locations$code)]
      mismatched_types <- alias_rows$alias[target_types != alias_rows$type]
      if (length(mismatched_types) > 0) {
        add_error(paste(
          "Aliases target locations of a different type:",
          paste(mismatched_types, collapse = ", ")
        ))
      }
    }
  }

  polygon_locations <- location_data$locations.with.polygons
  duplicate_polygon_locations <- unique(
    polygon_locations[duplicated(polygon_locations)]
  )
  if (length(duplicate_polygon_locations) > 0) {
    add_error("locations.with.polygons contains duplicates")
  }
  missing_polygon_locations <- setdiff(polygon_locations, location_codes)
  if (length(missing_polygon_locations) > 0) {
    add_error(paste(
      "Polygon flags reference missing locations:",
      paste(missing_polygon_locations, collapse = ", ")
    ))
  }

  if (length(errors) > 0) {
    stop_location_data_validation(unique(errors))
  }

  invisible(TRUE)
}

collect_code_aliases <- function(alias_codes) {
  rows <- lapply(names(alias_codes), function(type) {
    aliases <- alias_codes[[type]]
    if (length(aliases) == 0) {
      return(NULL)
    }
    data.frame(
      type = rep(type, length(aliases)),
      alias = names(aliases),
      target = unname(unlist(aliases, use.names = FALSE)),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame(
      type = character(), alias = character(), target = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

relationship_graph_has_cycle <- function(relationships) {
  if (nrow(relationships) == 0) {
    return(FALSE)
  }

  edges <- unique(relationships[, c("sub", "super")])
  nodes <- unique(c(edges$sub, edges$super))
  indegree <- tabulate(match(edges$super, nodes), nbins = length(nodes))
  children <- split(edges$super, edges$sub)
  queue <- nodes[indegree == 0]
  visited <- 0L

  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    visited <- visited + 1L
    next_nodes <- children[[current]]
    if (is.null(next_nodes)) {
      next
    }
    for (next_node in next_nodes) {
      index <- match(next_node, nodes)
      indegree[index] <- indegree[index] - 1L
      if (indegree[index] == 0L) {
        queue <- c(queue, next_node)
      }
    }
  }

  visited != length(nodes)
}

stop_location_data_validation <- function(errors) {
  stop(
    paste(
      c("Invalid location_data:", paste0("- ", errors)),
      collapse = "\n"
    ),
    call. = FALSE
  )
}
