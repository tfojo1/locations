#' @importFrom R6 R6Class
Location.Manager = R6::R6Class("LocationManager",
  class = FALSE,
  clone = FALSE,
  active = list (
    get.type.matrix = function() {
      return(private$type.matrix)
    },
    read.location.list = function() {
      return(private$locations_df)
    }
  ),
  private = list (
    # Core data storage (data.frames)
    locations_df = NULL,       # data.frame: code, name, type, lat, long, has_poly

    # Hash indexes (environments for O(1) lookup)
    code_index = NULL,         # code -> row index in locations_df
    type_index = NULL,         # type -> character vector of codes

    # Relationship indexes
    contains_complete = NULL,  # code -> character vector of completely contained codes
    contains_partial = NULL,   # code -> character vector of partially contained codes
    contained_by_complete = NULL, # code -> character vector of completely enclosing codes
    contained_by_partial = NULL,  # code -> character vector of partially enclosing codes

    # Other data (unchanged from original)
    alias.names = list(),
    alias.codes = list(),
    types = list(),
    type.matrix = matrix(nrow = 0, ncol = 0),
    compressed.poly.data = list(),
    poly.index = 0,

    # -- Internal helpers --

    lookup_row = function(code) {
      # Returns the row index for a code, or NA if not found
      idx <- private$code_index[[code]]
      if (is.null(idx)) NA_integer_ else idx
    },

    check.is.type = function (type) {
      type %in% names(private$types)
    },
    check.code.validity = function(code) {
      grepl("^[A-Za-z0-9.-]*$", code)
    },
    check.location = function (location, suggest.options) {
      location <- toupper(location)

      # Check if it's a known code
      if (!is.null(private$code_index[[location]])) {
        return(TRUE)
      }

      # Check if it's an alias
      location.codes = c()
      for (type in names(private$alias.codes)) {
        if (location %in% names(private$alias.codes[[type]])) {
          location.codes = c(location.codes, private$alias.codes[[type]][[location]])
        }
      }

      if (length(location.codes) == 1) {
        return(TRUE)
      }

      if (suggest.options) {
        types = names(private$alias.codes)
        for (type in types) {
          if (location %in% names(private$alias.codes[[type]])) {
            location.code <- private$alias.codes[[type]][[location]]
            cat(sprintf("Possible Result : %s alias of location %s, %s\n", type, location.code, self$get.names(location.code)))
          }
        }
      }
      return(FALSE)
    },
    resolve.code = function(code, fail.on.unknown=T) {
      # Resolves a single location code from potential alias to actual code
      code <- toupper(code)

      if (is.null(private$code_index[[code]])) {
        # Not a direct code, check aliases
        types = names(private$alias.codes)
        alias.target = character()

        for (type in types) {
          if (code %in% names(private$alias.codes[[type]])) {
            alias.target <- c(alias.target, private$alias.codes[[type]][[code]])
          }
        }

        result.length = length(alias.target)

        if (result.length != 0) {
          if (result.length == 1) {
            code <- alias.target
          } else {
            stop(paste0("LOCATION.MANAGER: BUG - Currently code aliases are unique across all types ", alias.target))
          }
        } else {
          if (fail.on.unknown) {
            stop(paste0("LOCATION.MANAGER: The location code used (", code, ") cannot be recognized, stopping"))
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
        if (length(name) == 2) {
          name[1] <- sub("(Saint)(.*Louis)", "St.\\2", name[1])
          name[1] <- sub("N Havn","New Haven", name[1])
          state.split = trimws(toupper(gsub("\\.","",strsplit(name[2], "-")[[1]])))
          state.replace = sapply(state.split, function(res) {
            if (res %in% names(state.map)) {
              return (state.map[res])
            }
            return (res)
          })
          state.final = paste(state.replace, collapse = "-")
          return (paste0(name[1], ", ", state.final))
        } else if (length(name) != 1)  {
          warning(paste0("An additional split on ", as.vector(name)))
        } else {
          name = unlist(strsplit(name, " (?=[^ ]*$)", perl = TRUE))
        }
        return (paste(name, collapse=","))
      })
    },

    # -- Relationship helpers --

    get_sub_codes = function(code, completely) {
      if (completely) {
        private$contains_complete[[code]] %||% character(0)
      } else {
        private$contains_partial[[code]] %||% character(0)
      }
    },
    get_super_codes = function(code, completely) {
      if (completely) {
        private$contained_by_complete[[code]] %||% character(0)
      } else {
        private$contained_by_partial[[code]] %||% character(0)
      }
    }
  ),
  public = list (
    initialize = function () {
      private$locations_df <- data.frame(
        code = character(0),
        name = character(0),
        type = character(0),
        lat = numeric(0),
        long = numeric(0),
        has_poly = logical(0),
        stringsAsFactors = FALSE
      )
      private$code_index <- new.env(hash = TRUE, parent = emptyenv())
      private$type_index <- new.env(hash = TRUE, parent = emptyenv())
      private$contains_complete <- new.env(hash = TRUE, parent = emptyenv())
      private$contains_partial <- new.env(hash = TRUE, parent = emptyenv())
      private$contained_by_complete <- new.env(hash = TRUE, parent = emptyenv())
      private$contained_by_partial <- new.env(hash = TRUE, parent = emptyenv())
    },
    number.polygons = function(df) {
      df$poly <- rep(private$poly.index, nrow(df))
      current_lat <-  df$latitude[1]
      current_long <- df$longitude[1]
      poly.reset = FALSE

      for (i in 2:nrow(df)) {
        df$poly[i] = private$poly.index

        if (poly.reset) {
          current_lat = df$latitude[i]
          current_long = df$longitude[i]
        }

        if (df$latitude[i] == current_lat && df$longitude[i] == current_long && !poly.reset) {
          if (i != nrow(df)) {
            private$poly.index = private$poly.index + 1
          }
          poly.reset = TRUE
        } else {
          poly.reset = FALSE
        }
      }

      private$poly.index = private$poly.index + 1
      return (df)
    },
    add.poly.data = function(type, dataframe) {
      private$compressed.poly.data[[type]] = memCompress(serialize(dataframe, NULL), type = "bzip2")
    },
    add.to.poly.data = function(type, dataframe) {
      if (is.null(private$compressed.poly.data[[type]])) {
        self$add.poly.data(type, dataframe)
      } else {
        current_poly_data_type = unserialize(memDecompress(private$compressed.poly.data[[type]], type = "bzip2"))
        current_poly_data_type = rbind(current_poly_data_type, dataframe)
        private$compressed.poly.data[[type]] = memCompress(serialize(current_poly_data_type, NULL), type = "bzip2")
      }
    },
    get.polys.for.type = function(type) {
      if (!is.null(private$compressed.poly.data[[type]])) {
        return (unserialize(memDecompress(private$compressed.poly.data[[type]], type = "bzip2")))
      }
      return (NA)
    },
    sanitize.codes = function(codes) {
      clean.codes = unlist(lapply(codes, private$resolve.code))
      setNames(clean.codes, codes)
    },
    type.composition = function(super.location.type, sub.location.type) {
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
      returned.coords = unlist(lapply(locations, function(x) { private$resolve.code(x, F) }))
      returned.coords[!is.na(returned.coords)] = unlist(lapply(returned.coords[!is.na(returned.coords)], function(x) {
        idx <- private$lookup_row(x)
        if (is.na(idx)) return(NA)
        lat <- private$locations_df$lat[idx]
        long <- private$locations_df$long[idx]
        if (any(is.na(c(lat, long)))) {
          return(NA)
        } else {
          return(paste(lat, long, sep = ","))
        }
      }))
      names(returned.coords) = locations
      returned.coords
    },
    has.polygon = function(location) {
      clean.code = private$resolve.code(location, F)
      if (is.na(clean.code)) return(NA)
      idx <- private$lookup_row(clean.code)
      if (is.na(idx)) return(NA)
      return(private$locations_df$has_poly[idx])
    },
    has.lat.lon = function(location) {
      clean.code = private$resolve.code(location, F)
      if (is.na(clean.code)) return(NA)
      idx <- private$lookup_row(clean.code)
      if (is.na(idx)) return(NA)
      return(!is.na(private$locations_df$long[idx]))
    },
    has.location = function(location) {
      clean.code = private$resolve.code(location, F)
      return(!is.na(clean.code))
    },
    get.names = function(locations) {
      returned.names = unlist(lapply(locations, function(x) { private$resolve.code(x, F) }))
      returned.names[!is.na(returned.names)] = unlist(lapply(returned.names[!is.na(returned.names)], function(x) {
        idx <- private$lookup_row(x)
        if (is.na(idx)) return(NA)
        private$locations_df$name[idx]
      }))
      names(returned.names) = locations
      returned.names
    },
    get.codes.from.names = function(location.names, types, search.aliases) {
      mapply(function(location, type) {
        if (is.na(location)) {
          return(NA)
        }
        # Check if location is itself a code of the right type
        idx <- private$lookup_row(location)
        if (!is.na(idx) && private$locations_df$type[idx] == type) {
          return(location)
        }
        # Search by name among locations of this type
        type_codes <- private$type_index[[type]]
        if (is.null(type_codes) || length(type_codes) == 0) return(NA)

        type_rows <- vapply(type_codes, private$lookup_row, integer(1))
        type_names <- private$locations_df$name[type_rows]

        indexes = which(type_names == location)
        if (length(indexes) == 0) {
          indexes = grep(location, type_names)
        }

        rv = type_codes[indexes]

        if (length(rv) == 0) {
          if (search.aliases) {
            if (location %in% names(private$alias.codes[[type]])) {
              return(private$alias.codes[[type]][[location]])
            }
          }
          return(NA)
        } else {
          return(rv)
        }
      }, location.names, toupper(types), SIMPLIFY = FALSE)
    },
    get.by.alias = function(aliases, types) {
      types = toupper(types)
      types = if (length(types) == 1) rep(types, length(aliases)) else types

      setNames(mapply(function(alias, type) {
        if (alias %in% names(private$alias.codes[[type]])) {
          return(private$alias.codes[[type]][alias])
        } else {
          return(NA)
        }
      }, toupper(aliases), types, SIMPLIFY = T), aliases)
    },
    get.types = function(locations) {
      returned.types = unlist(lapply(locations, function(x) { private$resolve.code(x, F) }))
      returned.types[!is.na(returned.types)] = unlist(lapply(returned.types[!is.na(returned.types)], function(x) {
        idx <- private$lookup_row(x)
        if (is.na(idx)) return(NA)
        private$locations_df$type[idx]
      }))
      names(returned.types) = locations
      returned.types
    },
    get.type.by.name = function(names, chosen.type, warnings = T) {
      chosen.type = toupper(chosen.type)
      if (!private$check.is.type(chosen.type)) {
        stop(paste0("Calling get.type.from.name with invalid type ", chosen.type))
      }
      sapply(private$normalize.names(names), function(name) {
        split = private$trim.white.space(gsub("\\(.*?\\)", "", strsplit(name,",")[[1]]))
        name.check = self$get.codes.from.names(split[1], chosen.type, T)[[1]]
        if (is.na(name.check[1])) {
          second.split = private$trim.white.space(strsplit(split[1], "[-/]+")[[1]])
          name.check = self$get.codes.from.names(second.split[1], chosen.type, T)[[1]]
          if (is.na(name.check[1])) {
            if (!grepl("division", name, ignore.case = T)) {
              msg = paste0("Couldn't find anything for ", name, ", even after '-' split")
              if (warnings) {
                warning(msg)
              } else {
                print(msg)
              }
            }
            return(NA)
          }
        }
        if (length(name.check) == 1) {
          return(name.check)
        } else {
          additional.splits = length(split) - 1
          if (additional.splits == 0) {
            msg = paste0("Muiltiple results for ", name, " without additional data")
            if (warnings) {
              warning(msg)
            } else {
              print(msg)
            }
            return(NA)
          } else if (additional.splits == 1) {
            state = split[2]
            index = which(sapply(name.check, function(possibility) {
              returned.states = self$get.containing(possibility, "STATE", F)
              any(returned.states %in% state)
            }) == TRUE)
            return(name.check[index])
          } else {
            msg = paste0("More than 2 divisions for ", name)
            if (warnings) {
              warning(msg)
            } else {
              print(msg)
            }
            return(NA)
          }
        }
      })
    },
    get.prefix = function(location.types) {
      location.types <- toupper(location.types)
      rv = lapply(seq_along(location.types), function(index) {
        if (location.types[index] %in% names(private$types)) {
          return(private$types[[location.types[index]]][1])
        } else {
          return(NA)
        }
      })
      names(rv) = location.types
      rv
    },
    check.many.locations = function(locations, suggest.options) {
      rv = sapply(locations, function(l) {
        return(private$check.location(l, suggest.options))
      })
      names(rv) = locations
      return(rv)
    },
    get.contained = function(locations, sub.type, limit.to.completely.enclosing, return.list = F, throw.error.if.unregistered.type = T) {
      sub.type = toupper(sub.type)

      if (throw.error.if.unregistered.type) {
        if (!sub.type %in% names(private$types)) {
          stop(paste0("LOCATION.MANAGER$get.contained: Type ", sub.type, " not registered, aborting"))
        }
      }

      codes = unlist(lapply(locations, function(x) { private$resolve.code(x, F) }))

      # BFS to find all contained locations
      all.sub.locations = lapply(codes, function(code) {
        if (is.na(code)) return(NA)

        # BFS over completely contained
        visited <- code
        queue <- private$get_sub_codes(code, TRUE)

        while (length(queue) > 0) {
          current <- queue[1]
          queue <- queue[-1]
          if (!current %in% visited) {
            visited <- c(visited, current)
            children <- private$get_sub_codes(current, TRUE)
            queue <- c(queue, setdiff(children, visited))
          }
        }

        if (!limit.to.completely.enclosing) {
          # Also add partially contained children of all visited nodes
          partial <- character(0)
          for (v in visited) {
            partial <- c(partial, private$get_sub_codes(v, FALSE))
          }
          visited <- unique(c(visited, partial))
        }

        visited
      })

      # Filter by sub.type
      all.sub.locations = lapply(all.sub.locations, function(locs) {
        if (length(locs) == 1 && is.na(locs[1])) return(NA)
        # Look up types for all codes
        types_for_locs <- vapply(locs, function(loc) {
          idx <- private$lookup_row(loc)
          if (is.na(idx)) return(NA_character_)
          private$locations_df$type[idx]
        }, character(1))
        filtered <- locs[types_for_locs == sub.type & !is.na(types_for_locs)]
        if (length(filtered) == 0) return(NA)
        names(filtered) <- sapply(filtered, function(id) self$get.names(id))
        filtered
      })

      if (return.list) {
        return(setNames(all.sub.locations, codes))
      }

      rv = unname(unlist(lapply(all.sub.locations, function(l) {
        l[!is.na(l)]
      })))

      if (length(rv) == 0) {
        return(character())
      }

      setNames(rv, sapply(rv, function(loc_id) self$get.names(loc_id)))
    },
    get.overlapping = function(locations, type, return.list = F, throw.error.if.unregistered.type = T) {
      type = toupper(type)

      if (throw.error.if.unregistered.type) {
        if (!type %in% names(private$types)) {
          stop(paste0("LOCATION.MANAGER$get.overlapping: Type ", type, " not registered, aborting"))
        }
      }

      codes = unlist(lapply(locations, function(x) { private$resolve.code(x, F) }))

      result = lapply(codes, function(code) {
        contained.results = self$get.contained(code, type, TRUE, FALSE, throw.error.if.unregistered.type)
        containing.results = self$get.containing(code, type, FALSE, FALSE, throw.error.if.unregistered.type)

        combined.cont = c(contained.results, containing.results)

        # Find overlapping via shared counties
        list.of.counties.in.locations = self$get.contained(code, "COUNTY", FALSE, FALSE, throw.error.if.unregistered.type)

        list.of.all.by.type = self$get.all.type(type)
        all.counties.for.types = setNames(
                                   lapply(
                                     list.of.all.by.type,
                                     function(id) self$get.contained(id, "COUNTY", FALSE, FALSE, throw.error.if.unregistered.type)
                                   ),
                                 list.of.all.by.type)

        matches = sapply(all.counties.for.types, function(ids) any(ids %in% list.of.counties.in.locations))

        matching.locations = names(matches[matches == TRUE])

        if (length(matching.locations) > 0) {
          names(matching.locations) = self$get.names(matching.locations)
        }

        combined = c(matching.locations, combined.cont)
        combined = combined[!duplicated(combined)]
        return(combined)
      })

      if (return.list) {
        return(setNames(result, codes))
      }

      rv = unname(unlist(lapply(result, function(l) {
        l[!is.na(l)]
      })))

      if (length(rv) == 0) {
        return(character())
      }

      rv = rv[!duplicated(rv)]
      setNames(rv, sapply(rv, function(id) self$get.names(id)))
    },
    get.containing = function(locations, super.type, limit.to.completely.enclosing, return.list = F, throw.error.if.unregistered.type = T) {
      super.type = toupper(super.type)

      if (throw.error.if.unregistered.type) {
        if (!super.type %in% names(private$types)) {
          stop(paste0("LOCATION.MANAGER$get.containing: Type ", super.type, " not registered, aborting"))
        }
      }

      codes = unlist(lapply(locations, function(x) { private$resolve.code(x, F) }))

      # BFS to find all containing locations
      all.super.locations = lapply(codes, function(code) {
        if (is.na(code)) return(list())

        # BFS over completely enclosing
        visited <- character(0)
        queue <- private$get_super_codes(code, TRUE)

        while (length(queue) > 0) {
          current <- queue[1]
          queue <- queue[-1]
          if (!current %in% visited) {
            visited <- c(visited, current)
            parents <- private$get_super_codes(current, TRUE)
            queue <- c(queue, setdiff(parents, visited))
          }
        }

        if (!limit.to.completely.enclosing) {
          # Also add partially enclosing parents of the code and all visited nodes
          all_to_check <- c(code, visited)
          partial <- character(0)
          for (v in all_to_check) {
            partial <- c(partial, private$get_super_codes(v, FALSE))
          }
          visited <- unique(c(visited, partial))
        }

        visited
      })

      # Filter by super.type
      all.super.locations = lapply(all.super.locations, function(locs) {
        if (length(locs) == 0) return(NA)
        types_for_locs <- vapply(locs, function(loc) {
          idx <- private$lookup_row(loc)
          if (is.na(idx)) return(NA_character_)
          private$locations_df$type[idx]
        }, character(1))
        filtered <- locs[types_for_locs == super.type & !is.na(types_for_locs)]
        if (length(filtered) == 0) return(NA)
        names(filtered) <- sapply(filtered, function(id) self$get.names(id))
        filtered
      })

      if (return.list) {
        return(setNames(all.super.locations, codes))
      }

      rv = unname(unlist(lapply(all.super.locations, function(l) {
        l[!is.na(l)]
      })))

      if (length(rv) == 0) {
        return(character())
      }

      setNames(rv, sapply(rv, function(loc_id) self$get.names(loc_id)))
    },
    get.name.aliases = function(locations, alias.name, throw.error.if.unregistered.alias) {
      codes = unlist(lapply(locations, function(x) { private$resolve.code(x, F) }))

      result = unlist(lapply(codes, function(l) {
        if (is.na(l)) return(NA)
        idx <- private$lookup_row(l)
        if (is.na(idx)) return(NA)
        t <- private$locations_df$type[idx]
        n = names(private$alias.names[[t]])

        for (x in seq_along(n)) {
          if (private$alias.names[[t]][[n[x]]][[2]] == alias.name
              && l == private$alias.names[[t]][[n[x]]][[1]]) {
            return(n[x])
          }
        }
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
      type <- toupper(type)
      prefix <- toupper(prefix)

      invisible(Map(function(t, p, p.l) {
        if (!private$check.code.validity(p)) {
          stop("LOCATION.MANAGER$register.types: We are not allowed characters outside of the letters, numbers, . and -")
        }
        if (!t %in% names(private$types)) {
          private$types[[t]] = c(p, p.l)
          private$alias.codes[[t]] = list()
          if (nrow(private$type.matrix) == 0) {
            private$type.matrix = matrix(FALSE, nrow = 1, ncol = 1)
            rownames(private$type.matrix) = t
            colnames(private$type.matrix) = t
          } else {
            matrix.values <- rep(FALSE, nrow(private$type.matrix) + 1)
            suppressWarnings ({
              private$type.matrix <- rbind(private$type.matrix, matrix.values)
              private$type.matrix <- cbind(private$type.matrix, matrix.values)
            })
            new.names <- c(rownames(private$type.matrix)[-length(rownames(private$type.matrix))], t)
            rownames(private$type.matrix) <- new.names
            colnames(private$type.matrix) <- new.names
          }
        } else {
          stop(paste0("LOCATION.MANAGER$register.types: Type ", t, " already exists in the system, aborting"))
        }
      }, type, prefix, prefix.longform))
    },
    get.registered.types = function (simple) {
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
        return(NA)
      }
      codes <- private$type_index[[type]]
      if (is.null(codes)) return(character(0))
      return(codes)
    },
    get.aliases.for.type = function(type) {
      return(private$alias.codes[[toupper(type)]])
    },
    register = function (types, location.names, codes) {
      codes <- toupper(codes)
      types <- toupper(types)

      if (!all(types %in% names(private$types))) {
        stop("LOCATION.MANAGER$register: Type not previously registered - ",
             types[which(is.na(match(types, names(private$types))))[1]])
      }

      # Add prefixes
      codes = unname(mapply(function(pre, name) {
        if (substr(name, 1, nchar(pre)) == pre) {
          return(name)
        } else {
          return(paste0(pre, name))
        }
      }, self$get.prefix(types), codes, SIMPLIFY = TRUE))

      # Check for duplicates
      if (any(codes %in% private$locations_df$code)) {
        stop("LOCATION.MANAGER: Attempting to add a code that already exists in the manager")
      }

      if (!all(private$check.code.validity(codes))) {
        stop("LOCATION.MANAGER: Attempting to enter a code with invalid characters (only letters, numbers, . and - allowed")
      }

      # Add to data.frame and indexes
      new_rows <- data.frame(
        code = codes,
        name = location.names,
        type = types,
        lat = NA_real_,
        long = NA_real_,
        has_poly = FALSE,
        stringsAsFactors = FALSE
      )
      start_idx <- nrow(private$locations_df) + 1L
      private$locations_df <- rbind(private$locations_df, new_rows)

      for (i in seq_along(codes)) {
        row_idx <- start_idx + i - 1L
        private$code_index[[codes[i]]] <- row_idx

        # Update type index
        existing <- private$type_index[[types[i]]]
        private$type_index[[types[i]]] <- c(existing, codes[i])
      }
    },
    register.name.aliases = function(code, name.aliases, names.alias.labels) {
      code <- private$resolve.code(code)
      idx <- private$lookup_row(code)
      location.type = private$locations_df$type[idx]

      if (!location.type %in% names(private$alias.names)) {
        private$alias.names[[location.type]] = list()
      } else {
        if (any(name.aliases %in% names(private$alias.names[[location.type]]))) {
          stop(paste0("LOCATION.MANAGER: Attempting to add an alias for type ", location.type, " that already exists."))
        }
      }
      private$alias.names[[location.type]][name.aliases] = mapply(c, rep(code, length(names.alias.labels)), names.alias.labels, SIMPLIFY = F)
    },
    register.code.aliases = function(code, code.aliases) {
      code <- private$resolve.code(code)
      code.aliases <- toupper(code.aliases)

      idx <- private$lookup_row(code)
      location.type <- private$locations_df$type[idx]

      if (any(code.aliases %in% names(private$alias.codes[[location.type]]))) {
        stop("LOCATION.MANAGER: One of the location code aliases are already registered")
      }

      private$alias.codes[[location.type]][code.aliases] = code
    },
    register.lat.long = function(code, lat, long) {
      valid.code <- private$resolve.code(code, F)
      if (is.na(valid.code)) {
        warning(paste0("Code ", code, " not found, lat and long not set"))
      } else {
        idx <- private$lookup_row(valid.code)
        private$locations_df$lat[idx] <- lat
        private$locations_df$long[idx] <- long
      }
    },
    register.polygons = function(code) {
      valid.code <- private$resolve.code(code, F)
      if (is.na(valid.code)) {
        warning(paste0("Code ", code, " not found, polygon data not set"))
      } else {
        idx <- private$lookup_row(valid.code)
        private$locations_df$has_poly[idx] <- TRUE
      }
    },
    register.hierarchy = function(sub, super, fully.contains, fail.on.unknown = T) {
      if (fail.on.unknown) {
        sub = unlist(lapply(sub, private$resolve.code))
        super = unlist(lapply(super, private$resolve.code))
        for (i in seq_along(sub)) {
          # Update contains indexes
          if (fully.contains[i]) {
            existing <- private$contains_complete[[super[i]]]
            private$contains_complete[[super[i]]] <- c(existing, sub[i])

            existing <- private$contained_by_complete[[sub[i]]]
            private$contained_by_complete[[sub[i]]] <- c(existing, super[i])
          } else {
            existing <- private$contains_partial[[super[i]]]
            private$contains_partial[[super[i]]] <- c(existing, sub[i])

            existing <- private$contained_by_partial[[sub[i]]]
            private$contained_by_partial[[sub[i]]] <- c(existing, super[i])
          }
        }
      } else {
        sub = unlist(lapply(sub, function(x) { private$resolve.code(x, F) }))
        super = unlist(lapply(super, function(x) { private$resolve.code(x, F) }))
        for (i in seq_along(sub)) {
          if (!any(is.na(c(super[i], sub[i])))) {
            if (fully.contains[i]) {
              existing <- private$contains_complete[[super[i]]]
              private$contains_complete[[super[i]]] <- c(existing, sub[i])

              existing <- private$contained_by_complete[[sub[i]]]
              private$contained_by_complete[[sub[i]]] <- c(existing, super[i])
            } else {
              existing <- private$contains_partial[[super[i]]]
              private$contains_partial[[super[i]]] <- c(existing, sub[i])

              existing <- private$contained_by_partial[[sub[i]]]
              private$contained_by_partial[[sub[i]]] <- c(existing, super[i])
            }
          }
        }
      }
    }
  )
)
