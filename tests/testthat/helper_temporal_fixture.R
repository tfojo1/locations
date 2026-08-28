add_temporal_fixture_row <- function(data, table_name, values) {
  row <- data[[table_name]][0, , drop = FALSE]
  for (column_name in names(row)) {
    row[1L, column_name] <- if (is.numeric(row[[column_name]])) NA_real_ else ""
  }
  for (column_name in names(values)) row[1L, column_name] <- values[[column_name]]
  data[[table_name]] <- rbind(data[[table_name]], row)
  data
}
temporal_county_fixture <- function() {
  data <- locations:::new_temporal_location_data()
  data <- add_temporal_fixture_row(data, "metadata", list(
    data_version = "fixture-2022.1",
    default_reference_date = "2022-01-01"
  ))

  sources <- list(
    list(
      source_id = "src_codes", publisher = "U.S. Census Bureau",
      title = "County code fixture source", source_vintage = "2022",
      reference_date = "2022-01-01", retrieved_at = "2026-08-28",
      url = "https://www.census.gov/geographies/reference-files.2022.html",
      license_status = "US-government-work", checksum = "fixture-codes"
    ),
    list(
      source_id = "src_ct", publisher = "U.S. Census Bureau",
      title = "2022 Connecticut county-equivalent change fixture",
      source_vintage = "2022", reference_date = "2022-01-01",
      retrieved_at = "2026-08-28",
      url = paste0(
        "https://www.census.gov/programs-surveys/acs/technical-documentation/",
        "table-and-geography-changes/2022/geography-changes.html"
      ),
      license_status = "US-government-work", checksum = "fixture-ct"
    ),
    list(
      source_id = "src_ak", publisher = "U.S. Census Bureau",
      title = "2010s county change fixture", source_vintage = "2010s",
      reference_date = "2020-01-01", retrieved_at = "2026-08-28",
      url = paste0(
        "https://www.census.gov/programs-surveys/geography/",
        "technical-documentation/county-changes/2010.html"
      ),
      license_status = "US-government-work", checksum = "fixture-ak"
    ),
    list(
      source_id = "src_mt", publisher = "U.S. Census Bureau",
      title = "1990s county change fixture", source_vintage = "1990s",
      reference_date = "1998-01-01", retrieved_at = "2026-08-28",
      url = paste0(
        "https://www.census.gov/programs-surveys/geography/",
        "technical-documentation/county-changes.1990.html"
      ),
      license_status = "US-government-work", checksum = "fixture-mt"
    )
  )
  for (source in sources) {
    data <- add_temporal_fixture_row(data, "sources", source)
  }

  data <- add_temporal_fixture_row(data, "code_systems", list(
    code_system_id = "census_county_geoid",
    publisher = "U.S. Census Bureau",
    geography_level = "county_or_equivalent",
    description = "Two-digit state ANSI plus three-digit county ANSI code",
    source_id = "src_codes"
  ))

  entities <- list(
    list("loc_ct_hartford", "statistical", "src_ct"),
    list("loc_ct_capitol", "statistical", "src_ct"),
    list("loc_ak_valdez_cordova", "statistical", "src_ak"),
    list("loc_ak_chugach", "statistical", "src_ak"),
    list("loc_ak_copper_river", "statistical", "src_ak"),
    list("loc_mt_yellowstone_park", "statistical", "src_mt"),
    list("loc_mt_gallatin", "legal", "src_mt"),
    list("loc_mt_park", "legal", "src_mt")
  )
  for (entity in entities) {
    data <- add_temporal_fixture_row(data, "entities", list(
      location_id = entity[[1L]], entity_kind = entity[[2L]],
      created_from_source_id = entity[[3L]]
    ))
  }

  versions <- list(
    list(
      "ver_ct_hartford_pre2022", "loc_ct_hartford", "Hartford County",
      "", "unknown", "2022-01-01", "day", "superseded", "src_ct"
    ),
    list(
      "ver_ct_capitol_2022", "loc_ct_capitol", "Capitol Planning Region",
      "2022-01-01", "day", "", "unknown", "", "src_ct"
    ),
    list(
      "ver_ak_valdez_pre2019", "loc_ak_valdez_cordova",
      "Valdez-Cordova Census Area", "", "unknown", "2019-01-02", "day",
      "split", "src_ak"
    ),
    list(
      "ver_ak_chugach_2019", "loc_ak_chugach", "Chugach Census Area",
      "2019-01-02", "day", "", "unknown", "", "src_ak"
    ),
    list(
      "ver_ak_copper_2019", "loc_ak_copper_river", "Copper River Census Area",
      "2019-01-02", "day", "", "unknown", "", "src_ak"
    ),
    list(
      "ver_mt_yellowstone_pre1997", "loc_mt_yellowstone_park",
      "Yellowstone National Park (county equivalent)", "", "unknown",
      "1997-11-07", "day", "split", "src_mt"
    ),
    list(
      "ver_mt_gallatin_1997", "loc_mt_gallatin", "Gallatin County",
      "", "unknown", "", "unknown", "", "src_mt"
    ),
    list(
      "ver_mt_park_1997", "loc_mt_park", "Park County",
      "", "unknown", "", "unknown", "", "src_mt"
    )
  )
  for (version in versions) {
    data <- add_temporal_fixture_row(data, "versions", list(
      location_version_id = version[[1L]], location_id = version[[2L]],
      type = "COUNTY", preferred_name = version[[3L]],
      valid_from = version[[4L]], valid_from_precision = version[[5L]],
      valid_to = version[[6L]], valid_to_precision = version[[7L]],
      end_reason = version[[8L]], source_id = version[[9L]]
    ))
  }

  codes <- list(
    list("code_ct_hartford", "loc_ct_hartford", "09003", "", "unknown", "2022-01-01", "day", "src_ct"),
    list("code_ct_capitol", "loc_ct_capitol", "09110", "2022-01-01", "day", "", "unknown", "src_ct"),
    list("code_ak_valdez", "loc_ak_valdez_cordova", "02261", "", "unknown", "2019-01-02", "day", "src_ak"),
    list("code_ak_chugach", "loc_ak_chugach", "02063", "2019-01-02", "day", "", "unknown", "src_ak"),
    list("code_ak_copper", "loc_ak_copper_river", "02066", "2019-01-02", "day", "", "unknown", "src_ak"),
    list("code_mt_yellowstone", "loc_mt_yellowstone_park", "30113", "", "unknown", "1997-11-07", "day", "src_mt"),
    list("code_mt_gallatin", "loc_mt_gallatin", "30031", "", "unknown", "", "unknown", "src_mt"),
    list("code_mt_park", "loc_mt_park", "30067", "", "unknown", "", "unknown", "src_mt")
  )
  for (code in codes) {
    data <- add_temporal_fixture_row(data, "codes", list(
      location_code_id = code[[1L]], location_id = code[[2L]],
      code_system_id = "census_county_geoid", code = code[[3L]],
      valid_from = code[[4L]], valid_from_precision = code[[5L]],
      valid_to = code[[6L]], valid_to_precision = code[[7L]],
      source_id = code[[8L]]
    ))
  }

  for (index in seq_len(nrow(data$versions))) {
    version <- data$versions[index, ]
    data <- add_temporal_fixture_row(data, "names", list(
      location_name_id = paste0("name_", index),
      location_id = version$location_id,
      name = version$preferred_name,
      name_kind = "official",
      valid_from = version$valid_from,
      valid_from_precision = version$valid_from_precision,
      valid_to = version$valid_to,
      valid_to_precision = version$valid_to_precision,
      source_id = version$source_id
    ))
  }

  successions <- list(
    list("succ_ak_chugach", "loc_ak_valdez_cordova", "loc_ak_chugach", "split", "2019-01-02", "src_ak"),
    list("succ_ak_copper", "loc_ak_valdez_cordova", "loc_ak_copper_river", "split", "2019-01-02", "src_ak"),
    list("succ_mt_gallatin", "loc_mt_yellowstone_park", "loc_mt_gallatin", "replaced_by", "1997-11-07", "src_mt"),
    list("succ_mt_park", "loc_mt_yellowstone_park", "loc_mt_park", "replaced_by", "1997-11-07", "src_mt")
  )
  for (succession in successions) {
    data <- add_temporal_fixture_row(data, "successions", list(
      succession_id = succession[[1L]], from_location_id = succession[[2L]],
      to_location_id = succession[[3L]], succession_kind = succession[[4L]],
      effective_date = succession[[5L]], source_id = succession[[6L]]
    ))
  }

  edges <- list(
    list("cross_ak_chugach", "ver_ak_valdez_pre2019", "ver_ak_chugach_2019", "src_ak"),
    list("cross_ak_copper", "ver_ak_valdez_pre2019", "ver_ak_copper_2019", "src_ak"),
    list("cross_mt_gallatin", "ver_mt_yellowstone_pre1997", "ver_mt_gallatin_1997", "src_mt"),
    list("cross_mt_park", "ver_mt_yellowstone_pre1997", "ver_mt_park_1997", "src_mt")
  )
  for (edge in edges) {
    data <- add_temporal_fixture_row(data, "crosswalk_edges", list(
      crosswalk_id = edge[[1L]], from_version_id = edge[[2L]],
      to_version_id = edge[[3L]], relation_kind = "replacement",
      coverage = "exhaustive", source_id = edge[[4L]]
    ))
  }

  data
}
