#' Check and clean GPS data
#'
#' Check and clean GPS data either from ARU logs or user-supplied GPS data
#' (preferred). GPS points are checked for obvious problems (expected range,
#' distance cutoffs and timing) then attached to the meta data frame.
#'
#' @param meta Data frame. Output of `clean_metadata()`.
#' @param gps Data frame. GPS data points (optional, but recommended).
#' @param dist_cutoff Numeric. Maximum distance (m) between GPS points within a
#'   site. Default is 100m but can be set to `Inf` to skip.
#' @param dist_crs Numeric. Coordinate Reference System to use when calculating
#'   distance (should be one with m).
#' @param verbose Logical.
#'
#' @return
#' @export
#'
#' @examples

clean_gps <- function(meta, gps = NULL, dist_cutoff = 100, dist_crs = 3161,
                      skip_bad = FALSE, verbose = FALSE) {

  # CHECK meta
  # CHECK gps
  # CHECK dist_crs
  # Other checks...

  # Clean depending on source
  if(is_null(gps)) {
    gps <- clean_gps_logs(meta, skip_bad, verbose)
  } else {
    gps <- clean_gps_df(gps, verbose)
  }

  # Check start/end dates

  # Check distances (skips if dist_cutoff = Inf)
  gps <- check_gps_dist(gps, crs = dist_crs, dist_cutoff = dist_cutoff)

  gps
}

clean_gps_logs <- function(meta, skip_bad, verbose) {

  gps <- dplyr::filter(meta, .data$type == "gps")

  if(nrow(gps) == 0) {
    rlang::abort(
      "No GPS data provided and no GPS log files recorded in `meta`",
      call = NULL)
  }

  rlang::inform(c("No GPS data provided, using GPS log files",
                  "*" = "Note GPS log files are notoriously unreliable..."))
  gps |>
    # Check columns and get skips
    check_gps_files(skip_bad) |>
    dplyr::filter(!is.na(.data$skip)) |>
    dplyr::mutate(

      # Read files
      gps_data = purrr::map2(
        path, skip,
        ~readr::read_csv(.x, skip = .y - 1, show_col_types = verbose,
                         name_repair = "unique_quiet",
                         progress = FALSE)),
      # Format data
      gps_data = purrr::map(.data$gps_data, fmt_gps)) |>

    # Clean up
    dplyr::select(-"date_time", -"date", -"path", -"skip") %>%
    tidyr::unnest("gps_data", keep_empty = TRUE)
}


    # Check user-supplied GPS locations

  }

  # Check distances
  if(check_dists) {
    site_distances <- check_gps_distances(gps_log_full, crs_m = crs_m, dist_cutoff = dist_cutoff)
    gps_log_full <- left_join(gps_log_full, site_distances, by = "SiteID")
  }



coord_dir <- function(col, pattern) {
  all(stringr::str_detect(col, paste0("^[", pattern, "]{1}$")) & !is.na(col),
      na.rm = TRUE)
}

fmt_gps <- function(df) {
  opts <- getOption("ARUtools")

  df_fmt <- df |>
    dplyr::rename_with(~"latitude", .cols = dplyr::matches(opts$pat_gps_coords[1])) |>
    dplyr::rename_with(~"longitude", .cols = dplyr::matches(opts$pat_gps_coords[2])) |>
    dplyr::rename_with(~"date", .cols = dplyr::matches(opts$pat_gps_date)) |>
    dplyr::rename_with(~"time", .cols = dplyr::matches(opts$pat_gps_time)) |>

    # Format times
    dplyr::mutate(
      date_time_chr = paste(.data$date, .data$time),
      date_time = lubridate::parse_date_time(
        .data$date_time_chr, orders = c("Ymd HMS", "dmY HMS")),
      date = lubridate::as_date(date_time),
      )

  # Fix coords - Check and apply -/+ if N/S/E/W columns present
  dir <- dplyr::select(df_fmt, dplyr::where(~coord_dir(.x, "NnSsEeWw")))
  if(ncol(dir) > 0) {
    df_fmt <- df_fmt |>
      dplyr::rename_with(~ "ns", .cols = dplyr::where(~coord_dir(.x, "NnSs"))) |>
      dplyr::rename_with(~ "ew", .cols = dplyr::where(~coord_dir(.x, "EeWw"))) |>
      # Define direction shift
      dplyr::mutate(dplyr::across(
        dplyr::any_of(c("ns", "ew")),
        ~ stringr::str_replace_all(tolower(.x),
                                   c("w" = "-", "e" = "",
                                     "s" = "-", "n" = "")))) |>
      # Apply direction shift (i.e. merge)
      tidyr::unite("latitude", dplyr::any_of(c("ns", "latitude")), sep = "") |>
      tidyr::unite("longitude", dplyr::any_of(c("ew", "longitude")), sep = "")
  }

  # Clean up
  df_fmt |>
    dplyr::mutate(latitude = as.numeric(.data$latitude),
                  longitude = as.numeric(.data$longitude)) |>
    dplyr::select("longitude", "latitude", "date", "date_time")
}


#' Check distances between points from GPS log
#'
#' @param gps Data frame of gps sites and coordinates
#' @param crs Numeric. CRS to use for measuring distances. Should be in meters
#' @param dist_cutoff Distance cutoff in meters. Can be set to Inf to avoid this check.
#'
#' @return Returns data frame with maximum distances between gps points at site.
#'
#' @noRd
check_gps_dist <- function(gps, crs, dist_cutoff){
  if(dist_cutoff < Inf) {
    max_dist <- gps |>
      dplyr::filter(!is.na(.data$longitude), !is.na(.data$latitude),
                    !is.na(.data$site_id))

    if(nrow(max_dist) == 0) {
      rlang::inform(
        c("Skipping distance check:",
          "All records missing at least one of longitude, latitude and site_id"))
    } else {
      max_dist <- max_dist |>

        sf::st_as_sf(coords= c("longitude", "latitude"), crs = 4326) |>
        sf::st_transform(crs) |>
        dplyr::group_by(site_id) |>
        dplyr::summarize(max_dist = max(sf::st_distance(geometry, geometry)),
                         .groups = 'drop') |>
        sf::st_drop_geometry()

      if(any(max_dist$max_dist > units::set_units(dist_cutoff, "m"))) {
        rlang::abort(
          c("Within site distances are greater than cutoff",
            "x" = paste0("Distances among ARUs within a site must be less than ",
                         "`dist_cutoff` (currently ", dist_cutoff, "m)"),
            "i" = "Set `dist_cutoff` to `Inf` to skip this check (e.g. moving ARUs)"),
          call = NULL)
      }
      gps <- dplyr::left_join(gps, max_dist, by = "site_id")
    }
  }
  gps
}
