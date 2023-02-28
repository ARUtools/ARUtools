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





  # Check start/end dates

}

fmt_gps <- function(df) {
  df |>
    dplyr::rename_with(~"latitude", .cols = dplyr::matches("lat")) |>
    dplyr::rename_with(~"longitude", .cols = dplyr::matches("lon")) |>
    dplyr::rename_with(~"date", .cols = dplyr::matches("date|(DD/MM/YY)")) |>
    dplyr::rename_with(~"time", .cols = dplyr::matches("time|(HH/MM)")) |>
    dplyr::mutate(date_time_chr = paste(.data$date, .data$time),
                  date_time = lubridate::dmy_hms(.data$date_time_chr),
                  date = lubridate::as_date(date_time)) |>
    dplyr::select("longitude", "latitude", "date_time")
}


#' Check distances between points from GPS log
#'
#' @param gps_log gps log file generated from process_gps_barlt
#' @param crs_m  CRS for measurement of distances. Should be in meters
#' @param dist_cutoff Distance cutoff in meters. Can be set to Inf to avoid this check.
#'
#' @return Returns a data frame with maximum distances between gps points at site.
check_gps_distances <- function(gps_log, crs_m = 3161, dist_cutoff = 100){
  max_distances <-
    gps_log |>
    sf::st_as_sf(coords= c("longitude_decimal_degrees",
                           "latitude_decimal_degrees"),
                 crs = 4326) |>
    sf::st_transform(crs_m) |>
    dplyr::group_by(SiteID) |>
    dplyr::summarize(max_dist = max(sf::st_distance(geometry, geometry)),
                     .groups = 'drop') |>
    sf::st_drop_geometry()

  if(any(max_distances$max_dist>units::set_units(dist_cutoff, "m"))) #browser()
    abort(c("Within Site distance is greater than cuttoff",
            "x" = "Distance must be less than `dist_cutoff`",
            "i" = "Set dist_cutoff to Inf to avoid this (e.g. moving ARU)"))

  return(max_distances)

}
