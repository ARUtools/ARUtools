clean_gps <- function(meta, gps = NULL, check_dists = TRUE, dist_cutoff = 100,
                      verbose = FALSE) {

  browser()

  if(is_null(gps)) {

    gps <- dplyr::filter(meta, .data$type == "gps")

    if(nrow(gps) == 0) {
      rlang::abort(
        "No GPS data provided and no GPS log files recorded in `meta`",
        call = NULL)
    }

    rlang::inform(c("!" = "No GPS data provided, using GPS log files",
                    "*" = "Note GPS log files are notoriously unreliable..."))

    gps |>
      dplyr::select("dir", "file_name", "aru_type", "aru_id", "site_id") |>
      dplyr::mutate(gps = file.path(.data$dir, .data$file_name)) |>
      dplyr::mutate(
        gps_data = purrr::map(gps, ~readr::read_csv(.x, skip = 1, show_col_types = verbose)),
        gps_data = purrr::map(gps_data, fmt_gps)) |>
      tidyr::unnest("gps_data")


    gps_locations <- process_gps_barlt(base_folder = folder_base,
                                       site_pattern = site_pattern,
                                       file_list= list_files,dist_cutoff=dist_cutoff,
                                       deploy_start_date = deploy_start_date,
                                       check_dists)

    gps_locations <- process_gps_SM(folder_base = folder_base, list_files = list_files,
                                    site_pattern = site_pattern)

  } else {

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
