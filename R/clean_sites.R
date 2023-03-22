#' Title
#'
#' @param site_index
#' @param col_aru_id
#' @param col_site_id
#' @param col_date_time
#' @param col_coords
#'
#' @return
#' @export
#'
#' @examples
clean_site_index <- function(site_index,
                             col_aru_id = "aru_id",
                             col_site_id = "site_id",
                             col_date_time = "date",
                             col_coords = c("longitude", "latitude"),
                             col_extra = NULL,
                             resolve_overlaps = TRUE) {

  # Checks
  check_df_file(site_index)
  check_text(col_aru_id)
  check_text(col_site_id)
  check_text(col_date_time, n_max = 2)
  check_text(col_coords, n_max = 2)
  check_text(col_extra, not_null = FALSE, n_max = Inf)
  check_logical(resolve_overlaps)

  if(is.data.frame(site_index)) {
    site_index <- suppressMessages(readr::type_convert(site_index)) |>
      dplyr::as_tibble()
  } else {
    type <- fs::path_ext(site_index)
    check_ext(type, c("csv", "xlsx"))

    # Let readr do the index checking
    if(type == "csv") {
      site_index <- readr::read_csv(site_index,
                                    progress = FALSE,
                                    show_col_types = FALSE)
    } else if(type == "xlsx") {
      site_index <- readxl::read_excel(site_index, progress = FALSE)
    }
  }

  site_index <- dplyr::rename_with(site_index, tolower)

  # Check cols
  check_cols(site_index, c(col_site_id, col_date_time, col_aru_id, col_coords,
                           col_extra),
             name = "site_index",
             extra = "See ?clean_site_index")

  # Check dates
  check_dates(site_index, col_date_time)

  if(length(col_date_time) == 1) {
    dt <- "date_time"
    d <- "date"
  } else {
    dt <- c("date_time_start", "date_time_end")
    d <- c("date_start", "date_end")
  }

  # Prepare for renaming

  if(length(names(col_extra)) == 0) {
    col_extra <- stats::setNames(nm = col_extra)
  }

  cols <- c(
    "site_id" = col_site_id,
    "aru_id" = col_aru_id,
    setNames(col_date_time, dt),
    if(!is.null(col_coords)) setNames(col_coords, c("longitude", "latitude")),
    col_extra) |>
    tolower()

  site_index <- site_index |>
    # Grab and rename columns
    dplyr::select(dplyr::all_of(cols)) |>
    # Get dates
    dplyr::mutate(
      dplyr::across(dplyr::all_of(dt), lubridate::as_datetime),
      dplyr::across(dplyr::all_of(dt), lubridate::as_date, .names = "{d}")) |>
    dplyr::relocate(dplyr::any_of(c("longitude", "latitude")),
                    .after = dplyr::last_col()) |>
    dplyr::relocate(dplyr::any_of(names(col_extra)), .after = dplyr::last_col())

  # For date ranges, check if only using dates
  if(resolve_overlaps &&
     length(dt) == 2 &&
     all(site_index$date_time_start == site_index$date_start) &&
     all(site_index$date_time_end == site_index$date_end)) {

    site_index$date_time_end %in% site_index$date_time_start

    by_site <- dplyr::group_by(site_index, .data$site_id) |>
      dplyr::filter(.data$date_time_end %in% .data$date_time_start) |>
      nrow()
    by_aru <- dplyr::group_by(site_index, .data$aru_id) |>
      dplyr::filter(.data$date_time_end %in% .data$date_time_start) |>
      nrow()

    if(by_site > 0 | by_aru > 0) {
      rlang::inform(
        c("There are overlapping date ranges",
          "Shifting start/end times to 'noon'",
          #"Use `dt_type = \"date_time\"` in `add_sites()`",
          "Skip this with `resolve_overlaps = FALSE`"))

      lubridate::hour(site_index$date_time_start) <- 12
      lubridate::hour(site_index$date_time_end) <- 12
    }
  }

  site_index
}

#' Check and clean GPS data
#'
#' Check and clean GPS data from ARU logs. GPS points are checked for obvious
#' problems (expected range, distance cutoffs and timing) then attached to the
#' meta data frame. Note that it is often safer and more reliable to create
#' your own Site Index file including site ids, and GPS coordinates. This file
#' can be cleaned and prepared with `clean_site_index()` instead.
#'
#' @param meta Data frame. Output of `clean_metadata()`.
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

clean_gps <- function(meta = NULL,
                      dist_cutoff = 100, dist_crs = 3161,
                      dist_by = "site_id",
                      skip_bad = FALSE, verbose = FALSE) {

  # TODO:
  check_data(meta, type = "meta", ref = "clean_metadata()")
  check_num(dist_cutoff)
  check_num(dist_crs)
  check_text(dist_by, n = c(1, Inf))
  check_logical(skip_bad)
  check_logical(verbose)

  # Load, combine and clean gps files
  gps <- clean_gps_files(meta, skip_bad, verbose)

  # Check distances (skips if dist_cutoff = Inf)
  gps <- check_gps_dist(gps, crs = dist_crs, dist_cutoff = dist_cutoff,
                        dist_by = dist_by)

  # Flag problems -----------------------------
  n <- nrow(gps)
  f_dt <- sum(is.na(gps$date_time))
  f_coord <- sum(is.na(gps$longitude) | is.na(gps$latitude))
  f_zero <- sum(gps$longitude == 0 | gps$latitude == 0)

  if(any(c(f_dt, f_coord, f_zero) > 0)) {
    msg <- c("Identified possible problems with GPS extraction:")
    msg <- c(msg, report_missing(f_dt, n, "date/times"))
    msg <- c(msg, report_missing(f_coord, n, "coordinates"))
    if(f_zero > 0) {
      msg <- c(
        msg,
        "Some coordinates detected as zero (can occur in Song Meters if not set)",
        paste0("Replacing zero coordinates with NA (", f_zero, "/", n, ")"))
    }

    gps <- dplyr::mutate(
      gps, dplyr::across(c("latitude", "longitude"), ~dplyr::na_if(.x, 0)))

    rlang::inform(msg)
  }

  gps
}

clean_gps_files <- function(meta, skip_bad, verbose) {

  gps <- dplyr::filter(meta, .data$type == "gps")

  if(nrow(gps) == 0) {
    rlang::abort(
      "No GPS data provided and no GPS log files recorded in `meta`",
      call = NULL)
  }

  rlang::inform(c("Note: GPS log files are notoriously unreliable... ",
                  "Consider supplying your own GPS records and using `clean_site_index()`"))

  # TODO: CHECK IF HAVE SERIAL? FIRMWARE?

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
    dplyr::select(-"date_time", -"date", -"skip") %>%
    tidyr::unnest("gps_data", keep_empty = TRUE)

}

check_gps_files <- function(gps_files, skip_bad) {

  lines <- setNames(nm = gps_files$path) |>
    purrr::imap(~readr::read_lines(.x, n_max = 5))

  # Set patterns
  opts <- getOption("ARUtools")
  pattern_date <- stringr::regex(opts$pat_gps_date, ignore_case = TRUE)
  pattern_time <- stringr::regex(opts$pat_gps_time, ignore_case = TRUE)
  pattern_coords <- stringr::regex(paste0(opts$pat_gps_coords, collapse = "|"),
                                   ignore_case = TRUE)

  # Get skips
  skips <- purrr::map(lines,
                      ~stringr::str_which(.x, pattern_coords) |> dplyr::first())

  # Check for columns
  dt <- purrr::map_lgl(lines, ~!any(stringr::str_detect(.x, pattern_date)))
  tm <- purrr::map_lgl(lines, ~!any(stringr::str_detect(.x, pattern_time)))
  ll <- is.na(skips)

  if(any(c(dt, tm, ll))) {
    w <- which(dt | tm | ll)
    msg <- paste0("Detected problems in some GPS files (indices: ",
                  paste0(w, collapse = ", "), ")")

    t <- "Could not detect columns in all files: "
    cols <- vector()
    if(any(dt)) cols <- c(cols, "date")
    if(any(tm)) cols <- c(cols, "time")
    if(any(ll)) cols <- c(cols, "lat and lon")
    t <- paste0(t, paste0(cols, collapse = ", "))

    msg <- c(msg,
             c("!" = t,
               "!" = paste0(
                 "GPS files are expected to have named columns with ",
                 "date, time, latitude, and longitude (names can be loose)")))

    if(skip_bad) {
      rlang::inform(c(msg, c("!" = "Skipping problematic file(s)")))
      skips[w] <- NA_integer_
    } else rlang::abort(msg, call = NULL)
  }

  dplyr::mutate(gps_files, skip = unlist(.env$skips))
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
check_gps_dist <- function(gps, crs, dist_cutoff, dist_by){

  if(dist_cutoff < Inf) {
    max_dist <- gps |>
      dplyr::filter(dplyr::if_all(
        dplyr::any_of(c("latitude", "longitude", dist_by)), ~!is.na(.)))

    if(nrow(max_dist) == 0) {
      if(!is.null(dist_by)) dist_by <- paste0(", `", dist_by, "`") else dist_by <- ""
      rlang::inform(
        c("Skipping distance check:",
          paste0("All records missing at least one of ",
                 "`longitude`, `latitude`", dist_by)))
    } else {
      n <- dplyr::count(max_dist, .data$latitude, .data$longitude, .data[[dist_by]])

      if(all(n$n == 1)) {
        rlang::inform(
          c("Skipping distance check:",
            paste0("No records with more than one set of coordinates per `",
                   dist_by, "`")))
      }

      max_dist <- max_dist |>
        sf::st_as_sf(coords= c("longitude", "latitude"), crs = 4326) |>
        sf::st_transform(crs) |>
        dplyr::group_by(.data[[dist_by]]) |>
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
      gps <- dplyr::left_join(gps, max_dist, by = dist_by)
    }
  }
  gps
}
