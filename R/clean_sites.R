#' Prepare and clean site index file
#'
#' A site index file contains information on when specific ARUs were deployed
#' where. This function cleans a file (csv, xlsx) or data frame in preparation
#' for adding these details to the output of `clean_metadata()`. It can be used
#' to specify missing information according to date, such as GPS lat/lons and
#' site ids.
#'
#' @param site_index Data frame (can be spatial) or file path. Site index data
#'   to clean. If file path, must be to a local csv or xlsx file.
#' @param col_aru_id Character. Column name that contains ARU ids. Default `aru_id`.
#' @param col_site_id Character. Column name that contains site ids.
#' @param col_date_time Character. Column name that contains dates or
#'   date/times. Can be vector of two names if there are both 'start' and 'end'
#'   columns.
#' @param col_coords Character. Column names that contain longitude and
#'   latitude (in that order). Ignored if `site_index` is spatial.
#' @param col_extra Character. Column names for extra data to include. If a named
#'  vector, will rename the columns (see examples).
#' @param resolve_overlaps Logical. Whether or not to resolve date overlaps by
#'   shifting the start/end dates to noon (default `TRUE`). This assumes that
#'   ARUs are generally *not* deployed/removed at midnight (the official
#'   start/end of a day) and so noon is used as an approximation for when an ARU
#'   was deployed or removed. If possible, use specific deployment times to
#'   avoid this issue.
#'
#' @return Standardized site index data frame
#' @export
#'
#' @examples
#'
#' s <- clean_site_index(example_sites,
#'                       col_aru_id = "ARU",
#'                       col_site_id = "Sites",
#'                       col_date_time = c("Date_set_out", "Date_removed"),
#'                       col_coords = c("lon", "lat"))
#'
#' s <- clean_site_index(example_sites,
#'                       col_aru_id = "ARU",
#'                       col_site_id = "Sites",
#'                       col_date_time = c("Date_set_out", "Date_removed"),
#'                       col_coords = c("lon", "lat"),
#'                       col_extra = c("plot" = "Plots"))
#'
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
  check_text(col_date_time, n = c(1, 2))
  check_text(col_extra, not_null = FALSE, n = c(1, Inf))
  check_logical(resolve_overlaps)

  is_sf <- inherits(site_index, "sf")

  if(!is_sf) {
    check_text(col_coords, n = 2)
  } else {
    check_points(site_index)
    col_coords <- NULL
  }


  # Format different inputs

  if(is_sf) {
    # SF - (create tibble sf https://github.com/r-spatial/sf/issues/951#issuecomment-455735564)
    site_index <- dplyr::as_tibble(site_index) |> sf::st_as_sf()
  } else if(is.data.frame(site_index)) {
    # Data frames
    site_index <- suppressMessages(readr::type_convert(site_index)) |>
      dplyr::as_tibble()
  } else {
    # Files
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
    stats::setNames(col_date_time, dt),
    if(!is.null(col_coords)) stats::setNames(col_coords, c("longitude", "latitude")),
    col_extra) |>
    tolower()

  site_index <- site_index |>
    # Grab and rename columns
    dplyr::select(dplyr::all_of(cols)) |>
    # Get dates
    dplyr::mutate(
      dplyr::across(dplyr::all_of(dt), lubridate::as_datetime),
      dplyr::across(dplyr::all_of(dt), lubridate::as_date, .names = "{d}")) |>
    dplyr::relocate(dplyr::any_of(c("longitude", "latitude", "geometry")),
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
#' If checking for a maximum distance (`dist_cutoff`) among GPS points within a
#' group (`dist_by`), the returned data frame will include a column `max_dist`,
#' which represents the largest distance among points within that group.
#'
#' @param meta Data frame. Output of `clean_metadata()`.
#' @param dist_cutoff Numeric. Maximum distance (m) between GPS points within a
#'   site. Default is 100m but can be set to `Inf` to skip.
#' @param dist_crs Numeric. Coordinate Reference System to use when calculating
#'   distance (should be one with m).
#' @param dist_by Character. Column which identifies sites within which to
#'   compare distance among GPS points. Only valid if `dist_cutoff` is not
#'   `Inf`.
#' @param skip_bad Logical. Skip GPS files which create errors.
#' @param verbose Logical. Show extra loading information. Default `FALSE`.
#'
#' @return Data frame of site-level metadata.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'   m <- clean_metadata(project_dir = "my_project")
#'   g <- clean_gps(meta = m)
#' }

clean_gps <- function(meta = NULL,
                      dist_cutoff = 100, dist_crs = 3161,
                      dist_by = c("site_id", "aru_id"),
                      skip_bad = FALSE, verbose = FALSE) {

  # Checks
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

  # Flag problems
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

  rlang::inform(c("Note: GPS log files can be unreliable... ",
                  "Consider supplying your own GPS records and using `clean_site_index()`"))

  # TODO: CHECK IF HAVE SERIAL? FIRMWARE?
  gps |>
    # Check columns and get skips for non-GPX files
    check_gps_files(skip_bad) |>
    # Omit text files without a skip value
    dplyr::filter(!(.data$ext != "gpx" & is.na(.data$skip))) |>
    dplyr::mutate(

      # Read files
      gps_data = purrr::pmap(
        list(.data$path, .data$skip, .data$ext),
        ~load_gps(..1, ..2, ..3, verbose = verbose),
        .progress = list(
          show_after = 0,
          format = "Loading GPS files {cli::pb_percent} [{cli::pb_elapsed}]")),

      # Format data
      gps_data = purrr::map2(
        .data$gps_data, .data$ext, fmt_gps,
        .progress = list(
          show_after = 0,
          format = "Formating GPS files {cli::pb_percent} [{cli::pb_elapsed}]"))) |>

    # Clean up
    dplyr::select(-"date_time", -"date", -"skip", -"ext") %>%
    tidyr::unnest("gps_data", keep_empty = TRUE)

}

load_gps <- function(path, skip, ext, verbose) {
  if(ext == "gpx") {
    g <- sf::st_read(path, layer = "waypoints", quiet = TRUE)
  } else {
    g <- readr::read_csv(path, skip = skip - 1, show_col_types = verbose,
                         guess_max = Inf,
                         name_repair = "unique_quiet",
                         progress = FALSE)
  }
  g
}

check_gps_files <- function(gps_files, skip_bad) {

  # Get text-based GPS logs (i.e. anything but GPX files)
  gps_files <- dplyr::mutate(gps_files,
                             ext = tolower(fs::path_ext(.data$path)))

  gps_txt <- gps_files |>
    dplyr::filter(.data$ext != "gpx") |>
    dplyr::pull(path)

  lines <- stats::setNames(nm = gps_txt) |>
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
  ll <- is.na(skips) | length(skips) == 0

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

  dplyr::tibble(path = names(skips),
                skip = unlist(skips)) |>
    dplyr::full_join(gps_files, by = "path")
}


coord_dir <- function(col, pattern) {
  # Not all missing
  !all(is.na(col)) &
    # all a direction pattern
    all(stringr::str_detect(col, paste0("^[", pattern, "]{1}$")), na.rm = TRUE)
}

fmt_gps <- function(df, ext) {
  if(ext == "gpx") g <- fmt_gps_gpx(df) else g <- fmt_gps_txt(df)
  g |>
    dplyr::select("longitude", "latitude", "date", "date_time")
}

fmt_gps_gpx <- function(df) {
  df |>
    sf::st_drop_geometry() |>
    dplyr::bind_cols(sf::st_coordinates(df)) |>
    dplyr::select("date_time" = "time", "latitude" = "Y", "longitude" = "X") |>
    dplyr::mutate(date_time = lubridate::as_datetime(.data$date_time),
                  date = lubridate::as_date(.data$date_time))
}


fmt_gps_txt <- function(df) {

  opts <- getOption("ARUtools")

  df_fmt <- df |>

    dplyr::rename(
      "latitude" = dplyr::matches(opts$pat_gps_coords[1]),
      "longitude" = dplyr::matches(opts$pat_gps_coords[2]),
      "date" = dplyr::matches(opts$pat_gps_date),
      "time" = dplyr::matches(opts$pat_gps_time)) |>

    # Some summary files have MICROTYPE and DATE
    dplyr::filter(!(is.character(.data$date) && any(.data$date == "DATE"))) |>

    # Format times
    dplyr::mutate(
      date_time_chr = paste(.data$date, .data$time),
      date_time = lubridate::parse_date_time(
        .data$date_time_chr, orders = c("Ymd HMS", "dmY HMS")),
      date = lubridate::as_date(.data$date_time),
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
#' @param gps Data frame of gps sites and coordinates. Requires latitude,
#'   longitude, and any columns in `dist_by`.
#' @param crs Numeric. CRS to use for measuring distances. Should be in meters
#' @param dist_cutoff Distance cutoff in meters. Can be set to Inf to avoid this
#'   check.
#' @param dist_by Character. Column names to use in grouping GPS points before
#'   calculating within group distances.
#'
#' @return Returns data frame with maximum distances between gps points within a
#' group.
#'
#' @noRd
check_gps_dist <- function(gps, crs, dist_cutoff, dist_by){

  if(dist_cutoff < Inf) {
    max_dist <- gps |>
      dplyr::filter(dplyr::if_all(
        dplyr::any_of(c("latitude", "longitude", dist_by)), ~!is.na(.)))

    if(nrow(max_dist) == 0) {
      if(!is.null(dist_by)) {
        dist_by <- paste0(", `", paste0(dist_by, collapse = "`, `"), "`")
      } else dist_by <- ""
      rlang::inform(
        c("Skipping distance check:",
          paste0("All records missing at least one of ",
                 "`longitude`, `latitude`", dist_by)))
    } else {
      n <- max_dist |>
        dplyr::select(dplyr::all_of(c("latitude", "longitude", dist_by))) |>
        dplyr::distinct() |>
        dplyr::count(dplyr::across(dplyr::all_of(dist_by)))

      if(all(n$n == 1)) {
        rlang::inform(
          c("Skipping distance check:",
            paste0("No records with more than one set of coordinates per unique`",
                   paste0(dist_by, collapse = "`/`"), "`")))
      } else {

        max_dist <- max_dist |>
          dplyr::select(dplyr::all_of(c(dist_by, "longitude", "latitude"))) |>
          dplyr::distinct() |>
          sf::st_as_sf(coords= c("longitude", "latitude"), crs = 4326) |>
          sf::st_transform(crs) |>
          dplyr::group_by(dplyr::across(dplyr::all_of(dist_by))) |>
          dplyr::summarize(
            max_dist = max(sf::st_distance(.data$geometry, .data$geometry)),
            .groups = 'drop') |>
          sf::st_drop_geometry()

        if(any(max_dist$max_dist > units::set_units(dist_cutoff, "m"))) {
          rlang::warn(
            c("Within site distances are greater than cutoff",
              "x" = paste0("Distances among ARUs within a site must be less than ",
                           "`dist_cutoff` (currently ", dist_cutoff, "m)"),
              "i" = "Set `dist_cutoff` to `Inf` to skip this check (e.g. moving ARUs)"),
            call = NULL)
        }
        gps <- dplyr::left_join(gps, max_dist, by = dist_by)
      }
    }
  }
  gps
}
