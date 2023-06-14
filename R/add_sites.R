#' Add site-level data to the metadata
#'
#' Uses dates to join site-level data to the meta data. If the sites data has
#' only single dates, then a buffer before and after is used to determine which
#' recordings belong to that site observation.
#'
#' @param sites Data frame. Site-level data from `clean_site_index()`.
#' @param buffer_before Numeric. Number of hours before a deployment in which to
#'   include recordings. `NULL` means include the time up to the last
#'   deployment. Coupled with `buffer_after`, this creates a window around a
#'   date/time in which to join recordings to the site-level data. Ignored if
#'   `sites` has both a start and end column for date/times. Default 0.
#' @param buffer_after Numeric. Number of hours after the deployment in which to
#'   include recordings. `NULL` means include the time up to the next
#'   deployment. Coupled with `buffer_before`, creates a window around a
#'   date/time in which to join recordings to the site-level data. Ignored if
#'   `sites` has both a start and end column for date/times. Default `NULL`.
#' @param by Character. Columns which identify a deployment in `sites` as well
#'   as `meta`, besides date/time, which are used to join the data. Default is
#'   `site_id` and `aru_id`.
#' @param by_date Character. Date/time type to join data by. `date` is faster
#'   but `date_time` is more precise. Default `date_time`. `NULL` means ignore
#'   dates and join only with `by` columns (`dplyr::left_join()`).
#'
#' @inheritParams common_docs
#'
#' @return A data frame of metadata with site-level data joined in.
#' @export
#'
#' @examples
#' m <- clean_metadata(project_files = example_files)
#' s <- clean_site_index(example_sites_clean,
#'                       col_date = c("date_time_start", "date_time_end"))
#' m <- add_sites(m, s)
#'
#' # Without dates (by site only)
#' m <- clean_metadata(project_files = example_files)
#' eg <- dplyr::select(example_sites_clean, -date_time_start, -date_time_end)
#' s <- clean_site_index(eg, col_date_time = NULL)
#' m <- add_sites(m, s, by_date = NULL)
#'
add_sites <- function(meta, sites, buffer_before = 0, buffer_after = NULL,
                      by = c("site_id", "aru_id"),
                      by_date = "date_time",
                      quiet = FALSE) {

  # Checks
  check_data(meta, type = "meta", ref = "clean_metadata()")
  check_data(sites, type = "sites", ref = "clean_sites_index()` or `clean_gps()")
  check_text(by)
  check_text(by_date, opts = c("date", "date_time"), n = 1, not_null = FALSE)
  check_num(buffer_before, not_null = FALSE, n = 1)
  check_num(buffer_after, not_null = FALSE, n = 1)
  meta <- check_UTC(meta)
  sites <- check_UTC(sites, stringr::str_subset(names(sites), "^date_time"))

  # Check 'by's
  by_date_cols <- check_date_joins(sites, by_date, quiet)
  by <- check_by(by, meta, cols_omit = c("date", "date_time"), quiet)

  # If sf, convert to df
  crs <- sf::st_crs(sites) # Hold on for converting back
  sites <- sf_to_df(sites)

  # Clean and prep
  sites <- prep_sites(sites, by, by_date_cols)
  meta <- prep_meta(meta, names(sites), by, by_date_cols, quiet)

  # Check that there are columns to add (check after prep_xxx())
  add <- names(sites)[!names(sites) %in% c(by, by_date_cols)]
  if(length(add) < 1) {
    rlang::abort(paste0(
      "No new columns in `sites` to add to `meta` ",
      "(all columns in `by` or used in date matching)"),
      call = NULL)
  }

  # Do the joins
  if(!is.null(by_date)) {
    meta_sites <- add_sites_date(sites, meta,
                                 by, by_date, by_date_cols,
                                 buffer_before, buffer_after,
                                 quiet)
  } else {
    meta_sites <- dplyr::left_join(meta, sites, by = by)
  }

  # Finalize
  meta_sites |>
    df_to_sf(crs = crs) |>  # If was sf, convert back
    dplyr::arrange(dplyr::across(dplyr::any_of(c(by, by_date_cols, "path"))))
}

calc_buffers <- function(df, buffer_before, buffer_after, by_date_cols) {
  # If no buffers, use previous/next observation
  # for first/last, use the equivalent of an -Inf / +Inf date
  d1 <- lubridate::as_datetime("1900-01-01")
  d2 <- lubridate::as_datetime("2999-01-01")

  # Start time
  if(is.null(buffer_before)) {
    df <- dplyr::mutate(
      df, t1 = dplyr::lag(.data[[by_date_cols]], default = .env$d1) + lubridate::seconds(1))
  } else {
    df <- dplyr::mutate(
      df, t1 = .data[[by_date_cols]] - lubridate::hours(.env$buffer_before))
  }

  # End time
  if(is.null(buffer_after)) {
    df <- dplyr::mutate(
      df, t2 = dplyr::lead(.data[[by_date_cols]], default = .env$d2) - lubridate::seconds(1))
  } else {
    df <- dplyr::mutate(
      df, t2 = .data[[by_date_cols]] + lubridate::hours(.env$buffer_after))
  }

  df
}

prep_sites <- function(sites, by, by_date_cols) {

  # Omit unused dates
  omit_dts <- stringr::str_subset(names(sites), "date")
  omit_dts <- omit_dts[!omit_dts %in% by_date_cols]
  sites <- dplyr::select(sites, -dplyr::all_of(omit_dts))

  # Omit NAs
  sites <- tidyr::drop_na(
    sites,
    dplyr::any_of(c(by_date_cols, "longitude", "latitude")))

  # Fix output of clean_gps()
  if("type" %in% names(sites) && all(sites$type == "gps")) {
    omit <- c("file_name", "type", "path", "aru_type")
    omit <- omit[!omit %in% by]
    sites <- dplyr::select(sites, -dplyr::any_of(omit))
  }

  dplyr::distinct(sites)
}

prep_meta <- function(meta, cols_sites, by , by_date_cols, quiet) {

  meta <- dplyr::filter(meta, .data$type != "gps")

  # Omit columns from meta not in 'by'/'by_date_cols' (allows easy filling)
  omit_cols <- cols_sites[!cols_sites %in% c(by, by_date_cols)]
  omit_cols <- omit_cols[omit_cols %in% names(meta)]

  if(length(omit_cols) > 0) {
    meta <- dplyr::select(meta, -dplyr::all_of(omit_cols))

    # Don't report if all NA in meta
    report <- omit_cols
    for(i in report) if(all(is.na(meta[[i]]))) report <- report[report != i]

    if(length(report) > 0 & !quiet) {
      rlang::inform(c(
        "Some columns in both `meta` and `sites` are not used to join (`by`)",
        "*" = paste0(
          "These columns (`", paste0(omit_cols, collapse = "`, `"), "`) ",
          "will be overwritten in `meta`")
      ))
    }
  }
  meta
}

add_sites_date <- function(sites, meta, by, by_date, by_date_cols,
                           buffer_before, buffer_after, quiet) {

  # Clean up Dates
  if(by_date == "date") {
    dt_fun <- lubridate::as_date
  } else dt_fun <- lubridate::as_datetime

  sites <- dplyr::mutate(sites, dplyr::across(dplyr::all_of(by_date_cols), dt_fun))

  # Summarize multiple, coordinates within a date by grouping variables
  # Deals with multiple reads of GPS locations (e.g., hourly)
  if(by_date == "date") {
    n <- dplyr::count(
      sites,
      dplyr::across(.cols = c(dplyr::all_of(by), dplyr::matches("date(_(start|end))?"))))
    if(any(n$n > 1)) {

      sites <- sites |>
        dplyr::group_by(dplyr::across(
          .cols = c(dplyr::all_of(by), dplyr::matches("date(_(start|end))?")))) |>
        dplyr::summarize(sd_lon = sd(.data$longitude),
                         sd_lat = sd(.data$latitude),
                         longitude = mean(.data$longitude),
                         latitude = mean(.data$latitude)) |>
        dplyr::ungroup()

      if(!quiet) {
        rlang::inform(c(
          paste0("Multiple coordinates per date at each combination of `",
                 paste0(by, collapse = "`/`"), "`"),
          paste0("Taking mean coordinates ",
                 "(max sd lon ", round(max(sites$sd_lon, na.rm = TRUE), 4),
                 "; max sd lat ", round(max(sites$sd_lat, na.rm = TRUE), 4), ")"),
          "Consider using `by = \"date_time\"` for more fine-tuned control"))
      }

      sites <- dplyr::select(sites, -"sd_lon", -"sd_lat")
    }
  }

  # Create date/time range (from buffers as required)
  if(length(by_date_cols) == 1) {
    sites <- sites |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
      calc_buffers(buffer_before, buffer_after, by_date_cols) |>
      dplyr::mutate(dt_range = lubridate::interval(.data$t1, .data$t2)) |>
      dplyr::select(-dplyr::all_of(by_date_cols), -"t1", -"t2") |>
      dplyr::ungroup()
  } else {
    sites <- sites |>
      dplyr::mutate(dt_range = lubridate::interval(.data[[by_date_cols[1]]],
                                                   .data[[by_date_cols[[2]]]])) |>
      dplyr::select(-dplyr::all_of(by_date_cols))
  }

  # Join by date (add `...n` to track successfully merged records)
  sites <- dplyr::mutate(sites, `...n` = 1:dplyr::n())

  meta_sites <- date_join(meta, sites, by = by, id = "path",
                          col = by_date, int = "dt_range")

  # Flags
  fix_buffers <- c(
    "Consider adjusting `buffer_before` or `buffer_after`",
    paste0("Consider using date/time ranges by including `date_start`",
           "/`date_time_start` and `date_end`/`date_time_end` in `sites`"))

  if(nrow(meta_sites) > nrow(meta)) {
    msg <- c("Some sound files matched multiple site references.",
             "See the `n_matches` column for specifics")
    if(by_date == "date") {
      msg <- c(msg, "Consider matching by time, `by_date = \"date_time\"`")
    }
    if(length(by_date_cols) == 1) msg <- c(msg, fix_buffers)

    rlang::inform(msg)
  }

  if(any(is.na(meta_sites$`...n`))) {
    n <- nrow(meta)
    f <- sum(is.na(meta_sites$`...n`))
    msg <- c(paste0("Not all files were matched to a site reference (", f, "/", n, ")"),
             "Consider adjusting the `by` argument")
    if(length(by_date_cols) == 1)  msg <- c(msg, fix_buffers)

    rlang::inform(msg)
  }

  meta_sites |>
    dplyr::select(-"...n")
}
