#' Add site-level data to the metadata
#'
#' Uses dates to join site-level data to the meta data. If the sites data has
#' only single dates, then a buffer before and after is used to determine which
#' recordings belong to that site obseration.
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
#' @param dt_type Character. Date/time type to join data by. `date` is faster
#'   but `date_time` is more precise. Default `date_time`.
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
#'
add_sites <- function(meta, sites, buffer_before = 0, buffer_after = NULL,
                      by = c("site_id", "aru_id"),
                      dt_type = "date_time") {

  # Checks
  check_data(meta, type = "meta", ref = "clean_metadata()")
  check_data(sites, type = "sites", ref = "clean_sites_index()` or `clean_gps()")
  check_text(by)
  check_text(dt_type, opts = c("date", "date_time"), n = 1)
  check_num(buffer_before, not_null = FALSE, n = 1)
  check_num(buffer_after, not_null = FALSE, n = 1)

  if(any(c("date", "date_time") %in% by)) {
    rlang::abort(c("Cannot use 'date' or 'date_time' in `by`. ",
                   "Did you mean to use `dt_type`?"), call = NULL)
  }

  # If sf, convert to df
  crs <- sf::st_crs(sites) # Hold on for converting back
  sites <- sf_to_df(sites)

  by_date <- check_date_joins(sites, dt_type)

  meta <- dplyr::filter(meta, .data$type != "gps")

  # Check 'by's
  for(i in by) {
    if(all(is.na(meta[[i]]))) {
      by <- by[by != i]
      rlang::inform(c(
        "*" = paste0("Column '", i, "' in `meta` is all NA. ",
                     "Omitting from joins (`by`).")))
    }
  }

  if(dt_type == "date") {
    dt_fun <- lubridate::as_date
  } else dt_fun <- lubridate::as_datetime

  # Clean up and formatting
  omit_dts <- stringr::str_subset(names(sites), "date")
  omit_dts <- omit_dts[!omit_dts %in% by_date]
  sites <- tidyr::drop_na(sites, dplyr::any_of(c(by_date, "longitude", "latitude"))) |>
    dplyr::select(-dplyr::all_of(omit_dts)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(by_date), dt_fun))

  # Fix output of clean_gps()
  if("type" %in% names(sites) && all(sites$type == "gps")) {
    omit <- c("file_name", "type", "path", "aru_type")
    omit <- omit[!omit %in% by]
    sites <- dplyr::select(sites, -dplyr::any_of(omit))
  }

  # Omit columns from meta not in 'by'/'by_date' (allows easy filling)
  omit_cols <- names(sites)
  omit_cols <- omit_cols[!omit_cols %in% c(by, by_date)]
  omit_cols <- omit_cols[omit_cols %in% names(meta)]
  if(length(omit_cols) > 0) {
    meta <- dplyr::select(meta, -dplyr::all_of(omit_cols))

    # Don't report if all NA in meta
    report <- omit_cols
    for(i in report) if(all(is.na(meta[[i]]))) report <- report[report != i]

    if(length(report) > 0) {
      rlang::inform(c(
        "Some columns in both `meta` and `sites` are not used to join (`by`)",
        "*" = paste0(
          "These columns (`", paste0(omit_cols, collapse = "`, `"), "`) ",
          "will be overwritten in `meta`")
      ))
    }
  }

  # Check that columns to add
  add <- names(sites)[!names(sites) %in% c(by, by_date)]
  if(length(add) < 1) {
    rlang::abort(paste0(
      "No new columns in `sites` to add to `meta` ",
      "(all columns in `by` or used in date matching)"),
      call = NULL)
  }

  # Omit duplicates
  sites <- dplyr::distinct(sites)

  # Summarize multiple, coordinates within a date by grouping variables
  # Deals with multiple reads of GPS locations (e.g., hourly)
  if(dt_type == "date") {
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

      rlang::inform(c(
        paste0("Multiple coordinates per date at each combination of `",
               paste0(by, collapse = "`/`"), "`"),
        paste0("Taking mean coordinates ",
               "(max sd lon ", round(max(sites$sd_lon, na.rm = TRUE), 4),
               "; max sd lat ", round(max(sites$sd_lat, na.rm = TRUE), 4), ")"),
        "Consider using `by = \"date_time\"` for more fine-tuned control"))

      sites <- dplyr::select(sites, -"sd_lon", -"sd_lat")
    }
  }


  # Create date/time range (from buffers as required)
  if(length(by_date) == 1) {
    sites <- sites |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
      calc_buffers(buffer_before, buffer_after, by_date) |>
      dplyr::mutate(dt_range = lubridate::interval(.data$t1, .data$t2)) %>%
      dplyr::select(-dplyr::all_of(by_date), -"t1", -"t2") |>
      dplyr::ungroup()
  } else {
    sites <- sites |>
      dplyr::mutate(dt_range = lubridate::interval(.data[[by_date[1]]],
                                                   .data[[by_date[[2]]]])) |>
      dplyr::select(-dplyr::all_of(by_date))
  }

  # Join by date (add `...n` to track successfully merged records)
  sites <- dplyr::mutate(sites, `...n` = 1:dplyr::n())
  meta_sites <- date_join(meta, sites, by = by, id = "path", col = dt_type, int = "dt_range")

  # Flags
  fix_buffers <- c(
    "Consider adjusting `buffer_before` or `buffer_after`",
    paste0("Consider using date/time ranges by including `date_start`",
           "/`date_time_start` and `date_end`/`date_time_end` in `sites`"))

  if(nrow(meta_sites) > nrow(meta)) {
    msg <- c("Some sound files matched multiple site references.",
             "See the `n_matches` column for specifics")
    if(dt_type == "date") {
      msg <- c(msg, "Consider matching by time, `dt_type = \"date_time\"`")
    }
    if(length(by_date) == 1) msg <- c(msg, fix_buffers)

    rlang::inform(msg)
  }

  if(any(is.na(meta_sites$`...n`))) {
    n <- nrow(meta)
    f <- sum(is.na(meta_sites$`...n`))
    msg <- c(paste0("Not all files were matched to a site reference (", f, "/", n, ")"),
             "Consider adjusting the `by` argument")
    if(length(by_date) == 1)  msg <- c(msg, fix_buffers)

    rlang::inform(msg)
  }

  meta_sites |>
    dplyr::select(-"...n") |>
    # If was sf, convert back
    df_to_sf(crs = crs) |>
    dplyr::arrange(dplyr::across(dplyr::any_of(c(by, by_date, "path"))))
}

calc_buffers <- function(df, buffer_before, buffer_after, by_date) {
  # If no buffers, use previous/next observation
  # for first/last, use the equivalent of an -Inf / +Inf date
  d1 <- lubridate::as_datetime("1900-01-01")
  d2 <- lubridate::as_datetime("2999-01-01")

  # Start time
  if(is.null(buffer_before)) {
    df <- dplyr::mutate(
      df, t1 = dplyr::lag(.data[[by_date]], default = .env$d1) + lubridate::seconds(1))
  } else {
    df <- dplyr::mutate(
      df, t1 = .data[[by_date]] - lubridate::hours(.env$buffer_before))
  }

  # End time
  if(is.null(buffer_after)) {
    df <- dplyr::mutate(
      df, t2 = dplyr::lead(.data[[by_date]], default = .env$d2) - lubridate::seconds(1))
  } else {
    df <- dplyr::mutate(
      df, t2 = .data[[by_date]] + lubridate::hours(.env$buffer_after))
  }

  df
}
