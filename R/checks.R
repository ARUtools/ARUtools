
#' Check for a type of value within options
#'
#' @param x Value
#' @param nm Value name
#' @param type Value type (text, numeric, logical)
#' @param opts Value options (if NULL ignored, otherwise must be one of these)
#' @param not_null Whether value can be NULL or not.
#' @param n How many values can there be (must there be)
#'
#' @noRd
check_value <- function(x, nm, type, opts = NULL, not_null = TRUE, n = c(1, Inf)) {
  nm <- paste0("`", nm, "`")
  if(not_null && is.null(x)) {
    rlang::abort(paste(nm, "cannot be `NULL`"), call = NULL)
  } else if(!is.null(x)) {
    if((type == "text" && !is.character(x)) ||
       (type == "numeric" && !is.numeric(x)) ||
       (type == "logical" && !is.logical(x))) {
      rlang::abort(paste(nm, "must be", type), call = NULL)
    } else if(length(n) == 1 && length(x) != n) {
      rlang::abort(paste(nm, "must have", n, "value(s)"), call = NULL)
    } else if(length(n) > 1 && !dplyr::between(length(n), n[1], n[2])) {
      rlang::abort(paste(nm, "must have between", n[1], "and", n[2], "values"), call = NULL)
    } else if(!is.null(opts) && any(!x %in% opts)) {
      rlang::abort(paste0(nm, " must be among '",
                          paste0(opts, collapse = "', '"), "'"))
    }
  }
}

check_text <- function(x, ..., type = "text") {
  check_value(x, nm = deparse(substitute(x)), ..., type = type)
}

check_num <- function(x, ..., type = "numeric") {
  check_value(x, nm = deparse(substitute(x)), ..., type = type)
}

check_logical <- function(x, ..., type = "logical", n = 1) {
  check_value(x, nm = deparse(substitute(x)), ..., type = type)
}

#' Check that input data has the columns required
#' @noRd
check_data <- function(df, type, ref) {

  if(!is.null(df)) {
    if(!inherits(df, "data.frame")) {
      rlang::abort("`", type, "` must be a data frame. See `", ref, "`",
                   call = NULL)
    }

    e <- paste0("Should be output of `", ref, "`")

    if(inherits(df, "sf")) coords <- NULL else coords <- c("latitude", "longitude")

    if(type == "meta") {
      check_cols(
        df, c("file_name", "path", "site_id", "aru_id"),
        dates = TRUE,
        name = "meta",
        extra = e)
    } else if(type == "sites") {
      check_cols(
        df, c("site_id", "aru_id", coords),
        dates = FALSE, # Optional
        name = "sites",
        extra = e)
    } else if (type == "meta_sites") {
      check_cols(
        df, c("site_id", "aru_id", coords),
        dates = TRUE,
        name = "meta",
        extra = e)
    } else if (type == "meta_sun") {
      check_cols(
        df, c("site_id", "aru_id", coords, "tz", "t2sr", "t2ss"),
        dates = TRUE,
        name = "meta",
        extra = e)
    }

    # Check dates are dates
    dt <- stringr::str_subset(names(df), "date")
    check_dates(df, dt)
  }
}

#' Confirm file extension
#' @noRd
check_ext <- function(ext, opts)  {

  if(!ext %in% opts) {
    rlang::abort(
      paste0("File extension must be one of ", paste0(opts, collapse = ", ")),
      call = NULL)
  }
}

#' Check that columns exist in a data frame
#'
#' @param df Data frame to check
#' @param cols Cols to look for
#' @param name Argument name to use in messages
#' @param extra Extra messages to return
#' @param dates Whether to look for date/time columns (can be date, date_time or
#'   date ranges)
#' @noRd
check_cols <- function(df, cols = NULL, name, extra = NULL, dates = FALSE) {
  msg <- vector()
  for(i in cols) {
    if(!is.null(i) && !any(tolower(i) %in% names(df))) {
      msg <- c(msg, paste0("Column '", i, "' does not exist"))
    }
  }

  if(dates) {
    if(!any(c("date", "date_start") %in% names(df))) {
      msg <- c(msg, "No date or date range columns")
    }
    if(!any(c("date_time", "date_time_start") %in% names(df))) {
      msg <- c(msg, "No date/time or date/time range columns")
    }
  }

  if(length(msg) > 0) rlang::abort(c(paste0("Problems with data `", name, "`:"),
                                     msg, extra), call = NULL)
}

#' Check for date formats
#' @noRd
check_dates <- function(df, cols, extra = "") {
  msg <- vector()
  for(i in cols) {
    if(!(lubridate::is.POSIXct(df[[tolower(i)]]) |
         lubridate::is.Date(df[[tolower(i)]]) |
         is_dateable(df[[tolower(i)]]))) {
      msg <- c(msg, paste0(
        "Column `", i,
        "` is not a Date or Date-Time column in YYYY-MM-DD HH:MM:SS format"))
    }
  }
  if(length(msg) > 0) rlang::abort(c("Problems with dates: ", msg), call = NULL)
}

#' Check that datetimes are UTC
#'
#' If non-UTC timezone, let user know that we're forcing the tz label to UTC.
#' If there are multiple timezones, error because we don't know how to handle
#' that. And there should never be columns with different timezones.
#'
#' @param df Data farme to check
#' @param cols Columns in df to check
#'
#' @noRd
check_UTC <- function(df, cols = "date_time") {

  cols <- stats::setNames(cols, NULL)
  tz <- purrr::map_chr(sf::st_drop_geometry(df[cols]), lubridate::tz)

  if(length(unique(tz)) > 1) {
    rlang::abort(
      c("Multiple timezones detected in `date_time` columns. ",
        "`date_time`s should be in 'local' timezone marked with 'UTC'."),
      call = NULL)
  } else if (any(tz != "UTC")) {
    rlang::inform(
      c("`date_time` columns are assumed to be in 'local' time marked with 'UTC'",
        "Removing timezone specification"))
    df <- dplyr::mutate(df, dplyr::across(dplyr::all_of(cols), ~lubridate::force_tz(.x, "UTC")))
  }
  df
}

#' Check for data frame or file name
#'
#' If not a data frame, and not sf, and not character, then error
#'
#' - Let the readr/readxl functions test if the file can be opened
#' - Allows both data frames *and* spatial data frames
#'
#' @noRd
check_df_file <- function(input) {
  if(!is.data.frame(input) & !inherits(input, "sf")) {
    if(!is.character(input)) {
      rlang::abort(paste0(
        "`", deparse(substitute(input)), "` must be either ",
        "a data frame or the location of a CSV or Excel file to open"))
    }
  }
}

#' If spatial sf, ensure POINT geometry
#' @noRd
check_points <- function(df) {
  if(!all(sf::st_geometry_type(df) == "POINT")) {
    rlang::abort("Spatial data must use POINT geometries", call = NULL)
  }
}

#' Find and check the date/time columns to join by
#'
#' For use in `add_site()`
#'
#' Looks for date/date_time, or date_start/data_end/etc. versions.
#'
#' @noRd
check_date_joins <- function(df, by_date, quiet = FALSE) {

  n_single <- stringr::str_subset(names(df), paste0("^", by_date, "$"))
  n_range <- stringr::str_subset(names(df), paste0("^", by_date, "_(start|end)$"))

  n_all <- paste0(c(n_single, n_range), collapse = "`, `")
  if(n_all == "") n_all <- "none"

  # Single column
  if(length(n_range) == 0 & length(n_single) == 1) {

    use <- n_single
    if(!quiet) {
      rlang::inform(paste0("Joining by column `", n_single, "` using buffers"))
    }

    # Single column used, but half of ranged detected
  } else if(length(n_range) == 1 & length(n_single) == 1) {

    use <- n_single
    rlang::inform(
      paste0(
        "Joining by column `", n_single, "` using buffers\n",
        "(Only `", n_range, "` detected but both `",
        paste0(paste0("`", n_single, c("_start`", "_end`")), collapse = " and "),
        " are required to use a range.")
    )

    # Range
  } else if(length(n_range) == 2) {

    use <- n_range
    if(!quiet) {
      rlang::inform(
        paste0("Joining by columns ",
               paste0(paste0("`", n_range, "`"), collapse = " and ")))
    }

    # No dates but required
  } else if(!is.null(by_date)) {
    rlang::abort(
      c("Cannot find date/time columns for joining",
        paste0("Require either `date`/`date_time` or *both* ",
               "`date_start`/`date_time_start` and `date_end`/`date_time_end`"),
        paste0("Found: ", n_all),
        "To join using `by` only, specify `by_date = NULL`"
      ),
      call = NULL)

   # Dates Ignored
  } else {

    use <- NULL
    if(!quiet) {
      rlang::inform(
        "Ignoring dates - Joining with `by` columns only (`by_date == NULL`)")
    }
  }

  use
}

#' Check columns in join are available
#'
#' Used in `add_sites()` to check that `by` and `by_date` don't conflict.
#'
#' @param by Character vector. Columns to use in the join
#' @param df Data frame columns are in
#' @param cols_omit Character vector. Columns that cannot be used (i.e. because
#'   actually date columns to be used in by_date)
#' @param quiet Whether to return non-essential messages.
#'
#' @noRd
check_by <- function(by, df, cols_omit, quiet = FALSE) {
  if(any(cols_omit %in% by)) {
    rlang::abort(
      c("Cannot use '", paste0(cols_omit, collapse = "' or '"), "' in `by`. "),
      call = NULL)
  }

  nm <- deparse(substitute(df))
  for(i in by) {
    if(all(is.na(df[[i]]))) {
      by <- by[by != i]
      if(!quiet) {
        rlang::inform(c(
          "*" = paste0("Column '", i, "' in `", nm, "` is all NA. ",
                       "Omitting from joins (`by`).")))
      }
    }
  }
  by
}

#' Check that timezone is valid
#' Valid means either in `OlsonNames()` or is 'local'
#' @noRd
check_tz <- function(tz) {
  nm <- deparse(substitute(tz))
  if(!tz %in% c("local", OlsonNames())) {
    rlang::abort(
      paste0("`", nm, "` must be provided and be either 'local' or ",
             "a valid timezone listed in `OlsonNames()`."),
      call = NULL)
  }
}
