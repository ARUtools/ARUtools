check_data <- function(df, type, ref) {

  if(!is.null(df)) {
    if(!inherits(df, "data.frame")) {
      rlang::abort("`", type, "` must be a data frame. See `", ref, "`",
                   call = NULL)
    }

    e <- paste0("Should be output of `", ref, "`")

    if(type == "meta") {
      check_cols(
        df, c("file_name", "path", "site_id", "date", "date_time", "aru_id"),
        name = "meta",
        extra = e)
    } else if(type == "sites") {
      check_cols(
        df, c("site_id", "aru_id", "latitude", "longitude"),
        dates = TRUE,
        name = "sites",
        extra = e)
    } else if (type == "meta_sites") {
      check_cols(
        df, c("site_id", "aru_id", "latitude", "longitude"),
        dates = TRUE,
        name = "meta",
        extra = e)
    } else if (type == "meta_sun") {
      check_cols(
        df, c("site_id", "aru_id", "latitude", "longitude", "tz", "t2sr", "t2ss"),
        dates = TRUE,
        name = "meta",
        extra = e)
    }

    # Check dates are dates
    dt <- stringr::str_subset(names(df), "date")
    check_dates(df, dt)
  }
}

check_ext <- function(ext, opts)  {

  if(!ext %in% opts) {
    rlang::abort(
      paste0("File extension must be one of ", paste0(opts, collapse = ", ")),
      call = NULL)
  }
}


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

# Let the readr/readxl functions test if the file can be opened
# Allows both data frames *and* spatial data frames
check_df_file <- function(input) {
  if(!is.data.frame(input) & !inherits(input, "sf")) {
    if(!is.character(input)) {
      rlang::abort(paste0(
        "`", deparse(substitute(input)), "` must be either ",
        "a data frame or the location of a CSV or Excel file to open"))
    }
  }
}

check_points <- function(df) {
  if(!all(sf::st_geometry_type(df) == "POINT")) {
    rlang::abort("Spatial data must use POINT geometries", call = NULL)
  }
}

check_date_joins <- function(df, by_date) {

  n_single <- stringr::str_subset(names(df), paste0("^", by_date, "$"))
  n_range <- stringr::str_subset(names(df), paste0("^", by_date, "_(start|end)$"))

  n_all <- paste0(c(n_single, n_range), collapse = "`, `")
  if(n_all == "") n_all <- "none"

  if(length(n_range) == 0 & length(n_single) == 1) {
    rlang::inform(paste0("Joining by column `", n_single, "` using buffers"))
    use <- n_single
  } else if(length(n_range) == 1 & length(n_single) == 1) {
    rlang::inform(
      paste0("Joining by column `", n_single, "` using buffers\n",
             "(Only `", n_range, "` detected but both `",
             paste0(paste0("`", n_single, c("_start`", "_end`")), collapse = " and "),
             " are required to use a range.")
    )
    use <- n_single
  } else if(length(n_range) == 2) {
    rlang::inform(
      paste0("Joining by columns ",
             paste0(paste0("`", n_range, "`"), collapse = " and ")))
    use <- n_range
  } else {
    rlang::abort(
      c("Cannot find date/time columns for joining",
        paste0("Require either `date`/`date_time` or *both* ",
               "`date_start`/`date_time_start` and `date_end`/`date_time_end`"),
        paste0("Found: `", n_all, "`")
      ),
      call = NULL)
  }

  use
}

check_tz <- function(tz) {
  nm <- deparse(substitute(tz))
  if(!tz %in% c("local", OlsonNames())) {
    rlang::abort(
      paste0("`", nm, "` must be provided and be either 'local' or ",
             "a valid timezone listed in `OlsonNames()`."),
      call = NULL)
  }
}
