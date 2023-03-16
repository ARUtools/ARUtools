check_ext <- function(ext, opts)  {

  if(!ext %in% opts) {
    rlang::abort(
      paste0("File extension must be one of ", paste0(opts, collapse = ", ")),
      call = NULL)
  }
}

check_string <- function(arg, not_null = TRUE) {
  nm <- paste0("`", deparse(substitute(arg)), "`")
  if(not_null && is.null(arg)) {
    rlang::abort(nm, " cannot be `NULL`.", call = NULL)
  } else if(!is.null(arg) && !is.character(arg)) {
    rlang::abort(nm, " must be a text string", call = NULL)
  }
}


check_cols <- function(df, cols, name, extra = NULL) {
  msg <- vector()
  for(i in cols) {
    if(!is.null(i) && !any(tolower(i) %in% names(df))) {
      msg <- c(msg, paste0("Column '", i, "' does not exist"))
    }
  }
  if(length(msg) > 0) rlang::abort(c(paste0("Problems with data `", name, "`:"),
                                     msg, extra), call = NULL)
}


check_dates <- function(df, cols) {
  msg <- vector()
  for(i in cols) {
    if(!(lubridate::is.POSIXct(df[[tolower(i)]]) | lubridate::is.Date(df[[tolower(i)]]))) {
      msg <- c(msg, paste0("Column '", i, "' is not a standard Date (YYYY-MM-DD) column"))
    }
  }
  if(length(msg) > 1) rlang::abort(c("Problems with dates: ", msg), call = NULL)
}

check_index <- function(index) {
  if(!is.null(index)) {
    if(!inherits(index, "data.frame")) {
      rlang::abort("`site_index` must be a data frame. See `clean_site_index()`",
                   call = NULL)
    }
    check_cols(index, c("site_id", "date_start", "date_end"))
    check_dates(index, c("date_start", "date_end"))
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
