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
