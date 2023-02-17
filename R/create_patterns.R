create_pattern_sep <- function(sep, optional = TRUE) {
  if(!all(sep == "")) sep <- paste0("(", paste0(sep, collapse = "|"), ")")
  if(optional) sep <- paste0(sep, "?")
  sep
}

#' Create a pattern to match date
#'
#' Helper function to create a regular expression pattern to match different
#' types of date formats in file paths.
#'
#' @param order Character. Expected order of (y)ear, (m)onth and (d)ate.
#'   Default is "ymd" for Year-Month-Date order.
#' @param sep Character vector. Expected (optional) separators between the
#'   values. Can be "" for no separator. Otherwise all separators are optional.
#' @param n_years Numeric. Number of digits in Year, either 2 or 4.
#'
#' @examples
#' create_pattern_date()
#' create_pattern_date(sep = "")
#'
#' @export
create_pattern_date <- function(order = "ymd", sep = c("_", "-"), n_years = 4) {

  sep <- create_pattern_sep(sep)

  if(n_years == 4) {
    y <- paste0("([12]{1}\\d{3})")  # First must be 1 or 2
  } else if (n_years == 2) {
    y <- "(\\d{2})"
  }

  m <- "(\\d{2})"
  d <- "(\\d{2})"

  dplyr::case_when(order == "ymd" ~ paste0(y, sep, m, sep, d),
                   order == "mdy" ~ paste0(m, sep, d, sep, y),
                   order == "dmy" ~ paste0(d, sep, m, sep, y))
}

#' @export
create_pattern_time <- function(sep = c("_", "-", ":"), seconds = TRUE) {

  sep <- create_pattern_sep(sep)

  h <- "([0-2]{1}[0-9]{1})"
  m <- "([0-5]{1}[0-9]{1})"

  p <- paste0(h, sep, m)
  if(seconds) p <- paste0(p, sep, "([0-5]{1}[0-9]{1})")
  p
}

#' @examples
#' create_pattern_dt_sep()
#' create_pattern_dt_sep(c("T", "_", "-"))
#' @export
create_pattern_dt_sep <- function(sep = "T", optional = FALSE) {
  create_pattern_sep(sep, optional)
}

#' @export
create_pattern_aru_id <- function(arus = c("BARLT", "S\\d(A|U)", "SM\\d", "SMM", "SMA"),
                                  n_digits = c(4, 8), sep = c("_", "-")) {

  sep <- create_pattern_sep(sep)

  arus <- paste0("(", arus, ")", collapse = "|")

  paste0("(", arus, ")", sep, "\\d{", n_digits[1], ",", n_digits[2], "}")
}
