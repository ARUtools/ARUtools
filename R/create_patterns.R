# Pattern Docs ----------------------------------------------

#' Create a pattern to match date
#'
#' Helper functions to create regular expression patterns to match different
#' metadata in file paths.
#'
#' By default `create_pattern_aru_id()` matches many common ARU patterns like
#' `BARLT0000`, `S4A0000`, `SM40000`, `SMM0000`, `SMA0000`.
#'
#' `test_pattern()` is a helper function to see what a regular expression
#' pattern will pick out of some example text. Can be used to see if your
#' pattern grabs what you want. This is just a simple wrapper around
#' `stringr::str_extract()`.
#'
#' @return Either a pattern (`create_pattern_xxx()`) or the text extracted by a
#'   pattern (`test_pattern()`)
#'
#' @name create_pattern
NULL

#' @param order Character. Expected order of (y)ear, (m)onth and (d)ate.
#'   Default is "ymd" for Year-Month-Date order.
#' @param sep Character vector. Expected separator(s) between the
#'   values. Can be "" for no separator.
#' @param n_years Numeric. Number of digits in Year, either 2 or 4.
#'
#' @examples
#' create_pattern_date()  # Default matches 2020-01-01 (- or _ as optional separators)
#' create_pattern_date(sep = "") # Matches 20200101 (no separator)
#'
#' @export
#' @describeIn create_pattern Create a pattern to match a date

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

#' @param sep Character vector. Expected separators between the
#'   values. Can be "" for no separator.
#' @param seconds Logical. Whether seconds are included.
#'
#' @examples
#' create_pattern_time()  # Default matches 23_59_59 (_, -, :, as optional separators)
#' create_pattern_time(sep = "", seconds = FALSE) # Matches 2359 (no seconds no separators)
#'
#' @export
#'
#' @describeIn create_pattern Create a pattern to match a time
create_pattern_time <- function(sep = c("_", "-", ":"), seconds = TRUE) {

  sep <- create_pattern_sep(sep)

  h <- "([0-2]{1}[0-9]{1})"
  m <- "([0-5]{1}[0-9]{1})"

  p <- paste0(h, sep, m)
  if(seconds) p <- paste0(p, sep, "([0-5]{1}[0-9]{1})")
  p
}

#' @param optional Logical. Whether the separator should be optional or not.
#'   Allows matching on different date/time patterns.
#'
#' @examples
#' create_pattern_dt_sep()  # Default matches 'T' as a required separator
#' create_pattern_dt_sep(optional = TRUE) # 'T' as an optional separator
#' create_pattern_dt_sep(c("T", "_", "-")) # 'T', '_', or '-' as separators
#'
#' @export
#' @describeIn create_pattern Create a pattern to match a date/time separator

create_pattern_dt_sep <- function(sep = "T", optional = FALSE) {
  create_pattern_sep(sep, optional)
}


#' @param arus Character. Pattern identifying the ARU prefix (usually model specific).
#' @param n_digits Numeric. Number of digits expected to follow the `arus` pattern. Can be one or two (a range).
#'
#' @examples
#' create_pattern_aru_id()
#' create_pattern_aru_id(prefix = "CWS")
#' create_pattern_aru_id(n_digits = 12)
#'
#' @export
#' @describeIn create_pattern Create a pattern to match an ARU id
create_pattern_aru_id <- function(arus = c("BARLT", "S\\d(A|U)", "SM\\d", "SMM", "SMA"),
                                  n_digits = c(4, 8), sep = c("_", "-"),
                                  prefix = "", suffix = "") {

  sep <- create_pattern_sep(sep, optional = TRUE)
  if(length(n_digits) > 1) {
    n_digits <- paste0("\\d{", n_digits[1], ",", n_digits[2], "}")
  } else {
    n_digits <- paste0("\\d{", n_digits, "}")
  }

  arus <- paste0("(", arus, ")", collapse = "|")
  prefix <- pat_collapse(prefix)
  suffix <- pat_collapse(suffix)

  paste0(prefix, "(", arus, ")", sep, n_digits, suffix)
}

#' @param prefix Character. Prefixes (can be more than one) for site ids.
#' @param p_digits Numeric. Number of digits following the `prefix`.
#' @param sep Character. Separators between a prefix and suffix sections
#' @param suffix Character. Suffixes (can be more than one) for site ids.
#' @param s_digits Numeric. Number of digits following the `suffix`.
#'
#' @export
#'
#' @examples
#'
#' create_pattern_site_id() # Default matches P00-0
#' create_pattern_site_id(prefix = "site", p_digits = 3, sep = "",
#'                        suffix = c("a", "b", "c"), s_digits = 0) # Matches site000a
#'
#' @describeIn create_pattern Create a pattern to match a site id
create_pattern_site_id <- function(prefix = c("P", "Q"),
                                   p_digits = 2,
                                   sep = c("_", "-"),
                                   suffix = "",
                                   s_digits = 1) {

  sep <- create_pattern_sep(sep, optional = FALSE)

  prefix <- pat_collapse(prefix)
  if(p_digits > 0) prefix <- paste0(prefix, "\\d{", p_digits, "}")

  suffix <- pat_collapse(suffix)
  if(s_digits > 0) suffix <- paste0(suffix, "\\d{", s_digits, "}")
  if(suffix != "") suffix <- paste0(sep, suffix)

  paste0(prefix, suffix)
}

create_pattern_sep <- function(sep, optional = TRUE) {
  if(!all(sep == "")) {
    sep <- paste0("(", paste0(sep, collapse = "|"), ")")
    if(optional) sep <- paste0(sep, "?")
  } else sep <- ""
  sep
}


pat_collapse <- function(x) {
  if(any(x != "")) {
    paste0("(", paste0("(", x, ")", collapse = "|"), ")")
  } else ""
}




#' @param test Character. Example text to test.
#' @param pattern Character. Regular expression pattern to test.
#'
#' @export
#'
#' @examples
#' pat <- create_pattern_aru_id(prefix = "CWS")
#' test_pattern("CWS_BARLT1012", pat)            # No luck
#' pat <- create_pattern_aru_id(prefix = "CWS_")
#' test_pattern("CWS_BARLT1012", pat)            # Ah ha!
#' pat <- create_pattern_site_id()
#'
#' pat <- create_pattern_site_id()
#' test_pattern("P03", pat)   # Nope
#' test_pattern("P03-1", pat) # Success!
#'
#' pat <- create_pattern_site_id(prefix = "site", p_digits = 3, sep = "", s_digits = 0)
#' test_pattern("site111", pat)
#' pat <- create_pattern_site_id(prefix = "site", p_digits = 3, sep = "",
#'                              suffix = c("a", "b", "c"), s_digits = 0)
#' test_pattern("site100a", pat)
#'
#' @describeIn create_pattern Test patterns

test_pattern <- function(test, pattern) {
 stringr::str_extract(test, pattern)
}
