create_pattern_sep <- function(sep, optional = TRUE) {
  if(!all(sep == "")) sep <- paste0("(", paste0(sep, collapse = "|"), ")")
  if(optional) sep <- paste0(sep, "?")
  sep
}

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
# seconds = c("optional", "omit", "enforce")
create_pattern_time <- function(sep = c("_", "-", ":"), seconds = "optional") {

  sep <- create_pattern_sep(sep)

  h <- "([0-2]{1}[0-9]{1})"
  m <- "([0-5]{1}[0-9]{1})"
  s <- "([0-5]{1}[0-9]{1})"

  if(seconds == "optional") s <- paste0(s, "?")
  if(seconds == "omit") s <- ""

  paste0(h, sep, m, sep, s)
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


count_files <- function(project_dir, subset_dir = NULL, subset_type = "keep") {

  list_files(project_dir, subset_dir, subset_type, type = "directory") %>%
    dplyr::as_tibble() %>%
    dplyr::rename("dir" = "value") %>%
    dplyr::mutate(n = purrr::map_int(.data$dir, ~length(fs::dir_ls(.x, type = "file")),
                                     .progress = TRUE),
                  dir = stringr::str_remove(dir, project_dir))


}

#' @export
check_meta <- function(meta) {
  meta %>%
    dplyr::group_by(site, type, ARU_type, ARU_id) %>%
    dplyr::summarize(n_files = dplyr::n(),
                     n_dirs = dplyr::n_distinct(dir),
                     min_date = min(date_time),
                     max_date = max(date_time),
                     n_days = dplyr::n_distinct(date),
                     min_time = hms::as_hms(min(hms::as_hms(date_time))),
                     max_time = hms::as_hms(max(hms::as_hms(date_time))),
                     .groups = "drop") %>%
    dplyr::relocate("n_days", .before = "min_date")
}


