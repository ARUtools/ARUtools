#' Count files in a project directory
#'
#' Helper function to explore the number of files in a directory, recursively.
#'
#' @inheritParams common_docs
#'
#' @export
count_files <- function(project_dir, subset = NULL, subset_type = "keep") {

  list_files(project_dir, subset, subset_type, type = "directory") |>
    dplyr::as_tibble() |>
    dplyr::rename("dir" = "value") |>
    dplyr::mutate(n = purrr::map_int(
      .data$dir, ~length(fs::dir_ls(.x, type = "file")),
      .progress = TRUE),
      dir = stringr::str_remove(dir, project_dir))


}

#' Check output of `clean_metadata()`
#'
#' Cleaning metadata can take a series of tries. This function helps summarize
#' and explore the metadata for possible patterns which may help find problems.
#'
#' @inheritParams common_docs
#'
#' @return A data frame summarizing the metadata by site_id, aru_type, aru_id,
#' and (optionally) by date. Presents the number of files, directories, and days
#' worth of recordings, as well as the minimum and maximum recording times.
#'
#' @export
#'
#' @examples
#' m <- clean_metadata(project_files = example_files)
#'
#' check_meta(m)
#' check_meta(m, date = TRUE)
#'
check_meta <- function(meta, date = FALSE) {
  g <- c("site_id", "aru_type", "aru_id", "type")
  if(date) g <- c(g, "date")

  m <- meta |>
    dplyr::group_by(dplyr::across(dplyr::all_of(g))) |>
    dplyr::summarize(n_files = dplyr::n(),
                     n_dirs = dplyr::n_distinct(fs::path_dir(.data$path)),
                     min_date = min(.data$date_time),
                     max_date = max(.data$date_time),
                     n_days = dplyr::n_distinct(.data$date),
                     min_time = hms::as_hms(min(hms::as_hms(.data$date_time))),
                     max_time = hms::as_hms(max(hms::as_hms(.data$date_time))),
                     .groups = "drop") |>
    dplyr::relocate("n_days", .before = "min_date")

  if(date) m <- dplyr::select(m, -"min_date", -"max_date")
  m
}

#' Check problems in output of `clean_metadata()`
#'
#' Cleaning metadata can take a series of tries. This function helps summarize
#' and explore missing metadata (problems).
#'
#' @param df Data frame. Either meta data (`clean_metadata()`) or GPS
#'   coordinates (`clean_gps()`)
#' @param check Character. Character vector of columns to check for missing
#'   values. Default is `site_id`, `aru_id`, `date`, `date_time`, `longitude`
#'   and `latitude`.
#' @param path Logical. Whether to return just the file paths which have missing
#' attributes. Default `FALSE`
#'
#' @inheritParams common_docs
#'
#' @return A data frame summarizing the metadata by site_id, aru_type, aru_id,
#' and (optionally) by date. Presents the number of files, directories, and days
#' worth of recordings, as well as the minimum and maximum recording times.
#'
#' @export
#'
#' @examples
#'
#' m <- clean_metadata(project_files = example_files, pattern_aru_id = "test")
#'
#' check_problems(m)
#' check_problems(m, date = TRUE)
#' check_problems(m, path = TRUE)
check_problems <- function(df, check = c("site_id", "aru_id",
                                         "date", "date_time", "longitude",
                                         "latitude"),
                           path = FALSE, date = FALSE) {
  if(path & date) {
    rlang::abort(c("`date` summarizes problems, so `path` cannot be returned",
                   "Use one or the other"),
                 call = NULL)
  }

  # If a mix of gps and meta, remove gps

  if(any(df$type != "gps")) {
    df <- dplyr::filter(df, .data$type != "gps")
  }

  df <- df |>
    dplyr::filter(dplyr::if_any(dplyr::any_of(check), ~is.na(.x))) |>
    dplyr::select(-dplyr::any_of(c("type", "file_name", "aru_type")))


  if(date) {
    check <- check[check %in% c("longitude", "latitude", "date_time")]
    df <- df |>
      dplyr::group_by(
        dplyr::across(dplyr::any_of(c("site_id", "aru_id", "date")))) |>
      dplyr::summarize(
        dplyr::across(dplyr::any_of(check),
                      list("min" = min_q, "max" = max_q,
                           "n" = length,
                           "n_na" = ~sum(is.na(.x)))),
        .groups = "drop")
  }

  if(path) df <- dplyr::pull(df, "path")
  df
}

#' Explore a file
#'
#' Shows the first few lines in a text file. Useful for trying to understand
#' problems in GPS files.
#'
#' Wrapper around `readr::read_lines(n_max)`.
#'
#' @param file_name Character. File path to check.
#' @param n_max Numeric. Number of lines in the file to show. Default 10.
#' @param ... Arguments passed on to `readr::read_lines()`
#'
#' @export
check_file <- function(file_name, n_max = 10, ...) {
  readr::read_lines(file_name, n_max = n_max, ...)
}

