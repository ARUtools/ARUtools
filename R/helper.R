#' Count files in a project directory
#'
#' Helper function to explore the number of files in a directory, recursively.
#'
#' @inheritParams common_docs
#'
#' @export
count_files <- function(project_dir, subset = NULL, subset_type = "keep") {

  list_files(project_dir, subset, subset_type, type = "directory") %>%
    dplyr::as_tibble() %>%
    dplyr::rename("dir" = "value") %>%
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
#' @param by_date Logical. Whether to summarize metadata by date. Default
#'   `FALSE`.
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
#' check_meta(m, by_date = TRUE)
#'
check_meta <- function(meta, by_date = FALSE) {
  g <- c("site_id", "aru_type", "aru_id", "type")
  if(by_date) g <- c(g, "date")

  m <- meta %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(g))) %>%
    dplyr::summarize(n_files = dplyr::n(),
                     n_dirs = dplyr::n_distinct(fs::path_dir(.data$path)),
                     min_date = min(.data$date_time),
                     max_date = max(.data$date_time),
                     n_days = dplyr::n_distinct(.data$date),
                     min_time = hms::as_hms(min(hms::as_hms(.data$date_time))),
                     max_time = hms::as_hms(max(hms::as_hms(.data$date_time))),
                     .groups = "drop") %>%
    dplyr::relocate("n_days", .before = "min_date")

  if(by_date) m <- dplyr::select(m, -"min_date", -"max_date")
  m
}

#' Explore a file
#'
#' Shows the `n_max` first lines in a text file. Useful for trying to understand
#' problems in GPS files.
#'
#' Wrapper around `readr::read_lines(n_max)`.
#'
#' @param file_name Character. File path to check.
#' @param n_max Numeric. Number of lines in the file to show.
#'
#' @export
check_file <- function(file_name, n_max = 10) {
  readr::read_lines(file_name, n_max = n_max)
}

