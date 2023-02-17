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


