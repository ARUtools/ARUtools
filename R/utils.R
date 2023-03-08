
list_files <- function(project_dir, subset, subset_type,
                       type = c("file", "directory")) {
  fs::dir_ls(project_dir, type = type,
             # Add filters
             regexp = subset,
             invert = subset_type == "omit",
             recurse = TRUE)
}


extract_replace <- function(string, pattern) {
  string %>%
    stringr::str_extract(
      stringr::regex(paste0("(", names(pattern), ")", collapse = "|"),
                     ignore_case = TRUE)) %>%
    stringr::str_replace_all(stringr::regex(pattern, ignore_case = TRUE))
}

report_missing <- function(missing, total, name) {
  msg <- NULL
  if(missing > 0) {
    if(missing == total) type <- "No" else type <- "Not all"
    msg <- c("x" = paste0(type, " ", name, " were successfully detected (",
                          missing, "/", total, ")"))
  }
  msg
}

date_join <- function(x, y, by, id, col = "date", int = "date_range") {

  # Nested filters
  match <- y |>
    dplyr::ungroup() |>
    tidyr::nest(gps = -c(int)) |>
    dplyr::mutate(data = purrr::map2(
      .data[[int]], .data$gps,
      ~dplyr::filter(m, lubridate::`%within%`(.data[[col]], .x)) |>
        dplyr::inner_join(.y, by = .env$by))) |>
    dplyr::select(-dplyr::any_of(int), -"gps") |>
    tidyr::unnest(data)

  no_match <- dplyr::anti_join(x, match, by = id)

  all <- dplyr::bind_rows(match, no_match)

  if(nrow(x) != nrow(all)) {
    all <- dplyr::add_count(all, .data[[id]], name = "n_matches")
    all$n_matches[is.na(all$longitude)] <- NA_integer_
  }

  all
}


floor_dec <- function(x, digits) {
 floor(x * 10^digits) / 10^digits
}
