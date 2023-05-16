
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

date_join <- function(x, y, by, id, col = "date", int = "date_range",
                      check_col = "...n") {

  # Nested filters
  match <- y |>
    dplyr::ungroup() |>
    tidyr::nest(add = -dplyr::all_of(int)) |>
    dplyr::mutate(data = purrr::map2(
      .data[[int]], .data$add,
      ~dplyr::filter(x, lubridate::`%within%`(.data[[col]], ..1)) |>
        dplyr::inner_join(..2, by = .env$by))) |>
    dplyr::select(-dplyr::any_of(int), -"add") |>
    tidyr::unnest("data")

  no_match <- dplyr::anti_join(x, match, by = id)

  all <- dplyr::bind_rows(match, no_match)

  if(nrow(x) != nrow(all)) {
    all <- dplyr::add_count(all, .data[[id]], name = "n_matches")
    all$n_matches[is.na(all[[check_col]])] <- NA_integer_
  }

  all
}


floor_dec <- function(x, digits) {
 floor(x * 10^digits) / 10^digits
}


#' Check if character is easily convertable to Date
#'
#' Checks if `lubridate::as_date()` can convert the string.
#' If warning or error returns `FALSE` else returns `TRUE`.
#'
#' @param x Character/Date. Date in text (if Date, passes through, no problem).
#'
#' @return TRUE/FALSE
#'
#' @examples
#' is_dateable("2023-01-01")          # TRUE
#' is_dateable("20-01-01")            # TRUE
#' is_dateable("2023-01-01 01:00:00") # TRUE
#' is_dateable("05/16/2020")          # FALSE
#'
#' @noRd
is_dateable <- function(x) {
  tryCatch(
    expr = {
      lubridate::as_date(x)
      TRUE
    },
    error = \(x) FALSE,
    warning = \(x) FALSE)
}

#' Quiet min/max functions
#'
#' Quietly return NA if no non-missing values (not -Inf or Inf)
#'
#' @param x
#'
#' @noRd
min_q <- function(x) minmax_q(x, min)
max_q <- function(x) minmax_q(x, max)

minmax_q <- function(x, fun) {
  if(length(x) == 0) {
    r <- NA
  } else if(all(is.na(x))) {
    r <- x[1]
  } else r <- fun(x, na.rm = TRUE)
  r
}



#' Convert spatial data frames to non-spatial data frames and back
#'
#' Extract geometry as latitude and longitude columns.
#' Backwards conversion uses crs from original spatial data.
#'
#' @noRd
sf_to_df <- function(sf) {
  if(inherits(sf, "sf")){
    sf <- sf::st_transform(sf, crs = 4326)

    df <- sf |>
      sf::st_drop_geometry() |>
      dplyr::bind_cols(sf::st_coordinates(sf)) |>
      dplyr::rename("longitude" = "X", "latitude" = "Y")
  } else df <- sf
  df
}

df_to_sf <- function(df, sf) {
  if(inherits(sf, "sf")) {
    sf <- df |>
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
      sf::st_transform(sf::st_crs(sf))
  } else sf <- df
  sf
}
