#' Extract and clean ARU metadata from file names
#'
#' Using regular expressions, metadata is extracted from file names and
#' directory structure, checked and cleaned.
#'
#' See `vignette("customizing", package = "ARUtools")` for details on customizing
#' `clean_metadata()` for your project.
#'
#' @param file_type Character. Type of file (extension) to summarize. Default wav.
#' @param pattern_site_id Character. Regular expression to extract site ids. See
#'   `create_pattern_site_id()`.
#' @param pattern_aru_id Character. Regular expression to extract ARU ids. See
#'   `create_pattern_aru_id()`.
#' @param pattern_date Character. Regular expression to extract dates. See
#'   `create_pattern_date()`.
#' @param pattern_time Character. Regular expression to extract times. See
#'   `create_pattern_time()`.
#' @param pattern_dt_sep Character. Regular expression to mark separators betwen
#'   dates and times. See `create_pattern_dt_sep()`.
#' @param order_date Character. Order that the date appears in. One of "ymd"
#'   (default), "mdy", "dmy".
#' @param quiet Logical. Whether to suppress progress messages.
#'
#' @inheritParams common_docs
#'
#' @return Data frame with extracted metadata
#'
#' @examples
#' clean_metadata(project_files = example_files)
#' clean_metadata(project_files = example_files, subset = "P02")
#'
#' @export
clean_metadata <- function(
    project_dir = NULL,
    project_files = NULL,
    file_type = "wav",
    subset = NULL,
    subset_type = "keep",
    pattern_site_id = create_pattern_site_id(),
    pattern_aru_id = create_pattern_aru_id(),
    pattern_date = create_pattern_date(),
    pattern_time = create_pattern_time(),
    pattern_dt_sep = create_pattern_dt_sep(),
    order_date = "ymd",
    quiet = FALSE) {

  # Checks
  check_text(project_dir, not_null = FALSE)
  check_text(project_files, not_null = FALSE, n = c(1, Inf))
  check_text(file_type)
  check_text(subset, not_null = FALSE)
  check_text(subset_type)
  check_text(pattern_site_id)
  check_text(pattern_aru_id)
  check_text(pattern_date)
  check_text(pattern_dt_sep)
  check_text(order_date)
  check_logical(quiet)


  # TODO: more checks

  pattern_date_time <- paste0(pattern_date, pattern_dt_sep, pattern_time)
  file_type_pattern <- stringr::regex(paste0(file_type, "$"), ignore_case = TRUE)

  if(!is.null(project_dir)) {
    if(!quiet) rlang::inform("Fetching file list...")
    project_files <- list_files(project_dir, subset, subset_type,
                                type = "file")
  } else if(!is.null(subset)){
    project_files <- stringr::str_subset(project_files, subset,
                                         negate = subset_type == "omit")
  }

  # Check for files (either zero or all directories)
  if(length(project_files) == 0 || all(fs::is_dir(project_files))) {
    if(is.null(subset)) {
      msg <- "`project_dir`"
    } else {
      msg <- "`project_dir`/`subset`/`subset_type` combination"
    }

    rlang::abort(c(
      paste0("There are no files in the ", msg, " you have specified. Note:"),
      "i" = "Paths are case-sensitive",
      "i" = "Check folders using `list.dirs(path = PROJECT_DIR)`",
      "i" = "Check for files using `count_files(project_dir = PROJECT_DIR)`")
    )
  }

  # Check for file types
  n_ext <- sum(stringr::str_detect(project_files, file_type_pattern))
  if(n_ext == 0){
    rlang::abort(c(glue::glue("Did not find any '{file_type}' files."),
                   "i" = "Use `file_type` to change file extension for sound files",
                   "i" = "Check `project_dir`/`project_files` are correct"))
  }


  # Collect non-file-type files
  extra <- stringr::str_subset(project_files, file_type_pattern, negate = TRUE)
  gps <- stringr::str_subset(extra, stringr::regex("gps|summary", ignore_case = TRUE))
  focal <- stringr::str_subset(project_files, file_type_pattern)

  # Set up file path metadata
  meta <- dplyr::tibble(
    dir = fs::path_dir(focal),
    file_name = fs::path_file(focal),
    type = tolower(fs::path_ext(focal)))

  if(length(gps) > 1) {
    meta <- meta |>
      dplyr::add_row(dir = fs::path_dir(gps),
                     file_name = fs::path_file(gps),
                     type = "gps")
  }

  pattern_aru_type <- c("barlt" = "BarLT",
                        "SMM" = "SongMeter",
                        "SM\\d" = "SongMeter",
                        "S\\dA" = "SongMeter")

  if(!quiet) rlang::inform("Extracting ARU info...")

  # Extract ARU metadata -----------------------
  meta <- meta |>
    dplyr::mutate(
      path = file.path(.data$dir, .data$file_name),
      aru_type = extract_replace(.data$file_name, pattern_aru_type),
      aru_type = dplyr::if_else(is.na(.data$aru_type),
                                extract_replace(.data$dir, pattern_aru_type),
                                .data$aru_type),
      aru_id = stringr::str_extract(.data$file_name, pattern_aru_id),
      aru_id = dplyr::if_else(is.na(.data$aru_id),
                              stringr::str_extract(.data$dir, pattern_aru_id),
                              .data$aru_id))

  meta <- dplyr::mutate(meta, site_id = stringr::str_extract(.data$dir, .env$pattern_site_id))

  pattern_non_date <- paste0("(", pattern_site_id, ")|(",
                             pattern_aru_id, ")|(",
                             paste0("(", pattern_aru_type, ")", collapse = "|"),
                             ")")


  # Extract Date/time --------------------------
  if(!quiet) rlang::inform("Extracting Dates and Times...")

  meta <- meta |>
    dplyr::mutate(
      file_left = stringr::str_remove_all(.data$file_name, pattern_non_date),
      dir_left = stringr::str_remove_all(.data$dir, pattern_non_date),

      # Try file name
      date_time_chr = stringr::str_extract(.data$file_left, .env$pattern_date_time),
      # Try dir name
      date_time_chr = dplyr::if_else(
        is.na(.data$date_time_chr),
        stringr::str_extract(.data$dir_left, .env$pattern_date_time),
        .data$date_time_chr),
      # Get date_times
      date_time = lubridate::parse_date_time(
        .data$date_time_chr,
        orders = paste(order_date, "HMS")),
      date = lubridate::as_date(.data$date_time))

  if(any(is.na(meta$date))) {

    missing <- meta |>
      dplyr::filter(is.na(.data$date)) |>
      dplyr::mutate(
        # Try file name
        date_chr = stringr::str_extract(.data$file_left, .env$pattern_date),
        # Try dir name
        date_chr = dplyr::if_else(
          is.na(.data$date_chr),
          stringr::str_extract(.data$dir_left, .env$pattern_date),
          .data$date_chr),
        date = lubridate::parse_date_time(.data$date_chr, orders = order_date,
                                          quiet = TRUE),
        date = lubridate::as_date(.data$date)) |>
      dplyr::select("path", "date")

    if(any(!is.na(missing$date))) {
      # Add dates where missing
      meta <- dplyr::rows_patch(meta, missing, by = "path")
    }
  }

  # Report on details -------------------------
  # Extra files
  if(length(extra) > 1) {
    rlang::inform(
      c("!" = paste0("Omitted ", length(extra), " extra, non-",
                     file_type, "/GPS files")))
  }

  if(length(gps) > 1) {
    rlang::inform(c("!" = paste0("Detected ", length(gps), " GPS logs")))
  }

  # Flag problems
  f <- dplyr::filter(meta, .data$type == "wav")
  n <- nrow(f)
  f_d <- sum(is.na(f$date))
  f_dt <- sum(is.na(f$date_time))
  f_type <- sum(is.na(f$aru_type))
  f_id <- sum(is.na(f$aru_id))
  f_site <- sum(is.na(f$site_id))

  if(any(c(f_d, f_dt, f_type, f_id, f_site) > 0)) {
   msg <- c("Identified possible problems with metadata extraction:")
   msg <- c(msg, report_missing(f_d, n, "dates"))
   msg <- c(msg, report_missing(f_dt, n, "times"))
   msg <- c(msg, report_missing(f_type, n, "ARU types"))
   msg <- c(msg, report_missing(f_id, n, "ARU ids"))
   msg <- c(msg, report_missing(f_site, n, "sites"))
   rlang::inform(msg)
  }

  meta |>
    dplyr::arrange(.data$type != "gps", !is.na(.data$date_time), .data$path,
                   .data$site_id, .data$date_time) |>
    dplyr::select(-"file_left", -"dir_left", -"date_time_chr", -"dir")
}




