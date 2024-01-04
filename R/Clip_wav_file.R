#' Clip single wave file
#'
#' Clip and copy a single wave files to a given length. See `clip_wave()` for
#' processing multiple files.
#'
#' @param path_in Character. Path to the wave file to clip.
#' @param path_out Character. Path to copy the new clipped wave file to.
#' @param clip_length Numeric. Length of new clip in seconds.
#' @param start_time Numeric. Time in seconds where new clip should start.
#'   Default 0 (start).
#' @param wave_length Numeric. Length of the wave file in seconds (if `NULL`,
#'   default, will be calculated).
#' @param overwrite Logical. Whether to overwrite existing files when creating
#'   new clipped wave files. Default (`FALSE`) will error if the file already
#'   exists.
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' # Create test wave file
#' f <- temp_wavs(1)
#'
#' # Clip file and check it out
#' clip_wave_single(f, "new_file.wav", clip_length = 1)
#' tuneR::readWave("new_file.wav")
#' unlink("new_file.wav")
#' }

clip_wave_single <- function(path_in, path_out, clip_length, start_time = 0,
                             wave_length = NULL, overwrite = FALSE) {

  # Checks
  if(length(path_in) > 1) {
    rlang::abort(c("More than one file supplied",
                   "x" = "`clip_wave_single` processes one file at a time",
                   "i" = "See `clip_wave` to process multiple files."),
                 call = NULL)
  }
  if(length(path_out) > 1) {
    rlang::abort("Can only have one output file", call = NULL)
  }
  check_num(start_time, n = c(0, Inf))

  if(!overwrite && fs::file_exists(path_out)) {
    rlang::abort(
      c("`overwrite` is FALSE but `path_out` already exists: ",
        "!" = path_out,
        "*" = "Either use `overwrite = TRUE` or move the file"),
      call = NULL)
  } else if (overwrite) {
    fs::file_delete(path_out)
  }

  # No clipping if clip length the same as the file
  if(is.null(wave_length)) wave_length <- get_wav_length(path_in, return_numeric = TRUE)

  if(clip_length >= wave_length && start_time == 0) {
    fs::file_copy(path = path_in, new_path = path_out)
  } else {
    wav_clipped <- tuneR::readWave(
      path_in, from = start_time,
      to = start_time + clip_length,
      units = 'seconds')
    tuneR::writeWave(wav_clipped, path_out)
  }

  TRUE
}


#' Clip multiple wave files and format names
#'
#' Process multiple wave files by copying them with a new filename and/or
#' clipping to a given length.
#'
#' @param waves Data frame. Details of file locations.
#' @param dir_in Character. Directory wave files are read from. Default is
#'   `NULL` meaning the current working directory.
#' @param dir_out Character. Directory to output files to.
#' @param col_path_in Character. Column name that contains the current file paths. Default `path`.
#'   **Note: file paths must be either relative to `dir_in` or absolute**.
#' @param col_subdir_out Character. Column name(s) that contain the
#'   subdirectory(ies) in which to put output files. Default `subdir_out`.
#' @param col_filename_out Character. Column name that contains the output
#'   filenames. Default `filename_out`.
#' @param col_clip_length Character. Column name that contains the length of the
#'   new clip. Default `length`.
#' @param col_start_time Character. Column name that contains the start time of
#'   the new clip. Default `start_time`
#' @param overwrite Logical. Overwrite pre-existing files when clipping and moving. (Default `FALSE`)
#' @param create_dir Logical. Whether to create directory structure for newly
#'   formatted and clipped wave files.
#' @param diff_limit Numeric. How much longer in seconds clip lengths can be
#'   compared to file lengths before triggering an error. Default `30`.
#' @param use_job Logical. Use the {job} package to copy files (Default `FALSE`)
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#'
#' \dontrun{
#' w <- data.frame(path = temp_wavs(n = 4),
#'                 subdir_out = c("test1", "test2", "test3", "test4"),
#'                 subsubdir_out = c("a", "b", "c", "d"),
#'                 filename_out = c("wave1_clean.wav", "wave2_clean.wav", "wave3_clean.wav", "wave4_clean.wav"),
#'                 length = c(1, 1, 1, 2),
#'                 start_time = c(1.2, 0.5, 1, 0))
#'
#' clip_wave(w, dir_out = "clean",
#'           col_subdir_out = c("subdir_out", "subsubdir_out"))
#'
#' unlink("clean", recursive = TRUE) # Remove this new 'clean' directory
#' }
#'
#'
#'
clip_wave <- function(waves,
                      dir_out,
                      dir_in = NULL,
                      col_path_in = "path",
                      col_subdir_out = "subdir_out",
                      col_filename_out = "filename_out",
                      col_clip_length = "length",
                      col_start_time = "start_time",
                      overwrite = FALSE,
                      create_dir = TRUE,
                      diff_limit = 30,
                      use_job = FALSE) {

  # Checks
  if(missing(dir_out)) rlang::abort(paste0("Require an output directory",
                                           "(`dir_out`)"), call = NULL)
  check_cols(waves, cols = c(col_path_in, col_subdir_out, col_filename_out,
                             col_clip_length, col_start_time))


  # Prepare processing df
  wv <- dplyr::tibble(.rows = 4)

  # Check and complete input paths
  wv[["path_in"]] <- check_wave_path_in(waves[[col_path_in]], dir_in)

  # Get output paths
  wv[["path_out"]] <- check_wave_path_out(waves[col_subdir_out],
                                          wv[["path_in"]],
                                          dir_out,
                                          create_dir)

  # Check wave lengths
  wv[["wave_length"]] <- check_wave_lengths(wv[["path_in"]],
                                            clip_lengths = waves[[col_clip_length]],
                                            start_times = waves[[col_start_time]],
                                            diff_limit = diff_limit)

  # Get the final arguments
  wv[["clip_length"]] <- waves[[col_clip_length]]
  wv[["start_time"]] <- waves[[col_start_time]]
  wv[["overwrite"]] <- overwrite

  if(!use_job) {
    purrr::pmap(wv, clip_wave_single)
  } else {
    job::job(purrr::pmap(wv, clip_wave_single),
             import = wv,
             packages = "ARUtools")
  }

  TRUE
}



#' Get the length of a recording in seconds
#'
#' @param wave_file Character. Path to wave file
#' @param return_numeric Logical. Return numeric or character?
#'
#' @return Length of recording in seconds
#' @export
#'
#' @examples
#' f <- tempfile()
#' wav <- download.file("https://www2.cs.uic.edu/~i101/SoundFiles/StarWars3.wav", destfile = f)
#' get_wav_length(f)

get_wav_length <- function(wave_file, return_numeric = FALSE) {
  audio <- tuneR::readWave(wave_file, header = TRUE)
  l <- round(audio$samples / audio$sample.rate, 2)
  if(!return_numeric) l <- glue::glue("{l} seconds")
  l
}


check_wave_path_in <- function(path_in, dir_in) {
  abs_path_in <- fs::is_absolute_path(path_in)
  if(length(unique(abs_path_in)) != 1) {
    rlang::abort(paste0("All wave file paths must be either absolute or ",
                        "relative (not a mix of the two)"), call = NULL)
  } else if (all(abs_path_in) && !is.null(dir_in)) {
    rlang::warn("All wave file paths are absolute, ignoring `dir_in`.", call = NULL)
  } else if (all(!abs_path_in)) {
    if(is.null(dir_in)) {
      path_in <- fs::path_wd(path_in)
    } else path_in <- fs::path(dir_in, path_in)
  }

  # Check that file paths exist
  if(any(missing <- !fs::file_exists(path_in))) {
    rlang::abort(
      c(paste0(
        "Some wave files could not be found relative to ",
        if(is.null(dir_in)) fs::path_wd() else dir_in,
        ":\n"),
        stats::setNames(names(missing), rep("*", length(missing)))),
      call = NULL
    )
  }

  # Check file types
  if(any(!fs::path_ext(path_in) %in% c("wav", "wave"))) {
    r <- path_in[!fs::path_ext(path_in) %in% c("wav", "wave")]
    if(length(r) > 5) r <- c(r[1:5], "...")
    r <- paste0(r, collapse = "\n") |>
      stats::setNames("*")

    rlang::abort(c("Non-wav file found in files.",
                   "x" = "Only wav files are processed by `clip_wave()`",
                   "i" = "Check file names are correct",
                   r), call = NULL)
  }
  path_in
}

check_wave_path_out <- function(subdirs, path_in, dir_out, create_dir) {

  dir_out <- fs::path(dir_out, purrr::pmap_chr(subdirs, fs::path))
  path_out <- fs::path(dir_out, fs::path_file(path_in))

  # Create dirs if they do not exist
  if(create_dir) {
    fs::dir_create(dir_out)
  } else if(!all(fs::dir_exists(dir_out))) {
    err <- dir_out[!fs::dir_exists(dir_out)]
    rlang::abort(
      c("Not all output directories exist",
        "!" = "Either create them before hand or set `create_dir = TRUE`",
        stats::setNames(err, rep("x", length(err)))),
      call = NULL)
  }
  path_out
}

check_wave_lengths <- function(path_in, clip_lengths, start_times, diff_limit) {

  wave_lengths <- purrr::map_dbl(path_in, get_wav_length, return_numeric = T)
  clip_lengths <- clip_lengths + start_times

  # Check that starts before end of wave file
  if(any(start_times > wave_lengths)) {
    err <- path_in[start_times >= wave_lengths]
    rlang::abort(
      c("Some wave files have a clip start time greater than the length of the wave",
        stats::setNames(err, rep("x", length(err)))),
      call = NULL)
  }

  # Check that requested clip length less than wave file length +/- wiggle room
  # (accounting for start time)
  if(any((clip_lengths - diff_limit) >= wave_lengths)){
    err <- path_in[(clip_lengths - diff_limit) >= wave_lengths]
    rlang::abort(
      c(glue::glue("Some wave files are >={diff_limit}s shorter than the requested clip length given the `start_time`."),
      "i"= "Check file lengths. You can adjust the discrepency limit with `diff_limit` (default 30s)",
      stats::setNames(err, rep("x", length(err)))
    ), call = NULL)
  }

  wave_lengths
}
