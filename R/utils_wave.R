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
#' @param col_path_in Column. Unquoted column containing the current file paths.
#'   Default `path`.
#'   **Note: file paths must be either relative to `dir_in` or absolute**.
#' @param col_subdir_out Column. Unquoted column containing the
#'   subdirectories in which to put output files. Default `subdir_out`.
#' @param col_filename_out Column. Unquoted column containing the output
#'   filenames. Default `filename_out`.
#' @param col_clip_length Column. Unquoted column containing the length of the
#'   new clip. Default `length`.
#' @param col_start_time Column. Unquoted column containing the start time of
#'   the new clip. Default `start_time`
#' @param overwrite Logical. Overwrite pre-existing files when clipping and
#'   moving. (Default `FALSE`)
#' @param create_dir Logical. Whether to create directory structure for newly
#'   formatted and clipped wave files.
#' @param diff_limit Numeric. How much longer in seconds clip lengths can be
#'   compared to file lengths before triggering an error. Default `30`.
#' @param use_job Logical. Use the {job} package to copy files (Default `FALSE`)
#'
#' @inheritParams common_docs
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' w <- data.frame(
#'   path = temp_wavs(n = 4),
#'   subdir_out = c("test1/a", "test2/a", "test3/c", "test4/d"),
#'   filename_out = c("wave1_clean.wav", "wave2_clean.wav", "wave3_clean.wav", "wave4_clean.wav"),
#'   clip_length = c(1, 1, 1, 2),
#'   start_time = c(1.2, 0.5, 1, 0))
#'
#' clip_wave(w, dir_out = "clean")
#'
#' unlink("clean", recursive = TRUE) # Remove this new 'clean' directory
#' }

clip_wave <- function(waves,
                      dir_out,
                      dir_in = NULL,
                      col_path_in = path,
                      col_subdir_out = subdir_out,
                      col_filename_out = filename_out,
                      col_clip_length = clip_length,
                      col_start_time = start_time,
                      overwrite = FALSE,
                      create_dir = TRUE,
                      diff_limit = 30,
                      use_job = FALSE) {

  # Checks
  if(missing(dir_out)) rlang::abort(paste0("Require an output directory ",
                                           "(`dir_out`)"), call = NULL)

  check_cols(waves,
             cols = enquos(col_path_in, col_subdir_out, col_filename_out,
                           col_clip_length, col_start_time),
             name = "waves")

  # Prepare processing df
  #wv <- dplyr::tibble(.rows = nrow(waves))

  wv <- waves |>
    dplyr::select(
      "path_in" = {{ col_path_in }},
      "path_out" = {{ col_subdir_out }},
      "clip_length" =  {{ col_clip_length }},
      "start_time" = {{ col_start_time }}
      ) |>
    dplyr::mutate(
      # Check and complete input paths
      path_in = check_wave_path_in(
        .data[["path_in"]],
        .env$dir_in),
      # Get output paths
      path_out = check_wave_path_out(
        .data[["path_out"]],
        .data[["path_in"]],
        .env$dir_out,
        .env$create_dir),
      # Check wave lengths
      wave_length = check_wave_length(
        .data[["path_in"]],
        clip_length = .data[["clip_length"]],
        start_time = .data[["start_time"]],
        diff_limit = diff_limit),
      overwrite = .env$overwrite
    )

  if(!use_job) {
    purrr::pmap(wv, clip_wave_single)
  } else {
    job::job({purrr::pmap(wv, clip_wave_single)},
             import = c(wv),
             packages = "ARUtools")
  }

  TRUE
}



#' Get the length of a recording in seconds
#'
#' @param path Character. Path to wave file.
#' @param return_numeric Logical. Return numeric or character?
#'
#' @return Length of recording in seconds
#' @export
#'
#' @examples
#' f <- tempfile()
#' wav <- download.file("https://www2.cs.uic.edu/~i101/SoundFiles/StarWars3.wav", destfile = f)
#' get_wav_length(f)

get_wav_length <- function(path, return_numeric = FALSE) {
  audio <- tuneR::readWave(path, header = TRUE)
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
        "Some wave files could not be found:\n"),
        stats::setNames(names(missing[missing]), rep("*", sum(missing)))),
      call = NULL
    )
  }

  # Check file types
  if(any(!fs::path_ext(path_in) %in% c("wav", "wave"))) {
    r <- path_in[!fs::path_ext(path_in) %in% c("wav", "wave")]
    if(length(r) > 5) r <- c(r[1:5], "...")
    r <- set_names(r, "*")

    rlang::abort(c("Non-wav file found in files.",
                   "x" = "Only wav files are processed by `clip_wave()`",
                   "i" = "Check file names are correct",
                   r), call = NULL)
  }
  path_in
}

check_wave_path_out <- function(subdirs, path_in, dir_out, create_dir) {

  dir_out <- fs::path(dir_out, purrr::pmap_chr(list(subdirs), fs::path))
  path_out <- fs::path(dir_out, fs::path_file(path_in))


  if(create_dir) {
    # Create dirs if they do not exist
    fs::dir_create(dir_out)
  } else if(!all(fs::dir_exists(dir_out))) {
    # Alert if missing dirs
    err <- dir_out[!fs::dir_exists(dir_out)]
    rlang::abort(
      c("Not all output directories exist",
        "!" = "Either create them before hand or set `create_dir = TRUE`",
        stats::setNames(err, rep("x", length(err)))),
      call = NULL)
  }
  path_out
}

check_wave_length <- function(path_in, clip_length, start_time, diff_limit) {

  wave_length <- purrr::map_dbl(path_in, get_wav_length, return_numeric = T)
  clip_length <- clip_length + start_time

  # Check that starts before end of wave file
  if(any(start_time > wave_length)) {
    err <- path_in[start_time >= wave_length]
    rlang::abort(
      c("Some wave files have a clip start time greater than the length of the wave",
        stats::setNames(err, rep("x", length(err)))),
      call = NULL)
  }

  # Check that requested clip length less than wave file length +/- wiggle room
  # (accounting for start time)
  if(any((clip_length - diff_limit) >= wave_length)){
    err <- path_in[(clip_length - diff_limit) >= wave_length]
    rlang::abort(
      c(glue::glue("Some wave files are >={diff_limit}s shorter than the requested clip length given the `start_time`."),
      "i"= "Check file lengths. You can adjust the discrepency limit with `diff_limit` (default 30s)",
      stats::setNames(err, rep("x", length(err)))
    ), call = NULL)
  }

  wave_length
}

#' Create spectrogram image from wave file
#'
#' Using the external program `SoX` (the Swiss Army knife of sound processing
#' programs), create a spectrogram image file. Note that you must have `SoX`
#' installed to use this function. Spectrograms will be silently overwritten.
#'
#' Most arguments are passed through to the `sox` command.
#' - width and height correspond to the `-x` and `-y` options for the
#'   `spectrogram` effect.
#' - `start` and `end` are used by the `trim` effect
#' - `rate` is passed on to the `rate` effect
#'
#' Based on code from Sam Hache.
#'
#' @param prepend Character. Text to add to the start of the output file.
#'   Defaults to "spectro_".
#' @param width Numeric. Width of the spectrogram image in pixels.
#' @param height Numeric. Height of the spectrogram image in pixels.
#' @param start Numeric/Character. Start the spectrogram at this time (seconds
#'   or HH:MM:SS format).
#' @param end Numeric/Character. End time the spectrogram at this time (seconds
#'   or HH:MM:SS format).
#' @param rate Numeric. Audio sampling rate to display (used by the `rate`
#'   effect in `sox`). This effectively limits the upper frequency of the
#'   spectrogram to rate/2. The default ("20k"), limits the spectrogram to
#'   10kHz. Use `rate = NULL` for no limiting.
#' @param dry_run Logical. If `TRUE` show the sox command, but do not run (for
#'   debugging and understanding precise details).
#'
#' @inheritParams common_docs
#'
#' @return Does not return anything, but creates a spectrogram image in
#'   `dir_out`.
#' @export
#'
#' @examples
#' # Prep sample file
#' w <- tuneR::sine(440, duration = 300000)
#' tuneR::writeWave(w, "test_wave.wav")
#'
#' # Create spectrograms
#' sox_spectro("test_wave.wav")
#' sox_spectro("test_wave.wav", rate = NULL)
#' sox_spectro("test_wave.wav", start = 2, end = 3)
#' sox_spectro("test_wave.wav", start = "0:01", end = "0:04")
#' sox_spectro("test_wave.wav", prepend = "")
#'
#' # Clean up
#' unlink("test_wave.wav")
#' unlink("Spectrograms", recursive = TRUE)

sox_spectro <- function(path, dir_out = "Spectrograms",
                        prepend = "spectro_",
                        width = NULL, height = NULL,
                        start = NULL, end = NULL,
                        rate = "20k", dry_run = FALSE,
                        quiet = FALSE) {

  if(!fs::file_exists(path)) {
    rlang::abort(paste0("Cannot find wave file ", path), call = NULL)
  }

  # Create output path
  path_out <- path |>
    fs::path_file() |>
    fs::path_ext_set("png")
  path_out <- fs::path(dir_out, glue::glue("{prepend}{path_out}"))
  fs::dir_create(fs::path_dir(path_out))

  # Trim if required
  if(!is.null(start) || !is.null(end)) {
    if(is.null(start)) start <- 0
    if(is.null(end)) end <- get_wav_length(path, TRUE)
    trim <- glue::glue("trim {start} ={end}")
  } else trim <- ""

  # Get rate if required
  if(!is.null(rate)) rate <- glue::glue("rate {rate}") else rate <- ""

  # Create sox command
  cmd <- glue::glue("{path} -n {trim} {rate} spectrogram -o {path_out}")
  if(!is.null(width)) cmd <- glue::glue("{cmd} -x {width}")
  if(!is.null(height)) cmd <- glue::glue("{cmd} -y {height}")

  if(dry_run) {
    rlang::inform(cmd)
  } else {
    if(!quiet) rlang::inform(glue::glue("Writing spectrogram to {path_out}"))
    seewave::sox(cmd)
  }
}


#' Get acoustic complexity values
#'
#' Wrapper for soundecology package to calculate acoustic complexity, the
#' bioacoustic index, and acoustic diversity. See Value for details about
#' these indices.
#'
#' @param min_freq Numeric. Minimum frequency for acoustic complexity (see
#'   [soundecology::acoustic_complexity()])
#' @param max_freq Numeric. Maximum frequency for acoustic complexity (see
#'   [soundecology::acoustic_complexity()])
#' @param units Character. Wave file units for reading the file. Defaults to
#'   "samples" (see [tuneR::readWave()]).
#'
#' @inheritParams common_docs
#'
#' @return
#' Returns a data frame with acoustic indices. Those prefaced with
#'
#' - `complx_` are from [soundecology::acoustic_complexity()]
#' - `bio_` are from [soundecology::bioacoustic_index()]
#' - `div_` are from [soundecology::acoustic_diversity()]
#'
#' @export
#'
#' @examples
#' w <- tuneR::sine(440, duration = 300000) # > 5s
#' tuneR::writeWave(w, "test_wave.wav")
#' acoustic_indices("test_wave.wav")
#' acoustic_indices("test_wave.wav", quiet = TRUE)
#' unlink("test_wave.wav")

acoustic_indices <- function(path, min_freq = NA, max_freq = NA, units = "samples",
                             quiet = FALSE) {
  if(!requireNamespace("soundecology", quietly = TRUE) ||
     !requireNamespace("tuneR", quietly = TRUE)) {
    rlang::abort(
      c("Packages \"soundecology\" and \"tuneR\" must be installed to use `acoustic_indices()`",
        "Install with `install.packages(c(\"soundecology\", \"tuneR\"))`"),
      call = NULL
    )
  }

  file_name <- fs::path_file(path)
  if(!quiet) message(glue::glue("Calculating acoustic indices for {file_name}\n"))

  wave <- tuneR::readWave(path, units = units)

  complexity <- try(
    soundecology::acoustic_complexity(wave, min_freq = min_freq, max_freq = max_freq),
    silent = TRUE) |>
    suppressCat(quiet)

  if(inherits(complexity, "try-error")) {
    if(get_wav_length(path, TRUE) < 5) {
      rlang::abort(
        c("Error in `acoustic_complexity()` from the soundecology package",
          "i" = "Consider using a wave file >=5s long."),
        call = NULL)
    } else {
      rlang::abort(
        c("Error in `acoustic_complexity()` from the soundecology package:",
          complexity),
        call = NULL
      )
    }
  }

  complexity <- dplyr::as_tibble(complexity[1:4]) |>
    dplyr::rename_with(\(x) paste0("complx_", x))
  bioindex <- soundecology::bioacoustic_index(wave) |>
    suppressCat(quiet) |>
    dplyr::as_tibble() |>
    dplyr::rename_with(\(x) paste0("bio_", x))
  diversity <- soundecology::acoustic_diversity(wave) |>
    suppressCat(quiet) |>
    dplyr::as_tibble() |>
    dplyr::rename_with(\(x) paste0("div_", x))

  tibble::tibble(file = file_name) |>
    dplyr::bind_cols(complexity) |>
    dplyr::bind_cols(bioindex) |>
    dplyr::bind_cols(diversity)
}


