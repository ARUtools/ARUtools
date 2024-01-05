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
