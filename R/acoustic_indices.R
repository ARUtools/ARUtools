#' Get acoustic complexity values
#'
#' Wrapper for soundecology package to calculate
#'
#' @param min_freq Numeric. Minimum frequency for acoustic complexity (see
#'   [soundecology::acoustic_complexity()])
#' @param max_freq Numeric. Maximum frequency for acoustic complexity (see
#'   [soundecology::acoustic_complexity()])
#' @param units Character. Wave file units for reading the file. Defaults to
#'   "samples" (see [tuneR:readWave()]).
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
