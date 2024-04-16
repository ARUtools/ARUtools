#' Play random track
#'
#' Will play a random track from a wave file in a given folder.
#'
#' @param base_folder Base folder path from which to search from.
#' @param file_list Vector of strings with file locations. If it is null, will search for them manually. Should be relative to base_folder
#' @param random_seed numeric
#'
#' @return Will not return anything. It does open your media player
#' @export
#'
#' @examplesIf file.exists("path_to_wave_file.wav")
#' play_random_track("path_to_wave_file.wav")
play_random_track <- function(base_folder, file_list = NULL, random_seed = NULL) {
  if (!interactive()) abort("This program does not work outside of an interactive seesion")
  check_installed("tuneR","Package \"tuneR\" must be installed to use this function.")

  if (is_null(random_seed)) random_seed <- Sys.time()
  if (is.null(file_list)) list_waves <- list.files(base_folder, pattern = ".wav", recursive = T, full.names = F)
  if (length(file_list) == 0) abort("No wav files found. Check path")
  withr::with_seed(random_seed, {
    wav_ <- sample(file_list, 1)
  })
  t <- Sys.time()
  v <- job::job(
    {
      tuneR::play(glue::glue("{base_folder}/{wav_}"))
    },
    import = c(wav_, base_folder),
    packages = c("tuneR")
  )
  Sys.sleep(32)
  message(glue::glue("Playing {wav_}. Enjoy!"))
  if (Sys.time() - t > 30) rstudioapi::jobRemove(v)
}
