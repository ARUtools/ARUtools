#' Prepare files for WindNoiseDetection
#'
#' This function prepares a set of files that can be used with
#' the fork of WindNoiseDetection found at
#' https://github.com/dhope/WindNoiseDetection
#'
#' @param wav_files Vector of path to wav files
#' @param site_pattern Pattern to extract sites from file names
#' @param output_directory Directory path to export files to
#' @param write_to_file Logical Should the function write files to output_directory
#' @param chunk_size Numeric If not NULL, sets number of files to include in each chunk
#'
#' @return List of vectors for sites, filenames, and filePaths
#' @export
#'
prep_for_wind_detection <- function(wav_files, site_pattern, output_directory, write_to_file = FALSE,
                                    chunk_size = NULL) {
  if (any(grepl("[\\(,\\),\\+,\\[,\\]", wav_files))) {
    warn(
      c("Special characters detected in file paths",
        x = "Wind Detection is may not work"
      )
    )
  }
  gen_output <- function(file_list) {
    filePaths <- file_list |>
      # stringr::str_replace( "J:/", "/cygdrive/j/") |>
      stringr::str_remove("/[^/]+.wav")

    sites <- file_list |>
      stringr::str_extract(site_pattern)

    filenames <- file_list |>
      stringr::str_extract("/[^/]+.wav") |>
      stringr::str_remove(".wav$") |>
      stringr::str_remove("/")

    list(
      filePaths = filePaths,
      filenames = filenames,
      sites = sites
    )
  }

  if (is.null(chunk_size)) {
    output <- gen_output(wav_files)
    if (isTRUE(write_to_file)) {
      readr::write_lines(output$filePaths, glue::glue("{output_directory}/pathlist.txt"))
      readr::write_lines(output$filenames, glue::glue("{output_directory}/filelist.txt"))
      readr::write_lines(output$sites, glue::glue("{output_directory}/sitelist.txt"))
    }
  } else {
    chunks <- parallel::splitIndices(length(wav_files), ncl = ceiling(length(wav_files) / (chunk_size)))
    output <- purrr::map(chunks, ~ gen_output(wav_files[.x]))

    if (isTRUE(write_to_file)) {
      dir_ <- purrr::walk(
        1:length(chunks),
        ~ {
          dir.create(glue::glue("{output_directory}/{.x}"))
          readr::write_lines(output[[.x]]$filePaths, here::here(glue::glue("{output_directory}/{.x}/pathlist.txt")))
          readr::write_lines(output[[.x]]$filenames, here::here(glue::glue("{output_directory}/{.x}/filelist.txt")))
          readr::write_lines(output[[.x]]$sites, here::here(glue::glue("{output_directory}/{.x}/sitelist.txt")))
        }
      )
    }
  }
  return(output)
}
