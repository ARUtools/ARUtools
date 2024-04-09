#' Summarize wind detection results
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function takes output from the command line program and summarizes it.
#' Details of the wind detection software can be found at
#' [https://github.com/dhope/WindNoiseDetection](https://github.com/dhope/WindNoiseDetection).
#'
#'
#' @param f filepath for json
#' @param json_string_regex string. Regex to extract from file path
#'
#' @export
#'
#' @return tibble of summarized data from json file
wind_detection_summarize_json <- function(f, json_string_regex = "/[\\w|\\d|_|-]+") {
  lifecycle::signal_stage("experimental", "wind_detection_summarize_json()")
  warn("Function in development. Use at own risk")
  if (!is_installed("jsonlite")) abort("sum_json requires {jsonlite} package")

  s <- purrr::safely(jsonlite::read_json)

  jsonfile <- s(f)
  if (is_null(jsonfile$result)) {
    return(dplyr::tibble(
      jsonF = stringr::str_remove(
        stringr::str_extract(f, "/[\\w|\\d|_|-]+.json"), "/"
      )
    ))
  } else {
    jsonfile <- jsonfile$result
  }
  nm <- purrr::pluck(jsonfile, "FileName")
  dets <- purrr::pluck(jsonfile, "Time History") |>
    purrr::transpose() |>
    purrr::pluck("Te") |>
    purrr::list_c() |>
    max()
  if (is_empty(jsonfile$`Wind free regions`)) {
    return(dplyr::tibble(
      name = nm, totalwindless = 0,
      length = dets,
      pwindless = 0,
      n = 0, mean_windless = 0,
      jsonF = stringr::str_remove(
        stringr::str_extract(
          f,
          glue::glue("{json_string_regex}.json")
        ),
        "/"
      )
    ))
  }

  tmp <- purrr::pluck(jsonfile, "Wind free regions") |> purrr::transpose()
  nm <- purrr::pluck(jsonfile, "FileName")
  e <- tmp |>
    purrr::pluck("e") |>
    purrr::list_c() # |>
  # purrr::list_flatten()
  s <- purrr::list_c(purrr::pluck(tmp, "s"))
  # browser()
  dplyr::tibble(s, e) |>
    dplyr::mutate(t = e - s) |>
    dplyr::summarize(
      totalwindless = sum(t),
      pwindless = totalwindless / dets,
      n = dplyr::n(),
      length = dets,
      mean_windless = mean(t),
      name = nm,
      jsonF = stringr::str_remove(
        stringr::str_extract(
          f,
          glue::glue("{json_string_regex}.json")
        ),
        "/"
      )
    )
}



#' Pre-processing of files for Wind Detection program
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function takes a vector of wave file names and returns a list
#'  of three vectors that can be provided to the wind detection software or
#'  written to files that the software can read. Details of the usable fork of the
#'  wind detection software can be found at
#' [https://github.com/dhope/WindNoiseDetection](https://github.com/dhope/WindNoiseDetection)
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
#' @return List including filePath, filenames, and sites suitable for wind software.
#'
wind_detection_pre_processing <- function(wav_files, site_pattern, output_directory, write_to_file = FALSE,
                                             chunk_size = NULL) {
  lifecycle::signal_stage("experimental", "wind_detection_pre_processing()")
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

