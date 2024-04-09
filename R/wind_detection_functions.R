

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
wind_detection_summarize_json <- function(f, json_string_regex = "/[\\w|\\d|_|-]+"){
  lifecycle::signal_stage("experimental", "wind_detection_summarize_json()")
  warn("Function in development. Use at own risk")
  if(!is_installed("jsonlite")) abort("sum_json requires {jsonlite} package")

  s <- purrr::safely(jsonlite::read_json)

  jsonfile <- s(f)
  if(is_null(jsonfile$result)){
    return(  dplyr::tibble(
      jsonF = stringr::str_remove(
        stringr::str_extract(f, "/[\\w|\\d|_|-]+.json"), "/")) )
  }
  else jsonfile <- jsonfile$result
  nm <- purrr::pluck(jsonfile, "FileName")
  dets <- purrr::pluck(jsonfile, "Time History") |>
    purrr::transpose() |> purrr::pluck("Te") |>
    purrr::list_c() |>  max()
  if(is_empty(jsonfile$`Wind free regions`)){
    return(dplyr::tibble(name = nm, totalwindless = 0,
                  length=dets,
                  pwindless   =0,
                  n=0, mean_windless=0,
                  jsonF = stringr::str_remove(
                    stringr::str_extract(f,
                                         glue::glue("{json_string_regex}.json")),
                    "/")) )
  }

  tmp <- purrr::pluck(jsonfile, "Wind free regions") |> purrr::transpose()
  nm <- purrr::pluck(jsonfile, "FileName")
  e <- tmp |>
    purrr::pluck("e") |>
    purrr::list_c()# |>
    # purrr::list_flatten()
  s <-  purrr::list_c(purrr::pluck(tmp, "s"))
  # browser()
  dplyr::tibble(s,e) |>
    dplyr::mutate(t=e-s) |>
    dplyr::summarize(totalwindless = sum(t),
              pwindless = totalwindless/dets,
              n=dplyr::n(),
              length = dets,
              mean_windless = mean(t),
              name = nm,
              jsonF = stringr::str_remove(
                stringr::str_extract(f,
                                     glue::glue("{json_string_regex}.json")),
                "/"))
}



#' Pre-processing of files for Wind Detection program
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function takes a vector of wave file names and returns a list
#' of three vectors that can be provided to the wind detection software or
#' written to files that the software can read. Details of the wind detection
#' software can be found at
#' [https://github.com/dhope/WindNoiseDetection](https://github.com/dhope/WindNoiseDetection)
#'
#'
#' @param wav_vector vector of wave files
#' @param site_pattern regular expression to extract site names
#' @param chunksize numeric size of chunks to split wav_vector into
#' @param file_drive Path to replace for use of cygwin
#' @param output_directory string - output directory to write files to
#' @param write_output logical - if should write output to files.
#'
#' @export
#'
#' @return List including filePath, filenames, and sites suitable for wind software.
#'
wind_detection_pre_processing <- function(wav_vector,site_pattern,
                                          chunksize,file_drive,
                                          output_directory=NULL, write_output=FALSE){
  lifecycle::signal_stage("experimental", "wind_detection_pre_processing()")
  warn(c("Function in development.",
         "i" = "Use cases very limited for now.",
         "*"= "Improvements expected in later versions"))
  if(!grepl("Windows", utils::osVersion))
    abort("Currently only implemented for  Windows due to drive naming and for use with cygwin")
  # Currently need to set drive manually
  # don't need this for non-windows
  # drive <- stringr::str(wav_vector, 1,1)
  chunks <- parallel::splitIndices(length(wav_vector), ncl = ceiling(length(wav_vector)/(chunksize)))

  output_vec <- vector(mode = 'list',length = length(chunks))

  for(i in 1:length(chunks)){

    tmp_wav <- wav_vector[chunks[[i]]]

    filePaths <-  tmp_wav |>
      stringr::str_replace(glue::glue("{file_drive}:/"),
                           glue::glue("/cygdrive/{file_drive}/")) |>
      stringr::str_remove("/[^/]+.wav") |>
      stringr::str_replace("\\(", r"(\\\()") |>
      stringr::str_replace("\\)", r"(\\\))") |>
      stringr::str_replace("\\s", r"(\\\ )") |>
      stringr::str_replace("\\+", "\\\\+") |>
      stringr::str_replace("\\[", "\\\\[") |>
      stringr::str_replace("\\]", "\\\\]")

    sites <- tmp_wav |>
      stringr::str_extract(site_pattern) |>
      stringr::str_replace("/", "-")

    filenames <- tmp_wav |>
      stringr::str_extract("/[^/]+.wav") |>
      stringr::str_remove(".wav$") |>
      stringr::str_remove("/") |>
      stringr::str_replace("\\(", r"(\\\()") |>
      stringr::str_replace("\\)", r"(\\\))") |>
      stringr::str_replace("\\s", r"(\\\ )") |>
      stringr::str_replace("\\+", "\\\\+") |>
      stringr::str_replace("\\[", "\\\\[") |>
      stringr::str_replace("\\]", "\\\\]")
    if(!is_null(output_directory)&is_true(write_output)){
    dir_ <- glue::glue("{output_directory}/{i}")

    dir.create(dir_, showWarnings = T, recursive = F)

    readr::write_lines(filePaths, here::here(glue::glue("{dir_}/pathlist.txt")))
    readr::write_lines(filenames, here::here(glue::glue("{dir_}/filelist.txt")))
    readr::write_lines(sites, here::here(glue::glue("{dir_}/sitelist.txt")))

    }
    output_vec[[i]] <- list(chunk = i,
                       filePaths = filePaths,
                       filenames = filenames,
                       sites = sites)

  }
  return(output_vec)

}


