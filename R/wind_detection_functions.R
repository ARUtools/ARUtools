

#' Summarize wind detection results
#'
#' @param f filepath for json
#' @param json_string_regex string. Regex to extract from file path
#'
#' @export
#'
#' @return
wind_detection_summarize_json <- function(f, json_string_regex = "/[\\w|\\d|_|-]+"){
  warn("Function in development. Use at own risk")
  if(!rlang::is_installed("jsonlite")) rlang::abort("sum_json requires {jsonlite} package")

  s <- purrr::safely(jsonlite::read_json)

  jsonfile <- s(f)
  if(is_null(jsonfile$result)){
    return(  tibble::tibble(
      jsonF = stringr::str_remove(
        stringr::str_extract(f, "/[\\w|\\d|_|-]+.json"), "/")) )
  }
  else jsonfile <- jsonfile$result
  nm <- purrr::pluck(jsonfile, "FileName")
  dets <- purrr::pluck(jsonfile, "Time History") |>
    purrr::transpose() |> purrr::pluck("Te") |>
    purrr::list_c() |>  max()
  if(is_empty(jsonfile$`Wind free regions`)){
    return(tibble::tibble(name = nm, totalwindless = 0,
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
  tibble::tibble(s,e) |>
    dplyr::mutate(t=e-s) |>
    dplyr::summarize(totalwindless = sum(t),
              pwindless = totalwindless/dets,
              n=n(),
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
#' @param wav_vector
#' @param site_pattern
#' @param chunksize
#' @param file_drive
#' @param output_directory
#' @param write_output
#'
#' @return
#'
wind_detection_pre_processing <- function(wav_vector,site_pattern,
                                          chunksize,file_drive,
                                          output_directory=NULL, write_output=FALSE){
  abort("Function in development. Check back later.")
  if(!grepl("Windows", osVersion))
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


