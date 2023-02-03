prep_for_wind_detection <- function(wav_files,site_pattern, output_directory, write_to_file = FALSE){
  if(any(grepl("[\\(,\\),\\+,\\[,\\]", wav_files))) rlang::warn(
    c("Special characters detected in file paths",
      x = "Wind Detection is may not work") )


  filePaths <- wav_files |>
    stringr::str_replace( "J:/", "/cygdrive/j/") |>
    stringr::str_remove("/[^/]+.wav")

  sites <- wav_files |>
    stringr::str_extract(site_pattern)

  filenames <- wav_files |>
    stringr::str_extract("/[^/]+.wav") |>
    stringr::str_remove(".wav$") |>
    stringr::str_remove("/")


  if(isTRUE(write_to_file)) {

    readr::write_lines(filePaths, glue::glue("{output_directory}/pathlist.txt"))
    readr::write_lines(filenames,  glue::glue("{output_directory}/filelist.txt"))
    readr::write_lines(sites,  glue::glue("{output_directory}/sitelist.txt"))

  }

  list(filePaths=filePaths,
       filenames=filenames,
       sites=sites)


}
