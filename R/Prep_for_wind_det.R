prep_for_wind_detection <- function(wav_files,site_pattern, output_directory, write_to_file = FALSE,
                                    chunk_size = NULL){
  if(any(grepl("[\\(,\\),\\+,\\[,\\]", wav_files))) rlang::warn(
    c("Special characters detected in file paths",
      x = "Wind Detection is may not work") )
  gen_output <- function(file_list){
    filePaths <- wav_files #|>
    # stringr::str_replace( "J:/", "/cygdrive/j/") |>
    # stringr::str_remove("/[^/]+.wav")

    sites <- wav_files |>
      stringr::str_extract(site_pattern)

    filenames <- wav_files |>
      stringr::str_extract("/[^/]+.wav") |>
      stringr::str_remove(".wav$") |>
      stringr::str_remove("/")

    list(filePaths=filePaths,
         filenames=filenames,
         sites=sites)

  }

  if(is.null(chunk_size)){
    output <- gen_output(wav_files)
    if(isTRUE(write_to_file)) {

      readr::write_lines(output$filePaths, glue::glue("{output_directory}/pathlist.txt"))
      readr::write_lines(output$filenames,  glue::glue("{output_directory}/filelist.txt"))
      readr::write_lines(output$sites,  glue::glue("{output_directory}/sitelist.txt"))

    }

  }else{
    chunks <- parallel::splitIndices(length(wav_files), ncl = ceiling(length(wav_files)/(chunk_size)))
    output <- purrr::map(chunks, ~gen_output(wav_files[.x]) )

    if(isTRUE(write_to_file)) {
      dir_ <- purrr::walk(1:length(chunks),
              ~{
                dir.create(glue::glue("{output_directory}/{.x}"))
                readr::write_lines(output[[.x]]$filePaths, here::here(glue::glue("{output_directory}/{.x}/pathlist.txt")))
      readr::write_lines(output[[.x]]$filenames, here::here(glue::glue("{output_directory}/{.x}/filelist.txt")))
      readr::write_lines(output[[.x]]$sites, here::here(glue::glue("{output_directory}/{.x}/sitelist.txt")))

              })
    }

  }
  return(output)



  }


