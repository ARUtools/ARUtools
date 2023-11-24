#' Clip single wave file to length from given start time in recording
#'
#' @param in_file
#' @param out_file
#' @param length_clip
#' @param StartTime
#' @param warn
#' @param l
#'
#' @return
#' @export
#'
#' @examples
format_clip_wave_single <- function(in_file, out_file, length_clip, StartTime, warn=T, l=NULL){
  if(length(in_file)>1){rlang::abort(c("seg is of length greater than 1",
                               "x" = "Currently can only process one file at a time",
                               "i" = "Use purrr::map or lapply to iterate along rows for multiple files.
                               Future developments will fix this."))}

  if(isTRUE(warn) & isTRUE(interactive())) {
    yn <- menu(c("Yes", "No"),
               title = glue::glue("Current function will copy {in_file} to {out_file}.
               This will overwrite any existing file there.\n
                                Do you want to continue?"))
    if(yn!=1) return("Function cancelled")
  }
  if(is.null(l))  l <- get_wav_length(in_file, return_numeric = T)

  if(length_clip>= l){

    file.copy(from = in_file,
              to =  out_file)
  } else{
    in_wav <-
      tuneR::readWave(
        in_file, from = StartTime,
        to = StartTime + length_clip,
        units = 'seconds')
    tuneR::writeWave(in_wav,
                     out_file)
    return(TRUE)
  }
}


#' Process multiple wav files by copying them with a new filename or clipping
#' to a given length.
#'
#' @param segment_df   Data frame with details of file locations.
#' @param in_base_directory String. Directory where wav files are read from.
#' @param out_base_directory String. Directory to output files to
#' @param length_clip_col String with column name for the length of clip in seconds
#' @param sub_dir_out_col String or vector of strings with column name for directories to output to, nested in out_base_directory
#' @param filepath_in_col String with column name for path to file, either nested in base directory or absolute
#' @param out_filename_col String with column name for output filename
#' @param filewarn Logical. Default to TRUE. Should function provide warnings about file movements
#' @param use_job Logical. Default to FALSE. Use the {job} package to copy files.
#'
#' @return logical or logical vector of status of file copy.
#' @export
#'
format_clip_wave <- function(segment_df,in_base_directory,
                     out_base_directory,
                     length_clip_col, sub_dir_out_col, filepath_in_col,
                     out_filename_col,
                     filewarn=T, use_job=F, ...){
  list2env(list(...), envir = environment())
  if(!exists("diff_limit")) diff_limit <- 30
  if(length(sub_dir_out_col)>1){
    output_subfolders <- segment_df[,sub_dir_out_col] |>
      dplyr::rowwise() |>
      dplyr::mutate(output = glue::glue_collapse(c_across(cols = dplyr::everything()), sep = "/")) |>
      dplyr::ungroup() |>
      dplyr::pull(output)
  } else{output_subfolders <- segment_df[[sub_dir_out_col]]}
  if(any(!grepl(".wav$", segment_df[[filepath_in_col]] )) ){
    rlang::abort(c("Non-wav file found in files.",
                   "x"="Only wav files are processed by format_clip_wave",
                   "i" = "Check file names are correct.") )
  }
  outfiles <- glue::glue("{out_base_directory}/{output_subfolders}/{segment_df[[out_filename_col]]}.wav")
  if(all(grepl(in_base_directory, segment_df[[filepath_in_col]]))){

    ll <- purrr::map_dbl(segment_df[[filepath_in_col]], get_wav_length, return_numeric = T)
    infiles <- segment_df[[filepath_in_col]]
  } else{
    ll <- purrr::map_dbl(segment_df[[filepath_in_col]], ~get_wav_length(
      file =  glue::glue("{in_base_directory}/{.x}"),
      return_numeric = T) )
    infiles <- glue::glue("{in_base_directory}/{segment_df[[filepath_in_col]]}")
  }
  if(length(infiles)!=length(outfiles)) browser()
  if(nrow(segment_df)==1) {
    x <- format_clip_wave_single(in_file = infiles,
                                 out_file= outfiles,
                                 length_clip = segment_df[[length_clip_col]],
                                 StartTime = segment_df[["StartTime"]],
                                 warn=filewarn, l=ll)
    return(x)
  }

  if(any((segment_df[[length_clip_col]]-diff_limit)>= ll) ){
    err <- which((segment_df[[length_clip_col]]-diff_limit)>= ll)
    rlang::abort(c(glue::glue("One of the files is {diff_limit} seconds or more shorter than requested"),
                 "i"= "Check file lengths. You can adjust the limit with diff_limit = 30 (default) ",
                 "x"=glue::glue("File: {segment_df[err,][[filepath_in_col]]}, Segment requests: {segment_df[[length_clip_col]][err]}s,File lengths: {ll[err]}s")
                                ))

  }


  if(all((segment_df[[length_clip_col]]-2)>= ll) ) {
    xx <- file.copy(from = infiles,#glue::glue("{in_base_directory}/{segment_df[[out_filename_col]]}"),
              to =  outfiles)
    return(xx)
  }
  if(!isTRUE(use_job)){

    xx <- purrr::map(1:nrow(segment_df),
                     ~format_clip_wave_single(
                       in_file = infiles[[.x]],
                       out_file= outfiles[[.x]],
                       length_clip = segment_df[[length_clip_col]][[.x]],
                       StartTime = segment_df[["StartTime"]][[.x]],
                       warn=filewarn, l=ll[[.x]]
                     ) )
    return(xx)
  }

  if(isTRUE(use_job)){
    job::job({
      xx <- purrr::map(1:nrow(segment_df),
                       ~format_clip_wave_single(
                         in_file = infiles[[.x]],
                         out_file= outfiles[[.x]],
                         length_clip = segment_df[[length_clip_col]][[.x]],
                         StartTime = segment_df[["StartTime"]][[.x]],
                         warn=filewarn, l=ll[[.x]]
                       ) )
      },
      import =
      c(infiles,
                 outfiles,
                 segment_df,
                 length_clip_col,
                 filewarn,
                 ll),
      packages = c("ARUtools"))
  }





}



#' Get the length of a recording in seconds
#'
#' @param wave_file Character. Path to wave file
#' @param return_numeric Logical. Return numeric or character?
#'
#' @return Length of recording in seconds
#' @export
#'
#' @examples
#' f <- tempfile()
#' wav <- download.file("https://www2.cs.uic.edu/~i101/SoundFiles/StarWars3.wav", destfile = f)
#' get_wav_length(f)

get_wav_length <- function(wave_file, return_numeric = FALSE){
  audio <- tuneR::readWave(file, header = TRUE)
  l <- round(audio$samples / audio$sample.rate, 2)
  if(!return_numeric) l <- glue::glue("{l} seconds")
  l
}
