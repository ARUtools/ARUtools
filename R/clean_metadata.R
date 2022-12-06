#' Clean aru metadata
#'
#' @param type BarLT or SM4
#' @param folder_base Folder base directory
#' @param included_subfolders Defaults to 'all', which means all subfolders included.
#'        To include only some folders (at first level only),
#'        provide a vector with folder names to include.
#' @param gps_locations GPS locations in lat/lon format with SiteID
#'                       Must also include timezone (tz) and deploy date.
#' @param file_split_pattern Regular expression to split sound file name into pieces.
#'                      May need to adjust if non-standard naming convention is used.
#' @param site_pattern  Regular expression to extract site name from folder path.
#' @param ... Extra parameters for devleopment. Currently works with:
#'                 - file_ext: default is '.wav'
#'                 - site_in_filename: Logical: default is FALSE
#'                 - deploy_start_date:  default is 2 days before first recording (dmy format)
#'                 - check_dists: Logical to check distances from GPS within site. Default is TRUE
#'                 - dist_cutoff: Cutoff to raise error in distance check. Default is 100 (in meters).
#'
#' @return
#' @export
clean_metadata <- function(type,
                           folder_base,
                           included_subfolders = 'all',
                           gps_locations = NULL,
                           file_split_pattern = "T|\\-|\\_|\\.",
                           site_pattern =  "[P|Q]\\d+_\\d",
                           list_files = NULL,
                           folder_names =NULL,
                           WaveFileName_Strings = NULL,
                           ... ){
  BarLT <- (grepl("bar", type, ignore.case = T) & grepl("lt", type, ignore.case = T))
  SM <- (grepl("sm", type, ignore.case = T) & grepl("3|4", type, ignore.case = T))
  Mixed <- (grepl("mixed", type, ignore.case = T))
  if(!any(BarLT ,SM, Mixed) ){
    abort("Currently only BarLT and SM4 ARUs are supported. Will add more as needed.")
  }
  list2env(list(...), envir = environment())
  if(!exists("file_ext")) file_ext <- ".wav"
  if(!exists("return_full_metadata")) return_full_metadata <- FALSE
  # browser()
  # If file list not provided, scan it from base folder.
  if(is_null(list_files)){
  list_files <- list.files(folder_base, recursive = T, full.names = F, include.dirs = F)
  }
  # Include only subfolders provided if not set to all
  if(!any(grepl("all", included_subfolders)) | length(included_subfolders)>1) {
     list_files <- purrr::map(glue::glue("{included_subfolders}/"),
                                        ~list_files[grepl(.x, list_files)]) |>
       unlist()}
  # Check file vector is not empty
  if(length(list_files)==0){
       abort(c("You have listed subfolders to include, but no files are returned",
               "x" = "Folders must be case-sensitive and be only at one level below folder_base",
               "i" = "Check folder structure using list.dirs(folder_base, full.names = F, recursive = F)")
       )
  }
  list_waves <- list_files[grep(file_ext, list_files)]
  if(length(list_waves)==0){
    abort(c("Did not find any wav files. Default is set to '.wav'",
            "x" = "Sound file locations needed to process.",
            "i" = "Use file_ext to change file extension for sound files and check folder_base is correct."))
    }
  # if(!exists("folder_sep")) {
  #   if(sum(grepl(list_waves, pattern = "Data"))>0) {folder_sep <- "Data"
  #   } else if(sum(grepl(list_waves, pattern = "Wav"))>0) {folder_sep <- "Wav"
  #   } else abort("Tried 'Wav' and 'Data' as folder separators. Please use your own using folder_sep argument")
  # }
  # |- BarLT   ----------------

  ## Create file name log (moving this from Prep_dates.R)
  ll <- stringr::str_split(list_waves, "/") |>
       purrr::map_int(.f=length) |> unique()
  if(length(ll)>1)  abort(
    c("More than one folder structure detected",
      "x" = "Differing folder structures are likely to lead to errors",
      "i" = "Run clean_metadata on each folder type separately"))

  if(is_null(folder_names)) {pathnames <- c(glue::glue("Folder{1:(ll-1)}"), "WaveFilename")
  } else if(length(folder_names) == ll){
    if(folder_names[length(folder_names)]!= "WaveFilename")folder_names[length(folder_names)] <-  "WaveFilename"
    pathnames <-  folder_names
  } else{abort(
    c("You have provided a folder_names vector that is not the same length as the number of folders",
      "x" = glue::glue("folder_names is {length(folder_names)}, while number of folders is {ll}"),
      "i" = "Run clean_metadata on each folder type separately") ) }
  if(!exists("site_in_filename")) site_in_filename <- FALSE
  wav_names_log <- tibble::tibble(filename=list_waves) %>%
    {if(ll==1){
      dplyr::mutate(., WaveFilename=filename)
    } else{

      tidyr::separate(., remove=F, col = filename, sep = "/",
                      into = pathnames, extra = 'merge') } }
  if(is_null(WaveFileName_Strings)){
  WaveFileName_Strings <- if_else(site_in_filename,
                                  glue::glue_collapse(c("ARUName",
                                                        "yyyymmdd",
                                                        "hhmmss", "utm",
                                                        "SR_SS",
                                                        "wav"), sep = ";"),
                                  glue::glue_collapse(c("yyyymmdd",
                                                        "hhmmss", "utm",
                                                        "SR_SS",
                                                        "wav"), sep = ";") )
  }

  # Check filenames will separate correctly
  WaveFileNames_vect <- stringr::str_split(WaveFileName_Strings, ";")[[1]]
  fileName_sep_list <- stringr::str_split(wav_names_log$WaveFilename,
                                     pattern =  file_split_pattern)
  ll_names <-  fileName_sep_list |>
    purrr::map_int(.f=length) |> unique()
  if(length(ll_names)>1)  abort(
    c("More than one filename structure detected",
      "x" = "Differing filename structures are likely to lead to errors",
      "i" = "Run clean_metadata on each filename type separately"))


  if(length(WaveFileNames_vect)!=ll_names){
    abort(
      c("File name structure does not match naming structure.
        This will likely lead to errors in time and date processing",
        "x" =
        glue::glue("WaveFileName_Strings is {length(WaveFileNames_vect)}, while number of file lengths is {ll_names}"),
        "i" = "Manually specify WaveFileName_Strings as a string separated by ';'"))
  }
  fileName_sep <-
    tibble::as_tibble(x = do.call(rbind, fileName_sep_list), .name_repair = 'minimal')
  names(fileName_sep) <- WaveFileNames_vect


  wav_names_log <- wav_names_log %>%
    bind_cols(fileName_sep)  |>
    dplyr::mutate(
      SiteID = stringr::str_extract(filename, site_pattern)) %>%
    {
      if(Mixed) {
        mutate(.,
               ARU_type = case_when(
                 grepl("barlt", filename,ignore.case = T )~"BarLT",
                 grepl("SM\\d", filename,ignore.case = T )~"SongMeter",
                 grepl("S\\dA", filename,ignore.case = T )~"SongMeter",
                 TRUE~NA_character_
               ) )
      } else if(BarLT){
        mutate(.,
               ARU_type ="BarLT"
               )
      } else if(SM){
        mutate(.,
               ARU_type ="SongMeter"
        )
      } else{abort("Error assessing Songmeter type")}

    }

  if(sum(is.na(wav_names_log$ARU_type))>0){abort("Error assessing ARU type, some rows have NAs")}
  # browser()
    BarLTfiles <- dplyr::filter(wav_names_log,ARU_type=="BarLT" )

    if(nrow(BarLTfiles)>0){

    log_files <- list_files[grep("logfile", list_files)]
    log_data <- purrr::map(glue::glue("{folder_base}/{log_files}"), read_log_barlt) |>
      purrr::transpose()

    if(!exists("site_in_filename")) site_in_filename <- FALSE

    if(!exists("deploy_start_date")) deploy_start_date <-
      list_waves |>
      stringr::str_extract("\\d{8}") |>
      lubridate::ymd() %>% { min(.) -2 } |>
      format("%d/%m/%Y") |> lubridate::dmy()
    if(!exists("check_dists")) check_dists <- TRUE


    if(is_null(gps_locations)){
      if(!exists("dist_cutoff")) dist_cutoff <- 100
      gps_locations_barLT <- process_gps_barlt(base_folder = folder_base,
                                         site_pattern = site_pattern,
                      file_list= list_files,dist_cutoff=dist_cutoff,
                      deploy_start_date = deploy_start_date,
                      check_dists)
    } else{

      gps_locations_barLT <- gps_locations
    }


  }
  # |- SM4   ----------------
    SMfiles <- dplyr::filter(wav_names_log,ARU_type=="SongMeter" )

    if(nrow(SMfiles)>0){
      # browser()
    if(grepl("3", type)) warn("Default site_pattern is set for SM4. Recommend changing based on file structure")
    if(!exists("site_pattern")) site_pattern <-  "S4A\\d{5}"
    # file.location <- "//WRIV02DTSTDNT1/RecordStor20172019/BetweenRivers_2019"

      # list.files(folder_base, "_Summary.txt", recursive = T, full.names = T)



    if(is_null(gps_locations)){
      gps_locations_SM <- process_gps_SM(folder_base = folder_base, list_files = list_files,
                                      site_pattern = site_pattern)
    } else{ gps_locations_SM <- gps_locations}
    if(!exists("site_in_filename")) site_in_filename <- TRUE
    }

    if(exists("gps_locations_barLT")& exists("gps_locations_SM")){
      gps_locations <- bind_rows(gps_locations_barLT, gps_locations_SM)
    } else if(exists("gps_locations_barLT")& !exists("gps_locations_SM")){
      gps_locations <- gps_locations_barLT
    } else if(!exists("gps_locations_barLT")& exists("gps_locations_SM")){
      gps_locations <- gps_locations_SM
    } else{abort("Error combining gps locations. Check ARU type")}
    # if(length(unique(gps_locations$tz ))>1) browser()
    wav_names_log <- parse_datetimes(wav_names_log,
                                     tz_loc = unique(gps_locations$tz ))


    if(exists("Filter_gps_sites")){
      if(isTRUE(Filter_gps_sites)){
        gps_locations <- gps_locations |>
          dplyr::filter(SiteID %in% wav_names_log$SiteID)
      }
    }

    recording_log <- prep_sunrise_sunset(gps_locations = gps_locations,
                                            wav_names_log = wav_names_log)


    # browser()

    if(isTRUE(return_full_metadata)){
      list(gps_locations = gps_locations,
           recording_log = recording_log,
           )

      } else{
        return(recording_log)
      }


}
