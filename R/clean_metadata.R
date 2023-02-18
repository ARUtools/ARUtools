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
#'
#' @examples
#'
#' clean_metadata(subset = "P71")
#' clean_metadata()
#'
#' @export
clean_metadata <- function(
    project_dir = NULL,
    project_files = NULL,
    file_type = "wav",
    subset = NULL,
    subset_type = "keep",
    site_from_date = NULL,
    pattern_site = "[P|Q]\\d+(_|-)\\d",
    pattern_aru_id = create_pattern_aru_id(),
    pattern_date = create_pattern_date(),
    pattern_time = create_pattern_time(),
    pattern_dt_sep = create_pattern_dt_sep(),
    pattern_model = NULL,
    order_date = "ymd",
    quiet = FALSE) {

  # CHECKS

  pattern_date_time <- paste0(pattern_date, pattern_dt_sep, pattern_time)
  file_type <- stringr::regex(paste0(file_type, "$"), ignore_case = TRUE)

  if(!is.null(project_dir)) {
    if(!quiet) rlang::inform("Fetching file list...")
    project_files <- list_files(project_dir, subset, subset_type,
                                type = "file")
  } else if(!is.null(subset)){
    project_files <- stringr::str_subset(project_files, subset,
                                         negate = subset_type == "omit")
  }

  # Check for files (either zero or all directories)
  if(length(project_files) == 0 || all(fs::is_dir(project_files))) {
    if(is.null(subset)) {
      msg <- "`project_dir`"
    } else {
      msg <- "`project_dir`/`subset`/`subset_type` combination"
    }

    rlang::abort(c(
      paste0("There are no files in the ", msg, " you have specified. Note:"),
      "i" = "Paths are case-sensitive",
      "i" = "Check folders using `list.dirs(path = PROJECT_DIR)`",
      "i" = "Check for files using `count_files(project_dir = PROJECT_DIR)`")
    )
  }

  n_ext <- sum(stringr::str_detect(project_files, file_type))

  if(length(n_ext) == 0){
    rlang::abort(c(glue::glue("Did not find any '{file_type}' files."),
                   "i" = "Use `file_type` to change file extension for sound files",
                   "i" = "Check `project_dir` is correct"))
  }


  if(!is.null(project_dir)) {
    project_files <- stringr::str_remove(project_files, project_dir)
  }

  # Collect non-file-type files
  extra <- stringr::str_subset(project_files, file_type, negate = TRUE)
  gps <- stringr::str_subset(extra, stringr::regex("gps", ignore_case = TRUE))
  focal <- stringr::str_subset(project_files, file_type)

  # Set up file path metadata
  meta <- dplyr::tibble(
    dir = fs::path_dir(focal),
    file_name = fs::path_file(focal),
    type = tolower(fs::path_ext(focal)))

  if(length(gps) > 1) {
    meta <- meta %>%
      dplyr::add_row(dir = fs::path_dir(gps),
                     file_name = fs::path_file(gps),
                     type = "gps")
  }

  pattern_aru <- c("barlt" = "BarLT",
                   "SMM" = "SongMeter",
                   "SM\\d" = "SongMeter",
                   "S\\dA" = "SongMeter")

  if(!quiet) rlang::inform("Extracting ARU info...")
  # Get ARU info
  meta <- meta %>%
    dplyr::mutate(
      ARU_type = extract_replace(.data$file_name, pattern_aru),
      ARU_type = dplyr::if_else(is.na(.data$ARU_type),
                                extract_replace(.data$dir, pattern_aru),
                                .data$ARU_type),
      ARU_id = stringr::str_extract(.data$file_name, pattern_aru_id),
      ARU_id = dplyr::if_else(is.na(.data$ARU_id),
                              stringr::str_extract(.data$dir, pattern_aru_id),
                              .data$ARU_id))

  # Get sites
  meta <- dplyr::mutate(meta, site = stringr::str_extract(.data$dir, .env$pattern_site))

  # Option for site by date? From a separate file?

  pattern_non_date <- paste0("(", pattern_site, ")|(",
                             pattern_aru_id, ")|(",
                             paste0("(", pattern_aru, ")", collapse = "|"),
                             ")")

  if(!quiet) rlang::inform("Extracting Dates and Times...")

  meta <- meta %>%
    dplyr::mutate(
      file_left = stringr::str_remove_all(.data$file_name, pattern_non_date),
      dir_left = stringr::str_remove_all(.data$dir, pattern_non_date),

      # Try file name
      date_time_chr = stringr::str_extract(.data$file_left, .env$pattern_date_time),
      # Try dir name
      date_time_chr = dplyr::if_else(
        is.na(.data$date_time_chr),
        stringr::str_extract(.data$dir_left, .env$pattern_date_time),
        .data$date_time_chr),
      # Get date_times
      date_time = lubridate::parse_date_time(
        .data$date_time_chr,
        orders = paste(order_date, "HMS")),
      date = lubridate::as_date(.data$date_time))

  if(any(is.na(meta$date))) {
    missing <- meta %>%
      dplyr::filter(is.na(.data$date)) %>%
      dplyr::mutate(
        # Try file name
        date_chr = stringr::str_extract(file_name, .env$pattern_date),
        # Try dir name
        date_chr = dplyr::if_else(
          is.na(.data$date_chr),
          stringr::str_extract(.data$dir, .env$pattern_date),
          NA_character_),
        date = lubridate::parse_date_time(.data$date_chr, orders = order_date),
        date = lubridate::as_date(.data$date)) %>%
      dplyr::select("dir", "file_name", "date")

    # Add dates where missing
    meta <- dplyr::rows_patch(meta, missing, by = c("dir", "file_name"))
  }

  # Extra files
  if(length(extra) > 1) {
    rlang::inform(
      c("!" = paste0("Omitted ", length(extra), " extra, non-",
                     meta$type[1], " files")))
  }

  if(length(gps) > 1) {
    rlang::inform(c("!" = paste0("Detected ", length(gps), " GPS logs")))
  }

  # Flag problems
  f <- dplyr::filter(meta, type == "wav")
  n <- nrow(f)
  f_dt <- sum(is.na(f$date_time))
  f_type <- sum(is.na(f$ARU_type))
  f_id <- sum(is.na(f$ARU_id))
  f_site <- sum(is.na(f$site))

  if(any(c(f_dt, f_type, f_id, f_site) > 0)) {
   msg <- c("!" = "Identified possible problems with metadata extraction:")
   if(f_dt > 0) msg <- c(msg, "x" = paste0("Not all date/times were successfully detected (", f_dt, "/", n, ")"))
   if(f_type > 0) msg <- c(msg, "x" = paste0("Not all ARU types were successfully detected (", f_type, "/", n, ")"))
   if(f_id > 0) msg <- c(msg, "x" = paste0("Not all ARU ids were successfully detected (", f_id, "/", n, ")"))
   if(f_site > 0) msg <- c(msg, "x" = paste0("Not all sites were successfully detected (", f_site, "/", n, ")"))
   rlang::inform(msg)
  }

  dplyr::arrange(meta, type != "gps", !is.na(date_time), dir, file_name, site, date_time) %>%
    dplyr::select(-"file_left", -"dir_left", -"date_time_chr")
}


clean_site_index <- function(index) {
  type <- fs::path_ext(index)
  if(type == "csv") site_index <- readr::read_csv(index)
  if(type == "xlsx") site_index <- readxl::read_excel(index)
}



clean_gps <- function(meta, gps = NULL) {


  # |- BarLT   ----------------

  ## Create file name log (moving this from Prep_dates.R)
  ll <- stringr::str_split(project_files, "/") |>
       purrr::map_int(.f=length) |> unique()
  if(length(ll)>1)  abort(
    c("More than one folder structure detected",
      "x" = "Differing folder structures are likely to lead to errors",
      "i" = "Run clean_metadata on each folder type separately"))

  if(rlang::is_null(folder_names)) {pathnames <- c(glue::glue("Folder{1:(ll-1)}"), "WaveFilename")
  } else if(length(folder_names) == ll){
    if(folder_names[length(folder_names)]!= "WaveFilename")folder_names[length(folder_names)] <-  "WaveFilename"
    pathnames <-  folder_names
  } else{abort(
    c("You have provided a folder_names vector that is not the same length as the number of folders",
      "x" = glue::glue("folder_names is {length(folder_names)}, while number of folders is {ll}"),
      "i" = "Run clean_metadata on each folder type separately") ) }
  if(!exists("site_in_filename")) site_in_filename <- FALSE
  browser()
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

  wav_names_log$SiteID <- "Site1"

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
    browser()

  }
  # |- SM4   ----------------
    SMfiles <- dplyr::filter(wav_names_log,ARU_type=="SongMeter" )

    if(nrow(SMfiles)>0){
      # browser()
    if(grepl("3", type)) warn("Default site_pattern is set for SM4. Recommend changing based on file structure")
    if(!exists("site_pattern")) site_pattern <-  "S4A\\d{5}"
    # file.location <- "//WRIV02DTSTDNT1/RecordStor20172019/BetweenRivers_2019"

      # list.files(folder_base, "_Summary.txt", recursive = T, full.names = T)

    browser()

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


     browser()

    if(isTRUE(return_full_metadata)){
      list(gps_locations = gps_locations,
           recording_log = recording_log,
           )

      } else{
        return(recording_log)
      }


}


