#' Parse the log file for BarLT
#'
#' Returns the serial number, firware version
#' and log file parsed as a tibble.
#'
#' @param filename Character. File location of log file
#'
#' @return
#'
read_log_barlt <- function(filename){
  # Read the full file in
  full_file <- readr::read_lines(file = filename)
  # Parse out serial number
  serial <- full_file[grep("Serial Number:", full_file)] |>
    stringr::str_extract_all(pattern ="((\\d{5,}\\-)+\\d{5,}|\\d{5,})") |> unique()
  firmware <- full_file[grepl("Firmware:", full_file)] |>
     stringr::str_remove(" Firmware: ") |> unique()

  if(length(serial)>1 ){
    warn(c(glue::glue("Multiple serial numbers read from file {filename}"),
            "x" = "Cannot have multiple serial numbers in single log file",
          "i"=glue::glue("Current serial numbers are {glue::glue_collapse(serial, sep ='; ')}.
                              Check original log file for errors.") ))
    tmp_action <- menu(c(glue::glue("Use {serial}"), "Abort program"),
                       title = glue::glue("Multiple serial numbers detected for {filename}. What would you like to do?"))

  if (tmp_action == (length(serial) + 1)) {
    abort("Check ARU log file and try again")
  } else{
    serial <- serial[[tmp_action]]
  }
  }

    if(length(firmware)>1 ){
      warn(c(glue::glue("Multiple firmware version read from file {filename}"),
             "x" = "Should not have multiple firmware versions in single log file",
             "i"=glue::glue("Current firmware versions are {glue::glue_collapse(firmware, sep ='; ')}.
                              Check original log file for errors.") ))
      tmp_action2 <- menu(c(glue::glue("Use {firmware}"), "Abort program"),
                         title = glue::glue("Multiple firmware versions detected for {filename}. What would you like to do?"))

      if (tmp_action2 == (length(firmware) + 1)) {
        abort("Check ARU log file and try again")
      } else{
        firmware <- firmware[[tmp_action2]]
      }
    }



  dated_logs <- full_file[grepl("^\\d\\d\\/", full_file)] |>
    stringr::str_remove("\xffffffb0") |>
    stringr::str_conv("UTF-8") |>
    tibble::tibble(row = . ) |>
    tidyr::separate(col = row, into = c("Date", "Time", "Log"),
                    sep = c(10,20), extra = 'merge') |>
    mutate(filename = filename,
           serial = serial,
           firmware = firmware)

  return(list(serial_number = serial,
              firmware = firmware,
              log_entries = dated_logs))



}


#' Parse SM4 logfiles
#'
#' @param filename Character string or vector of character strings of file locations.
#'                  Can be a single file or a vector of file locations.
#' @param SiteID_pattern Pattern for extracting SiteID from filename. Defaults to 'SM4A\\d{5}',
#'                         which is SM4, followed by 5 digits.
#'
#' @return Returns data frame of log files.
#'
read_summary_SM4 <- function(filename, SiteID_pattern = "SM4A\\d{5}"){
  if(length(filename)!=1 & !(length(filename)>1)) abort("Filename in not correct format. Cannot read SM logs.")
  if(length(filename)>1){

  out <- purrr::map_df(summText, ~{read.csv(.x) |>
      dplyr::mutate(SiteID = stringr::str_extract(.x, SiteID_pattern))})

  if(any(is.na(out$SiteID))) abort("Some SiteID were not parsed. Check SiteID_pattern is correct")
  }
  if(length(filename)==1){
    out <- read.csv(filename) |>
             dplyr::mutate(SiteID = stringr::str_extract(.x, SiteID_pattern))
    if(any(is.na(out$SiteID))) abort("Some SiteID were not parsed. Check SiteID_pattern is correct")

  }


  return(out)


}
#' Process GPS locations for SongMeters
#'
#' @param folder_base Base folder were summary folders locationed
#' @param list_files List of files in folder_base
#' @param site_pattern site pattern to separate out siteid
#'
#' @return Returns a data frame with lat/lon for each location and date collected
process_gps_SM <- function(folder_base, list_files, site_pattern){
    warn("process_gps_SM is depreciated. Use process_log_SM")
    process_log_SM(folder_base, list_files, site_pattern,
                   return_gps=T, return_log=F)

  }
#' Process log files from SongMeters
#'
#' @param folder_base Base folder were summary folders locationed
#' @param list_files List of files in folder_base
#' @param site_pattern site pattern to separate out siteid
#' @param return_gps Logical. Should function return GPS locations?
#' @param return_log Logical. Should the function return the full log?
#'
#' @return Returns a data frame with lat/lon for each location and date collected
#' @export
process_log_SM <- function(folder_base, list_files, site_pattern, return_gps, return_log){

  summText <- list_files[grep("_Summary.txt", list_files)]
  # browser()
  summaries <- purrr::map_df(summText, ~{read.csv(glue::glue("{folder_base}/{.x}")) |>
      mutate(across(.fns = as.character)) |>
      dplyr::mutate(SiteID = stringr::str_extract(.x, site_pattern))}) |>
    tibble::as_tibble()
  if(any(is.na(summaries$SiteID)))abort("Some SiteID were not parsed. Check SiteID_pattern is correct")
  if(isTRUE(return_gps)){
  needed_names <- c("LAT", "LON", "DATE", "TIME")
  if(!all( needed_names%in% names(summaries))) abort(
    glue::glue(
      "Check your summary files. Should include {glue::glue_collapse(needed_names, sep = '; ')}"
      )
    )
  # browsers()
  suppressWarnings(
  gps_locations <- summaries |>
    filter(!is.na(as.numeric(LON) )& !is.na(as.numeric(LAT))) |>
    # HH/MM,DD/MM/YY
    mutate(date = lubridate::ymd(DATE),
           time =  lubridate::hms(TIME),
           dd_mm_yy = as.character(format(date, "%d/%m%/%Y")),
           hh_mm = as.character(TIME)) |>
    dplyr::arrange(SiteID,date,time) |>
    dplyr::distinct(SiteID, LAT, LON,
                    .keep_all = T) |>
    dplyr::select(SiteID,dd_mm_yy, hh_mm, LAT, LON) |>
    dplyr::mutate(longitude_decimal_degrees = -1*as.numeric(LON),
                  latitude_decimal_degrees = as.numeric(LAT)) #|>
  )
    # sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326) |>
    # dplyr::bind_cols(
    #   tibble::as_tibble(sf::st_coordinates(.))
    # ) |>
    # dplyr::rename(longitude_decimal_degrees=LON,
    #                    latitude_decimal_degrees=LAT)
    # sf::st_drop_geometry()
  # browser()
  if(any(gps_locations$latitude_decimal_degrees==0)|any(gps_locations$longitude_decimal_degrees==0)){
    if(
      menu(c("Yes", "No"),
           title =
           "Lat or Lon detected as zero.\nThis can occur if not set in Song Meters.
           \nTime to sunrise/sunset will be incorrect (unless you are deploying at 0,0).
           \nDo you want to continue?"
           )==2) abort(c("Latitude or longides invalid. It is reccomend you provide lat/lon manually"))
  }

  gps_locations$tz <- lutz::tz_lookup_coords(lat = gps_locations$latitude_decimal_degrees,
                                             lon = gps_locations$longitude_decimal_degrees,
                                             method = 'accurate')
  stopifnot("Multiple time zones detected, please run separately"=
              length(unique(gps_locations$tz))==1)
  }
  if(all(is_true(return_gps), is_true(return_log)))
    return(list(gps_locations = gps_locations,
                metadata = summaries))
  if(all(is_true(return_gps), is_false(return_log)))  return(gps_locations)
  if(all(is_false(return_gps), is_true(return_log)))  return(summaries)
  if(all(is_false(return_gps), is_false(return_log)))
    abort(c("I don't know what to return", "i" = "one or both of return_gps or return_log must be TRUE") )
}


#' Process GPS log file for BarLT
#'
#' Extracts out lat/lon locations from gps log files
#'   will drop locations before a given date, but does
#'   not drop duplicate locations
#'
#' @param base_folder base_folder where files are stored
#' @param file_list  file list relative to base_folder
#' @param deploy_start_date  first date of deployment
#'
#' @return Returns a data frame with lat/lon for each location and date collected
process_gps_barlt <- function(base_folder, file_list, deploy_start_date,check_dists,site_pattern,...){
  crs_m <- 3161
  dist_cutoff <- 100
  list2env(list(...), environment())
  # browser()
  # TODO fix this process for updated firmware, which includes GPX files
  # browser()
  gps_files_txt <- glue::glue("{base_folder}/{file_list[grepl('GPS\\\\w+.csv', file_list)]}")
  gps_files_gpx <- glue::glue("{base_folder}/{file_list[grepl('GPS\\\\w+.gpx', file_list)]}")
  folder_gps <- stringr::str_remove(gps_files_txt, base_folder)
  n_fold <- stringr::str_split(folder_gps, "/") |> purrr::map_dbl(length) |> unique()
  stopifnot(length(n_fold)==1)
  gps_log_full <- purrr::map_df(gps_files_txt,read_and_check_barlt_gps) |>
    mutate(dd_mm_yy_raw = dd_mm_yy,
      dd_mm_yy = lubridate::dmy(dd_mm_yy),
      SiteID = stringr::str_extract(filepath, site_pattern)) |>
    tidyr::separate(filepath, remove=F, sep = paste0(base_folder), into = c("dropme", "Other"), extra = 'merge') |>
    tidyr::separate(Other, into =c("dropme2",
                                   glue::glue( "Folder_{1:(n_fold-2)}"),
                                   "Filename"), sep = "/", extra = 'merge') |>
    # Placeholder to drop empty columns. For now it drops date columns also
    # dplyr::select_if(
    #     ~!(all(is.na(.x))||all(.x == ""))
    #
    # ) |>
    dplyr::select(-matches("^dropme")) |>
    dplyr::filter(dd_mm_yy >= deploy_start_date)

  if(any(is.na(gps_log_full$SiteID)))abort("Some SiteID were not parsed from GPS log. Check SiteID_pattern is correct")
  tz_loc <- lutz::tz_lookup_coords(lat = gps_log_full$latitude_decimal_degrees,
                                   lon = gps_log_full$longitude_decimal_degrees,
                                   method = 'accurate')
  gps_log_full$tz <- tz_loc

  if(dplyr::n_distinct(tz_loc)>1) warn("Multple time zones detected. This may affect sunrise/sunset accuracy")

  if(isTRUE(check_dists)) {
    site_distances <- check_gps_distances(gps_log_full, crs_m = crs_m, dist_cutoff = dist_cutoff)
    gps_log_full <- left_join(gps_log_full, site_distances, by = "SiteID")
    }



  # # rec_log <- readr::read_csv(glue::glue("{folder_base}/{list_files[grepl('Reclog', list_files)]}"),
  # #                            skip = 1, col_names = T, col_types = readr::cols()) |>
  # #   janitor::clean_names()
  #
  # # gps_loc <- gps_log[(gps_log$dd_mm_yy %in% rec_log$date_dd_mm_yyyy)|(gps_log$dd_mm_yy %in% (rec_log$date_dd_mm_yyyy+1)) ,]
  # gps_loc <- gps_log_full[nrow(gps_log_full) ,]
  # if(nrow(gps_loc)>1) warn("Multple GPS locations. Please check")
  # if(nrow(gps_loc)==0) abort("GPS location did not pull.")
  #
  # message("Full GPS log contains the following locations")
  # print(gps_log_full)
  # message("However only the last one is being used for ARU location.")
  # if (menu(c("Yes", "No"), title = "Do you wish to continue?") != 1) {
  #   abort("Check gps locations and try again")
  # }
  return(gps_log_full)


}





#' Read barlt gps and check locations
#'
#' @param .x Path to gps file from barLT
#' @param return_all_locs logical. Return all gps locations from file. Default is false
#'
#' @return Returns paths chosen by user
read_and_check_barlt_gps <- function(.x, return_all_locs=F){
  csv_tmp <- readr::read_csv(.x,skip = 1, col_names = T, col_types = readr::cols()) |>
    janitor::clean_names() |> dplyr::mutate(filepath = .x)
   if(!"hh_mm_ss" %in% names(csv_tmp)&"hh_mm"%in% names(csv_tmp))
     csv_tmp$hh_mm_ss <- csv_tmp$hh_mm



  if(nrow(csv_tmp)!=1){
    if(!interactive()|isTRUE(return_all_locs)){tmp_action <- 1:nrow(csv_tmp)
    rlang::warn(glue::glue("Multple locations found for {.x}, but no option to select specifics. Run in interactive session to allow selection."))
    } else{
    tmp_action <- coda::multi.menu(c(glue::glue("DMY: {csv_tmp$dd_mm_yy}, Time: {csv_tmp$hh_mm}, Lat: {csv_tmp$latitude_decimal_degrees }, Lon: {csv_tmp$longitude_decimal_degrees}"), "Abort program"),
                       title = glue::glue("Multiple locations detected for {.x}. Which would you like to use?"))
    }
    # browser()
    if (max(tmp_action) == (nrow(csv_tmp) + 1)) {
      abort("Check ARU log file and try again")
    } else{
      gps_loc <-csv_tmp[tmp_action,]
    }
  } else{ gps_loc <- csv_tmp}
  return(gps_loc)


}
