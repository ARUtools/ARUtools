#' Default parameters for use in `gen_dens_sel_simulation` and `calc_sel_pr`
#'
#' @description
#' A generated list as defaults for use in selection parameters for
#' selecting ARU recordings by time of day and day of year.
#' See gen_dens_sel_simulation and calc_sel_pr for examples of use.
#'
#' @format ## `default_selection_parameters`
#' A list with 9 objects:
#' \describe{
#'   \item{min_range}{Range of minutes relative to sun event}
#'   \item{doy_range}{Range of day of year}
#'   \item{mean_min}{Average minutes to sun event in selection}
#'   \item{sd_min}{Standard deviation in distribution for selection distrution minutes to sun event}
#'   \item{mean_doy}{Average day of year for selection}
#'   \item{sd_doy}{Standard deviation of day of year for selection}
#'   \item{off}{Offset to shift for time of day.}
#'   \item{log_}{Log the density in the selection function?}
#'   \item{fun}{Selection function. Options are 'lognorm','norm', or 'cauchy'.}
#' }
#' @source data-raw/default_selection_parameters.R
"default_selection_parameters"

#' Example recording files
#'
#' A vector of examples ARU recording files.
#'
#' @format ## `example_files`
#' A vector with 42 file paths
#' @source data-raw/data_test.R
"example_files"

#' Example site-level meta data
#'
#' A data frame with examples of incorrectly formatted site-level data.
#'
#' @format ## `example_sites`
#' A data frame with 10 rows and 8 columns:
#' \describe{
#'   \item{Sites}{Site ids}
#'   \item{Date_set_out}{Deployment start date}
#'   \item{Date_removed}{Deployment end date}
#'   \item{ARU}{ARU ids}
#'   \item{lon}{Longitude in decimal degrees}
#'   \item{lat}{Latitude in decimal degrees}
#'   \item{Plots}{Hypothetical extra plot column}
#'   \item{Subplot}{Hypothetical extra subplot column}
#' }
#' @source data-raw/data_test.R
"example_sites"

#' Example cleaned site-level meta data
#'
#' A data frame with examples of correctly formatted site-level data.
#'
#' @format ## `example_sites_clean`
#' A data frame with 10 rows and 8 columns:
#' \describe{
#'   \item{site_id}{Site ids}
#'   \item{aru_id}{ARU ids}
#'   \item{date_time_start}{Deployment start date/time}
#'   \item{date_time_end}{Deployment end date/time}
#'   \item{date_start}{Deployment start date}
#'   \item{date_end}{Deployment end date}
#'   \item{longitude}{Latitude in decimal degrees}
#'   \item{latitude}{Longitude in decimal degrees}
#' }
#' @source data-raw/data_test.R
"example_sites_clean"

#' Example cleaned recording meta data
#'
#' A data frame with examples of correctly formatted metadata with added
#' site-level information
#'
#' @format ## `example_clean`
#' A data frame with 42 rows and 10 columns:
#' \describe{
#'   \item{file_name}{Name of the file}
#'   \item{type}{File type}
#'   \item{path}{Relative file path including file name}
#'   \item{aru_type}{ARU model}
#'   \item{aru_id}{ARU ids}
#'   \item{site_id}{Site ids}
#'   \item{date_time}{Recording date/time}
#'   \item{date}{Recording date}
#'   \item{longitude}{Latitude in decimal degrees}
#'   \item{latitude}{Longitude in decimal degrees}
#' }
#' @source data-raw/data_test.R
"example_clean"




#' Example template of tasks for WildTrax
#'
#' A data frame with tasks generated from `example_clean` using
#' the wildRtrax::wt_make_aru_tasks() function
#'
#' @format ## `task_template`
#' A data frame with 14 rows and 13 columns:
#' \describe{
#'   \item{location}{Site location name}
#'   \item{recording_date_time}{Date time of the recording}
#'   \item{method}{Method of interpretation (generally '1SPT')}
#'   \item{taskLength}{Length of recording in seconds}
#'   \item{transcriber}{Transcriber ID, to be filled in with function}
#'   \item{rain}{Empty character for filling in WildTrax}
#'   \item{wind}{Empty character for filling in WildTrax}
#'   \item{industryNoise}{Empty character for filling in WildTrax}
#'   \item{audioQuality}{Empty character for filling in WildTrax}
#'   \item{taskComments}{Empty character for filling in WildTrax}
#'   \item{internal_task_id}{Empty character for filling in WildTrax}
#' }
#' @source data-raw/task_template.R
"example_clean"



