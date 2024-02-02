
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
