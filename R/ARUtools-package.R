#' ARUtools: CWS Ontario Tools for working with ARU files
#'
#' ARU tools that some might find useful for working with ARU files.
#'
#' @docType package
#' @name ARUtools
#' @import spsurvey
#' @import patchwork
#' @import rlang
#' @import dplyr
#'
"_PACKAGE"

rlang::on_load(rlang::local_use_cli())


#' Set options for matching column headers in GPS text logs
#' @noRd
.onLoad <- function(libname, pkgname) {
  # Set column patterns (case insensitive)
  options(ARUtools =
            list(pat_gps_date = "date|(DD/MM/YY)",
                 pat_gps_time = "time|(HH/MM(/SS)?)|(HH:MM(:SS)?)", # Optional seconds
                 pat_gps_coords = c("lon", "lat")
            )
  )
}
