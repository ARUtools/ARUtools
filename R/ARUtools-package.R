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
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(
      # Vars used in Non-Standard Evaluations, declare here to
      # avoid notes. Here are Defaults for various functions
      c("site_id", "psel_std", "t2sr", # sample_recordings()
        "path", "subdir_out", "filename_out", "clip_length", "start_time" # clip_wave()
      )
    )

}
