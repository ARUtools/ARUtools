#' Create directory structure for ARU folders
#'
#' @param hexagons Character vector. Hexagon or cluster names for folder names.
#' @param aru_ids  Character vector. ARU unit ids. Should include the
#'   hexagon/cluster id in the name.
#' @param base_dir Character. Base directory to build directory structure in.
#'
#' @return Does not return anything if run. If cancelled returns string notification.
#' @export
#'
#' @examples
#' # Default is to do a dry-run (don't actually create the directories)
#' create_dirs(hexagons = c("site1", "site2", "site3"),
#'             aru_ids = c("site1_sm01", "site1_sm02", "site2_sm03", "site2_sm04",
#'                         "site3_sm05", "site3_sm06"),
#'             base_dir = "Recordings")
#'
#' # Get a list of directories which would be created
#' create_dirs(hexagons = c("site1", "site2", "site3"),
#'             aru_ids = c("site1_sm01", "site1_sm02", "site2_sm03", "site2_sm04",
#'                         "site3_sm05", "site3_sm06"),
#'             base_dir = "Recordings", dir_list = TRUE)
#'
#' \dontrun{
#' # Create directories AND return a list of those created
#' d <- create_dirs(hexagons = c("site1", "site2", "site3"),
#'                  aru_ids = c("site1_sm01", "site1_sm02", "site2_sm03", "site2_sm04",
#'                              "site3_sm05", "site3_sm06"),
#'                  base_dir = "Recordings", dir_list = TRUE,
#'                  dry_run = FALSE)
#' d
#' }

create_dirs <- function(hexagons, aru_ids, base_dir = NULL, dir_list = FALSE,
                        dry_run = TRUE) {

  # Get absolute path so user can be *really* sure they want to do this
  if(is.null(base_dir)) base_dir <- fs::path_wd() else base_dir <- fs::path_abs(base_dir)

  # Calculate directories
  d <- vector()
  for(h in hexagons) d <- c(d, fs::path(base_dir, h, stringr::str_subset(aru_ids, h)))

  # Create directories
  if(!dry_run) {
    fs::dir_create(d)

    if(rlang::is_installed("sessioninfo")) {
      readr::write_lines(sessioninfo::session_info(), glue::glue("{base_dir}/session_info.md"))
    } else {
      readr::write_lines(sessionInfo(), glue::glue("{base_dir}/session_info.md"))
    }
  } else {
    msg <- "This is a dry run, no directories are created"
    if(!dir_list) msg <- c(msg, "i" = "Use `dir_list = TRUE` to return a list of directories to be created")
    rlang::inform(msg)
  }
  if(dir_list) d
}
