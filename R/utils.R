
list_files <- function(project_dir, subset, subset_type,
                       type = c("file", "directory")) {
  fs::dir_ls(project_dir, type = type,
             # Add filters
             regexp = subset,
             invert = subset_type == "omit",
             recurse = TRUE)
}


extract_replace <- function(string, pattern) {
  string %>%
    stringr::str_extract(
      stringr::regex(paste0("(", names(pattern), ")", collapse = "|"),
                     ignore_case = TRUE)) %>%
    stringr::str_replace_all(stringr::regex(pattern, ignore_case = TRUE))
}

report_missing <- function(missing, total, name) {
  msg <- NULL
  if(missing > 0) {
    if(missing == total) type <- "No" else type <- "Not all"
    msg <- c("x" = paste0(type, " ", name, " were successfully detected (",
                          missing, "/", total, ")"))
  }
  msg
}
