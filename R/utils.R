
list_files <- function(project_dir, subset_dir, subset_type,
                       type = c("file", "directory")) {
  fs::dir_ls(project_dir, type = type,
             # Add filters
             regexp = subset_dir,
             invert = subset_type == "omit",
             recurse = TRUE)
}
