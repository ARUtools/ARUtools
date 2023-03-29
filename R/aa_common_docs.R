# common_docs ------------------
#' Common arguments and documentation for various functions
#'
#' @param project_dir Character. Directory where project files are stored. File
#'   paths will be used to extract information Must have one of `project_dir` or
#'   `project_files`.
#' @param project_files Character. Vector of project file paths. Must have one
#'   of `project_dir` or `project_files`.
#' @param subset Character. Text pattern to mark a subset of files/directories
#'   to either `keep` or `omit` (see `subset_type`)
#' @param subset_type Character. Either `keep` (default) or `omit`
#'   files/directories which match the pattern in `subset`.
#' @param meta Data frame. Recording metadata. Output of `clean_metadata()`.
#' @param meta_sites Data frame. Recording metadata with added coordinates.
#'   Output of `clean_metadata()` and then `add_sites()`.
#'
#' @details
#' Use `@inheritParams common_docs` to include the above in any function
#' documentation with a matching argument (will only include matching args)
#'
#' @keywords internal
#' @name common_docs
NULL
