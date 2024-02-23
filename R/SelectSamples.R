#' Sample recordings
#'
#' Sample recordings based on selection weights from `calc_selection_weights()`
#' using `spsurvey::grts()`. If non-spatial, data will be converted to a spatial
#' data frame for sampling using the specified `crs`.
#'
#' @param meta_weights (Spatial) Data frame. Recording meta data selection
#'   weights. Output of `calc_selection_weights()`. Must have at least the
#'   columns identified by `col_site_id` and `col_sel_weights`, as well as the
#'   probability of selection columns (those starting with `psel`) and `doy`.
#' @param n Numeric, Data frame, Vector, or List. Number of base samples to
#'   choose. For stratification by site, a named vector/list of samples per site, or
#'   a data frame with columns `n` for samples, `n_os` for oversamples and the
#'   column matching that identified by `col_site_id`.
#' @param os Numeric, Vector, or List. Over sample size (proportional) or named
#'   vector/list of number of samples per site Ignored if `n` is a data
#'   frame.
#' @param col_site_id Column. Unquoted name of column containing site strata IDs
#'   (defaults to `site_id`).
#' @param col_sel_weights Column. Unquoted name of column identifying selection
#'   weights (defaults to `psel_std`)
#' @param seed Numeric. Random seed to use for random sampling. Seed only
#'   applies to specific sampling events (does not change seed in the
#'   environment). `NULL` does not set a seed.
#' @param ... Extra named arguments passed on to `spsurvey::grts()`.

#'
#' @return A sampling run from grts
#' @export
#' @examples
#' s <- clean_site_index(example_sites_clean,
#'                       col_date_time = c("date_time_start", "date_time_end"))
#' m <- clean_metadata(project_files = example_files) |>
#'   add_sites(s) |>
#'   calc_sun()
#'
#' params <- sim_selection_weights()
#' w <- calc_selection_weights(m, params = params)
#'
#' # No stratification by site
#' samples <- sample_recordings(w, n = 10, os = 0.1, col_site_id = NULL)
#'
#' # Stratification by site defined by...
#'
#' # lists
#' samples <- sample_recordings(w, n = list(P01_1 = 2, P02_1 = 5, P03_1 = 2), os = 0.2)
#'
#' # vectors
#' samples <- sample_recordings(w, n = c(P01_1 = 2, P02_1 = 5, P03_1 = 2), os = 0.2)
#'
#' # data frame
#' samples <- sample_recordings(
#'   w,
#'   n = data.frame(site_id = c("P01_1", "P02_1", "P03_1"),
#'                  n = c(2, 5, 2),
#'                  n_os = c(0, 0, 1)))
#'

sample_recordings <- function(meta_weights,
                              n, os = NULL,
                              col_site_id = site_id,
                              col_sel_weights = psel_std,
                              seed = NULL, ...) {

  col_site_id <- enquo(col_site_id)
  col_sel_weights <- enquo(col_sel_weights)
  sites_name <- nse_name(col_site_id)

  # TODO: CHECKS
  check_cols(meta_weights, enquos(col_site_id, col_sel_weights), name = "meta_weights")
  if(is.data.frame(n)) check_cols(n, c(sites_name, "n", "n_os"), name = "n")

  if(!rlang::is_named(os) && length(os) == 1 && (os < 0 || os > 1)) {
    rlang::abort(
      "`os` as a single value is a proportion, and must range between 0 and 1",
      call = NULL)
  }

  if(is.null(os) && !inherits(n, "data.frame")) {
    rlang::abort(
      "`os` can only be NULL if `n` is a data frame with a column `n_os`",
      call = NULL)
  }

  # If sf, convert to df
  meta_weights <- sf_to_df(meta_weights)

  # Convert to time-spatial
  meta_weights_sf <- sf::st_as_sf(meta_weights,
                                  coords = c("doy", meta_weights$psel_by[1]),
                                  crs = 3395)

  # Assemble n and os (based on BASSR::run_grts_on_BASS() ---------------------

  # Check for stratification - Create a list of problems
  s <- c(inherits(n, "data.frame") | length(n) > 1 | rlang::is_named(n)) # Stratification exists in samples

  # Not stratified
  if (all(!s)) {

    if(!quo_is_null(col_site_id)) {
      warn("No stratification by site included in `n` or `os`. Ignoring `col_site_id`", call = NULL)
    }

    if(length(os) > 1) {
      rlang::abort("`os` must be a single value unless using stratification by site",
                   call = NULL)
    }

    sites_name <- NULL
    n_sites <- rep(n, length(n))
    n_os <- round(n * os)
    if(n_os == 0) n_os <- NULL

    # Stratified
  } else {

    # Missing site column name
    if(quo_is_null(col_site_id)) {
      abort_strat("`col_site_id` must contain site names matching those in `n`")
    }

    # Missing appropriate n object
    if(!(inherits(n, "data.frame") |
         length(n) > 1 |
         rlang::is_named(n))) {
      abort_strat()
    }

    sites <- meta_weights |>
      dplyr::pull({{ col_site_id }}) |>
      unique()

    # Get n_site and n_os depending on inputs

    # Check data frame
    if(inherits(n, "data.frame")) {

      # Problem: Wrong strata
      n_sites <- dplyr::pull(n, {{ col_site_id }})
      if(!all(n_sites %in% sites)) abort_strat()

      # Convert from data frame
      n_os <- as.list(n$n_os) |>
        stats::setNames(n_sites)
      n <- as.list(n$n) |>
        stats::setNames(n_sites)

    } else {
      # Check list (convert if vector)
      n <- as.list(n)
      if(length(os) > 1) os <- as.list(os)

      # Problem: List not named correctly
      if(!(rlang::is_named(n) && all(names(n) %in% sites))) {
        abort_strat()
      }

      # Problem: List not named correctly (and not length = 1)
      if(!((rlang::is_named(os) && all(names(os) %in% sites)) ||
           length(os) == 1)) {
        abort_strat("`os` must be a single value, or a vector/list named by strata")
      }

      if(!rlang::is_named(os) && length(os) == 1) {
        n_os <- lapply(n, \(x) round(x * os))

        # If all 0, use NULL
        if(all(vapply(n_os, \(x) x == 0, logical(1)))) n_os <- NULL
      } else n_os <- os
    }

    # Problem: Chose stratification, but only one strata
    if(length(n) == 1 || (rlang::is_named(n_os) && length(n_os) == 1)) {
      abort_strat("There is only one stratum")
    }
  }

  # Check sample sizes
  msg <- NULL
  n_check <- unlist(n) + unlist(n_os)
  if(length(n_check) == 1) {
    if(n_check > nrow(meta_weights)) {
      msg <- c("i" = paste0(n_check, " samples, but only ", nrow(meta_weights), " data"))
    }
  } else {
    cnts <- sf::st_drop_geometry(meta_weights) |>
      dplyr::count({{ col_site_id }}) |>
      tibble::deframe()
    if(any(n_check > cnts[names(n)])) {
      msg <- c("i" = paste0("Selected more samples than exist in some sites (",
                            paste0(names(n)[n_check > cnts[names(n)]], collapse = ", "), ")"))
    }
  }

  if(!is.null(msg)) abort(c("Cannot sample (n + oversampling) more points than there are in the data", msg),
                          call = NULL)

  set_seed(seed, {
    spsurvey::grts(sframe = meta_weights_sf,
                   n_over = n_os,
                   n_base = n,
                   stratum_var = sites_name,
                   DesignID = "sample",
                   aux_var =  nse_name(col_sel_weights),
                   ...)
  })

}


#' Abort during stratification
#'
#' Wrapper around `rlang::abort()` for consistent messaging when stratification
#' arguments are not correct.
#'
#' @param msg Alternative message if required (otherwise returns default message
#'   regarding the `n` parameter)
#'
#' @noRd
abort_strat <- function(msg = NULL) {
  m <- "Not all requirements met for sampling with stratification by site"
  if(is.null(msg)) {
    msg <- paste0("`n` must be a data frame with appropriate ",
                  "columns, or vector/list named by site")
  }
  rlang::abort(c(m, "x" = msg), call = NULL)
}
