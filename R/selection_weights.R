#' Create parameters and simulate selection weights
#'
#' This function creates and explores parameters for generating selections.
#' These parameters define the selection distribution of minutes (`min`) around
#' the sun event (sunrise/sunset), as well as of days (`day`).
#'
#' @param min_range Numeric vector. Range of the sampling distribution of
#'   minutes around the sun event.
#' @param min_mean Numeric. Mean of the sampling distribution of minutes to the
#'   sun event.
#' @param min_sd Numeric. SD in minutes of the sampling distribution of minutes
#'   around the sun event.
#' @param day_range Date/Datetime/Numeric vector. Range of sampling distribution
#'   of days. Can be Dates, Date-times, or DOY (day-of-year, 1-366).
#' @param day_mean Date/Datetime/Numeric. Mean date of the sampling distribution
#'   of days. Can be Date, Date-time, or DOY (day-of-year, 1-366).
#' @param day_sd Numeric. SD in days of the sampling distribution of days.
#' @param offset Numeric. Offset to shift for time of day in minutes.
#' @param return_log Logical. Log the density in the selection function?
#' @param selection_fun Character. Selection function to use. Options are
#'   `lognorm`,`norm` (default), or `cauchy`.
#' @param selection_var Character. Selection variable to plot
#'   (if `plot = TRUE`). Options are are `psel`, `psel_doy`, `psel_min`,
#'   `psel_std`, `psel_scaled`, or `psel_normalized` (default).
#' @param return_params Logical. Return parameter list for use in
#'   calc_selection_weights()?
#' @param plot Logical. Create plot of simulated selection weights? If
#'   `return_param = TRUE` and `plot = TRUE` plot is created as a side effect.
#'   Other wise, plot is returned directly.
#'
#' @return Returns either a list of selection parameters or a plot of simulated
#'   selection weights
#' @export
#'
#' @examples
#' params <- simulate_selection_weights()

sim_selection_weights <- function(
    min_range = c(-70, 240), min_mean = 30, min_sd = 60,
    day_range = c(120, 201), day_mean = 161, day_sd = 20,
    offset = 0, return_log = TRUE, selection_fun = "norm",
    selection_var = "psel_normalized", return_params = TRUE, plot = TRUE) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    rlang::abort(
      c("Package \"ggplot2\" must be installed to use this function",
        "!" = "Use `install.packages(\"ggplot2\") to install"),
      call = NULL
    )
  }

  # Create parameter list
  params <- list(
    min_range = min_range, min_mean = min_mean, min_sd = min_sd,
    day_range = day_range, day_mean = day_mean, day_sd = day_sd,
    offset = offset, return_log = return_log,
    selection_fun = selection_fun)

  # Checks
  params <- check_selection_params(params)
  check_logical(return_params)
  check_logical(plot)
  check_text(selection_var, n = 1,
             opts = c("psel_normalized", "psel", "psel_day", "psel_min",
                      "psel_std", "psel_scaled", "psel_normalized"))

  # Simulate weights
  if(plot) {
    weights <- expand.grid(date = seq(day_range[1], day_range[2]),
                           min = seq(min_range[1], min_range[2]),
                           site_id = 1) |>
      calc_selection_weights(params = params, col_min = "min") |>
      dplyr::mutate(date = lubridate::ymd("2022-01-01") + date - 1)

    p1 <- ggplot2::ggplot(weights, ggplot2::aes(date, psel_doy)) +
      ggplot2::geom_line() +
      ggplot2::labs(x = "Date", y = "Selection weight (date)")

    p2 <- ggplot2::ggplot(weights, ggplot2::aes(min, psel_min)) +
      ggplot2::geom_line() +
      ggplot2::labs(x = "Time to sun event", y = "Selection weight (time)")

    p3 <- ggplot2::ggplot(weights, ggplot2::aes(date, min, fill = .data[[selection_var]])) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::labs(x = "Date", y = "Time", fill = "Selection\nWeight")

    p <- (p1 + p2) / p3 &
      ggplot2::theme_minimal(base_size = 14)

    if(return_params) print(p) # Print if not returning
  }

  if(return_params) r <- params else r <- p
  r
}



#' Calculate Selection Weights
#'
#' @param meta_sun (Spatial) Data frame. Recording meta data with time to
#'   sunrise/sunset. Output of `calc_sun()`. Must have at least `col_min`, `col_day`, and `col_site_id`
#' @param col_min Minutes column. Should not be quoted.
#' @param col_day Day column. Should not be quoted.
#' @param params list of parameters. See defaults for examples.
#'                 Should include min_range, doy_range, mean_min, sd_min,
#'                mean_doy, sd_doy, off, log_, fun.
#'
#' @inheritParams common_docs
#'
#' @return   Returns data with selection weights columns
#' @export
#'
#' @examples
#' m <- clean_metadata(project_files = example_files)
#' s <- clean_site_index(example_sites_clean,
#'                       col_date_time = c("date_time_start", "date_time_end"))
#' m <- add_sites(m, s)
#' m <- calc_sun(m)
#'
#' params <- selection_params()
#' calc_selection_weights(m, params = params)

calc_selection_weights <- function(meta_sun,
                                   params,
                                   col_site_id = "site_id",
                                   col_min = "t2sr",
                                   col_day = "date") {

  # Checks
  check_cols(meta_sun, name = "meta_sun", cols = c(col_site_id, col_min, col_day))
  params <- check_selection_params(params)

  # Get params as environment objects
  list2env(params, envir = environment())

  # Prepare selection weights data frame
  sp <- dplyr::select(meta_sun, dplyr::all_of(c(col_site_id, col_min, col_day))) |>
    dplyr::mutate(doy = check_doy(.data[[col_day]])) |>
    dplyr::filter(.data[["doy"]] >= day_range[[1]],
                  .data[["doy"]] <= day_range[[2]],
                  .data[[col_min]] >= min_range[[1]],
                  .data[[col_min]] <= min_range[[2]])

  if(nrow(sp) == 0) rlang::abort(
    "No selections possible within this range of dates and times",
    call = rlang::caller_env())

  # Prepare selection functions
  min_fun <- switch(
    selection_fun,
    "lognorm" = function(x, m, sd, log) dlnorm(x + offset, log(m + offset), sd, log),
    "norm" = dnorm,
    "cauchy"= dcauchy
  )

  # Check offset
  if((min(meta_sun[[col_min]]) + offset) <= 0 && selection_fun == "lognorm") {
    rlang::abort(
      paste0("If `selection_fun = 'lognorm'` and any `min` values are less than 0\n",
             "you must provide an `offset` large enough to ensure all values are greater than 0."),
      call = NULL)
  }

  dens_min <- min_fun(seq(min_range[1], min_range[2]), min_mean, min_sd, log = return_log)
  dens_doy <-   dnorm(seq(day_range[1], day_range[2]), day_mean, day_sd, log = return_log)
  fun_psel <- function(x1, x2, return_log) if(return_log) exp(x1 + x2) else x1 * x2

  # Calculate selection weights
  sp |>
    dplyr::mutate(
      psel_min = min_fun(round(.data[[col_min]], 0), min_mean, min_sd, log = return_log) / max(abs(dens_min)),
      psel_doy = dnorm(.data[["doy"]], mean = day_mean, sd = day_sd, log = return_log) / max(abs(dens_doy)),
      psel = fun_psel(.data[["psel_min"]], .data[["psel_doy"]], return_log),
      psel_scaled = .data[["psel"]] / max(.data[["psel"]])) |>
    dplyr::group_by(.data[[col_site_id]]) |>
    dplyr::mutate(
      psel_std = .data[["psel"]] / max(.data[["psel"]]),
      psel_normalized = pmax(
        1e-3,
        (.data[["psel"]] - min(.data[["psel"]])) / (max(.data[["psel"]]) - min(.data[["psel"]]))
      )
    ) |>
    dplyr::ungroup()
}

check_selection_params <- function(params) {

  # Get params as environment objects
  list2env(params, envir = environment())

  # Check parameters values
  check_num(min_range, n = 2)
  check_num(min_mean, n = 1)
  check_num(min_sd, n = 1, range = c(0, Inf))
  day_range <- check_doy(day_range)
  day_mean <- check_doy(day_mean)
  check_num(day_sd, n = 1, range = c(0, Inf))
  check_num(offset, n = 1)
  check_text(selection_fun, n = 1, opts = c("norm", "lognorm", "cauchy"))

  check_logical(return_log)
  if(offset != 0 & selection_fun != "lognorm") {
    rlang::inform(
      c("Ignoring `offset`",
        "i" = paste("`offset` is only relevant when `selection_fun = `lognorm`",
                    "to shift the minutes range to > 0")))
  }

  list(
    min_range = min_range, min_mean = min_mean, min_sd = min_sd,
    day_range = day_range, day_mean = day_mean, day_sd = day_sd,
    offset = offset, return_log = return_log,
    selection_fun = selection_fun)
}
