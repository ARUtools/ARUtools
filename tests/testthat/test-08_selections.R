
test_that("sim_selection_weights()", {

  # Return params
  expect_silent(g <- sim_selection_weights(plot = FALSE))
  expect_type(g, "list")
  expect_named(g, c("min_range", "min_mean", "min_sd",
                    "day_range", "day_mean", "day_sd", "offset",
                    "return_log", "selection_fun"))

  expect_silent(g <- sim_selection_weights(selection_fun = "lognorm", offset = 71, plot = FALSE))
  expect_silent(g <- sim_selection_weights(selection_fun = "cauchy", plot = FALSE))
  expect_silent(g <- sim_selection_weights(return_log = FALSE, plot = FALSE))

  # Dates
  expect_silent(g <- sim_selection_weights(day_range = c("2023-01-01", "2023-09-01"),
                                           day_mean = "2023-06-02", plot = FALSE))
  expect_equal(g$day_range, lubridate::yday(c("2023-01-01", "2023-09-01")))
  expect_equal(g$day_mean, lubridate::yday("2023-06-02"))
  expect_silent(g <- sim_selection_weights(day_range = c(1, 244), plot = FALSE))
  expect_equal(g$day_range, c(1, 244))

  # Plots - Defaults
  withr::with_seed(123, expect_silent(g <- sim_selection_weights(return_params = FALSE)))
  vdiffr::expect_doppelganger("sim_selection_weights1", g)

  # Plots - Change defaults
  withr::with_seed(
    123,
    expect_silent(g <- sim_selection_weights(
      min_range = c(-70, 240),
      day_range = c(121, 201),
      day_sd = 20,
      return_log = TRUE,
      selection_var = "psel_normalized",
      return_params = FALSE))
  )
  vdiffr::expect_doppelganger("sim_selection_weights2", g)

  withr::with_seed(
    123,
    expect_silent(
      g <- sim_selection_weights(
        min_range = c(-60, 60*4),
        min_mean = -30,
        min_sd = 120,
        day_range = c(152, 210),
        day_mean = 170,
        day_sd = 200,
        return_log = FALSE,
        return_params = FALSE)
    )
  )
  vdiffr::expect_doppelganger("sim_selection_weights3", g)
})


test_that("calc_selection_weights()", {

  m <- clean_metadata(project_files = example_files, quiet = TRUE)
  s <- clean_site_index(example_sites_clean,
                        col_date = c("date_time_start", "date_time_end"))
  m <- add_sites(m, s, quiet = TRUE)
  m <- calc_sun(m)

  p <- sim_selection_weights(plot = FALSE)
  withr::with_seed(123, expect_silent(pr1 <- calc_selection_weights(m, params = p)))
  expect_snapshot_value(pr1, style = "json2", tolerance = 0.0005)

  # Use DOY rather than date
  m$doy <- lubridate::yday(m$date)
  withr::with_seed(123, expect_silent(pr2 <- calc_selection_weights(m, params = p, col_day = "doy")))

  expect_equal(dplyr::select(pr1, -"date"), pr2)

  # Check lognormal and offsets
  p <- sim_selection_weights(plot = FALSE, selection_fun = "lognorm")
  expect_error(calc_selection_weights(m, params = p, col_day = "doy"),
               "you must provide an `offset`")
  p <- sim_selection_weights(plot = FALSE, selection_fun = "lognorm", offset = 200)
  withr::with_seed(123, expect_silent(calc_selection_weights(m, params = p, col_day = "doy")))
})

