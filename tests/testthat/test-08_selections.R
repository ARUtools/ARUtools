
test_that("simulate_selection_probs()", {

  # Return params
  expect_silent(g <- simulate_selection_probs(plot = FALSE))
  expect_type(g, "list")
  expect_named(g, c("min_range", "min_mean", "min_sd",
                    "day_range", "day_mean", "day_sd", "offset",
                    "return_log", "selection_fun"))

  expect_silent(g <- simulate_selection_probs(selection_fun = "lognorm", offset = 71, plot = FALSE))
  expect_silent(g <- simulate_selection_probs(selection_fun = "cauchy", plot = FALSE))
  expect_silent(g <- simulate_selection_probs(return_log = FALSE, plot = FALSE))

  # Plots - Defaults
  withr::with_seed(123, expect_silent(g <- simulate_selection_probs(return_params = FALSE)))
  vdiffr::expect_doppelganger("simulate_selection_probs1", g)

  # Plots - Change defaults
  withr::with_seed(
    123,
    expect_silent(g <- simulate_selection_probs(
      min_range = c(-70, 240),
      day_range = c(121, 201),
      day_sd = 20,
      return_log = TRUE,
      selection_var = "psel_normalized",
      return_params = FALSE))
  )
  vdiffr::expect_doppelganger("simulate_selection_probs2", g)

  withr::with_seed(
    123,
    expect_silent(
      g <- simulate_selection_probs(
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
  vdiffr::expect_doppelganger("simulate_selection_probs3", g)
})


test_that("calc_selection_probs()", {

  m <- clean_metadata(project_files = example_files, quiet = TRUE)
  s <- clean_site_index(example_sites_clean,
                        col_date = c("date_time_start", "date_time_end"))
  m <- add_sites(m, s, quiet = TRUE)
  m <- calc_sun(m)

  p <- simulate_selection_probs(day_range = c(110, 180),
                                min_range = c(-60, 300),
                                min_mean = 30, min_sd = 30,
                                day_mean = 0, day_sd = 10,
                                selection_fun = "norm",
                                return_log = FALSE,
                                plot = FALSE)

  withr::with_seed(123, expect_silent(pr1 <- calc_selection_probs(m, params = p)))
  expect_snapshot_value(pr1, style = "json2", tolerance = 0.0005)

  # Use DOY rather than date
  m$doy <- lubridate::yday(m$date)
  withr::with_seed(123, expect_silent(pr2 <- calc_selection_probs(m, params = p, col_day = "doy")))

  expect_equal(dplyr::select(pr1, -"date"), pr2)
})

