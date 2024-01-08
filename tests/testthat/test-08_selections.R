test_that("calc_sel_pr()", {
  m <- clean_metadata(project_files = example_files, quiet = TRUE)
  s <- clean_site_index(example_sites_clean,
                        col_date = c("date_time_start", "date_time_end"))
  m <- add_sites(m, s, quiet = TRUE)
  m <- calc_sun(m) |>
    dplyr::mutate(doy = lubridate::yday(date))

  p <- sel_pr_params(doy_range = c(110, 180))

  withr::with_seed(123, {
    expect_silent(pr1 <- calc_sel_pr(m, params = p))
  })

  expect_snapshot_value(pr1, style = "json2", tolerance = 0.0005)


  # Use DOY rather than date
  withr::with_seed(123, {
    m$doy <- lubridate::yday(m$date)
    expect_silent(pr2 <- calc_sel_pr(m, params = p, col_date = "doy"))
  })

  expect_equal(pr1, pr2)
})


test_that("gen_dens_sel_simulation()", {

  params <- ARUtools::default_selection_parameters
  names(params)[names(params) == "log_"] <- "return_log"

  withr::with_seed(
    123,
    expect_warning(g <- gen_dens_sel_simulation(#min =  -70:240,doy = 121:201,
      parms = params,
      selection_variable = "psel_normalized", return_data = FALSE))
  )
  vdiffr::expect_doppelganger("gen_dens_sel_simulation1", g)


  withr::with_seed(
    123,
    expect_warning(
      g <- gen_dens_sel_simulation(
        parms = list(min_range =  seq(-60, 60*4),
                     mean_min =  -30,
                     sd_min = 120,
                     doy_range =  seq(152, 210),
                     mean_doy = 170, sd_doy = 200, return_log = FALSE,
                     fun = "norm", off = 60 # TODO: Does off do anything?
        ),
        return_data = FALSE)
    )
  )
  vdiffr::expect_doppelganger("gen_dens_sel_simulation2", g)
})
