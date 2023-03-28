# Data for tests ---------------------
expect_silent({
  dts <- dplyr::tibble(date = lubridate::as_date(c("2022-06-27", "2021-08-10")),
                       longitude = c(-94.10, -93.00),
                       latitude = c(52.02, 51.90))
})

test_that("calc_ss()", {

  # Compared to https://gml.noaa.gov/grad/solcalc/ ==> Good

  expect_silent(ss <- calc_ss(dts, tz = "America/Toronto"))
  expect_named(ss, c("date", "longitude", "latitude", "sunrise", "sunset"))
  expect_equal(ss[, 1:3], dts)

  # Results in UTC but correct 'local' time
  expect_equal(ss$sunrise,
               lubridate::as_datetime(c("2022-06-27 05:59:28",
                                        "2021-08-10 06:50:07"), tz = "UTC"))
  expect_equal(ss$sunset,
               lubridate::as_datetime(c("2022-06-27 22:41:50",
                                        "2021-08-10 21:47:13"), tz = "UTC"))

  # Handles different time zones
  expect_silent(ss2 <- calc_ss(dts, tz = "America/Winnipeg"))
  expect_equal(ss, dplyr::mutate(ss2, sunrise = sunrise + lubridate::hours(1),
                                 sunset = sunset + lubridate::hours(1)))

  # Suffix
  expect_silent(ss <- calc_ss(dts, tz = "America/Toronto", suffix = "_test"))
  expect_named(ss, c("date_test", "longitude", "latitude",
                     "sunrise_test", "sunset_test"))

})

test_that("calc_all_ss()", {
  expect_silent(ss <- calc_all_ss(dts, tz = "America/Toronto"))
  expect_named(ss, c("date", "longitude", "latitude", "sunrise", "sunset",
                     "date_before", "sunrise_before", "sunset_before",
                     "date_after", "sunrise_after", "sunset_after"))

  # Expect dates sequential
  expect_true(all(ss$date == dts$date))
  expect_true(all(ss$date_before == ss$date - lubridate::days(1)))
  expect_true(all(ss$date == ss$date_after - lubridate::days(1)))

  # Expect each day sunrise/sunset earlier (both dates after solstice)
  expect_true(all(ss$sunrise_after > ss$sunrise))
  expect_true(all(ss$sunrise > ss$sunrise_before))
  expect_true(all(ss$sunset_after > ss$sunset))
  expect_true(all(ss$sunset > ss$sunset_before))
})

test_that("sun_diff()", {
  t <- lubridate::as_datetime(c("2022-06-27 05:59:28",
                                "2022-06-27 06:30:00"))

  expect_silent(d1 <- sun_diff(t[1], t[2]))
  expect_type(d1, "double")
  expect_equal(d1, 30.53333, tolerance = 0.001)

  expect_silent(d2 <- sun_diff(t[2], t[1]))
  expect_equal(d1, d2)

})

test_that("calc_ss_diff()", {
  ss <- calc_all_ss(dts, tz = "America/Toronto") |>
    mutate(date_time = lubridate::as_datetime(paste0(date, " 06:30:00")))

  expect_silent(diff <- calc_ss_diff(ss))
  expect_true(all(c("t2sr", "t2sr_day_of", "t2sr_before", "t2sr_after",
                    "t2ss", "t2ss_day_of", "t2ss_before", "t2ss_after", "doy") %in%
                    names(diff)))
  expect_equal(diff$t2sr, pmin(diff$t2sr_day_of, diff$t2sr_after, diff$t2sr_before))
  expect_equal(diff$t2ss, pmin(diff$t2ss_day_of, diff$t2ss_after, diff$t2ss_before))
  expect_true(all(select(diff, starts_with("t2")) < 48*60)) # at most two day aways
})

test_that("calc_sun()", {

  # "local" timezone from lat/lon
  expect_silent(s1 <- calc_sun(clean))
  expect_equal(clean, dplyr::select(s1, -"tz", -"t2sr", -"t2ss"))
  expect_equal(unique(s1$tz), c(NA_character_, "America/Toronto",
                               "America/Detroit", "America/Winnipeg"))

  # Same sunrise/sunset for each unique combo of date, loc, and tz
  expect_equal(
    dplyr::distinct(s1, date, longitude, latitude, tz, t2sr, t2ss, t2event) |> nrow(),
    dplyr::distinct(s1, date, longitude, latitude, tz) |> nrow())

  # Specified timezone
  expect_silent(s2 <- calc_sun(clean, aru_tz = "America/Toronto"))
  expect_equal(clean, dplyr::select(s2, -"tz", -"t2sr", -"t2ss", -"t2event"))
  expect_equal(unique(s2$tz), "America/Toronto")

  # Same sunrise/sunset for each unique combo of date and loc
  expect_equal(
    dplyr::distinct(s2, date, longitude, latitude, t2sr, t2ss) |> nrow(),
    dplyr::distinct(s2, date, longitude, latitude) |> nrow())

  # Expect offsets compared to "local" timezones
  i <- which(s1$tz == "America/Winnipeg")
  expect_equal(s1$t2sr[i], s2$t2sr[i] - 60)

  # Expect others to be the same
  expect_equal(s1$t2sr[-i], s2$t2sr[-i])

})
