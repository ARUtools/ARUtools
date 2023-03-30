test_that("check_gps_files()", {


})

test_that("fmt_gps()", {

  # BARLT
  g1 <- dplyr::tibble(
    `Latitude (decimal degrees)` = c(45, 55),
    `Longitude (decimal degrees)` = c(-76, -84.3),
    `HH/MM` = c("07:30", "16:08"),
    `DD/MM/YY` = c("25/05/2021", "03/06/2021")) |>
    readr::type_convert() |>
    suppressMessages()

  # SongMeter
  g2 <- dplyr::tibble(
    `LAT` = c(45, 55),
    `...2` = c("N", "n"),
    `LON` = c(76, 84.3),
    `...3` = c("w", "W"),
    TIME = c("07:30", "16:08"),
    DATE =  c("2021-05-25", "2021-06-03")) |>
    readr::type_convert() |>
    suppressMessages()

  # Expect the same results
  for(i in list(g1, g2)) {
    expect_silent(f <- fmt_gps(i))
    expect_s3_class(f, "data.frame")
    expect_named(f, c("longitude", "latitude", "date", "date_time"))
    expect_equal(f$longitude, g1[[2]]) # Compare to g1
    expect_equal(f$latitude, g1[[1]])  # Compare to g1
    expect_equal(f$date, lubridate::as_date(c("2021-05-25", "2021-06-03")))
    expect_equal(f$date_time, lubridate::as_datetime(c("2021-05-25 07:30:00",
                                                       "2021-06-03 16:08:00")))
  }
})

test_that("clean_gps_files()", {


})

test_that("check_gps_dist()", {

})

test_that("clean_gps()", {


})
