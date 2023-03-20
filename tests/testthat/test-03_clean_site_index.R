test_that("clean_site_index()", {
  unlink("test.csv")
  readr::write_csv(site_meta, "test.csv")

  expect_error(clean_site_index("test.csv"), "Problems with data") |>
    suppressMessages()

  expect_silent(i1 <- clean_site_index(
    "test.csv",
    col_aru_id = "ARU",
    col_site_id = "Sites",
    col_date_time = c("Date_set_out", "Date_removed"),
    col_coords = NULL))

  expect_s3_class(i1, "data.frame")
  expect_named(i, c("site_id", "aru_id", "date_time_start", "date_time_end",
                    "date_start", "date_end"))
  expect_s3_class(i1[["date_time_start"]], "POSIXct")
  expect_s3_class(i1[["date_end"]], "Date")
  expect_s3_class(i1[["date_start"]], "Date")
  expect_s3_class(i1[["date_end"]], "Date")
  unlink("test.csv")

  expect_silent(i2 <- clean_site_index(
    site_meta,
    col_aru_id = "ARU",
    col_site_id = "Sites",
    col_date_time = c("Date_set_out", "Date_removed"),
    col_coords = NULL))

  expect_equal(i1, i2)

})

test_that("clean_site_index() coords", {

  expect_silent(i <- clean_site_index(
    site_meta,
    col_aru_id = "ARU",
    col_site_id = "Sites",
    col_date_time = c("Date_set_out", "Date_removed"),
    col_coords = c("lon", "lat")))

  expect_named(i, c("site_id", "aru_id", "date_time_start", "date_time_end",
                    "date_start", "date_end", "longitude", "latitude"))

  expect_equal(site_meta$lon, i$longitude)
  expect_equal(site_meta$lat, i$latitude)

})

test_that("clean_site_index() extra cols", {

  m <- dplyr::mutate(site_meta,
                     Plots = c("Plot1", "Plot1", "Plot2"),
                     Subplot = c("a", "a", "a"))

  expect_silent(i <- clean_site_index(
    m,
    col_aru_id = "ARU",
    col_site_id = "Sites",
    col_date_time = c("Date_set_out", "Date_removed"),
    col_coords = c("lon", "lat"),
    col_extra = c("Plots", "Subplot")))

  expect_named(i, c("site_id", "aru_id", "date_time_start", "date_time_end",
                    "date_start", "date_end", "longitude", "latitude",
                    "Plots", "Subplot"))

  expect_equal(m$Plots, i$Plots)
  expect_equal(m$Subplot, i$Subplot)


  expect_silent(i <- clean_site_index(
    m,
    col_aru_id = "ARU",
    col_site_id = "Sites",
    col_date_time = c("Date_set_out", "Date_removed"),
    col_coords = c("lon", "lat"),
    col_extra = c("plot" = "Plots", "sub" = "Subplot")))

  expect_named(i, c("site_id", "aru_id", "date_time_start", "date_time_end",
                    "date_start", "date_end", "longitude", "latitude",
                    "plot", "sub"))

  expect_equal(m$Plots, i$plot)
  expect_equal(m$Subplot, i$sub)

})

test_that("clean_site_index() overlapping dates", {

  m <- site_meta
  m$Sites <- "first"
  m$Date_removed[2] <- m$Date_set_out[3]

  expect_message(i <- clean_site_index(
    m,
    col_aru_id = "ARU",
    col_site_id = "Sites",
    col_date_time = c("Date_set_out", "Date_removed"),
    col_coords = c("lon", "lat")),
    "overlapping date ranges")

  expect_true(all(lubridate::hour(i$date_time_start) == 12))
  expect_true(all(lubridate::hour(i$date_time_end) == 12))
})

test_that("clean_site_index() single date", {
  expect_silent(i <- clean_site_index(
    site_meta,
    col_aru_id = "ARU",
    col_site_id = "Sites",
    col_date_time = c("Date_set_out"),
    col_coords = c("lon", "lat")))
})
