test_that("clean_site_index()", {

  # Text file
  unlink("test.csv")
  readr::write_csv(example_sites, "test.csv")

  expect_error(clean_site_index("test.csv"), "Problems with data") |>
    suppressMessages()

  expect_message(i1 <- clean_site_index(
    "test.csv",
    col_aru_id = "ARU",
    col_site_id = "Sites",
    col_date_time = c("Date_set_out", "Date_removed"),
    col_coords = c("lon", "lat")),
    "overlapping date ranges")

  expect_s3_class(i1, "data.frame")
  expect_named(i1, c("site_id", "aru_id", "date_time_start", "date_time_end",
                    "date_start", "date_end", "longitude", "latitude"))
  expect_s3_class(i1[["date_time_start"]], "POSIXct")
  expect_s3_class(i1[["date_end"]], "Date")
  expect_s3_class(i1[["date_start"]], "Date")
  expect_s3_class(i1[["date_end"]], "Date")
  expect_equal(example_sites$lon, i1$longitude)
  expect_equal(example_sites$lat, i1$latitude)
  unlink("test.csv")

  # Data Frame
  expect_message(i2 <- clean_site_index(
    example_sites,
    col_aru_id = "ARU",
    col_site_id = "Sites",
    col_date_time = c("Date_set_out", "Date_removed"),
    col_coords = c("lon", "lat")),
    "overlapping date ranges")

  expect_equal(i1, i2)

  # Tibble
  expect_message(i3 <- clean_site_index(
    dplyr::as_tibble(example_sites),
    col_aru_id = "ARU",
    col_site_id = "Sites",
    col_date_time = c("Date_set_out", "Date_removed"),
    col_coords = c("lon", "lat")),
    "overlapping date ranges")

  expect_equal(i1, i3)

  # sf
  example_sites_sf <- sf::st_as_sf(example_sites, coords = c("lon", "lat"),
                                   crs = 4326)
  expect_message(i4 <- clean_site_index(
    example_sites_sf,
    col_aru_id = "ARU",
    col_site_id = "Sites",
    col_date_time = c("Date_set_out", "Date_removed")),
    "overlapping date ranges")

  expect_equal(sf::st_drop_geometry(i4),
               dplyr::select(i2, -"longitude", -"latitude"))
})

test_that("clean_site_index() extra cols", {

  expect_message(i <- clean_site_index(
    example_sites,
    col_aru_id = "ARU",
    col_site_id = "Sites",
    col_date_time = c("Date_set_out", "Date_removed"),
    col_coords = c("lon", "lat"),
    col_extra = c("Plots", "Subplot")))

  expect_named(i, c("site_id", "aru_id", "date_time_start", "date_time_end",
                    "date_start", "date_end", "longitude", "latitude",
                    "Plots", "Subplot"))

  expect_equal(example_sites$Plots, i$Plots)
  expect_equal(example_sites$Subplot, i$Subplot)

  expect_message(i <- clean_site_index(
    example_sites,
    col_aru_id = "ARU",
    col_site_id = "Sites",
    col_date_time = c("Date_set_out", "Date_removed"),
    col_coords = c("lon", "lat"),
    col_extra = c("plot" = "Plots", "sub" = "Subplot")))

  expect_named(i, c("site_id", "aru_id", "date_time_start", "date_time_end",
                    "date_start", "date_end", "longitude", "latitude",
                    "plot", "sub"))

  expect_equal(example_sites$Plots, i$plot)
  expect_equal(example_sites$Subplot, i$sub)

})

test_that("clean_site_index() overlapping dates", {

  m <- example_sites
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
    example_sites,
    col_aru_id = "ARU",
    col_site_id = "Sites",
    col_date_time = c("Date_set_out"),
    col_coords = c("lon", "lat")))
})

test_that("clean_site_index() date/times", {
  expect_silent(clean_site_index(example_sites_clean,
                                 col_date_time = c("date_time_start")))
})

test_that("clean_site_index() errors etc.", {

  e <- dplyr::mutate(example_sites_clean, date = "2020-05-06 01:00:00")
  expect_silent(clean_site_index(e))

  e <- dplyr::mutate(example_sites_clean, date = "14/05/2020")
  expect_error(clean_site_index(e), "not a Date or Date-Time column")

})

