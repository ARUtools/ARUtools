test_that("clean_site_index()", {
  unlink("test.csv")
  readr::write_csv(site_meta, "test.csv")

  expect_error(clean_site_index("test.csv"), "Problems with data") |>
    suppressMessages()

  expect_silent(i <- clean_site_index(
    "test.csv",
    col_aru_id = "ARU",
    col_site_id = "Sites",
    col_dates = c("Date_set_out", "Date_removed"),
    col_coords = NULL))

  expect_s3_class(i, "data.frame")
  expect_named(i, c("site_id", "date_start", "date_end", "aru_id"))
  expect_s3_class(i[["date_start"]], "Date")
  expect_s3_class(i[["date_end"]], "Date")

  expect_silent(i <- clean_site_index(
    "test.csv",
    col_aru_id = "ARU",
    col_site_id = "Sites",
    col_dates = c("Date_set_out", "Date_removed"),
    col_coords = c("lon", "lat")))
  expect_named(i, c("site_id", "date_start", "date_end", "aru_id",
                    "longitude", "latitude"))
  unlink("test.csv")
})

test_that("clean_metadata()", {

  expect_message(m <- clean_metadata(project_files = example_files,
                                     pattern_site = "apple|cabin|lake"),
                 "Extracting ARU info..") %>%
    expect_message("Extracting Dates and Times...")

  expect_s3_class(m, "data.frame")
  expect_named(m, c("dir", "file_name", "type", "aru_type", "aru_id", "site_id",
                    "date_time", "date"))
  expect_equal(nrow(m), length(example_files))
  expect_equal(unique(m$aru_id), c("BARLT10962", "S4A01234"))
  expect_equal(unique(m$type), "wav")
  expect_equal(unique(m$aru_type), c("BarLT", "SongMeter"))
  expect_equal(unique(m$site_id), c("apple", "cabin", "lake"))
  expect_true(all(!is.na(m$date_time)))
  expect_true(all(!is.na(m$date)))

})

test_that("clean_metadata() - site_index", {
  i <- clean_site_index(site_meta,
                        col_aru_id = "ARU",
                        col_site_id = "Sites",
                        col_dates = c("Date_set_out", "Date_removed"),
                        col_coords = NULL)

  expect_message(m <- clean_metadata(project_files = example_files,
                                     site_index = i),
                 "Extracting ARU info..") |>
    expect_message("Extracting Dates and Times...") |>
    expect_message("Supplementing") |>
    expect_message("Identified possible problems")

  expect_equal(nrow(m), length(example_files))
  expect_equal(unique(m$aru_id), c("BARLT10962", "S4A01234"))
  expect_equal(unique(m$type), "wav")
  expect_equal(unique(m$aru_type), c("BarLT", "SongMeter"))
  expect_equal(sort(unique(m$site_id)), c("first", "second", "third"))
  expect_true(all(!is.na(m$date_time)))
  expect_true(all(!is.na(m$date)))

  days1 <- seq(as.Date("2020-01-01"), as.Date("2020-01-04"), by = "1 day")
  days2 <- seq(as.Date("2020-01-05"), as.Date("2020-01-07"), by = "1 day")
  days3 <- seq(as.Date("2020-01-10"), as.Date("2020-01-11"), by = "1 day")
  days4 <- seq(as.Date("2020-01-08"), as.Date("2020-01-09"), by = "1 day")

  expect_true(all(m$site_id[m$date %in% c(days1, days4)] %in% c(NA_character_, "first")))
  expect_true(all(m$site_id[m$date %in% days2] %in% c("first", "second")))
  expect_true(all(m$site_id[m$date %in% days3] %in% c(NA_character_, "third")))
})
