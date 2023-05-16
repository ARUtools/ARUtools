
test_that("create_pattern_XXXX()", {

  expect_silent(d <- create_pattern_date())
  expect_silent(t <- create_pattern_time())
  expect_silent(s <- create_pattern_dt_sep())

  p <- paste0(d, s, t)

  # Different separators
  test <- c("sdfs20210605T230000lkajsdlfj89", "aru2021_06_05T23_00_00_site2",
            "2021-06-05T23:00:00", "20210605T2300_siteX")
  expect_equal(stringr::str_extract(test, p),
               c("20210605T230000", "2021_06_05T23_00_00",
                 "2021-06-05T23:00:00", NA_character_)) # No seconds
  expect_silent(stringr::str_extract(test, p) %>% lubridate::ymd_hms())

  # Different order
  expect_silent(d <- create_pattern_date(order = "mdy", yr_digits = 2))
  p <- paste0(d, s, t)

  test <- c("sdfs061206T230000lkajsdlfj89", "aru06_05_10T23_00_00_site2",
            "06-05-21T23:00:00")
  expect_equal(stringr::str_extract(test, p),
               c("061206T230000", "06_05_10T23_00_00", "06-05-21T23:00:00"))
  expect_silent(stringr::str_extract(test, p) %>% lubridate::mdy_hms())
})
