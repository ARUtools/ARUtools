
test_that("check_ext()", {
  expect_silent(check_ext("csv", "csv"))
  expect_silent(check_ext("csv", c("xlsx", "csv")))
  expect_error(check_ext("csv", "txt"), "File extension must be")
  expect_error(check_ext("csv", c("txt", "xlsx")), "File extension must be")
})

test_that("check_value()", {
  expect_silent(check_value(1, "x", type = "numeric"))
  expect_error(check_value("1", "x", type = "numeric"))
  expect_silent(check_value(TRUE, "x", type = "logical"))
  expect_error(check_value("TRUE", "x", type = "logical"))
  expect_silent(check_value("birds", "x", type = "text"))
  expect_error(check_value(1, "x", type = "text"), "must be text")

  expect_silent(check_value(1:3, "x", type = "numeric", n = 3))
  expect_silent(check_value(NULL, "x", type = "numeric", not_null= FALSE))
  expect_silent(check_value(1, "x", opts = 1, type = "numeric"))
  expect_error(check_value(1:3, "x", type = "numeric", n = 1), "must have 1")
  expect_error(check_value(NULL, "x", type = "numeric"), "cannot be `NULL`")
  expect_error(check_value(1, "x", opts = 2, type = "numeric"), "must be among")
})

test_that("check_cols()", {

  col1 <- "cyl"
  col2 <- "mpg"
  col3 <- "testing"
  col4 <- NULL
  expect_silent(check_cols(mtcars, cols = c(col1, col2), name = "mtcars"))

  expect_error(check_cols(mtcars, cols = c(col1, col2, col3), name = "mtcars"),
               "Column 'testing' does not exist")

  expect_error(check_cols(mtcars, name = "mtcars", dates = TRUE),
               "No date or date range columns")


})

test_that("check_dates()", {
  s <- dplyr::mutate(site_meta, date = lubridate::ymd(Date_set_out),
                     date_time = as.POSIXct(date))

  expect_silent(check_dates(s, "date"))
  expect_silent(check_dates(s, c("date", "date_time")))
  expect_error(check_dates(s, cols = "Date_set_out"), "Problems with")
  expect_error(check_dates(s, cols = c("date", "Date_set_out")), "Problems with")

})

test_that("check_df_file()", {
  expect_silent(check_df_file("test.xlsx"))
  expect_silent(check_df_file(mtcars))
  expect_error(check_df_file(Sys.Date()))
})

test_that("check_date_joins()", {
  df <- data.frame(date = "2020-01-01")
  expect_message(v <- check_date_joins(df, by_date = "date"), "`date` using buffers")
  expect_equal(v, "date")
  expect_error(check_date_joins(df, by_date = "date_time"), "Cannot find")

  df <- data.frame(date_time = "2020-01-01 00:00:00")
  expect_message(v <- check_date_joins(df, by_date = "date_time"),
                 "`date_time` using buffers")
  expect_equal(v, "date_time")
  expect_error(check_date_joins(df, by_date = "date"), "Cannot find")

  df <- data.frame(date_start = "2020-01-01", date_end = "2020-02-01")
  expect_message(v <- check_date_joins(df, by_date = "date"),
                 "`date_start` and `date_end`")
  expect_equal(v, c("date_start", "date_end"))
  expect_error(check_date_joins(df, by_date = "date_time"), "Cannot find")

  df <- data.frame(date_time_start = "2020-01-01 00:00:00",
                   date_time_end = "2020-01-01 05:00:00")
  expect_message(v <- check_date_joins(df, by_date = "date_time"),
                 "`date_time_start` and `date_time_end`")
  expect_equal(v, c("date_time_start", "date_time_end"))
  expect_error(check_date_joins(df, by_date = "date"), "Cannot find")

})

test_that("check_tz()", {
  expect_silent(check_tz("local"))
  expect_silent(check_tz("America/Winnipeg"))
  expect_error(check_tz("lsdjkf/skdjfl"))
})

