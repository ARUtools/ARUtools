test_that("check_cols()", {

  col1 <- "cyl"
  col2 <- "mpg"
  col3 <- "testing"
  col4 <- NULL
  expect_silent(check_cols(mtcars, cols = c(col1, col2), name = "mtcars"))

  expect_error(check_cols(mtcars, cols = c(col1, col2, col3), name = "mtcars"),
               "Column 'testing' does not exist")

})
