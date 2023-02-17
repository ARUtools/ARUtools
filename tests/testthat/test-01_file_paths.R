
test_that("file path extraction", {


  f <- c("20210605_site13_230000", "2021_06_05_site13_23_00_00",
         "2021-06-05_site13_23:00:00", "20210605_site13_2300")

  expect_equal(stringr::str_extract(f, pattern_date),
               c("20210605", "2021_06_05", "2021-06-05", "20210605"))


  # Assume site removed first
  f <- c("20210605__230000", "2021_06_05__23_00_00",
         "2021-06-05__23:00:00", "20210605__2300")

  expect_equal(stringr::str_extract(f, pattern_time),
               c("230000", "23_00_00", "23:00:00", "2300"))


})
