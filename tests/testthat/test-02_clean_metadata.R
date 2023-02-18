test_that("clean_metadata()", {

  expect_message(m <- clean_metadata(project_files = example_files,
                                     pattern_site = "apple|cabin|lake"),
                 "Extracting ARU info..") %>%
    expect_message("Extracting Dates and Times...")


  expect_equal(unique(m$ARU_id), c("BARLT10962", "S4A01234"))
  expect_equal(unique(m$type), "wav")
  expect_equal(unique(m$ARU_type), c("BarLT", "SongMeter"))
  expect_equal(unique(m$site), c("apple", "lake", "cabin"))
  expect_true(all(!is.na(m$date_time)))
  expect_true(all(!is.na(m$date)))

})

