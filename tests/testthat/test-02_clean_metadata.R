
test_that("clean_metadata()", {

  expect_message(m <- clean_metadata(project_files = example_files),
                 "Extracting ARU info..") %>%
    expect_message("Extracting Dates and Times...")

  expect_s3_class(m, "data.frame")
  expect_named(m, c("file_name", "type", "path", "aru_type", "aru_id", "site_id",
                    "date_time", "date"))
  expect_equal(nrow(m), length(example_files))
  expect_equal(unique(m$aru_id), sort(unique(example_clean$aru_id)))
  expect_equal(unique(m$type), "wav")
  expect_equal(unique(m$aru_type), c("BarLT", "SongMeter"))
  expect_equal(sort(unique(m$site_id)), sort(unique(example_clean$site_id)))
  expect_true(all(!is.na(m$date_time)))
  expect_true(all(!is.na(m$date)))

})
