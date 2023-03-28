
test_that("add_sites()", {
  i <- clean_site_index(example_sites,
                        col_aru_id = "ARU",
                        col_site_id = "Sites",
                        col_date_time = c("Date_set_out", "Date_removed"),
                        col_coords = c("lon", "lat")) |>
    suppressMessages()

  # Add site_ids by date correctlyl
  expect_silent(m <- clean_metadata(project_files = example_files, quiet = TRUE))
  m2 <- dplyr::mutate(m, site_id = NA_character_)
  expect_message(m2 <- add_sites(m2, i, by = "aru_id")) |>
    suppressMessages()
  expect_equal(dplyr::arrange(m, file_name), dplyr::arrange(m2[names(m)], file_name))

  # Add lat/lon correctly
  dts <- dplyr::group_by(i, site_id, aru_id, latitude, longitude) |>
    dplyr::reframe(date = seq(date_start, date_end, by = "1 day"))
  # All site/aru/date/coord combos exist in site index data
  expect_equal(
    dplyr::anti_join(dplyr::select(m2, site_id, aru_id, date, latitude, longitude),
                     dts, by = c("site_id", "aru_id", "date")) |> nrow(),
    0)
})
