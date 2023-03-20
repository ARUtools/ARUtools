test_that("date_join()", {
  x <- data.frame(site_id = sort(rep(LETTERS, 12)),
                  aru_id = rep(c("aa", "bb"), 6)) %>%
    dplyr::arrange(site_id, aru_id) %>%
    dplyr::mutate(date = sort(seq(lubridate::as_date("2020-06-01"),
                                  by = "1 day",
                                  length.out = dplyr::n())),
                  file = 1:dplyr::n())

  y <- dplyr::mutate(x, gps = rep(c(1,1,1,2,2,2), dplyr::n()/6)) %>%
    dplyr::group_by(site_id, aru_id, gps) %>%
    dplyr::summarize(date1 = min(date), date2 = max(date), .groups = "drop") %>%
    dplyr::mutate(latitude = seq(43, 45, length.out = dplyr::n()),
                  longitude = seq(-85, -75, length.out = dplyr::n()),
                  date_range = lubridate::interval(date1, date2)) %>%
    dplyr::arrange(site_id, aru_id, gps) %>%
    dplyr::select(-date1, -date2)


  expect_silent(j <- date_join(x, y, by = c("site_id", "aru_id"), id = "file"))
  expect_true(all(j[1, c("latitude", "longitude")] == y[1, c("latitude", "longitude")]))
  expect_true(all(j[1 + 3*3, c("latitude", "longitude")] == y[1 + 3, c("latitude", "longitude")]))
  expect_true(all(j[1 + 6*3, c("latitude", "longitude")] == y[1 + 6, c("latitude", "longitude")]))

  x <- dplyr::select(x, -aru_id)
  y <- dplyr::select(y, -aru_id)
  expect_silent(date_join(x, y, by = c("site_id"), id = "file"))
})
