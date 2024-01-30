# in_tasks <- fs::file_temp("Input_task_file", ext = ".csv")
suppressWarnings({
  suppressMessages({
    task_template <-
      example_clean |>
      dplyr::transmute(
        location = site_id,
        recording_date_time = date_time,
        location = site_id,
        method = "1SPT",
        taskLength = 300,
        status = "New",
        transcriber = "",
        rain = "",
        wind = "",
        industryNoise = "",
        otherNoise = "",
        audioQuality = "",
        taskComments = "",
        internal_task_id = ""
        )
  })
})


usethis::use_data(task_template, overwrite = TRUE)
