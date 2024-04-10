# in_tasks <- fs::file_temp("Input_task_file", ext = ".csv")
suppressWarnings({
  suppressMessages({
    task_template <-
      example_clean |>
      dplyr::transmute(
        location = site_id,
        recording_date_time = as.character(date_time),
        location = site_id,
        method = "1SPT",
        taskLength = "300",
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


# Test template data file
template_observers <- tibble::tribble(
  ~transcriber, ~hrs,
  "Charles Dickens", 5,
  "John Von Jovie", 1,
  "John Yossarian", 6.5,
  "Rodion Romanovich Raskolnikov", 2.3
)

usethis::use_data(template_observers, overwrite = TRUE)
