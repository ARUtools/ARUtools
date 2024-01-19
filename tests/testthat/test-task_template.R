test_that("task_template",{
suppressWarnings({
  suppressMessages({
    task_template_wt <- wildRtrax::wt_make_aru_tasks(example_clean |>
                                                 mutate(
                                                   recording_date_time = date_time,
                                                   file_path = path, location = site_id,
                                                   length_seconds = 300),
                                               task_method = '1SPT', task_length = 300)
  })
})
expect_equal(task_template, task_template_wt)
}
)
