test_that("wt_assign_tasks()", {
expect_error(wt_assign_tasks("Not a proper file path"))
wt_assign_tasks(task_template)
}
)
