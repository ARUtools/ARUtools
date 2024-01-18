#' Assign tasks for wildtrax interpretation
#'
#' @param wt_task_template_in Path to csv template downloaded from WilldTrax website listing all tasks
#' @param interp_hours_file Path to number of hours for each interpreter. Must be csv and include
#' @param wt_task_output_file Path to csv of output file for uploading to wildtrax. If left as NULL will not write file
#' @param interp_hours_column LazyEval column name with hours for interpreters
#' @param random_seed Integer. Random seed to select with. If left NULL will use timestamp
#'
#' @return Returns a list with a tibble of assigned tasks and a summary tibble.
#' @export
wt_assign_tasks <- function(wt_task_template_in, interp_hours_file, wt_task_output_file,
                            interp_hours_column, random_seed = NULL){

  tasks <- readr::read_csv(wt_task_template_in,
                    col_types = glue::glue_collapse(rep("c", 13)),
                    na = character())
  hours <- readr::read_csv(interp_hours_file) |>
    dplyr::filter(!is.na({{interp_hours_column}})) |>
    dplyr::mutate(phrs = {{interp_hours_column}}/sum({{interp_hours_column}}))

  if(rlang::is_null(random_seed)) {random_seed <- Sys.time()
  rlang::warn(glue::glue("random_seed left NULL. Using seed {as.numeric(random_seed)}"))

  }
  withr::with_seed(random_seed,{
    assigned_tasks <- tasks |>
      # dplyr::group_by(taskLength) |>
      dplyr::mutate(transcriber = sample(hours$transcriber,size = n(),
                                  replace =T, prob = hours$phrs ))
      # dplyr::ungroup()
  })

  if(!rlang::is_null(wt_task_output_file)){
    readr::write_csv(x = assigned_tasks, file = wt_task_output_file)}

  task_summary <-
  dplyr::summarize(
    assigned_tasks,hrs_assigned=sum(as.numeric(taskLength))/60/60 ,
    .by = transcriber) |>
    dplyr::left_join(hours, by = dplyr::join_by(transcriber)) |>
    dplyr::mutate(updated_hrs_remain = {{interp_hours_column}}-hrs_assigned) #|> gt::gt()

  return(list(assigned_tasks = assigned_tasks,
              task_summary =task_summary
              ))


}
