#' Try to guess the ARU type from a file path
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
guess_ARU_type <- function(path){
   model_guess <- ARUtools:::extract_replace(path, get_pattern_aru_type())
  company_guess <- dplyr::case_when(
    is.na(model_guess)~NA_character_,
    model_guess=="BAR-LT"~"Frontier Labs",
    stringr::str_detect(model_guess,
                        "Song Meter")~"Wildlife Acoustics",
    TRUE~NA_character_)

  return(dplyr::tibble(manufacturer = company_guess,
                       model = model_guess,
                       aru_type = stringr::str_remove_all(model_guess, "-|\\s|\\d")))
}
