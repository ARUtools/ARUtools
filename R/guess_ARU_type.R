guess_ARU_type <- function(path){
  pattern_aru_type <- c(
    "barlt" = "BAR-LT",
    "SMM" = "Song Meter Mini",
    "SM(\\d)" = "Song Meter \\1",
    "S(\\d)A" = "Song Meter \\1"
  )
  model_guess <- ARUtools:::extract_replace(path, pattern_aru_type)
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
