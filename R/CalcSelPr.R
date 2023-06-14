#' Simulate selection probabilities for GRTS
#'
#' @param parms List of parameters for simulation see ?ARUtools::default_selection_parameters for names and default values
#' @param selection_variable unquoted variable name of which options are psel, psel_doy, psel_tod, psel_std, psel_scaled, or psel_normalized. Default is psel_normalized
#' @param return_dat Logical - return just the data or print basic plot
#'
#' @import patchwork
#'
#' @return Returns either a data frame with selection probabilities or plots of selection probabiliies.
#' @export
#'
#' @examples
#' # gen_dens_sel_simulation(min =  -70:240,doy = 121:201,
#' #               parms = ARUtools::default_selection_parameters,
#' #             selection_variable = psel_normalized , return_dat=F)
gen_dens_sel_simulation <- function( parms=ARUtools::default_selection_parameters,
                                     selection_variable=psel_normalized,
                                     return_dat =F , ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }
  warn("Since version 0.4 default selection parameter in gen_dens_sel_simulation is psel_normalize,
         which ranges from 0 to 1. If you wish to base decisions here off the simulation,
         you can adjust the `selection_variable` paramter, which is an unquoted variable
         name of which options are
       psel, psel_doy, psel_tod, psel_std, psel_scaled, or psel_normalized")
  # min_fun <- switch (fun,
  #   "lognorm" = function(x, m, sd, log) dlnorm(x+off,log(m+off), sd, log),
  #   "norm" = dnorm,
  #   "cauchy"=dcauchy
  # )

  # dens_min <- min_fun(min, mean_min, sd_min, log = log_)
  # dens_doy <- dnorm(doy, mean_doy, sd_doy, log=log_)
  list2env(list(...), envir = environment())
  par_existing <- c( exists("min"), exists("mean_min"), exists("sd_min"),
  exists("doy"), exists("mean_doy"), exists("sd_doy"))
  suggest_existing <- c(exists("fun"), exists("log_"), exists("off"))
  if(all(par_existing)  ){
    warn("Use of individual named parameters is depreciated. Use`parms` parameter to set values for simulation")
    if(!all(suggest_existing)){
      if(!exists("fun")) fun <- "norm"
      if(!exists("log_")) log_ <- FALSE
      if(!exists("off")) off <-NULL
    }
    min_fun <- switch (fun,
                       "lognorm" = function(x, m, sd, log) dlnorm(x+off,log(m+off), sd, log),
                       "norm" = dnorm,
                       "cauchy"=dcauchy
    )
    dens_min <- min_fun(min, mean_min, sd_min, log = log_)
    dens_doy <- dnorm(doy, mean_doy, sd_doy, log=log_)
    all <- expand.grid(doy = doy, min = min) |>
      dplyr::mutate(
        psel_tod = min_fun(round(min,0), mean_min, sd_min, log = log_)/max(abs(dens_min)),
        psel_doy = dnorm(doy,mean= mean_doy, sd = sd_doy, log=log_)/max(abs(dens_doy)),
        psel = dplyr::case_when(log_~ exp( psel_tod + psel_doy),
                                !log_~ psel_tod * psel_doy),
        psel_scaled = psel/max(psel),
        date = ymd("2022-01-01") + doy)
  } else if(any(par_existing)){
    a_1 <- c("Use of individual named parameters is depreciated.",
             "x" = "If using old method must include all of the following as parameters: \n\r
            min, mean_min, sd_min, doy, mean_doy, sd_doy\n\r,
            and should include values for  log_, fun , & off",
             "i" = "Suggest to use`parms` parameter to set values for simulation")
    if(!exists('min'))  abort(a_1)
    if(exists('min')) if(!inherits(min, "function")) abort(a_1)
      }



  all <- expand.grid(doy = seq(parms$doy_range[[1]],parms$doy_range[[2]]),
                     min = seq(parms$min_range[[1]],parms$min_range[[2]]),
                     location=1) |>
    calc_sel_pr(ARU_ID_col = location,
                min_col = min,
                day_col = doy,
                parms = parms) |>
    dplyr::mutate(
      date = lubridate::ymd("2022-01-01") + doy)

  if(isTRUE(return_dat))(return(all))
  ggplot2::theme_set(ggplot2::theme_minimal(base_size = 14, base_family = "Roboto Condensed"))

  p1 <- ggplot2::ggplot(all, ggplot2::aes(date, psel_doy)) + ggplot2::geom_line() +
    ggplot2::labs(x="Date", y = "Selection probability (date)")
  p2 <- ggplot2::ggplot(all, ggplot2::aes(min, psel_tod)) + ggplot2::geom_line() +
    ggplot2::labs(x="Time to sun event", y = "Selection probability (time)")
  p3 <- ggplot2::ggplot(all,
               ggplot2::aes(date,
                   min,
                   fill =
                     {{selection_variable}}
               )
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(x="Date", y = "Time",
        fill = "Prob\nselection")



  (p1 + p2) / p3


}



#' Calculate Inclusion Probabilities
#'
#' @param .data Data frame with Date, times, and ARU IDs
#' @param ARU_ID_col ARU id column. Should not be quoted.
#' @param min_col Minutes column. Should not be quoted.
#' @param day_col Day column. Should not be quoted.
#' @param parms list of parameters. See defaults for examples.
#'                 Should include min_range, doy_range, mean_min, sd_min,
#'                mean_doy, sd_doy, off, log_, fun.
#'
#' @return   Returns .data with selection probability columns
#' @export
calc_sel_pr <- function(.data,ARU_ID_col, min_col, day_col, parms = list(min_range = c(-60, 300),
                                                                         doy_range = c(150, 180),
                                                                         mean_min = 30, sd_min = 30,
                                                              mean_doy = 0, sd_doy = 10,off=0,
                                                              log_ = FALSE, fun = "norm")){

  list2env(parms, envir = environment())
  min_fun <- switch (fun,
                     "lognorm" = function(x, m, sd, log) dlnorm(x+off,log(m+off), sd, log),
                     "norm" = dnorm,
                     "cauchy"=dcauchy
  )
  # min_range <- range(pull(.data,{{min_col}}) )
  # doy_range <- range(pull(.data,{{min_col}}) )
  dens_min <- min_fun(seq(min_range[[1]], min_range[[2]]), mean_min, sd_min, log = log_)
  dens_doy <- dnorm(seq(doy_range[[1]], doy_range[[2]]), mean_doy, sd_doy, log=log_)
  # browser()
  .data |>
    dplyr::filter({{day_col}} >= doy_range[[1]] &
             {{day_col}} <= doy_range[[2]] &
             {{min_col}}>= min_range[[1]] &
             {{min_col}}<= min_range[[2]]
             ) |>
    dplyr::mutate(psel_tod = min_fun(round({{min_col}},0), mean_min, sd_min, log = log_)/max(abs(dens_min)),
           psel_doy = dnorm({{day_col}},mean= mean_doy, sd = sd_doy, log=log_)/max(abs(dens_doy)),
           psel = dplyr::case_when(log_~ exp( psel_tod + psel_doy),
                            !log_~ psel_tod * psel_doy),
           psel_scaled = psel/max(psel)) |>
    dplyr::group_by({{ARU_ID_col}}) |>
    dplyr::mutate(psel_std = psel / max(psel),
                  psel_normalized =  pmax(1e-3,(psel-min(psel))/(max(psel)-min(psel)))) |>
    dplyr::ungroup()



}




 # gen_dens_sel_simulation(min =  seq(-60,60*4 ),
 #             mean_min =  -30,
 #             sd_min = 120,
 #             doy =  seq(152, 210),
 #             mean_doy = 170,sd_doy = 200, log_ = F,
 #             return_dat = F, fun = "norm", off=60
 #             )
#  test <- gen_dens_sel(min =  seq(-31,120 ),
#               mean_log_min =  log(250),
#               sdlog_min = 0.94,
#               doy =  seq(152, 210),
#               mean_doy = 170,sd_doy = 200, return_dat = T
#  )
#
#
#
# sigma_min <- 50
# sigma_doy <-  75
# cor_m_doy <- 0.8
#
# cov_dm <- sigma_doy*sigma_min*cor_m_doy
# Sigma <- matrix(c(sigma_doy^2, cov_dm, cov_dm, sigma_min^2), ncol=2)
#
# test$mat <- mvtnorm::dmvnorm(test[,1:2], mean = c(180, -4), sigma = Sigma)
# ggplot(test,
#        aes(doy,
#            min,
#            fill =
#              mat
#        )
# ) +
#   geom_tile() +
#   scale_fill_viridis_c()

