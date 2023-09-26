#' Run the model function
#'
#' @param df Data frame to use with ARU information and selection probabilities
#' @param N Number of samples per stratum
#' @param os Oversample proportion
#' @param seed random number seed
#' @param strat_ Stratum ID. This is the column grts runs on
#' @param selprob_id Selection probability column
#' @param x Column in df for x (I've been using doy)
#' @param y Column in df for y -time of day or time to sunrise/sunset
#'
#' @return A sampling run from grts
#' @export
#'
fun_aru_samp <- function(df, N, os, seed, strat_, selprob_id, x, y, ...) {
    warn("Since version 0.4 default selection parameter in gen_dens_sel_simulation is psel_normalize,
         which ranges from 0 to 1. If you wish to base decisions here off the simulation,
         you can adjust the `selection_variable` paramter, which is an unquoted variable name of which
         options are psel, psel_doy, psel_tod, psel_std, psel_scaled, or psel_normalized")
    arus <- df |>
      dplyr::select({{ strat_ }}) |>
      dplyr::distinct() # morningChorus
    arus <- arus[[strat_]]
    # print(arus)
    if(packageVersion("spsurvey")<5){
    Stratdsgn <- vector(mode = "list", length = length(arus))
    names(Stratdsgn) <- arus
    for (a in arus) {
      Stratdsgn[[a]] <- list(
        panel = c(PanelOne = N),
        over = N * os,
        seltype = "Continuous"
      )
    }


    set.seed(seed)

    samp <- spsurvey::grts(
      design = Stratdsgn,
      DesignID = "ARU_Sample",
      type.frame = "finite",
      stratum_var = strat_, # "ARU_ID",
      # type.frame = "area",
      src.frame = "att.frame", # "shapefile",
      # in.shape="output/Sample_Frame_w_Legacy_LCC",
      att.frame = df, # morningChorus,
      mdcaty = selprob_id, # "psel_tod",
      xcoord = x, #' doy',
      startlev = 1,
      id = FileID,
      ycoord = y, # "min_to_Sunrise",
      shapefile = F
    )
    return(samp)
  }
  if(packageVersion("spsurvey")>=5){
  sf_df <- df |>
      sf::st_as_sf(coords = c(paste0(x),paste0(y)), crs = 3395)

  # browser()
  mindis <-  NULL
  maxtry <-  10
  DesignID <-  "Sample"
  list2env(list(...), envir = environment())

  print(c(mindis, maxtry, DesignID))
  # browser()
  if(is.data.frame(N)){
    if(all(c("N", "n_os") %in% names(N),arus %in% N[[strat_]] )  ) {
    Stratdsgn <- N$N
    n_os <- N$n_os
    names(Stratdsgn) <- names(n_os) <- N[[strat_]]
    } else{abort(c("Failed to parse N as data.frame.",
                   "x" = glue::glue("Need to include columns named 'N', 'n_os', and '{strat_}'"),
                   "i" = glue::glue("Check format of N as a data frame and ensure {strat_} is included in both df and N."))) }
  }else{
  if(length(N)==1){
  Stratdsgn <- rep(N, length(arus))
  if(length(os)==1){
    if(os==0) n_os <- NULL
    else n_os <-  rep(round(N*os), length(arus))
  } else{n_os <- os}
  if(!is.null(n_os))names(Stratdsgn) <- names(n_os) <- arus
  else names(Stratdsgn)  <- arus

  } else if ( all(arus %in% names(N)) ){
    Stratdsgn <- N
    if(length(os)==1){
    if(n_os==0){ n_os <- NULL
    } else  n_os <- lapply(FUN = function(x) x * os, X = Stratdsgn )
    } else if (!is_null(names(N)) & all(arus %in% names(N)) ){
      n_os <- os
    } else {simpleError("OS should either be single value or list with all strata ID. Not all Strata found in OS and OS has length >1")}
  } else {simpleError("N should either be single value or list with all strata ID. Not all Strata found in N and N has length >1")}
  }


  set.seed(seed)
  samp <- spsurvey::grts(sframe = sf_df,n_over = n_os,
                         n_base = Stratdsgn,
                         stratum_var = paste0(strat_),mindis = mindis,
                         DesignID = DesignID,
                         maxtry = maxtry,
                         # caty_n = stratum_var,
                         aux_var =  selprob_id)



  return(samp)
  }
}
