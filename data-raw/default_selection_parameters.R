## code to prepare `default_selection_parameters`
default_selection_parameters <- list(min_range = c(-70, 240), # Range of minutes relative to sun event
               doy_range = c(121, 201), # Range of day of year
               mean_min = 30, # Average minutes to sun event in selection
               sd_min = 60, # Standard deviation in distribution for
               mean_doy = 161, # Average day of year for selection
               sd_doy = 20, # Standard deviation of day of year for selection
               off=0, # Offset to shift for time of day.
               log_ = TRUE, # Log the density in the selection function?
               fun = "norm" # Selection function. Options are 'lognorm','norm', or 'cauchy'.
               )

usethis::use_data(default_selection_parameters, overwrite = TRUE)
