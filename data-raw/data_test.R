
# Example file list
sites <- paste0("P", stringr::str_pad(1:10, width = 2, pad = "0"), "_1")
id <- c("BARLT10962", "S4A01234")
dts <- seq(lubridate::as_datetime("2020-05-01"), length.out = 10, by = "1 day")
dts <- dts + lubridate::minutes(c(300, 320, 325, 450, 600, 310, 333, 320, 300, 300))
dts <- format(dts, "%Y%m%dT%H%M%S")
random <- letters[c(1,10,15)]

example_files <- tidyr::expand_grid(d, random, id, sites, dts) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(f = paste0(random, "_", id, "_", sites, "/", sites, "_", dts, "_ARU.wav")) %>%
  dplyr::pull(f)

usethis::use_data(example_files, overwrite = TRUE)

# Example sites list
site_meta <- data.frame(
  Sites = sites,
  Date_set_out = c("2020-05-01", "2020-05-03", "2020-05-05", "2020-05-05", "2020-05-06",
                   "2020-05-08", "2020-05-08", "2020-05-10", "2020-05-10", "2020-05-10"),
  Date_removed = c("2020-05-03", "2020-05-05", "2020-05-06", "2020-05-07", "2020-05-07",
                   "2020-05-09", "2020-05-10", "2020-05-11", "2020-05-11", "2020-05-11"),
  ARU = rep(c("BARLT10962", "S4A01234", "BARLT10962", "BARLT10962", "BARLT10962"),
            2),
  lat = c(50.01, 52.68, 48.99, 45.00, 51.05, 52.00, 50.45, 48.999, 45.0, 50.01),
  lon = c(-85.03, -87.45, -90.38, -85.53, -88.45, -90.08, -86.03, -84.45, -91.38, 90.00))
usethis::use_data(site_meta, overwrite = TRUE)

# Example meta data for sun
clean <- clean_metadata(project_files = example_files) |>
  add_sites(clean_site_index(site_meta, col_site_id = "Sites",
                             col_aru_id = "ARU",
                             col_date_time = c("Date_set_out", "Date_removed"),
                             col_coords = c("lon", "lat")))
usethis::use_data(clean, overwrite = TRUE)
