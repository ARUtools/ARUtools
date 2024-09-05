sites <- paste0("P", stringr::str_pad(1:10, width = 2, pad = "0"), "_1")

# Example sites list
example_sites <- data.frame(
  Sites = sites,
  Date_set_out = c(
    "2020-05-01", "2020-05-03", "2020-05-05", "2020-05-05", "2020-05-06",
    "2020-05-08", "2020-05-08", "2020-05-10", "2020-05-10", "2020-05-10"
  ),
  Date_removed = c(
    "2020-05-03", "2020-05-05", "2020-05-06", "2020-05-07", "2020-05-07",
    "2020-05-09", "2020-05-10", "2020-05-11", "2020-05-11", "2020-05-11"
  ),
  ARU = c(
    "BARLT10962", "S4A01234", "BARLT10962", "BARLT11111", "BARLT10962",
    "BARLT10962", "S4A01234", "BARLT10962", "S4A02222", "S4A03333"
  ),
  lon = c(-85.03, -87.45, -90.38, -85.53, -88.45, -90.08, -86.03, -84.45, -91.38, -90.00),
  lat = c(50.01, 52.68, 48.99, 45.00, 51.05, 52.00, 50.45, 48.999, 45.0, 50.01),
  Plots = rep(c("Plot1", "Plot1", "Plot2", "Plot2", "Plot3"), 2),
  Subplot = rep(c("a", "a", "a", "a", "b"), 2)
)

usethis::use_data(example_sites, overwrite = TRUE)

# Example file list
random <- letters[c(1, 10, 15)]

dates <- example_sites |>
  select(Sites, ARU, Date_set_out, Date_removed) |>
  group_by(Sites, ARU) |>
  reframe(date = seq(lubridate::as_date(Date_set_out) + lubridate::days(1),
    lubridate::as_date(Date_removed),
    by = "1 day"
  )) |>
  mutate(
    tz_offset = "-0400",
    date_time = date +
      rep(lubridate::minutes(c(300, 320, 325, 450, 600, 300, 205)), 2),

    date_time = format(date_time, "%Y%m%dT%H%M%S"),
    date_time = dplyr::if_else(str_detect(ARU, "BARLT"),
                               paste(date_time, tz_offset, sep=""),
                               date_time)
  )


example_files <- tidyr::expand_grid(random, select(example_sites, Sites, ARU)) |>
  dplyr::left_join(dates, by = c("Sites", "ARU"), relationship = "many-to-many") |>
  dplyr::rowwise() |>
  dplyr::mutate(f = paste0(random, "_", ARU, "_", Sites, "/", Sites, "_", date_time, "_ARU.wav")) |>
  dplyr::pull(f)

usethis::use_data(example_files, overwrite = TRUE)


# Example meta data for sun
example_sites_clean <- clean_site_index(
  example_sites,
  name_site_id = "Sites",
  name_aru_id = "ARU",
  name_date_time = c("Date_set_out", "Date_removed"),
  name_coords = c("lon", "lat")
)

usethis::use_data(example_sites_clean, overwrite = TRUE)

example_clean <- clean_metadata(project_files = example_files) |>
  add_sites(example_sites_clean)

usethis::use_data(example_clean, overwrite = TRUE)
