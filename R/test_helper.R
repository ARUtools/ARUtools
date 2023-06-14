test_gps <- function(lat = "Latitude (decimal degrees)",
                     lon = "Longitude (decimal degrees)",
                     time = "HH:MM:SS",
                     date = "DD/MM/YY", skips = 2, path = "gps.csv") {

  p <- testthat::test_path(path)

  meta <- rep("GPS log file", skips - 1)
  for(i in p) {
    readr::write_lines(
      c(meta,
        paste(lat, lon, time, date, sep = ", "),
        "45,  -76,  07:30:00, 25/05/2021",
        "55,  -84.3, 16:08:00, 03/06/2021"),
      i)
  }

  p
}

temp_files <- function() {
  purrr::walk(fs::path_temp(example_files),
              ~{fs::dir_create(dirname(.x)); writeLines("test", .x)})

  fs::path_temp(example_files)
}
