
d <- paste0("P", stringr::str_pad(1:10, width = 2, pad = "0"))
id <- c("BARLT10962", "S4A01234")
sites <- c("apple", "lake", "cabin")
dts <- seq(as.POSIXct("2020-01-01 00:00:00"), length.out = 10, by = "1614 min") %>%
  format("%Y%m%dT%H%M%S")
random <- letters[c(1,10,15)]

example_files <- tidyr::expand_grid(d, random, id, sites, dts) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(f = paste0(d, "/", random, "_", id, "_", sites, "/", sites, "_", dts, "_ARU.wav")) %>%
  dplyr::pull(f)

usethis::use_data(example_files, overwrite = TRUE)


site_meta <- data.frame(Sites = c("first", "second", "third"),
                        Date_set_out = c("2020-01-01", "2020-01-05", "2020-01-10"),
                        Date_removed = c("2020-01-09", "2020-01-07", "2020-01-11"),
                        ARU = c("BARLT10962", "S4A01234", "BARLT10962"),
                        lat = c(50.006, 52.678, 48.999),
                        lon = c(-85.03, -87.45, -90.38))
usethis::use_data(site_meta, overwrite = TRUE)
