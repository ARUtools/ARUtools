
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
