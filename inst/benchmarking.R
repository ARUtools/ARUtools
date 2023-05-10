library(bench)
library(fs)

project_dir <- "../ARUtools - Extra/ARUtools_file_examples"

# fs vs. base -------------------------------

# fs package faster
bench::mark(t1 <- list.files(project_dir, recursive = TRUE),
            t2 <- fs::dir_ls(project_dir, all = FALSE, recurse = TRUE, type = "file"),
            check = FALSE)


# pattern checking faster in dir_ls rather than after - For selecting small
bench::mark(t1 <- fs::dir_ls(project_dir, all = FALSE, recurse = TRUE, type = "file",
                             regexp = "P71") %>% as.character(),
            t2 <- fs::dir_ls(project_dir, all = FALSE, recurse = TRUE, type = "file") %>%
              stringr::str_subset("P71")
)

# no real different if selecting large
bench::mark(t1 <- fs::dir_ls(project_dir, all = FALSE, recurse = TRUE, type = "file",
                             regexp = "P71", invert = TRUE) %>% as.character(),
            t2 <- fs::dir_ls(project_dir, all = FALSE, recurse = TRUE, type = "file") %>%
              stringr::str_subset("P71", negate = TRUE)
)


# sun_diff faster but equivalent to using intervals ---------------------
t1 <- Sys.time()
t2 <- t1 - lubridate::hours(6)

mark(sun_diff(t1, t2),
     abs(lubridate::int_length(lubridate::interval(t1, t2))/60),
     min_time = 5)

# GPX - gpx package vs sf ----------------------------
# SF is MUCH faster and doesn't convert datetimes to timezone specific
p <- "../ARUtools - Extra/ARUtools_file_examples/James_Bay_Lowlands_Boreal_Shield_Transition_2022/P028/1A_BARLT10962/GPS_log.gpx"
bench::mark(
  {
    g1 <- gpx::read_gpx(p)$waypoints |>
      select("time" = Time, "latitude" = "Latitude", "longitude" = "Longitude") |>
      mutate(time = lubridate::with_tz(time, "America/Toronto"),
             time = lubridate::force_tz(time, "UTC"))
  },
  {
    g2 <- sf::st_read(p, layer = "waypoints")
    g2 <- bind_cols(sf::st_drop_geometry(g2), sf::st_coordinates(g2)) |>
      select("time", "latitude" = "Y", "longitude" = "X") |>
      mutate(time = lubridate::force_tz(time, "UTC"))
  }, check = FALSE
)
