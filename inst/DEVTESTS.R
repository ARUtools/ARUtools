# Workflows
#
# m <- clean_metadata()
# s <- clean_site_index() # From user-supplied file
# m <- add_sites(s)
# m <- calc_sun(m)
#
# m <- clean_metadata()
# g <- clean_gps(m)    # From GPS summary log files
# m <- add_sites(s)
# m <- calc_sun(m)


# eg File output ----------
out <- readr::read_rds("../Boreal_Shield_Lowlands_transition_Selection/Meta_data_cleaned.rds")

# - Only wave files
# - GPS data added
# - Sunset/sunrise added

# eg 1 - Date sep ------------------------------------------------
d <- "../ARUtools - Extra/ARUtools_file_examples/HudsonBayCoast_2021/"

count_files(d)

m <- clean_metadata(project_dir = d,
                    pattern_site = "BP_ARU0(1|2)",
                    pattern_dt_sep = "(_|T)")

check_meta(m)


# OR
m <- clean_metadata(project_dir = d,
                    pattern_site = "BP_ARU0(1|2)",
                    pattern_dt_sep = create_pattern_dt_sep(c("T", "_")))

g <- clean_gps(m) # Error
g <- clean_gps(m, dist_cutoff = Inf)

m <- add_sites(m, g)

filter(m, is.na(longitude)) %>% select(-path) # Look at where no GPS match
filter(g, site_id == "BP_ARU01") # Ah, no date overlap

# eg 2 - GPS files / Site Index ------------------------------------
d <- "../ARUtools - Extra/ARUtools_file_examples/LutherMarsh_2021/"
count_files(d)

m <- clean_metadata(project_dir = d)

# Hmm, check for problems
check_problems(m)              # Show relevant columns
check_problems(m, path = TRUE) # Show just paths
check_meta(m)

# Try again
m <- clean_metadata(project_dir = d,
                    pattern_dt_sep = "_")

check_problems(m) # No site-related information in names
check_meta(m)

# Create site index
sites <- readr::read_csv("../ARUtools - Extra/Scripts/LutherMarsh_2021/2022-02-23_Locations_LutherMarsh_2021.csv") |>
  mutate(aru_id = stringr::str_remove(location, "LutherMarsh2021-"),
         date_start = min(m$date_time, na.rm = TRUE),
         date_end = max(m$date_time, na.rm = TRUE)) |>
  clean_site_index(col_site_id = "location",
                   col_date_time = c("date_start", "date_end"))

# GPS from logs
g <- clean_gps(m, dist_by = "aru_id")

# Compare
select(g, aru_id, longitude, latitude) |> distinct()
sites

# Join (although could easily just use a simpler left_join(), because not dependent on dates)
m <- add_sites(m, sites)
check_problems(m)

m <- calc_sun(m, aru_tz = "America/Toronto")

m



# eg 3 - Big set of files -------------------------
d <- "../ARUtools - Extra/ARUtools_file_examples/JamesBayLowlands_2021/"
m <- clean_metadata(project_dir = d)

check_problems(m)
check_problems(m, path = TRUE) # No ARU types or IDs in file names, ignore.
check_meta(m)

g <- clean_gps(m)

# Check problematic GPS file
m$path[183] # file path
check_file(m$path[183]) # lines in file

# Try skipping for now
g <- clean_gps(m, skip_bad = TRUE)
g <- clean_gps(m, skip_bad = TRUE, dist_cutoff = Inf)


m1 <- add_sites(m, g, dt_type = "date") # Faster, but averages over coordinates
m2 <- add_sites(m, g)                   # Slow but matches more precisely

check_problems(m2, check = "longitude")
check_problems(m2, check = "longitude", by_date = TRUE)

filter(g, site_id == "P68_2") # No GPS log for this site

# eg 4 - File lists -----------------------------
files <- readr::read_rds("../Boreal_Shield_Lowlands_transition_Selection/Transition_all_files.rds")

m <- clean_metadata(
  project_files = files,
  pattern_site = "(P|Q)\\d{3}",
  pattern_dt_sep = create_pattern_dt_sep(sep = c("T", "_")),
  subset = "ZOOM|Zoom", subset_type = "omit"
  )
m

# eg 5 - Site from file ----------------------
files <- readr::read_rds("../RiverTrips_DataPrep/all_files_vec.rds")
i <- "../RiverTrips_DataPrep/NRT2022_SMM_Logfile_Compile.xlsx"

m <- clean_metadata(project_files = files,
                    pattern_aru_id = create_pattern_aru_id(prefix = "CWS-"),
                    pattern_dt_sep = "_")

check_problems(m) # Some funny files 'zoom'

# Omit 'zoom' files (no sites)
m <- clean_metadata(project_files = files,
                    subset = "Zoom", subset_type = "omit",
                    pattern_aru_id = create_pattern_aru_id(prefix = "CWS-"),
                    pattern_dt_sep = "_")

check_problems(m)
check_meta(m)

# Add sites by date
sites <- clean_site_index(i) # Expect standard column names - No good
sites <- clean_site_index(i, # Supply column names
                          col_aru_id = "ARU_ID",
                          col_site_id = "SiteID_WildTrax",
                          col_date_time = c("Date_Deploy", "Date_Retrieve"),
                          col_extra = c("river" = "NorthernRiverTrip"))

f <- add_sites(m, sites) # See that it omits "site_id" from by (included by default)

# Check Problems (non-matched) - These are either
# a) not in the date range in the sites index, or
# b) missing a record in the sites index
filter(f, is.na(site_id)) |>
  check_meta(by_date = TRUE)

check_problems(f, by_date = TRUE)

# All not in the date range of the sites file
semi_join(sites, check_problems(f, by_date = TRUE), by = c("aru_id"))


s <- calc_sun(f, aru_tz = "America/Toronto")

# eg 6 - BAR-LT GPS file examples ------------------------------------------
d <- "../ARUtools - Extra/ARUtools_file_examples/James_Bay_Lowlands_Boreal_Shield_Transition_2022/"
#file.create(file.path(d, "/P028/1A_BARLT10962/", "0000.wav")) # Just for testing

m <- clean_metadata(project_dir = d,
                    pattern_site_id = create_pattern_site_id(s_digits = 0))

g <- clean_gps(m)


# eg 7 - Site Index with timezones ------------------------------------
d <- "../ARUtools - Extra/ARUtools_file_examples/LutherMarsh_2021/"
m <- clean_metadata(project_dir = d, pattern_dt_sep = "_")

# Create site index
sites <- readr::read_csv("../ARUtools - Extra/Scripts/LutherMarsh_2021/2022-02-23_Locations_LutherMarsh_2021.csv") |>
  mutate(aru_id = stringr::str_remove(location, "LutherMarsh2021-"),
         date_start = min(m$date_time, na.rm = TRUE),
         date_end = max(m$date_time, na.rm = TRUE),
         date_start = lubridate::force_tz(date_start, "America/Toronto"),
         date_end = lubridate::force_tz(date_end, "America/Toronto")) |>
  clean_site_index(col_site_id = "location",
                   col_date_time = c("date_start", "date_end"))

# GPS from logs
g <- clean_gps(m, dist_by = "aru_id")

# Compare
select(g, aru_id, longitude, latitude) |> distinct()
sites

# Join (although could easily just use a simpler left_join(), because not dependent on dates)
m <- add_sites(m, sites)
check_problems(m)

m <- calc_sun(m, aru_tz = "America/Toronto")

m

