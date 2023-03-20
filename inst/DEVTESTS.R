# Workflows
#
# m <- clean_metadata()
# s <- clean_site_index() # From user-supplied file
# m <- add_sites(s)
#
# m <- clean_metadata()
# g <- clean_gps(m)    # From GPS summary log files
# m <- add_sites(s)


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

# eg 2 - 'Summary' GPS files -------------------------------------------
d <- "../ARUtools - Extra/ARUtools_file_examples/LutherMarsh_2021/"
count_files(d)

m <- clean_metadata(project_dir = d)

# Hmm, check for problems
m$path[1:15]
check_meta(m)

# Try again
m <- clean_metadata(project_dir = d,
                    pattern_dt_sep = "_")
m
g <- clean_gps(m, dist_by = "aru_id")


# eg 3 - -------------------------
d <- "../ARUtools - Extra/ARUtools_file_examples/JamesBayLowlands_2021/"
m <- clean_metadata(project_dir = d)

check_meta(m)

g <- clean_gps(m)

# Check problematic file
m$path[183]
check_file(m$path[183])

# Try skipping for now
g <- clean_gps(m, skip_bad = TRUE)
g <- clean_gps(m, skip_bad = TRUE, dist_cutoff = Inf)


m_full <- add_sites(m, g)
filter(m_full, n_matches > 1)
filter(m_full, is.na(longitude))

# try again
m_full <- add_sites(m, g, dt_type = "date_time")
filter(m_full, is.na(longitude))

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

# Some funny files 'zoom'
t <- clean_metadata(project_files = files,
                    pattern_dt_sep = "_")

filter(t, is.na(aru_id))

# Omit 'zoom' files
t <- clean_metadata(project_files = files,
                    subset = "Zoom", subset_type = "omit",
                    pattern_aru_id = create_pattern_aru_id(prefix = "CWS-"),
                    pattern_dt_sep = "_")

# Add sites by date
sites <- clean_site_index(i) # Expect standard column names
sites <- clean_site_index(i, # Supply column names
                          col_aru_id = "ARU_ID",
                          col_site_id = "SiteID_WildTrax",
                          col_date_time = c("Date_Deploy", "Date_Retrieve"),
                          col_extra = c("river" = "NorthernRiverTrip"))

f <- add_sites(t, sites, dt_type = "date_time") # Need to omit "site_id" from by
f <- add_sites(t, sites, by = "aru_id", dt_type = "date_time")

# Now check non-matched - These are either
# a) not in the date range in sites, or
# b) missing a record in sites
filter(f, is.na(site_id)) |>
  check_meta()
