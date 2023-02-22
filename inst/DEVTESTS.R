
# example 1 - different date formats
d <- "../ARUtools - Extra/ARUtools_file_examples/HudsonBayCoast_2021/"
t <- clean_metadata(project_dir = d,
                    pattern_site = "BP_ARU0(1|2)",
                    pattern_dt_sep = "(_|T)")

# OR
t <- clean_metadata(project_dir = d,
                    pattern_site = "BP_ARU0(1|2)",
                    pattern_dt_sep = create_pattern_dt_sep(c("T", "_")))

# example 2
d <- "../ARUtools - Extra/ARUtools_file_examples/JamesBayLowlands_2021/"
t <- clean_metadata(project_dir = d)

# example 3
files <- readr::read_rds("../Boreal_Shield_Lowlands_transition_Selection/Transition_all_files.rds")

t <- clean_metadata(
  project_files = files,
  pattern_site = "(P|Q)\\d{3}",
  pattern_dt_sep = create_pattern_dt_sep(sep = c("T", "_")))
t

# example 4 - with sites from file
files <- readr::read_rds("../RiverTrips_DataPrep/all_files_vec.rds")
i <- "../RiverTrips_DataPrep/NRT2022_SMM_Logfile_Compile.xlsx"

# Expect standard column names
sites <- clean_site_index(i)

# Supply column names
sites <- clean_site_index(i,
                          col_aru_id = "ARU_ID",
                          col_site_id = "SiteID_WildTrax",
                          col_dates = c("Date_Deploy", "Date_Retrieve")) %>%
  dplyr::mutate(aru_id = stringr::str_remove(aru_id, "^CWS-"))

# no sites
t <- clean_metadata(project_files = files,
                    pattern_dt_sep = "_")

# most sites
t <- clean_metadata(project_files = files,
                    site_index = sites,
                    pattern_aru_id = create_pattern_aru_id(arus = "SMM"),
                    pattern_dt_sep = "_")

# Split for diff date/time formats
t1 <- clean_metadata(project_files = files,
                     site_index = sites,
                     subset = "Zoom", subset_type = "omit",
                     pattern_aru_id = create_pattern_aru_id(arus = "SMM"),
                     pattern_dt_sep = "_")

t2 <- clean_metadata(project_files = files,
                     subset = "Zoom",
                     pattern_date = create_pattern_date(n_years = 2),
                     pattern_dt_sep = "-")

# t1 is great, all sites all date/times
# t2 is problematic
#  - no sites (no clear pattern and no aru_id to match to index)
#  - Only some date/times

