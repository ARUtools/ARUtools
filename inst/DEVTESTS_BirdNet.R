test.dir <- "H:/RecordStor20222023/Railbed_E_of_Nakina_202324"
library(ARUtools)
library(dplyr)
library(sf)
library(ggplot2)
library(lubridate)
m <- clean_metadata(project_dir = test.dir, pattern_site_id = "SM4_\\d+",pattern_dt_sep = "_"

                    ) |> mutate(week = week(date))


locs <- clean_gps(m)

aru_locs <-
locs |> dplyr::distinct(site_id, aru_id, longitude, latitude) #|>
locs_sf <- aru_locs |> st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) |>
  st_transform(3161)
m <- add_sites(m, aru_locs, by_date =NULL)


ggplot(locs_sf) +
  geom_sf()


# --lat {m$latitude[[i]]
# }
# --lon {m$longitude[[i]]}
library(furrr)
plan(multisession(workers = 8))
iteration_min <- 40001
iteration_max <- nrow(m)
x <- Sys.time()
future_walk(iteration_min:iteration_max, ~{
  i <- .x
  n_bird_list_loc <- "D:/SHARED_WORKSPACE/BirdNet_spp_list/"
  birdnet_output <- "D:/SHARED_WORKSPACE/BirdNet_outputs/"
of <- stringr::str_replace(m$file_name[[i]], "wav", "txt")
call_ <- glue::glue("BirdNet-Analyzer.exe --i {
                    m$path[[i]]
                    } --o {
                    glue::glue('{birdnet_output}{of}')} --lat {m$latitude[[i]]
} --lon {m$longitude[[i]]
} --week {m$week[[i]]
                    } --slist {n_bird_list_loc}  --sensitivity 1.5 --overlap 1.5 --rtype 'r'  --threads 1 ")
print(call_)
system(
  call_,
)
})
plan(sequential)
Sys.time() - x
file.create(
  "C:/Users/HopeD/OneDrive - EC-EC/Scratchpad/BirdnetRun_complete.ping"
)
