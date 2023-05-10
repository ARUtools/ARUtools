# dev
* `clean_gps()` can handle GPX files via `sf` now
* `clean_gps()` distance cutoff results in a warning (not an error), returning
  the data with the max_dist column, so users can see which site was problematic.
* `clean_gps()` now checks for distance by site groups `aru_id` and `site_id` by 
  default
* `create_pattern_site_id()` ids do not have to have a suffix
