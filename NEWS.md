# dev

* `clean_gps()`
  * can handle GPX files via `sf` now
  * distance cutoff results in a warning (not an error), returning
    the data with the max_dist column, so users can see which site was problematic.
  * now checks for distance by site groups `aru_id` and `site_id` by default
  * pattern matches for GPS column headers have been expanded

* `create_pattern_XXX()` 
  * Now accept multiple options
  * all separators are non-optional, but provide `""` as pseudo-optional
  * `create_pattern_site_id()` ids do not have to have a suffix
  
* `clean_metadata()` accepts multiple pattern options

* `add_sites()` takes mean of multiple sites with `dt_type = "date"` (instead of truncating)

* Workflow now works with sf input (must be POINT geometries)
  * `clean_site_index()`
  * `add_sites()`
  * `calc_sun()`

* Timezones are now more explicit
  * Expect local time marked with UTC
  * Existing non-UTC timezones are stripped with a message
  * Errors returned if there are more than one relevant date_time column with 
    different timezones
    
* Vignettes
  * Mini spatial workflow (vignettes/spatial.Rmd)
  * Explaining timezones (vignettes/timezones.Rmd)
