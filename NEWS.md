# dev

* `clean_gps()`
  * can handle GPX files via `sf` now
  * distance cutoff results in a warning (not an error), returning
    the data with the `max_dist` column, so users can see which site was problematic.
  * now checks for distance by site groups `aru_id` and `site_id` by default
  * pattern matches for GPS column headers have been expanded
  * catches errors but continues reporting on failed loading (remove `skip_bad` argument)
  * `check_problems()` now also checks GPS meta data

* `create_pattern_XXX()` 
  * Now accept multiple options
  * all separators are non-optional, but provide `""` as pseudo-optional
  * `create_pattern_site_id()` ids do not have to have a suffix
  
* `clean_metadata()` accepts multiple pattern options

* `clean_site_index()` allows no date columns (`col_date_time = NULL`)

* `add_sites()`
  * Rename `dt_type` to `by_date`
  * Take mean of multiple sites with `by_date = "date"` (instead of truncating)
  * Use `by_date = NULL` to skip joining by date range

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
