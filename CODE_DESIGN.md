# Code Design

This file contains notes about code design and conventions with the aim of
making collaboration and future modifications easier.

## Naming
- Snake case is used wherever possible
- Test files are named `test-XX_DESCRIPTION.R`, where `XX` is the order they 
  should be run (try to test lower order functions first).

## Column names
- In the initial cleaning functions (`clean_metadata()`, `clean_site_index()`)
  users can specify existing column names, but they will then be formatted
  to standard names
- Later functions will not allow users to specify different column names and
  they are assumed to be standardized
  - `file_name`, `path`
  - `date`, `date_start`, `date_end`, `date_time`, `date_time_start`, `date_time_end`
  - `aru_id`, `aru_type`, `site_id`
  - `longitude`, `latitude`
  - `tz`, `t2sr`, `t2ss`
- `add_sites()` does allow adding and keeping extra columns (`col_extra = ...`)

## Regular Expression patterns
- Patterns that are expected to vary (i.e file names) have helper functions
  - `create_pattern_XXX()`
- Patterns that are not expected to vary, but either a) might, or b) need to
  be used in several places, are stored as options set in `ARUtools-package.R`
    - e.g., patterns for extracting data from GPS files
