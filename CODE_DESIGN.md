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
- Later functions will not allow users to specify different column names (generally) and
  they are assumed to be standardized
  - `file_name`, `path`
  - `date`, `date_start`, `date_end`, `date_time`, `date_time_start`, `date_time_end`
  - `aru_id`, `aru_type`, `site_id`
  - `longitude`, `latitude`
  - `tz`, `t2sr`, `t2ss`
- `add_sites()` does allow adding and keeping extra columns (`name_extra = ...`)
- Functions that could legitimately use different standardized column names 
  (ie. selection functions and clip wave functions) use NSE and name arguments are defined as `col_COLNAME`
- Non-NSE col arguments are defined as `name_COLNAME`, including when converted
  to character within a function for use in other ways.
- NSE col names can be referenced with {{ }} in tidyverse functions, but occasionally 
  they must be `enquo()`d before use in other functions (like `nse_name()`, or
  `quo_is_null()` for example). Therefore in those cases they are `enquo()`d right at the 
  start of the function (e.g., `sample_recordings()`)

## Regular Expression patterns
- Patterns that are expected to vary (i.e file names) have helper functions
  - `create_pattern_XXX()`
- Patterns that are not expected to vary, but either a) might, or b) need to
  be used in several places, are stored as options set in `ARUtools-package.R`
    - e.g., patterns for extracting data from GPS files
    
## Nitty gritty of patterns (esp for date/times)
- Multiple patterns can be created by supplying multiple arguments to the 
  `create_pattern_XXX()` functions, *or* by supplying a vector of patterns to
  `clean_metadata()` (for example). 
- Date/time patterns and order need to be specified in the pattern creation, but
  *also* in `clean_metadata()` (`order_date`) because they are two steps, extracting
  the pattern and then parsing the pattern. 
- Currently, although seconds can be enforced, omitted or be optional in the 
  `create_pattern_time()` function, they are *always* optional in the parsing
  function (i.e. `lubridate::parse_date_time(... truncated = 1)`). 
  If necessary, they could be made optional for parsing by adding another argument
  `optional_sec` or similar, but this may be overkill for now
- Right now, users supply date, sep, and time patterns, but possibly, it might
  be worth having the option to supply a single date/time pattern that would take
  precedence. It's unclear how often this would be necessary, however.
- Where there is a possibility of matching different numbers of numbers (i.e.
  can match 2 year digits or 4 year digits), always use `rev(sort(digits))`, 
  (or `sort(digits, decreasing = TRUE)`) to ensure that longer patterns can
  be matched before comparing to shorter patterns
  
  
# Verbosity
- ARUtools is pretty chatty as a result of having to be clear about how weird 
  data is being handled
- `quiet` is an argument that is FALSE by default. If TRUE, non-essential, FYI
  messages are suppressed (but not warnings, and not informative problem messages)
- `verbose` is an argument (currently only for `clean_gps()`) that is by default
  FALSE. If TRUE, shows even more information (generally unnecessary unless
  troubleshooting a specific problem)
