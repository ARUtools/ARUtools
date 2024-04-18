## Resubmission
This is a resubmission. In this version I have:

 - Added link to webservices in DESCRIPTION for WildTrax <https://wildtrax.ca>
 - Removed the single quotes around Song Meter and BarLT, Wildtrax 
   (first two are products, final is a web platform).
 - Corrected case errors for 'WildTrax', Song Meter and BAR-LT.
 - Removed \dontrun from `clip_wave_single()` examples.
 - Renamed internal function `set_seed()` to `run_with_seed_if_provided()` in order to clarify that
     the seed is not set, but instead the code run with a seed if provided. Global or environmental
     seed is not modified. In cases where `seed = NULL`, global seed is used 
     (`test-11_wt_assign_tasks.R` confirms this).
- Removed the function `play_random_track()`


## Previous resubmission

In previous resubmission I:

 - corrected URLs in CITATION to include "https://"
 
 
## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

There are no references that describe the methods in this package.


