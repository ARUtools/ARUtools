# Things to do when wrapping up a release

# - Update Version in DESCRIPTION
# - Update NEWS.md

# - Check coverage (excluding original functions and non-function files)
p <- covr::package_coverage(
  line_exclusions = list("R/ARUtools-package.R"),

  function_exclusions = c(
    "play_random_track", "prep_for_wind_detection", "wind_detection_summarize_json",
    "wind_detection_pre_processing"))

# Interactive report to see which parts are missing coverage
covr::report(p)

# June 14 2023 - 56% (> 82% for new work)
# Feb 23 2024 - 88.64% (each file > 80% coverage)
# Mar 1 2024 - 88.75% (each file > 84% coverage)

devtools::build_readme()


# For checking mid-point -----------------------
# Add non-finalized .R files to .Rbuildignore -> JUST FOR CHECKING!
