# Things to do when wrapping up a release

# - Update Version in DESCRIPTION
# - Update NEWS.md

# - Check coverage
covr::package_coverage()  # June 14 2023 - 56% (> 82% for new work)

devtools::build_readme()
