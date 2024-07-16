# Create internal environment
.arutools <- rlang::new_environment(parent = rlang::empty_env())

## Pattern type patterns for
.arutools$pattern_aru_type <- c(
  "barlt" = "BAR-LT",
  "SMM" = "Song Meter Mini",
  "SM(\\d)" = "Song Meter \\1",
  "S(\\d)A" = "Song Meter \\1"
)
