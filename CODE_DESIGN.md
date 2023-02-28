# Code Design

## Regular Expression patterns
- Patterns that are expected to vary (i.e file names) have helper functions
  - `create_pattern_XXX()`
- Patterns that are not expected to vary, but either a) might, or b) need to
  be used in several places, are stored as options set in `ARUtools-package.R`
