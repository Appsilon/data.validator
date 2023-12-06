# data.validator (development version)

- Swapping of `error` and `warning` arguments in `save_summary()` fixed
- Fixed `validate()` function to correctly return `data-name` attribute when used in pipe chains with `%>%` or `|>` operator.
- `save_results()` now uses function passed to `method` argument to write results

# data.validator 0.2.1

- Bug fix for error table in modal that returned up to 6 rows. Now it returns full data frame.

# data.validator 0.2.0

- `validate_cols()` and `validate_rows()` will use all columns in dataframe if no column is passed
- Added a defensive wrapper (`data.validator:::check_assertr_expression()`) to add evaluation error to regular validation errors
- Removed deprecated `.data` calls in `dplyr::select()`
- `shiny.semantic::accordion()` bug fixes

# data.validator 0.1.6

No changes. Released to restore `data.validator` on CRAN, previously archived due to the archiving of `shiny.semantic` dependency.

# data.validator 0.1.5

- `assert_cols`, `assert_rows` and `assert_if` renamed to `validate_cols`, `validate_rows` and `validate_if`

# data.validator 0.1.4

- First release
