# data.validator 0.2.0

-   `validate_cols()` and `validate_rows()` will use all columns in dataframe if no column is passed

-   Added a defensive wrapper (`data.validator:::check_assertr_expression()`) to add evaluation error to regular validation errors

-   New examples added:

    -   Shiny App example demo example added
    -   Usage examples added in README
    -   Connect workflow examples added

-   Removed deprecated `.data` calls in `dplyr::select()`

-   `shiny.semantic::accordion()` bug fixes

# data.validator 0.1.6

-   Dependencies updated

# data.validator 0.1.5

-   `assert_cols`, `assert_rows` and `assert_if` renamed to `validate_cols`, `validate_rows` and `validate_if`

# data.validator 0.1.4

-   First release
