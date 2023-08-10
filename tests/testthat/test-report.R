context("report")

get_advanced_view <- function(log) {
  # Extracting the table structure from the exported log
  lines <- log[grep("type", log):length(log)]

  #' Removing lines from the 'lines' variable that consist solely of
  #' whitespace or special characters. This step ensures that
  #' only meaningful lines, without any special characters used
  #' for table structuring, are retained for further processing.
  lines <- lines[!grepl("^[[:blank:]+-=:_|]*$", lines)]
  out <- t(
    as.data.frame(
      strsplit(gsub(" ", "", lines), "\\|"),
      row.names = 1
    )
  )
  out <- data.frame(out)

  return(out)
}

test_that("exported CSV matched results obtained from get_results", {
  tmp <- file.path(tempdir(), "test.csv")
  on.exit(unlink(tmp))

  report <- report_test()
  save_results(report, tmp)

  actual <- read.csv(tmp) %>%
    dplyr::select(-"X") %>%
    dplyr::mutate_all(as.character)

  expected <- get_results(report, unnest = TRUE) %>%
    as.data.frame() %>%
    dplyr::mutate_all(as.character)

  expect_equal(actual, expected)
})

test_that(
  "'get_results' saves file via custom function 'write_rds' and matches the original object", {
    tmp <- file.path(tempdir(), "test.rds")
    on.exit(unlink(tmp))

    report <- report_test()
    write_rds <- function(x, file, ...) {
      saveRDS(object = x, file = file, ...)
    }
    save_results(report, tmp, method = write_rds)

    actual <- readRDS(tmp) %>%
      dplyr::mutate_all(as.character)

    expected <- get_results(report, unnest = TRUE) %>%
      dplyr::mutate_all(as.character)

    expect_equal(actual, expected)
  }
)

test_that("'get_results' saves file via custom function 'base_save' and matches the original object", {
    tmp <- file.path(tempdir(), "test")
    on.exit(unlink(tmp))

    report <- report_test()
    base_save <- function(x, file) {
      save(x, file = file)
    }
    save_results(report, tmp, method = base_save)

    load(tmp)
    actual <- x %>%
      dplyr::mutate_all(as.character)

    expected <- get_results(report, unnest = TRUE) %>%
      dplyr::mutate_all(as.character)

    expect_equal(actual, expected)
  })

test_that("'get_results' saves file via readr::write_excel_csv and matches the original object", {
  tmp <- file.path(tempdir(), "test.csv")
  on.exit(unlink(tmp))

  report <- report_test()
  save_results(report, tmp, method = readr::write_excel_csv)

  actual <- readr::read_csv(tmp) %>%
    dplyr::mutate_all(as.character)

  expected <- get_results(report, unnest = TRUE) %>%
    dplyr::mutate_all(as.character)

  expect_equal(actual, expected)
})

test_that("'get_results' fails when passing a function without file argument", {
  tmp <- file.path(tempdir(), "test.csv")
  on.exit(unlink(tmp))

  report <- report_test()
  expect_error(save_results(report, tmp, method = utils::alarm))
})

test_that("it's possible to exclude the success results in the exported log.", {
  tmp <- file.path(tempdir(), "hide_success_log.txt")
  on.exit(unlink(tmp))

  report <- report_test()
  save_summary(report, success = FALSE, file_name = tmp)

  report_hide_success <- readLines(tmp)

  expect_length(grep("Number of success validations", report_hide_success), 0)
  expect_false("success" %in% get_advanced_view(report_hide_success)$type)
})

test_that("it's possible to exclude the warning results in the exported log.", {
  tmp <- file.path(tempdir(), "hide_warning_log.txt")
  on.exit(unlink(tmp))

  report <- report_test()
  save_summary(report, warning = FALSE, file_name = tmp)

  report_hide_warning <- readLines(tmp)

  expect_length(grep("Number of validations with warnings", report_hide_warning), 0)
  expect_false("warning" %in% get_advanced_view(report_hide_warning)$type)
})

test_that("it's possible to exclude the error results in the exported log.", {
  tmp <- file.path(tempdir(), "hide_error_log.txt")
  on.exit(unlink(tmp))

  report <- report_test()
  save_summary(report, error = FALSE, file_name = tmp)

  report_hide_error <- readLines(tmp)

  expect_length(grep("Number of failed validations", report_hide_error), 0)
  expect_false("error" %in% get_advanced_view(report_hide_error)$type)
})
