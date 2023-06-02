context("report")

report_test <- function() {
    dat <- data.frame(
        V1 = c(1, 2, 3),
        V2 = c("a", "b", "c")
    )

    report <- data_validation_report()

    validate(dat)  %>%
        validate_cols(is.numeric)  %>%
        validate_if(V1 > 0)  %>%
        add_results(report)

    return(report)
}

get_advanced_view <- function(log) {
    lines <- log[grep("type", log):length(log)]
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
    save_results(report, tmp, method = write)

    actual <- read.csv(tmp) %>%
        dplyr::select(-"X") %>%
        dplyr::mutate_all(as.character)

    expected <- get_results(report, unnest = TRUE) %>%
        as.data.frame() %>%
        dplyr::mutate_all(as.character)

    expect_equal(actual, expected)
})

test_that("can choose not to present success result in exported log", {
    tmp <- file.path(tempdir(), "hide_success_log.txt")
    on.exit(unlink(tmp))

    report <- report_test()
    save_summary(report, success = FALSE, file_name = tmp)

    report_hide_success <- readLines(tmp)

    expect_length(grep("Number of success validations", report_hide_success), 0)
    expect_false("success" %in% get_advanced_view(report_hide_success)$type)
})

test_that("can choose not to present warning result in exported log", {
    tmp <- file.path(tempdir(), "hide_warning_log.txt")
    on.exit(unlink(tmp))

    report <- report_test()
    save_summary(report, warning = FALSE, file_name = tmp)

    report_hide_warning <- readLines(tmp)

    expect_length(grep("Number of validations with warnings", report_hide_warning), 0)
    expect_false("warning" %in% get_advanced_view(report_hide_warning)$type)
})

test_that("can choose not to present error result in exported log", {
    tmp <- file.path(tempdir(), "hide_error_log.txt")
    on.exit(unlink(tmp))

    report <- report_test()
    save_summary(report, error = FALSE, file_name = tmp)

    report_hide_error <- readLines(tmp)

    expect_length(grep("Number of failed validations", report_hide_error), 0)
    expect_false("error" %in% get_advanced_view(report_hide_error)$type)
})
