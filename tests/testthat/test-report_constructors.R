context("Report constructor")
local_edition(3)

library(htmltools)

expect_has_props <- function(component, props) {
  test <- grepl(props, as.character(component))
  testthat::expect_true(test)
}

mark_failed <- "red big remove"
mark_success <- "big green checkmark"
mark_warning <- "big blue exclamation circle"

test_that("segment() generates div with inputted title", {
  title  <- "A title"
  ui <- segment(title)
  tagq <- tagQuery(ui)
  tagq <- tagq$find("div")$selectedTags()

  expect_equal(gsub("<.*?>", "", as.character(tagq)), title)
})

test_that("create_summary_row() generates ui with specified arguments", {
  id <- "failed_total"
  n_fails <- 3
  color <- "red"
  label  <- "Failed"

  ui <- create_summary_row(id = id, number = n_fails, color = color, label = label)
  tagq <- tagQuery(ui)

  expect_has_props(
    tagq$find("div")$parent()$selectedTags(),
    paste0("id=", '"', id, '"')
  )
  expect_true(tagq$find("div")$hasClass("red"))
  expect_has_props(tagq$allTags(), n_fails)
  expect_has_props(tagq$allTags(), label)
})

test_that("make_summary_table creates a table tag", {
  ui <- make_summary_table(n_passes = 2, n_fails = 1, n_warns = 0)
  ui <- as.character(ui)

  expect_true(startsWith(ui, "<table"))
  expect_true(endsWith(ui, "</table>"))
})

test_that("make_summary_table generates data cells with supplied n_passes, n_fails & n_warns", {
  n_passes <- 3
  n_fails  <- 2
  n_warns <- 0

  ui <- make_summary_table(n_passes, n_fails, n_warns)

  tags <- tagQuery(ui)$
    find("td")$
    filter(function(x, i) tagHasAttribute(x, "id"))$
    find("div")$
  selectedTags()

  has_n <- function(tags, n) lapply(tags, function(y) grepl(n, y))

  expect_true(TRUE %in% has_n(tags, n_passes))
  expect_true(TRUE %in% has_n(tags, n_warns))
  expect_true(TRUE %in% has_n(tags, n_fails))
})

test_that("when n_fails is 0, the generated data cell is not highlighted in red", {
  ui <- make_summary_table(n_passes = 3, n_warns = 0, n_fails = 0)
  tagq <- tagQuery(ui)

  expect_equal(sum(tagq$find("div")$hasClass("red")), 0)
})

test_that("make_accordion_container() generates ui with 'ui styled accordion' class", {
  tagq <- tagQuery(make_accordion_container())
  expect_true(tagq$hasClass("ui styled accordion"))
})

test_that("prepare_modal_content() includes results' error message", {
  results  <- results_test()
  ui <- prepare_modal_content(results)

  tagq <- tagQuery(ui)
  tags <- tagq$find("td")$selectedTags()

  expect_true(grepl(results$message[1], tags))
})

test_that("make_table_row() creates a table row tag", {
  results_passed <- results_test() %>%
    dplyr::filter(.data$type == success_id)

  ui <- make_table_row(results_passed, mark = mark_success, type = success_id)
  ui <- as.character(ui)

  expect_true(startsWith(ui, "<tr>"))
  expect_true(endsWith(ui, "</tr>"))
})

test_that("make_table_row() generates modal content for results with failed validations", {
  results_failed <- results_test() %>%
    dplyr::filter(.data$type == error_id)

  ui <- make_table_row(results_failed, mark = mark_failed, type = error_id)

  tagq <- tagQuery(ui)
  tags <- tagq$
    find(paste0("#", results_failed$assertion.id))$
    find("table")$
    parent()$ #nolint
    selectedTags() #nolint

  text <- as.character(tags) %>%
    gsub("(.*)\n</div>", "\\1", .) %>%
    gsub("<div class=\"description\">\n  ", "", .)  %>%
    gsub("\\s", "", .)


  modal <- prepare_modal_content(results_failed) %>%
    as.character() %>%
    gsub("\\s", "", .)

  expect_equal(text, modal)
})

test_that("result_table return 'No cases to display' when number of rows is 0", {
  results <- results_test() %>%
    dplyr::filter(.data$type == warning_id)

  expect_match("No cases to display.", result_table(results, warning_id, mark_warning))
})

test_that("result_table return a table tag when number of rows > 0", {
  results_success <- results_test() %>%
    dplyr::filter(.data$type == success_id)

  ui <- result_table(results_test(), success_id, mark_success)
  ui <- as.character(ui)

  expect_true(startsWith(ui, "<table"))
  expect_true(endsWith(ui, "</table>"))
})

test_that("when active = TRUE, make_accordion_element attach 'active' class to generated ui", {
  results_success <- results_test() %>%
    dplyr::filter(.data$type == success_id)

  ui <- make_accordion_element(
    results_success,
    active = TRUE,
    label = "Passed",
    type = success_id,
    mark = mark_success
  )

  tagq <- tagQuery(ui)
  expect_true(sum(tagq$hasClass("active")) > 0)
})

test_that("make_accordion_element generates ui with number of results and specified label", {
  results_success <- results_test() %>%
    dplyr::filter(.data$type == success_id)
  n_passes <- length(
    unique(results_success[results_success$type == success_id, ]$assertion.id)
  )
  label <- "Passed"

  ui <- make_accordion_element(
    results_success,
    label = label,
    type = success_id,
    mark = mark_success
  )

  tagq <- tagQuery(ui)
  tags <- tagq$find("div")$parent()$selectedTags()

  expect_has_props(tags, label)
  expect_has_props(tags, n_passes)
})

test_that("display_results generates empty accordion container if all n_* is NULL", {
  results <- results_test()
  results_success <- results_test() %>%
    dplyr::filter(.data$type == success_id)
  n_passes <- length(
    unique(results_success[results_success$type == success_id, ]$assertion.id)
  )

  ui <- display_results(results, n_passes = NULL, n_fails = NULL, n_warns = NULL)
  tag <- tagQuery(ui)$
    find("*")$
    filter(function(x, i) grepl("accordion", x))$
    selectedTags()#nolint

  ui_not_null <- display_results(results, n_passes = n_passes, n_fails = NULL, n_warns = NULL)
  tag_not_null <- tagQuery(ui_not_null)$
    find("*")$
    filter(function(x, i) grepl("accordion", x))$
    selectedTags() #nolint

  expect_equal(gsub("<.*?>", "", tag), "")
  expect_false(gsub("<.*?>", "", tag_not_null) == "")
})
