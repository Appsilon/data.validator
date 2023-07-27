context("utils")

describe("get_ast", {
  it("generates a list for simple calls", {
    result <- get_ast(quote(x + y))
    expect_true(is.list(result))
  })

  it("handles nested calls", {
    result <- get_ast(quote((x + y) * z))
    expect_equal(length(result), 3)
  })
})

describe("is_complex_command", {
  it("returns FALSE for simple identifiers", {
    expect_false(is_complex_command("x"))
  })

  it("returns TRUE for complex expressions", {
    expect_true(is_complex_command("x + y"))
  })
})

describe("find_first_noncall", {
  it("returns the first non-call part of a call", {
    result <- find_first_noncall(quote(x + y))
    expect_equal(as.character(result), "x")
  })

  it("recurses into nested calls", {
    result <- find_first_noncall(quote((x + y) * z))
    expect_equal(as.character(result), "x")
  })

  it("finds the first non-call object in a list created from a pipe expression", {
    pipe_expression <- quote(data %>% dplyr::filter() %>% dplyr::mutate())
    pipe_list <- as.list(pipe_expression)
    result <- find_first_noncall(pipe_list)
    expect_equal(as.character(result), "data")
  })
})

describe("get_first_name", {
  test_data <- data.frame(col_1 = c(0, 1, 2), col_2 = c(3, 4, 5))
  test_fun <- function(data) {
    get_first_name()
  }

  it("returns the name of the first object in a pipe chain", {
    result <- test_data %>% test_fun()
    expect_equal(result, "test_data")
  })

  it("returns the name with multiple %>% operations", {
    result <- test_data %>% dplyr::filter() %>% dplyr::mutate() %>% test_fun()
    expect_equal(result, "test_data")
  })
})
