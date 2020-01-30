sample_data_validated <- sample_data %>%
  chain_start(store_success = TRUE) %>%
  verify(description = "x should have character class", has_class("x", class = "character")) %>%
  assert(description = "x and y should have be in a, b, c", in_set(c("a", "b", "c")), x, y) %>%
  assert(description = "x and y should have be in a, b", in_set(c("a", "b")), x, y) %>%
  verify(description = "y should have numeric class", has_class("y", class = "numeric")) %>%
  verify(description = "y should have date class", has_class("y", class = "Date")) %>%
  verify(description = "z should have Date class", skip_chain_opts = TRUE,
         error_fun = warning_append, has_class("z", class = "Date")) %>%
  chain_end(error_fun = error_append)

validator$add_validations(sample_data_validated, "example/validation_rules.R#L1")

sample_data2_validated <- sample_data2 %>%
  clear_results() %>%  chain_start() %>%
  verify(description = "a should have character class", has_class(a, class = "character")) %>%
  verify(description = "b should have numeric class", has_class(b, class = "numeric")) %>%
  verify(description = "b should have date class", has_class(b, class = "Date")) %>%
  verify(description = "c should have Date class", ignore_chain_funs = TRUE,
         error_fun = append_as_warning, has_class(x, class = "Date")) %>%
  chain_end(error_fun = error_append)

validator$add_validations(sample_data2_validated, "example/validation_rules.R#L12")
