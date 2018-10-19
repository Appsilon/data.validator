sample_data_validated <- sample_data %>%
  clear_results() %>%  chain_start() %>%
  verify(title = "x should have character class", v_class(x) == "character") %>%
  verify(title = "y should have numeric class", v_class(y) == "numeric") %>%
  verify(title = "y should have date class", v_class(y) == "Date") %>%
  verify(title = "z should have Date class", ignore_chain_funs = TRUE,
         error_fun = append_as_warning, v_class(z) == "Date") %>%
  chain_end(error_fun = error_append)

validator$add_validations(sample_data_validated, "example/validation_rules.R#L1")

sample_data2_validated <- sample_data2 %>%
  clear_results() %>%  chain_start() %>%
  verify(title = "a should have character class", v_class(a) == "character") %>%
  verify(title = "b should have numeric class", v_class(b) == "numeric") %>%
  verify(title = "b should have date class", v_class(b) == "Date") %>%
  verify(title = "c should have Date class", ignore_chain_funs = TRUE,
         error_fun = append_as_warning, v_class(c) == "Date") %>%
  chain_end(error_fun = error_append)

validator$add_validations(sample_data2_validated, "example/validation_rules.R#L12")
