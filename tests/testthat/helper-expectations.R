expect_distr_fun <- function(input, distr_type, type) {
  expect_true(is.function(input))
  expect_is(input, distr_type)
  expect_false(is.null(meta(input)))
  expect_equal(meta(input, "type"), type)
}
