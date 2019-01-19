context("test-utils-as")


# y_from_p_grid -----------------------------------------------------------
# Tested in `as_p.default()`


# assert_as_def_args ------------------------------------------------------
# Tested in `as_*.default()` functions


# assert_tot_prob ---------------------------------------------------------
test_that("assert_tot_prob works", {
  expect_error(assert_tot_prob(0), "probability.*positive")
  expect_silent(assert_tot_prob(0.1))
})


# remove_zero_edge_y ------------------------------------------------------
test_that("remove_zero_edge_y works", {
  input_x_tbl <- data.frame(x = 1:8, y = c(0, 0, 1, 0, 0, 1, 0, 0))
  n <- nrow(input_x_tbl)
  output_ref <- input_x_tbl[2:7, ]

  expect_equal(remove_zero_edge_y(input_x_tbl), output_ref)
  expect_equal(remove_zero_edge_y(input_x_tbl[-1, ]), output_ref)
  expect_equal(remove_zero_edge_y(input_x_tbl[-n, ]), output_ref)
  expect_equal(remove_zero_edge_y(output_ref), output_ref)
})


# format_support ----------------------------------------------------------
test_that("format_support works", {
  expect_equal(format_support(NULL), c(NA_real_, NA_real_))
  expect_equal(format_support(c(1, NA)), c(1, NA))
})
