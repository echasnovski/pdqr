context("test-print")


# pdqr_print --------------------------------------------------------------
# Main functionality is tested in `*_fun()` functions
test_that("pdqr_print works with bad input", {
  input_1 <- function(x) {x}
  environment(input_1) <- new.env(parent = emptyenv())
  expect_output(
    pdqr_print(input_1, "Temp"), "unknown type.*Support: not proper"
  )

  input_2 <- function(x) {x}
  environment(input_2) <- new.env(parent = emptyenv())
  assign("support", c(1, Inf), environment(input_2))
  expect_output(pdqr_print(input_2, "Temp"), "Support: not proper")
})

test_that("pdqr_print targets output to number of rows in `x_tbl`", {
  d_one <- new_d(1, "discrete")
  expect_output(pdqr_print(d_one, "Temp"), "1 element")

  d_con_one <- new_d(data.frame(x = 1:2, y = c(1, 1)), "continuous")
  expect_output(pdqr_print(d_con_one, "Temp"), "1 interval")
})


# line_title --------------------------------------------------------------
# Tested in `new_*()` functions


# line_support ------------------------------------------------------------
# Tested in `new_*()` functions


# n_x_tbl_info ------------------------------------------------------------
# Main functionality is tested in `*_fun()` functions
test_that("n_x_tbl_info works with bad input", {
  bad_input_1 <- function(x) {x}
  environment(bad_input_1) <- new.env(parent = emptyenv())
  expect_equal(n_x_tbl_info(bad_input_1), "")

  bad_input_2 <- function(x) {x}
  environment(bad_input_2) <- new.env(parent = emptyenv())
  assign("type", "a", environment(bad_input_2))
  assign("x_tbl", x_dis_x_tbl, environment(bad_input_2))
  expect_equal(n_x_tbl_info(bad_input_2), "")
})


# bold --------------------------------------------------------------------
test_that("bold works", {
  expect_equal(bold("a", force_color = TRUE), "\033[1ma\033[22m")
})


# use_color ---------------------------------------------------------------
# Tested in `new_*()` functions


# meta_type_print_name ----------------------------------------------------
# Tested in `new_*()` functions


# get_approx_sign ---------------------------------------------------------
# Tested in `new_*()` functions
