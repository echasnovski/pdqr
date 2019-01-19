context("test-print")


# pdqr_print --------------------------------------------------------------
# Main functionality is tested in `*_fun()` functions
test_that("pdqr_print works with bad input", {
  input_1 <- structure(1, meta = list(type = "a"))
  expect_output(
    pdqr_print(input_1, "Temp"), "unknown number.*Support: not proper"
  )

  input_2 <- structure(1, meta = list(type = "fin", support = c(1, Inf)))
  expect_output(pdqr_print(input_2, "Temp"), "Support: not proper")
})

test_that("pdqr_print targets output to number of rows in `x_tbl`", {
  d_one <- new_d(1, "fin")
  expect_output(pdqr_print(d_one, "Temp"), "1 element")

  d_infin_one <- new_d(data.frame(x = 1:2, y = c(1, 1)), "infin")
  expect_output(pdqr_print(d_infin_one, "Temp"), "1 interval")
})


# line_title --------------------------------------------------------------
# Tested in `new_*()` functions


# line_support ------------------------------------------------------------
# Tested in `new_*()` functions


# n_x_tbl_info ------------------------------------------------------------
# Main functionality is tested in `*_fun()` functions
test_that("n_x_tbl_info works with bad input", {
  expect_equal(n_x_tbl_info(1), "")

  bad_input <- p_fin
  attr(bad_input, "meta")[["type"]] <- "a"
  expect_equal(n_x_tbl_info(bad_input), "")
})


# bold --------------------------------------------------------------------
test_that("bold works", {
  expect_equal(bold("a", force_color = TRUE), "\033[1ma\033[22m")
})


# use_color ---------------------------------------------------------------
# Tested in `new_*()` functions


# meta_type_print_name ----------------------------------------------------
# Tested in `new_*()` functions
