context("test-print")


# pdqr_print --------------------------------------------------------------
# Main functionality is tested in `*_fun()` functions
test_that("pdqr_print works with bad input", {
  input_1 <- structure(1, meta = list(type = "a"))
  expect_output(
    pdqr_print(input_1, "Temp"), "unknown type.*Support: not proper"
  )

  input_2 <- structure(1, meta = list(type = "raw", support = c(1, Inf)))
  expect_output(pdqr_print(input_2, "Temp"), "Support: not proper")

  input_3 <- p_raw_withx
  attr(input_3, "meta")[["x"]] <- "a"
  expect_output(pdqr_print(input_3, "Temp"), "but isn't numeric")
})


# line_title --------------------------------------------------------------
# Tested in `new_*()` functions


# line_support ------------------------------------------------------------
# Tested in `new_*()` functions


# line_attached -----------------------------------------------------------
# Tested in `new_*()` functions


# appendix_num_x ----------------------------------------------------------
# Tested in `pdqr_print()`


# bold --------------------------------------------------------------------
test_that("bold works", {
  expect_equal(bold("a", force_color = TRUE), "\033[1ma\033[22m")
})


# use_color ---------------------------------------------------------------
# Tested in `new_*()` functions


# get_meta_type -----------------------------------------------------------
# Tested in `new_*()` functions


# elements ----------------------------------------------------------------
# Tested in `new_*()` functions
