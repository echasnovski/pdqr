context("test-print")


# pdqr_print --------------------------------------------------------------
# Main functionality is tested in `*_fun()` functions
test_that("pdqr_print works with bad input", {
  input <- structure(1, meta = list(type = "a"))

  expect_output(
    pdqr_print(input, "Temp"), "unknown type.*Support: not correct"
  )
})

test_that("pdqr_print correctly computes number of elements in 'extra'", {
  input_1 <- structure(1, meta = list(extra = 1:10))
  expect_output(pdqr_print(input_1, "Temp"), "Extra.*(1 element)")

  input_2 <- structure(1, meta = list(extra = list(1:10, 2:1)))
  expect_output(pdqr_print(input_2, "Temp"), "Extra.*(2 elements)")
})


# line_title --------------------------------------------------------------
# Tested in `*_fun()` functions


# line_support ------------------------------------------------------------
# Tested in `*_fun()` functions


# line_attached -----------------------------------------------------------
# Tested in `*_fun()` functions


# bold --------------------------------------------------------------------
test_that("bold works", {
  expect_equal(bold("a", force_color = TRUE), "\033[1ma\033[22m")
})


# use_color ---------------------------------------------------------------
# Tested in `*_fun()` functions


# get_meta_type -----------------------------------------------------------
# Tested in `*_fun()` functions


# extra_length ------------------------------------------------------------
# Tested in `*_fun()` functions


# elements ----------------------------------------------------------------
# Tested in `*_fun()` functions
