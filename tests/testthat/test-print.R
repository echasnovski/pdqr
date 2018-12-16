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
})


# line_title --------------------------------------------------------------
# Tested in `new_*()` functions


# line_support ------------------------------------------------------------
# Tested in `new_*()` functions


# bold --------------------------------------------------------------------
test_that("bold works", {
  expect_equal(bold("a", force_color = TRUE), "\033[1ma\033[22m")
})


# use_color ---------------------------------------------------------------
# Tested in `new_*()` functions


# get_meta_type -----------------------------------------------------------
# Tested in `new_*()` functions


# elements ----------------------------------------------------------------
test_that("elements works", {
  expect_match(elements(1), "1 element")
  expect_match(elements(2), "2 elements")
})
