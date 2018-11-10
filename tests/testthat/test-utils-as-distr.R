context("test-utils-as-distr")


# distr_from_meta ---------------------------------------------------------
# Tested in tests for `as_*()` (about working with present "x" in metadata)


# as_distr_impl_def -------------------------------------------------------
# Tested in tests for construction of `*_fun` from user-defined function


# assert_missing_args -----------------------------------------------------
# Tested in tests for construction of `*_fun` from user-defined function


# as_distr_impl_r ---------------------------------------------------------
# Tested in tests for conversion to `*_fun` from `r_fun`


# assert_support ----------------------------------------------------------
test_that("assert_support works", {
  expect_silent(assert_support(c(0, 1)))
  expect_silent(assert_support(c(1, 1)))

  expect_error(assert_support("a"), "support.*numeric")
  expect_error(assert_support(1), "support.*length 2")
  expect_error(assert_support(c(2, 1)), "[fF]irst.*not bigger")
})


# warn_conversion_from_p_raw ----------------------------------------------
# Tested in `as_q()` and `as_r()`
