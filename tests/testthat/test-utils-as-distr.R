context("test-utils-as-distr")


# as_distr_impl_def -------------------------------------------------------
# Tested in tests for construction of `*_fun` from user-defined function


# as_distr_impl_r ---------------------------------------------------------
# Tested in tests for conversion to `*_fun` from `r_fun`


# assert_domain -----------------------------------------------------------
test_that("assert_domain works", {
  expect_silent(assert_domain(c(0, 1), "dom"))
  expect_silent(assert_domain(c(1, 1), "dom"))

  expect_error(assert_domain("a", "bbb"), "bbb.*length.*2")
  expect_error(assert_domain(1, "bbb"), "bbb.*numeric")
  expect_error(assert_domain(c(2, 1), "bbb"), "[fF]irst.*not bigger")
})
