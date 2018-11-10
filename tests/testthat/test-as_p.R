context("test-as_p")

set.seed(1111)


# as_p --------------------------------------------------------------------
test_that("as_p works with user-defined function", {
  expect_distr_fun(
    as_p(user_p, type = "smooth", support = c(0, 1)), "p_fun", "smooth"
  )
  expect_error(as_p(user_p), "p_fun.*supply.*type.*support")
  expect_error(as_p(user_p, type = "smooth"), "p_fun.*supply.*support")
  expect_error(as_p(user_p, support = c(0, 1)), "p_fun.*supply.*type")
})

test_that('as_p returns self in case of "p_fun"', {
  expect_identical(as_p(p_raw_withx), p_raw_withx)
  expect_identical(as_p(p_raw_nox), p_raw_nox)
  expect_identical(as_p(p_smooth_withx), p_smooth_withx)
  expect_identical(as_p(p_smooth_nox), p_smooth_nox)
  expect_identical(as_p(p_custom), p_custom)
})

test_that('as_p works with "d_fun"', {
  expect_equal_distr(
    as_p(d_raw_withx), p_raw_withx,
    grid = x_raw_vec_ext
  )

  expect_silent(as_p(d_raw_nox, warn_precision = FALSE))
  expect_warning(
    p_from_d_raw_nox <- as_p(d_raw_nox),
    "from.*density.*raw.*not.*precise"
  )

  expect_equal_distr(
    p_from_d_raw_nox, p_raw_nox,
    grid = x_raw_vec_ext, thres = 10^(-3)
  )
  expect_equal_distr(
    as_p(d_smooth_withx), p_smooth_withx,
    grid = x_smooth_vec_ext
  )
  expect_equal_distr(
    as_p(d_smooth_nox), p_smooth_nox,
    grid = x_smooth_vec_ext
  )
  expect_equal_distr(
    as_p(d_custom), p_custom,
    grid = x_custom
  )
})

test_that('as_p works with "q_fun"', {
  expect_equal_distr(
    as_p(q_raw_withx), p_raw_withx,
    grid = x_raw_vec_ext
  )
  # In case of no "x" in metadata precision is quite bad in case
  # `type = "raw"` at the points of future discontinuity in p_fun.
  # That is why `grid = x_raw_vec_seq` and not usual `grid = x_smooth_vec_ext`
  expect_equal_distr(
    as_p(q_raw_nox), p_raw_nox,
    grid = x_raw_vec_seq, thres = 10^(-6)
  )
  expect_equal_distr(
    as_p(q_smooth_withx), p_smooth_withx,
    grid = x_smooth_vec_ext
  )
  expect_equal_distr(
    as_p(q_smooth_nox), p_smooth_nox,
    grid = x_smooth_vec_ext, thres = 10^(-6)
  )
  expect_equal_distr(
    as_p(q_custom), p_custom,
    grid = x_custom
  )
})

test_that('as_p works with "r_fun"', {
  expect_equal_distr(
    as_p(r_raw_withx), p_raw_withx,
    grid = x_raw_vec_ext
  )
  expect_equal_distr(
    as_p(r_raw_nox), p_raw_nox,
    # Support shouldn't be the same as random sampling is done
    grid = x_raw_vec_ext, thres = 0.01, check_supp = FALSE
  )
  expect_equal_distr(
    as_p(r_smooth_withx), p_smooth_withx,
    grid = x_smooth_vec_ext
  )
  expect_equal_distr(
    as_p(r_smooth_nox), p_smooth_nox,
    # Support shouldn't be the same as random sampling is done
    grid = x_smooth_vec_ext, thres = 0.01, check_supp = FALSE
  )
  expect_equal_distr(
    as_p(r_custom), p_custom,
    # Support shouldn't be the same as random sampling is done
    # Using truncated version because of "extending" property on the support
    # edges in case `type = "smooth"`.
    grid = x_custom_trunc, thres = 0.01, check_supp = FALSE
  )
})

test_that("as_p asserts extra arguments of methods", {
  # Default method
  expect_error(as_p(1, "smooth", c(0, 1)), "f.*function")
  expect_error(as_p(user_p, 1, c(0, 1)), "type.*string")
  expect_error(as_p(user_p, "a", c(0, 1)), "type.*raw.*smooth")
  expect_error(as_p(user_p, "smooth", "a"), "support.*numeric")
  expect_error(as_p(user_p, "smooth", 1), "support.*length 2")
  expect_error(as_p(user_p, "smooth", c(1, 0)), "support.*bigger")

  # Converting from `r_fun`
  expect_error(as_p(r_smooth_nox, n_sample = "a"), "n_sample.*numeric")
})


# as_p.default ------------------------------------------------------------
# Tested in `as_p()`


# as_p.d_fun --------------------------------------------------------------
# Tested in `as_p()`


# as_p.q_fun --------------------------------------------------------------
# Tested in `as_p()`


# as_p.r_fun --------------------------------------------------------------
# Tested in `as_p()`


# p_from_d_raw ------------------------------------------------------------
# Tested in `as_p()` tests for converting from 'd_fun' in case `type = "raw"`


# detect_support_raw ------------------------------------------------------
# Tested in `as_p()` tests for converting from 'd_fun' in case `type = "raw"`


# p_from_d_smooth ---------------------------------------------------------
# Tested in `as_p()` tests for converting from 'd_fun' in case `type = "smooth"`

# integrate_right ---------------------------------------------------------
test_that("integrate_right works", {
  my_f <- function(x) {x * x}
  at <- sample(seq(0, 10, by = 0.01))

  expect_equal(integrate_right(my_f, from = 0, at = at), at*at*at / 3)
})
