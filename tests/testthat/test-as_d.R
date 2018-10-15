context("test-as_d")

set.seed(2222)


# Input data --------------------------------------------------------------
my_d <- function(q) {
  stats::dbeta(q, shape1 = 1, shape2 = 2)
}


# as_d --------------------------------------------------------------------
test_that("as_d works with user-defined function", {
  expect_distr_fun(
    as_d(my_d, type = "smooth", domain_in = c(0, 1)), "d_fun", "smooth"
  )
})

test_that('as_d works with "p_fun"', {
  expect_equal_distr(
    as_d(p_raw_withx), d_raw_withx,
    grid = x_raw_vec_ext, domain = "domain_in"
  )
  expect_equal_distr(
    as_d(p_raw_nox), d_raw_nox,
    grid = x_raw_vec_ext, domain = "domain_in", thres = 10^(-3)
  )
  expect_equal_distr(
    as_d(p_smooth_withx), d_smooth_withx,
    grid = x_smooth_vec_ext, domain = "domain_in"
  )
  expect_equal_distr(
    as_d(p_smooth_nox), d_smooth_nox,
    grid = x_smooth_vec_ext, domain = "domain_in", thres = 10^(-3)
  )
})

test_that('as_d returns self in case of "d_fun"', {
  expect_identical(as_d(d_raw_withx), d_raw_withx)
  expect_identical(as_d(d_raw_nox), d_raw_nox)
  expect_identical(as_d(d_smooth_withx), d_smooth_withx)
  expect_identical(as_d(d_smooth_nox), d_smooth_nox)
})

test_that('as_d works with "q_fun"', {
  expect_equal_distr(
    as_d(q_raw_withx), d_raw_withx,
    grid = x_raw_vec_ext, domain = "domain_in"
  )
  expect_equal_distr(
    as_d(q_raw_nox), d_raw_nox,
    grid = x_raw_vec_ext, domain = "domain_in", thres = 10^(-3)
  )
  expect_equal_distr(
    as_d(q_smooth_withx), d_smooth_withx,
    grid = x_smooth_vec_ext, domain = "domain_in"
  )
  expect_equal_distr(
    as_d(q_smooth_nox), d_smooth_nox,
    grid = x_smooth_vec_ext, domain = "domain_in", thres = 10^(-3)
  )
})

test_that('as_d works with "r_fun"', {
  expect_equal_distr(
    as_d(r_raw_withx), d_raw_withx,
    grid = x_raw_vec_ext, domain = "domain_in"
  )
  expect_equal_distr(
    as_d(r_raw_nox), d_raw_nox,
    # Domain shouldn't be the same as random sampling is done
    grid = c(x_raw_vec_ext, x_raw_vec), domain = NULL, thres = 0.01
  )
  expect_equal_distr(
    as_d(r_smooth_withx), d_smooth_withx,
    grid = x_smooth_vec_ext, domain = "domain_in"
  )
  expect_equal_distr(
    as_d(r_smooth_nox), d_smooth_nox,
    # Domain shouldn't be the same as random sampling is done
    # Building smooth density from random generation function has somewhat worse
      # precision than building CDF
    grid = x_smooth_vec_ext, domain = NULL, thres = 0.05
  )
})

test_that("as_d asserts extra arguments of methods", {
  # Default method
  expect_error(as_d(1, "smooth", c(0, 1)), "f.*function")
  expect_error(as_d(my_d, 1, c(0, 1)), "type.*string")
  expect_error(as_d(my_d, "a", c(0, 1)), "type.*raw.*smooth")
  expect_error(as_d(my_d, "smooth", "a"), "domain_in.*numeric")
  expect_error(as_d(my_d, "smooth", 1), "domain_in.*length 2")
  expect_error(as_d(my_d, "smooth", c(1, 0)), "domain_in.*bigger")

  # Converting from `r_fun`
  expect_error(as_d(r_smooth_nox, n_sample = "a"), "n_sample.*numeric")
})


# as_d.default ------------------------------------------------------------
# Tested in `as_d()`


# as_d.p_fun --------------------------------------------------------------
# Tested in `as_d()`


# as_d.q_fun --------------------------------------------------------------
# Tested in `as_d()`


# as_d.r_fun --------------------------------------------------------------
# Tested in `as_d()`
