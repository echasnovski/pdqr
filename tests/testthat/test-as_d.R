context("test-as_d")

set.seed(2222)


# as_d --------------------------------------------------------------------
test_that("as_d works with user-defined function", {
  expect_distr_fun(
    as_d(user_d, type = "smooth", support = c(0, 1)), "d_fun", "smooth"
  )
  expect_error(as_d(user_d), "d_fun.*supply.*type.*support")
  expect_error(as_d(user_d, type = "smooth"), "d_fun.*supply.*support")
  expect_error(as_d(user_d, support = c(0, 1)), "d_fun.*supply.*type")
})

test_that("as_d rewrites metadata on user-defined function", {
  input <- structure(user_d, meta = list(type = "raw", new = 1), old = 2)
  output <- as_d(input, type = "smooth", support = c(0, 1))
  expect_true("old" %in% names(attributes(output)))
  expect_equal(meta(output), list(support = c(0, 1), type = "smooth"))
})

test_that('as_d works with "p_fun"', {
  expect_equal_distr(
    as_d(p_raw_withx), d_raw_withx,
    grid = x_raw_vec_ext
  )
  expect_equal_distr(
    as_d(p_raw_nox), d_raw_nox,
    grid = x_raw_vec_ext, thres = 10^(-3)
  )
  expect_equal_distr(
    as_d(p_smooth_withx), d_smooth_withx,
    grid = x_smooth_vec_ext
  )
  expect_equal_distr(
    as_d(p_smooth_nox), d_smooth_nox,
    grid = x_smooth_vec_ext, thres = 10^(-3)
  )
  expect_equal_distr(
    as_d(p_custom), d_custom,
    # Behavior on support edges (here - in point 0) doesn't match the true
    # density because it is not continuous. In this package "d_fun" functions
    # are ("forcibly" on edges) are constructed as continuous.
    # Run `as_d(p_custom)(0 - 0.000001 * (5:1))`.
    # That is why edges are removed.
    grid = x_custom_inner
  )
})

test_that('as_d returns self in case of "d_fun"', {
  expect_identical(as_d(d_raw_withx), d_raw_withx)
  expect_identical(as_d(d_raw_nox), d_raw_nox)
  expect_identical(as_d(d_smooth_withx), d_smooth_withx)
  expect_identical(as_d(d_smooth_nox), d_smooth_nox)
  expect_identical(as_d(d_custom), d_custom)
})

test_that('as_d works with "q_fun"', {
  expect_equal_distr(
    as_d(q_raw_withx), d_raw_withx,
    grid = x_raw_vec_ext
  )
  expect_equal_distr(
    as_d(q_raw_nox), d_raw_nox,
    grid = x_raw_vec_ext, thres = 10^(-3)
  )
  expect_equal_distr(
    as_d(q_smooth_withx), d_smooth_withx,
    grid = x_smooth_vec_ext
  )
  expect_equal_distr(
    as_d(q_smooth_nox), d_smooth_nox,
    grid = x_smooth_vec_ext, thres = 10^(-3)
  )
  expect_equal_distr(
    # The reason edges are removed is described in test for "p_fun".
    as_d(q_custom), d_custom,
    grid = x_custom_inner, thres = 10^(-3)
  )
})

test_that('as_d works with "r_fun"', {
  expect_equal_distr(
    as_d(r_raw_withx), d_raw_withx,
    grid = x_raw_vec_ext
  )
  expect_equal_distr(
    as_d(r_raw_nox), d_raw_nox,
    # Support shouldn't be the same as random sampling is done
    grid = c(x_raw_vec_ext, x_raw_vec), thres = 0.01, check_supp = FALSE
  )
  expect_equal_distr(
    as_d(r_smooth_withx), d_smooth_withx,
    grid = x_smooth_vec_ext
  )
  expect_equal_distr(
    as_d(r_smooth_nox), d_smooth_nox,
    # Support shouldn't be the same as random sampling is done
    # Building smooth density from random generation function has somewhat worse
      # precision than building CDF
    grid = x_smooth_vec_ext, thres = 0.05, check_supp = FALSE
  )
  expect_equal_distr(
    as_d(r_custom), d_custom,
    # Support shouldn't be the same as random sampling is done
    # Using truncated version because of "extending" property on the support
    # edges in case `type = "smooth"`. Both this and discontinuous nature of
    # custom distribution (with big jump at 0) give bad precision.
    grid = x_custom_trunc, thres = 0.15, check_supp = FALSE
  )
  # Illustration of big impact of discontinuity:
  # x <- sort(x_custom)
  # plot(x, d_custom(x), type = "l")
  # lines(x, as_d(r_custom)(x), col = "red")
  # lines(x, as_d(r_custom, n_sample = 50000)(x), col = "blue")
})

test_that('as_d works with "pdqr_fun" (not adding duplicated class)', {
  input <- structure(dbeta, class = c("pdqr_fun", "function"))
  output <- as_d(input, type = "smooth", support = c(0, 1))
  expect_equal(class(output), c("d_fun", "pdqr_fun", "function"))
})

test_that("as_d asserts extra arguments of methods", {
  # Default method
  expect_error(as_d(1, "smooth", c(0, 1)), "f.*function")
  expect_error(as_d(user_d, 1, c(0, 1)), "type.*string")
  expect_error(as_d(user_d, "a", c(0, 1)), "type.*raw.*smooth")
  expect_error(as_d(user_d, "smooth", "a"), "support.*numeric")
  expect_error(as_d(user_d, "smooth", 1), "support.*length 2")
  expect_error(as_d(user_d, "smooth", c(1, 0)), "support.*bigger")

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
