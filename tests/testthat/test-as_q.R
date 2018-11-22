context("test-as_q")

set.seed(3333)


# as_q --------------------------------------------------------------------
test_that("as_q works with user-defined function", {
  expect_distr_fun(
    as_q(user_q, type = "smooth", support = c(0, 1)), "q_fun", "smooth"
  )
  expect_error(as_q(user_q), "q_fun.*supply.*type.*support")
  expect_error(as_q(user_q, type = "smooth"), "q_fun.*supply.*support")
  expect_error(as_q(user_q, support = c(0, 1)), "q_fun.*supply.*type")
})

test_that("as_q rewrites metadata on user-defined function", {
  input <- structure(user_q, meta = list(type = "raw", new = 1), old = 2)
  output <- as_q(input, type = "smooth", support = c(0, 1))
  expect_true("old" %in% names(attributes(output)))
  expect_equal(meta(output), list(support = c(0, 1), type = "smooth"))
})

test_that('as_q works with "p_fun"', {
  expect_equal_distr(
    as_q(p_raw_withx), q_raw_withx,
    grid = p_vec
  )

  expect_silent(as_q(p_raw_nox, warn_precision = FALSE))
  expect_warning(
    q_from_p_raw_nox <- as_q(p_raw_nox),
    "from.*cumulative.*raw.*not.*precise.*value"
  )

  expect_equal_distr(
    q_from_p_raw_nox, q_raw_nox,
    # Converting from "p_fun" in case `type = "raw"` is not precise around
    # actual values. So they are removed.
    grid = p_vec_wholed, thres = 10^(-3)
  )
  expect_equal_distr(
    as_q(p_smooth_withx), q_smooth_withx,
    grid = p_vec
  )
  expect_equal_distr(
    as_q(p_smooth_nox), q_smooth_nox,
    grid = p_vec, thres = 10^(-6)
  )
  expect_equal_distr(
    as_q(p_custom), q_custom,
    grid = p_vec, thres = 10^(-7)
  )
})

test_that('as_q works with "d_fun"', {
  expect_equal_distr(
    as_q(d_raw_withx), q_raw_withx,
    grid = p_vec
  )

  expect_silent(as_q(d_raw_nox, warn_precision = FALSE))
  expect_warning(
    q_from_d_raw_nox <- as_q(d_raw_nox),
    "from.*density.*raw.*not.*precise"
  )

  expect_equal_distr(
    q_from_d_raw_nox, q_raw_nox,
    # Converting from "d_fun" in case `type = "raw"` is not precise because
    # actual values are hard to find. Even if they are found (as the case in
    # these tests) precision around them is poor. So they are removed.
    grid = p_vec_wholed, thres = 10^(-3)
  )
  expect_equal_distr(
    as_q(d_smooth_withx), q_smooth_withx,
    grid = p_vec
  )
  # This takes rather much time to run
  expect_equal_distr(
    as_q(d_smooth_nox), q_smooth_nox,
    # Rather low precision is probably due to aggregating error from two
    # conversions: "d_fun" -> "p_fun" -> "q_fun"
    grid = p_vec, thres = 10^(-3)
  )
  # This takes rather much time to run.
  expect_equal_distr(
    as_q(d_custom), q_custom,
    grid = p_vec, thres = 10^(-7)
  )
})

test_that('as_q returns self in case of "q_fun"', {
  expect_identical(as_q(q_raw_withx), q_raw_withx)
  expect_identical(as_q(q_raw_nox), q_raw_nox)
  expect_identical(as_q(q_smooth_withx), q_smooth_withx)
  expect_identical(as_q(q_smooth_nox), q_smooth_nox)
  expect_identical(as_q(q_custom), q_custom)
})

test_that('as_q works with "r_fun"', {
  expect_equal_distr(
    as_q(r_raw_withx), q_raw_withx,
    grid = p_vec
  )
  expect_equal_distr(
    as_q(r_raw_nox), q_raw_nox,
    # Support shouldn't be the same as random sampling is done
    # Estimating distribution from random sampling is not exact around true
    # probabilities. It means that jumps in quantile functions are made only
    # near true probabilites. As jumps might have high "height", it means very
    # high error around true probabilities (by the "jump height").

    # That is why values of `p_vec` in close neighboorhood of actual cumulative
    # probabilities are not used here.

    # For illustration run this code:
    # set.seed(123)
    # p <- sort(p_vec)
    # plot(p, as_q(r_raw_nox)(p), type = "l")
    # lines(p, q_raw_nox(p), col = "red")
    grid = p_vec_bigwholed, check_supp = FALSE
  )
  expect_equal_distr(
    as_q(r_smooth_withx), q_smooth_withx,
    grid = p_vec
  )
  expect_equal_distr(
    as_q(r_smooth_nox), q_smooth_nox,
    # Support shouldn't be the same as random sampling is done
    # Using truncated version because of "extending" property on the support
    # edges in case `type = "smooth"`. This introduces errors around the edges.
    # That is why truncated version of `p_vec` is used.
    grid = p_vec_trunc, thres = 0.05, check_supp = FALSE
  )
  expect_equal_distr(
    as_q(r_custom), q_custom,
    # Support shouldn't be the same as random sampling is done
    # Using truncated version as described in test for conversion from
    # `r_smooth_nox()`.
    grid = p_vec_trunc, thres = 0.05, check_supp = FALSE
  )
})

test_that('as_q works with "pdqr_fun" (not adding duplicated class)', {
  input <- structure(qbeta, class = c("pdqr_fun", "function"))
  output <- as_q(input, type = "smooth", support = c(0, 1))
  expect_equal(class(output), c("q_fun", "pdqr_fun", "function"))
})

test_that("as_q respects `n_grid` argument", {
  expect_different_distr(
    as_q(p_smooth_nox), as_q(p_smooth_nox, n_grid = 101),
    grid = x_smooth_vec
  )
  expect_different_distr(
    as_q(d_smooth_nox), as_q(d_smooth_nox, n_grid = 101),
    grid = x_smooth_vec
  )
})

test_that("as_q asserts extra arguments of methods", {
  # Default method
  expect_error(as_q(1, "smooth", c(0, 1)), "f.*function")
  expect_error(as_q(user_q, 1, c(0, 1)), "type.*string")
  expect_error(as_q(user_q, "a", c(0, 1)), "type.*raw.*smooth")
  expect_error(as_q(user_q, "smooth", "a"), "support.*numeric")
  expect_error(as_q(user_q, "smooth", 1), "support.*length 2")
  expect_error(as_q(user_q, "smooth", c(1, 0)), "support.*bigger")

  # Converting from `r_fun`
  expect_error(as_q(r_smooth_nox, n_sample = "a"), "n_sample.*single number")
  expect_error(as_q(r_smooth_nox, n_sample = 10:11), "n_sample.*single number")
})


# as_q.default ------------------------------------------------------------
# Tested in `as_q()`


# as_q.p_fun --------------------------------------------------------------
# Tested in `as_q()`


# as_q.d_fun --------------------------------------------------------------
# Tested in `as_q()`


# as_q.r_fun --------------------------------------------------------------
# Tested in `as_q()`
