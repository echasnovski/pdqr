context("test-as_q")

set.seed(3333)


# as_q --------------------------------------------------------------------
test_that("as_q works with user-defined function", {
  expect_distr_fun(
    as_q(user_q, type = "smooth", domain_out = c(0, 1)), "q_fun", "smooth"
  )
})

test_that('as_q works with "p_fun"', {
  expect_equal_distr(
    as_q(p_raw_withx), q_raw_withx,
    grid = p_vec, domain = "domain_out"
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
    grid = p_vec_wholed, domain = "domain_out", thres = 10^(-6)
  )
  expect_equal_distr(
    as_q(p_smooth_withx), q_smooth_withx,
    grid = p_vec, domain = "domain_out"
  )
  expect_equal_distr(
    as_q(p_smooth_nox), q_smooth_nox,
    grid = p_vec, domain = "domain_out"
  )
  expect_equal_distr(
    as_q(p_custom), q_custom,
    grid = p_vec, domain = "domain_out"
  )
})

test_that('as_q works with "d_fun"', {
  expect_equal_distr(
    as_q(d_raw_withx), q_raw_withx,
    grid = p_vec, domain = "domain_out"
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
    grid = p_vec_wholed, domain = "domain_out", thres = 10^(-6)
  )
  expect_equal_distr(
    as_q(d_smooth_withx), q_smooth_withx,
    grid = p_vec, domain = "domain_out"
  )
  # This takes rather much time to run
  expect_equal_distr(
    as_q(d_smooth_nox), q_smooth_nox,
    # Rather low precision is probably due to aggregating error from two
    # conversions: "d_fun" -> "p_fun" -> "q_fun"
    grid = p_vec, domain = "domain_out", thres = 10^(-3)
  )
  # This takes rather much time to run.
  expect_equal_distr(
    as_q(d_custom), q_custom,
    grid = p_vec, domain = "domain_out"
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
    grid = p_vec, domain = "domain_out"
  )
  expect_equal_distr(
    as_q(r_raw_nox), q_raw_nox,
    # Domain shouldn't be the same as random sampling is done
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
    grid = p_vec_bigwholed, domain = NULL
  )
  expect_equal_distr(
    as_q(r_smooth_withx), q_smooth_withx,
    grid = p_vec, domain = "domain_out"
  )
  expect_equal_distr(
    as_q(r_smooth_nox), q_smooth_nox,
    # Domain shouldn't be the same as random sampling is done
    # Using truncated version because of "extending" property on the domain
    # edges in case `type = "smooth"`. This introduces errors around the edges.
    # That is why truncated version of `p_vec` is used.
    grid = p_vec_trunc, domain = NULL, thres = 0.05
  )
  expect_equal_distr(
    as_q(r_custom), q_custom,
    # Domain shouldn't be the same as random sampling is done
    # Using truncated version as described in test for conversion from
    # `r_smooth_nox()`.
    grid = p_vec_trunc, domain = NULL, thres = 0.05
  )
})

test_that("as_q asserts extra arguments of methods", {
  # Default method
  expect_error(as_q(1, "smooth", c(0, 1)), "f.*function")
  expect_error(as_q(user_q, 1, c(0, 1)), "type.*string")
  expect_error(as_q(user_q, "a", c(0, 1)), "type.*raw.*smooth")
  expect_error(as_q(user_q, "smooth", "a"), "domain_out.*numeric")
  expect_error(as_q(user_q, "smooth", 1), "domain_out.*length 2")
  expect_error(as_q(user_q, "smooth", c(1, 0)), "domain_out.*bigger")

  # Converting from `r_fun`
  expect_error(as_q(r_smooth_nox, n_sample = "a"), "n_sample.*numeric")
})


# as_q.default ------------------------------------------------------------
# Tested in `as_q()`


# as_q.p_fun --------------------------------------------------------------
# Tested in `as_q()`


# as_q.d_fun --------------------------------------------------------------
# Tested in `as_q()`


# as_q.r_fun --------------------------------------------------------------
# Tested in `as_q()`


# warn_converion_from_p_raw -----------------------------------------------
# Tested in `as_q()`
