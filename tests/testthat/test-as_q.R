context("test-as_q")

set.seed(3333)


# as_q --------------------------------------------------------------------
test_that("as_q works with user-defined function", {
  expect_distr_fun(
    as_q(user_q, type = "smooth", support = c(0, 1)), "q", "smooth"
  )
  expect_error(as_q(user_q), "q-function.*supply.*type.*support")
  expect_error(as_q(user_q, type = "smooth"), "q-function.*supply.*support")
  expect_error(as_q(user_q, support = c(0, 1)), "q-function.*supply.*type")
})

test_that("as_q rewrites metadata on user-defined function", {
  input <- structure(user_q, meta = list(type = "raw", new = 1), old = 2)
  output <- as_q(input, type = "smooth", support = c(0, 1))
  expect_true("old" %in% names(attributes(output)))
  expect_equal(meta(output), list(support = c(0, 1), type = "smooth"))
})

test_that("as_q adjusts user-defined function to be probability distribution", {
  # Adjusted function equals `new_q()` applied to sample on restricted support
  output_raw_ref <- new_q(
    x = x_raw[(x_raw >= 2) & (x_raw <= 6)], type = "raw"
  )
  # "raw_tbl" metadata is not created in `as_q.default()`
  expect_equal_distr(
    adj_q_raw, output_raw_ref,
    grid = p_vec, thres = 10^(-3), meta_not_check = "raw_tbl"
  )

  # Adjusted function stretches from 0 to 1 on support
  expect_equal(adj_q_smooth(c(0, 1)), c(0, 1))
})

test_that("as_q adjusts the same way as other `as_*()` functions", {
  expect_equal_distr(as_q(adj_p_raw), adj_q_raw, p_vec)
  expect_equal_distr(as_q(adj_d_raw), adj_q_raw, p_vec)

  expect_equal_distr(
    as_q(adj_d_smooth), adj_q_smooth,
    grid = p_vec, thres = 10^(-4)
  )
  expect_equal_distr(
    as_q(adj_p_smooth), adj_q_smooth,
    grid = p_vec, thres = 10^(-7)
  )
})

test_that("as_q.default throws errors", {
  new_q_raw <- q_raw
  attributes(new_q_raw) <- NULL
  expect_error(as_q(new_q_raw, "raw", c(10, 11)), "probability.*zero")
  expect_error(as_q(user_q, "smooth", c(2, 3)), "probability.*zero")
})

test_that('as_q works with "p"', {
  expect_equal_distr(
    as_q(p_raw), q_raw,
    # Converting from p-function in case `type = "raw"` is not precise around
    # actual values. So they are removed.
    grid = p_vec_wholed, thres = 10^(-3)
  )
  expect_equal_distr(
    as_q(p_smooth), q_smooth,
    grid = p_vec, thres = 10^(-6)
  )
  expect_equal_distr(
    as_q(p_custom), q_custom,
    grid = p_vec, thres = 10^(-7)
  )
})

test_that('as_q works with "d"', {
  expect_equal_distr(
    as_q(d_raw), q_raw,
    # Converting from d-function in case `type = "raw"` is not precise because
    # actual values are hard to find. Even if they are found (as the case in
    # these tests) precision around them is poor. So they are removed.
    grid = p_vec_wholed, thres = 10^(-3)
  )
  # This takes rather much time to run
  expect_equal_distr(
    as_q(d_smooth), q_smooth,
    # Rather low precision is probably due to aggregating error from two
    # conversions: "d" -> "p" -> "q"
    grid = p_vec, thres = 10^(-3)
  )
  # This takes rather much time to run.
  expect_equal_distr(
    as_q(d_custom), q_custom,
    grid = p_vec, thres = 10^(-7)
  )
})

test_that('as_q returns self in case of "q"', {
  expect_identical(as_q(q_raw), q_raw)
  expect_identical(as_q(q_smooth), q_smooth)
  expect_identical(as_q(q_custom), q_custom)
})

test_that('as_q works with "r"', {
  expect_equal_distr(
    as_q(r_raw), q_raw,
    # Estimating distribution from random sampling is not exact around true
    # probabilities. It means that jumps in quantile functions are made only
    # near true probabilites. As jumps might have high "height", it means very
    # high error around true probabilities (by the "jump height").

    # That is why values of `p_vec` in close neighboorhood of actual cumulative
    # probabilities are not used here.

    # For illustration run this code:
    # set.seed(123)
    # p <- sort(p_vec)
    # plot(p, as_q(r_raw)(p), type = "l")
    # lines(p, q_raw(p), col = "red")
    grid = p_vec_bigwholed,
    # Support and "raw_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("raw_tbl", "support")
  )
  expect_equal_distr(
    as_q(r_smooth), q_smooth,
    # Using truncated version because of "extending" property on the support
    # edges in case `type = "smooth"`. This introduces errors around the edges.
    # That is why truncated version of `p_vec` is used.
    grid = p_vec_trunc, thres = 0.05,
    # Support shouldn't be the same as random sampling is done
    meta_not_check = "support"
  )
  expect_equal_distr(
    as_q(r_custom), q_custom,
    # Support shouldn't be the same as random sampling is done
    # Using truncated version as described in test for conversion from
    # `r_smooth()`.
    grid = p_vec_trunc, thres = 0.05, meta_not_check = "support"
  )
})

test_that('as_q works with "pdqr" (not adding duplicated class)', {
  input <- structure(
    function(x) {qbeta(x, 1, 2)}, class = c("pdqr", "function")
  )
  output <- as_q(input, type = "smooth", support = c(0, 1))
  expect_equal(class(output), c("q", "pdqr", "function"))
})

test_that("as_q respects `n_grid` argument", {
  expect_different_distr(
    as_q(p_smooth), as_q(p_smooth, n_grid = 101),
    grid = x_smooth_vec
  )
  expect_different_distr(
    as_q(d_smooth), as_q(d_smooth, n_grid = 101),
    grid = x_smooth_vec
  )
})

test_that("as_q methods throw error with corrupt input", {
  expect_error(
    as_q(structure(user_p, class = c("p", "pdqr"))), "f.*proper.*type"
  )
  expect_error(
    as_q(structure(user_d, class = c("d", "pdqr"))), "f.*proper.*type"
  )
  expect_error(
    as_q(structure(user_r, class = c("r", "pdqr"))), "f.*proper.*type"
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

  # Converting from r-function
  expect_error(as_q(r_smooth, n_sample = "a"), "n_sample.*single number")
  expect_error(as_q(r_smooth, n_sample = 10:11), "n_sample.*single number")
})


# as_q.default ------------------------------------------------------------
# Tested in `as_q()`


# as_q.p ------------------------------------------------------------------
# Tested in `as_q()`


# as_q.d ------------------------------------------------------------------
# Tested in `as_q()`


# as_q.r ------------------------------------------------------------------
# Tested in `as_q()`


# adjust_to_support_q -----------------------------------------------------
# Tested in `as_q()`
