context("test-as_q")

set.seed(3333)


# as_q --------------------------------------------------------------------
test_that("as_q works with user-defined function", {
  expect_distr_fun(as_q(user_q, support = c(0, 1)), "q", "smooth")
  expect_error(as_q(user_q), "q-function.*supply.*support")
})

test_that("as_q rewrites metadata on user-defined function", {
  input <- structure(user_q, meta = list(type = "raw", new = 1), old = 2)
  output <- as_q(input, support = c(0, 1))
  expect_true("old" %in% names(attributes(output)))
  expect_equal(
    meta(output), list(support = c(0, 1), type = "smooth", x_tbl = NULL)
  )
})

test_that("as_q uses `...` for currying on user-defined function", {
  output <- as_q(qnorm, c(0, 20), mean = 10, sd = 2)
  output_ref <- as_q(function(p) {qnorm(p, mean = 10, sd = 2)}, c(0, 20))
  expect_equal_distr(output, output_ref, p_vec)
})

test_that("as_q adjusts user-defined function to be probability distribution", {
  # Adjusted function stretches from 0 to 1 on support
  expect_equal(adj_q_smooth(c(0, 1)), c(0, 1))
})

test_that("as_q adjusts the same way as other `as_*()` functions", {
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
  expect_error(as_q(user_q, c(2, 3)), "probability.*zero")
})

test_that('as_q works with "p"', {
  expect_equal_distr(as_q(p_raw), q_raw, grid = p_vec)
  expect_equal_distr(as_q(p_smooth), q_smooth, grid = p_vec)
  expect_equal_distr(
    as_q(p_custom), q_custom,
    grid = p_vec, thres = 10^(-7)
  )
})

test_that('as_q works with "d"', {
  expect_equal_distr(as_q(d_raw), q_raw, grid = p_vec)
  expect_equal_distr(as_q(d_smooth), q_smooth, grid = p_vec)
  expect_equal_distr(as_q(d_custom), q_custom, grid = p_vec, thres = 10^(-7))
})

test_that('as_q returns self in case of "q"', {
  expect_identical(as_q(q_raw), q_raw)
  expect_identical(as_q(q_smooth), q_smooth)
  expect_identical(as_q(q_custom), q_custom)
})

test_that('as_q works with "r"', {
  expect_equal_distr(as_q(r_raw), q_raw, grid = p_vec)
  expect_equal_distr(as_q(r_smooth), q_smooth, grid = p_vec)
  expect_equal_distr(
    as_q(r_custom), q_custom,
    # Using truncated version because of "extending" property on the support
    # edges in case `type = "smooth"`. This introduces errors around the edges.
    # That is why truncated version of `p_vec` is used.
    grid = p_vec_trunc, thres = 0.05,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )
})

test_that('as_q works with "pdqr" (not adding duplicated class)', {
  input <- structure(
    function(x) {qbeta(x, 1, 2)}, class = c("pdqr", "function")
  )
  output <- as_q(input, support = c(0, 1))
  expect_equal(class(output), c("q", "pdqr", "function"))
})

test_that("as_q respects `n_grid` argument", {
  expect_different_distr(
    as_q(p_custom), as_q(p_custom, n_grid = 2),
    grid = x_smooth_vec
  )
  expect_different_distr(
    as_q(d_custom), as_q(d_custom, n_grid = 2),
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
  expect_error(as_q(1, c(0, 1)), "f.*function")
  expect_error(as_q(user_q, "a"), "support.*numeric")
  expect_error(as_q(user_q, 1), "support.*length 2")
  expect_error(as_q(user_q, c(1, 0)), "support.*bigger")

  # Converting from r-function
  expect_error(as_q(r_custom, n_sample = "a"), "n_sample.*single number")
  expect_error(as_q(r_custom, n_sample = 10:11), "n_sample.*single number")
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
