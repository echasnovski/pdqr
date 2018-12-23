context("test-as_p")

set.seed(1111)


# as_p --------------------------------------------------------------------
test_that("as_p works with user-defined function", {
  expect_distr_fun(as_p(user_p, support = c(0, 1)), "p", "smooth")
  expect_error(as_p(user_p), "p-function.*supply.*support")
})

test_that("as_p rewrites metadata on user-defined function", {
  input <- structure(user_p, meta = list(type = "raw", new = 1), old = 2)
  output <- as_p(input, support = c(0, 1))
  expect_true("old" %in% names(attributes(output)))
  expect_equal(meta(output), list(support = c(0, 1), type = "smooth"))
})

test_that("as_p adjusts user-defined function to be probability distribution", {
  # Adjusted function stretches from 0 to 1 on support
  expect_equal(adj_p_smooth(c(0, 1)), c(0, 1))
  # Adjusted function equals 0 and 1 outside of support
  expect_equal(adj_p_smooth(c(-0.25, 1.25)), c(0, 1))
})

test_that("as_p adjusts the same way as other `as_*()` functions", {
  expect_equal_distr(
    as_p(adj_d_smooth), adj_p_smooth,
    grid = x_smooth_vec, thres = 10^(-5)
  )
  expect_equal_distr(
    as_p(adj_q_smooth), adj_p_smooth,
    grid = x_smooth_vec, thres = 10^(-7)
  )
})

test_that("as_p.default throws errors", {
  expect_error(
    as_p(function(x) {rep(0, length.out = length(x))}, c(0, 1)),
    "probability.*zero"
  )
})

test_that('as_p returns self in case of "p"', {
  expect_identical(as_p(p_raw), p_raw)
  expect_identical(as_p(p_smooth), p_smooth)
  expect_identical(as_p(p_custom), p_custom)
})

test_that('as_p works with "d"', {
  expect_equal_distr(
    as_p(d_raw), p_raw,
    grid = x_raw_vec_ext, thres = 10^(-3)
  )
  expect_equal_distr(
    as_p(d_smooth), p_smooth,
    grid = x_smooth_vec_ext, thres = 10^(-6)
  )
  expect_equal_distr(
    as_p(d_custom), p_custom,
    grid = x_custom
  )
})

test_that('as_p works with "q"', {
  # Precision is quite bad in case `type = "raw"` at the points of future
  # discontinuity in p-function. That is why `grid = x_raw_vec_seq` and not
  # usual `grid = x_smooth_vec_ext`
  expect_equal_distr(
    as_p(q_raw), p_raw,
    grid = x_raw_vec_seq, thres = 10^(-6)
  )
  expect_equal_distr(
    as_p(q_smooth), p_smooth,
    grid = x_smooth_vec_ext, thres = 10^(-5)
  )
  expect_equal_distr(
    as_p(q_custom), p_custom,
    grid = x_custom, thres = 10^(-4)
  )
})

test_that('as_p works with "r"', {
  expect_equal_distr(
    as_p(r_raw), p_raw,
    grid = x_raw_vec_ext, thres = 0.01,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )
  expect_equal_distr(
    as_p(r_smooth), p_smooth,
    grid = x_smooth_vec_ext, thres = 0.01,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )
  expect_equal_distr(
    as_p(r_custom), p_custom,
    # Using truncated version because of "extending" property on the support
    # edges in case `type = "smooth"`.
    grid = x_custom_trunc, thres = 0.01,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )
})

test_that('as_p works with "pdqr" (not adding duplicated class)', {
  input <- structure(
    function(x) {pbeta(x, 1, 2)}, class = c("pdqr", "function")
  )
  output <- as_p(input, support = c(0, 1))
  expect_equal(class(output), c("p", "pdqr", "function"))
})

test_that("as_p respects `n_grid` argument", {
  expect_different_distr(
    as_p(d_smooth), as_p(d_smooth, n_grid = 101),
    grid = x_smooth_vec
  )
  expect_different_distr(
    as_p(q_smooth), as_p(q_smooth, n_grid = 101),
    grid = x_smooth_vec
  )
})

test_that("as_p methods throw error with corrupt input", {
  expect_error(
    as_p(structure(user_d, class = c("d", "pdqr"))), "f.*proper.*type"
  )
  expect_error(
    as_p(structure(user_q, class = c("q", "pdqr"))), "f.*proper.*type"
  )
  expect_error(
    as_p(structure(user_r, class = c("r", "pdqr"))), "f.*proper.*type"
  )
})

test_that("as_p asserts extra arguments of methods", {
  # Default method
  expect_error(as_p(1, c(0, 1)), "f.*function")
  expect_error(as_p(user_p, "a"), "support.*numeric")
  expect_error(as_p(user_p, 1), "support.*length 2")
  expect_error(as_p(user_p, c(1, 0)), "support.*bigger")

  # Converting from r-function
  expect_error(as_p(r_smooth, n_sample = "a"), "n_sample.*single number")
  expect_error(as_p(r_smooth, n_sample = 10:11), "n_sample.*single number")
})


# as_p.default ------------------------------------------------------------
# Tested in `as_p()`


# as_p.d ------------------------------------------------------------------
# Tested in `as_p()`


# as_p.q ------------------------------------------------------------------
# Tested in `as_p()`


# as_p.r ------------------------------------------------------------------
# Tested in `as_p()`


# p_from_d_raw ------------------------------------------------------------
# Tested in `as_p()` tests for converting from d-function in case `type = "raw"`


# p_from_d_smooth ---------------------------------------------------------
# Tested in `as_p()` tests for converting from d-function in case `type =
# "smooth"`


# adjust_to_support_p -----------------------------------------------------
# Tested in `as_p()`
