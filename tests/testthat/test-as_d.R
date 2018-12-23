context("test-as_d")

set.seed(2222)


# as_d --------------------------------------------------------------------
test_that("as_d works with user-defined function", {
  expect_distr_fun(as_d(user_d, support = c(0, 1)), "d", "smooth")
  expect_error(as_d(user_d), "d-function.*supply.*support")
})

test_that("as_d rewrites metadata on user-defined function", {
  input <- structure(user_d, meta = list(type = "raw", new = 1), old = 2)
  output <- as_d(input, support = c(0, 1))
  expect_true("old" %in% names(attributes(output)))
  expect_equal(meta(output), list(support = c(0, 1), type = "smooth"))
})

test_that("as_d adjusts user-defined function to be probability distribution", {
  out_integral <- stats::integrate(adj_d_smooth, 0, 1)
  # Adjusted function integrates to 1 on support
  expect_true(abs(out_integral[["value"]] - 1) <= out_integral[["abs.error"]])
  # Adjusted function equals 0 outside of support
  expect_equal(adj_d_smooth(c(-0.25, 1.25)), c(0, 0))
})

test_that("as_d adjusts the same way as other `as_*()` functions", {
  expect_equal_distr(
    as_d(adj_p_smooth), adj_d_smooth,
    grid = x_smooth_vec, thres = 10^(-4)
  )
  # Due to accumulated numerical errors this can give inaccurate results
  # near support edges
  expect_equal_distr(
    as_d(adj_q_smooth), adj_d_smooth,
    grid = x_smooth_vec, thres = 10^(-4)
  )
})

test_that("as_d.default throws errors", {
  expect_error(
    as_d(function(x) {rep(0, length.out = length(x))}, c(0, 1)),
    "probability.*zero"
  )
})

test_that('as_d works with "p"', {
  expect_equal_distr(
    as_d(p_raw), d_raw,
    grid = x_raw_vec_ext, thres = 10^(-3)
  )
  expect_equal_distr(
    as_d(p_smooth), d_smooth,
    grid = x_smooth_vec_ext, thres = 10^(-3)
  )
  expect_equal_distr(
    as_d(p_custom), d_custom,
    # Behavior on support edges (here - in point 0) doesn't match the true
    # density because it is not continuous. In this package d-functions are
    # ("forcibly" on edges) constructed as continuous. Run `as_d(p_custom)(0 -
    # 0.000001 * (5:1))`. That is why edges are removed.
    grid = x_custom_inner
  )
})

test_that('as_d for "p" uses left and right derivative on support endges', {
  p_beta_1 <- as_p(function(q) {pbeta(q, 1, 2)}, c(0, 1))
  expect_equal(as_d(p_beta_1, h = 10^(-6))(0), 2, tolerance = 10^(-6))

  p_beta_2 <- as_p(function(q) {pbeta(q, 2, 1)}, c(0, 1))
  expect_equal(as_d(p_beta_2, h = 10^(-6))(1), 2, tolerance = 10^(-6))
})

test_that('as_d for "p" throws error if "type" metadata is incorrect', {
  corrupt_p <- p_smooth
  attr(corrupt_p, "meta")[["type"]] <- "a"
  expect_error(as_d(corrupt_p), "type.*raw.*or.*smooth")
})

test_that('as_d returns self in case of "d"', {
  expect_identical(as_d(d_raw), d_raw)
  expect_identical(as_d(d_smooth), d_smooth)
  expect_identical(as_d(d_custom), d_custom)
})

test_that('as_d works with "q"', {
  expect_equal_distr(
    as_d(q_raw), d_raw,
    grid = x_raw_vec_ext, thres = 10^(-3)
  )
  expect_equal_distr(
    as_d(q_smooth), d_smooth,
    grid = x_smooth_vec_ext, thres = 10^(-3)
  )
  expect_equal_distr(
    # The reason edges are removed is described in test for "p".
    as_d(q_custom), d_custom,
    grid = x_custom_inner, thres = 10^(-2)
  )
})

test_that('as_d works with "r"', {
  expect_equal_distr(
    as_d(r_raw), d_raw,
    grid = c(x_raw_vec_ext, x_raw_vec), thres = 0.01,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )
  expect_equal_distr(
    as_d(r_smooth), d_smooth,
    # Building smooth density from random generation function has somewhat worse
      # precision than building CDF
    grid = x_smooth_vec_ext, thres = 0.05,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )
  expect_equal_distr(
    as_d(r_custom), d_custom,
    # Using truncated version because of "extending" property on the support
    # edges in case `type = "smooth"`. Both this and discontinuous nature of
    # custom distribution (with big jump at 0) give bad precision.
    grid = x_custom_trunc, thres = 0.15,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )
  # Illustration of big impact of discontinuity:
  # x <- sort(x_custom)
  # plot(x, d_custom(x), type = "l")
  # lines(x, as_d(r_custom)(x), col = "red")
  # lines(x, as_d(r_custom, n_sample = 50000)(x), col = "blue")
})

test_that('as_d works with "pdqr" (not adding duplicated class)', {
  input <- structure(
    function(x) {dbeta(x, 1, 2)}, class = c("pdqr", "function")
  )
  output <- as_d(input, support = c(0, 1))
  expect_equal(class(output), c("d", "pdqr", "function"))
})

test_that("as_d respects `n_grid` argument", {
  expect_different_distr(
    as_d(q_smooth), as_d(q_smooth, n_grid = 101),
    grid = x_smooth_vec
  )
})

test_that("as_d respects `h` argument", {
  expect_different_distr(
    as_d(p_smooth), as_d(p_smooth, h = 0.1),
    grid = x_smooth_vec
  )
})

test_that("as_d methods throw error with corrupt input", {
  expect_error(
    as_d(structure(user_p, class = c("p", "pdqr"))), "f.*proper.*type"
  )
  expect_error(
    as_d(structure(user_q, class = c("q", "pdqr"))), "f.*proper.*type"
  )
  expect_error(
    as_d(structure(user_r, class = c("r", "pdqr"))), "f.*proper.*type"
  )
})

test_that("as_d asserts extra arguments of methods", {
  # Default method
  expect_error(as_d(1, c(0, 1)), "f.*function")
  expect_error(as_d(user_d, "a"), "support.*numeric")
  expect_error(as_d(user_d, 1), "support.*length 2")
  expect_error(as_d(user_d, c(1, 0)), "support.*bigger")

  # Converting from r-function
  expect_error(as_d(r_smooth, n_sample = "a"), "n_sample.*single number")
  expect_error(as_d(r_smooth, n_sample = 10:11), "n_sample.*single number")
})


# as_d.default ------------------------------------------------------------
# Tested in `as_d()`


# as_d.p ------------------------------------------------------------------
# Tested in `as_d()`


# as_d.q ------------------------------------------------------------------
# Tested in `as_d()`


# as_d.r ------------------------------------------------------------------
# Tested in `as_d()`


# adjust_to_support_d -----------------------------------------------------
# Tested in `as_d()`


# adjust_d_impl -----------------------------------------------------------
# Tested in `as_d()`
