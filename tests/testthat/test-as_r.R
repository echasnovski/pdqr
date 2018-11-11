context("test-as_r")

set.seed(4444)


# Custom expectations -----------------------------------------------------
expect_equal_r_funs <- function(f_1, f_2, n_sample = 1000,
                                mean_thres = 0.1, sd_thres = 0.05) {
  smpl_1 <- f_1(n_sample)
  smpl_2 <- f_2(n_sample)

  expect_true(abs(mean(smpl_1) - mean(smpl_2)) <= mean_thres)
  expect_true(abs(sd(smpl_1) - sd(smpl_2)) <= sd_thres)

  expect_equal(class(f_1), class(f_2))
  expect_equal(meta(f_1), meta(f_2))
}


# as_r --------------------------------------------------------------------
test_that("as_r works with user-defined function", {
  expect_distr_fun(
    as_r(user_r, type = "smooth", support = c(0, 1)), "r_fun", "smooth"
  )
  expect_error(as_r(user_r), "r_fun.*supply.*type.*support")
  expect_error(as_r(user_r, type = "smooth"), "r_fun.*supply.*support")
  expect_error(as_r(user_r, support = c(0, 1)), "r_fun.*supply.*type")
})

test_that('as_r works with "p_fun"', {
  expect_equal_r_funs(as_r(p_raw_withx), r_raw_withx)

  expect_silent(as_r(p_raw_nox, warn_precision = FALSE))
  expect_warning(
    r_from_p_raw_nox <- as_r(p_raw_nox),
    "from.*cumulative.*raw.*not.*precise.*value"
  )

  # With "raw" type and low number of unique values errors might be quite big
  expect_equal_r_funs(r_from_p_raw_nox, r_raw_nox, mean_thres = 0.2)

  expect_equal_r_funs(as_r(p_smooth_withx), r_smooth_withx)
  expect_equal_r_funs(as_r(p_smooth_nox), r_smooth_nox)

  # Custom functions have smooth nature, so error should be lower
  expect_equal_r_funs(
    as_r(p_custom), r_custom,
    mean_thres = 0.05, sd_thres = 0.01
  )
})

test_that('as_r works with "d_fun"', {
  # With "raw" type and low number of unique values errors might be quite big
  expect_equal_r_funs(
    as_r(d_raw_withx), r_raw_withx,
    mean_thres = 0.25, sd_thres = 0.1
  )

  expect_silent(as_r(d_raw_nox, warn_precision = FALSE))
  expect_warning(
    r_from_d_raw_nox <- as_r(d_raw_nox),
    "from.*density.*raw.*not.*precise"
  )

  expect_equal_r_funs(
    r_from_d_raw_nox, r_raw_nox,
    mean_thres = 0.2, sd_thres = 0.1
  )

  expect_equal_r_funs(
    as_r(d_smooth_withx), r_smooth_withx,
    sd_thres = 0.075
  )
  expect_equal_r_funs(as_r(d_smooth_nox), r_smooth_nox)

  # Custom functions have smooth nature, so error should be lower
  expect_equal_r_funs(
    as_r(d_custom), r_custom,
    mean_thres = 0.05, sd_thres = 0.01
  )
})

test_that('as_r works with "q_fun"', {
  expect_equal_r_funs(
    as_r(q_raw_withx), r_raw_withx,
    sd_thres = 0.1
  )
  expect_equal_r_funs(
    as_r(q_raw_nox), r_raw_nox,
    mean_thres = 0.2, sd_thres = 0.1
  )

  expect_equal_r_funs(as_r(q_smooth_withx), r_smooth_withx)
  expect_equal_r_funs(as_r(q_smooth_nox), r_smooth_nox)

  # Custom functions have smooth nature, so error should be lower
  expect_equal_r_funs(
    as_r(q_custom), r_custom,
    mean_thres = 0.05, sd_thres = 0.01
  )
})

test_that('as_r returns self in case of "r_fun"', {
  expect_identical(as_r(r_raw_withx), r_raw_withx)
  expect_identical(as_r(r_raw_nox), r_raw_nox)
  expect_identical(as_r(r_smooth_withx), r_smooth_withx)
  expect_identical(as_r(r_smooth_nox), r_smooth_nox)
  expect_identical(as_r(r_custom), r_custom)
})

test_that('as_r works with "pdqr_fun" (not adding duplicated class)', {
  input <- structure(rbeta, class = c("pdqr_fun", "function"))
  output <- as_r(input, type = "smooth", support = c(0, 1))
  expect_equal(class(output), c("r_fun", "pdqr_fun", "function"))
})

test_that("as_r asserts extra arguments of methods", {
  # Default method
  expect_error(as_r(1, "smooth", c(0, 1)), "f.*function")
  expect_error(as_r(user_r, 1, c(0, 1)), "type.*string")
  expect_error(as_r(user_r, "a", c(0, 1)), "type.*raw.*smooth")
  expect_error(as_r(user_r, "smooth", "a"), "support.*numeric")
  expect_error(as_r(user_r, "smooth", 1), "support.*length 2")
  expect_error(as_r(user_r, "smooth", c(1, 0)), "support.*bigger")
})


# as_r.default ------------------------------------------------------------
# Tested in `as_r()`


# as_r.p_fun --------------------------------------------------------------
# Tested in `as_r()`


# as_r.d_fun --------------------------------------------------------------
# Tested in `as_r()`


# as_r.q_fun --------------------------------------------------------------
# Tested in `as_r()`


# as_r_impl ---------------------------------------------------------------
# Tested in `as_r()`
