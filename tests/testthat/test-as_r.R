context("test-as_r")

set.seed(4444)


# Custom expectations -----------------------------------------------------
expect_equal_r_funs <- function(f_1, f_2, n_sample = 10000,
                                mean_thres = 0.1, sd_thres = 0.05) {
  smpl_1 <- f_1(n_sample)
  smpl_2 <- f_2(n_sample)

  expect_true(abs(mean(smpl_1) - mean(smpl_2)) <= mean_thres)
  expect_true(abs(sd(smpl_1) - sd(smpl_2)) <= sd_thres)

  expect_equal(class(f_1), class(f_2))
  expect_equal(meta(f_1), meta(f_2))
}

expect_different_r_funs <- function(f_1, f_2, n_sample = 10000,
                                    mean_thres = 0.1, sd_thres = 0.05) {
  smpl_1 <- f_1(n_sample)
  smpl_2 <- f_2(n_sample)

  expect_true(abs(mean(smpl_1) - mean(smpl_2)) >= mean_thres)
  expect_true(abs(sd(smpl_1) - sd(smpl_2)) >= sd_thres)
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

test_that("as_r rewrites metadata on user-defined function", {
  input <- structure(user_r, meta = list(type = "raw", new = 1), old = 2)
  output <- as_r(input, type = "smooth", support = c(0, 1))
  expect_true("old" %in% names(attributes(output)))
  expect_equal(meta(output), list(support = c(0, 1), type = "smooth"))
})

test_that("as_r adjusts user-defined function to be probability distribution", {
  input_raw <- r_raw
  attributes(input_raw) <- NULL
  output_raw <- as_r(
    input_raw, type = "raw", support = c(2, 6), warn_not_adjusted = FALSE
  )
  # Output equals to `r_fun()` applied to sample on restricted support
  output_raw_ref <- r_fun(
    x = x_raw[(x_raw >= 2) & (x_raw <= 6)], type = "raw", attach_x = FALSE
  )
  expect_equal_r_funs(output_raw, output_raw_ref)

  output_smooth <- as_r(
    user_r, type = "smooth", support = c(0.3, 0.7), warn_not_adjusted = FALSE
  )
  # Output produces only data inside support
  output_smooth_smpl <- output_smooth(1000)
  expect_true(all((output_smooth_smpl >= 0.3) & (output_smooth_smpl <= 0.7)))
})

test_that("as_r adjusts the same way as other `as_*()` functions", {
  new_p_raw <- p_raw
  attributes(new_p_raw) <- NULL
  new_p_raw <- as_p(new_p_raw, type = "raw", support = c(2, 6))
  new_d_raw <- d_raw
  attributes(new_d_raw) <- NULL
  expect_warning(new_d_raw <- as_d(new_d_raw, type = "raw", support = c(2, 6)))
  new_q_raw <- q_raw
  attributes(new_q_raw) <- NULL
  new_q_raw <- as_q(new_q_raw, type = "raw", support = c(2, 6))
  new_r_raw <- r_raw
  attributes(new_r_raw) <- NULL
  new_r_raw <- as_r(
    new_r_raw, type = "raw", support = c(2, 6), warn_not_adjusted = FALSE
  )

  expect_equal_r_funs(expect_warning(as_r(new_d_raw)), new_r_raw)
  expect_equal_r_funs(expect_warning(as_r(new_p_raw)), new_r_raw)
  expect_equal_r_funs(as_r(new_q_raw), new_r_raw)

  user_p_smooth <- as_p(user_p, type = "smooth", support = c(0.3, 0.7))
  user_d_smooth <- as_d(user_d, type = "smooth", support = c(0.3, 0.7))
  user_q_smooth <- as_q(user_q, type = "smooth", support = c(0.3, 0.7))
  user_r_smooth <- as_r(
    user_r, type = "smooth", support = c(0.3, 0.7), warn_not_adjusted = FALSE
  )

  expect_equal_r_funs(as_r(user_p_smooth), user_r_smooth)
  expect_equal_r_funs(as_r(user_d_smooth), user_r_smooth)
  expect_equal_r_funs(as_r(user_q_smooth), user_r_smooth)
})

test_that("as_r.default throws errors and warnings", {
  new_user_r_smooth_1 <- as_r(
    user_r, type = "smooth", support = c(1.5, 2), warn_not_adjusted = FALSE
  )
  expect_error(new_user_r_smooth_1(1), "[Nn]o.*inside.*support")

  new_user_r_smooth_2 <- as_r(
    user_r, type = "smooth", support = c(0.5, 1), adjust_max_iter = 1,
    warn_not_adjusted = TRUE
  )
  expect_warning(new_user_r_smooth_2(100), "there.*are.*outside.*support")
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
  input <- structure(
    function(x) {rbeta(x, 1, 2)}, class = c("pdqr_fun", "function")
  )
  output <- as_r(input, type = "smooth", support = c(0, 1))
  expect_equal(class(output), c("r_fun", "pdqr_fun", "function"))
})

test_that("as_r respects `n_grid` argument", {
  expect_different_r_funs(
    as_r(p_smooth_nox), as_r(p_smooth_nox, n_grid = 3)
  )
  expect_different_r_funs(
    as_r(d_smooth_nox), as_r(d_smooth_nox, n_grid = 3)
  )
})

test_that("as_r methods throw error with corrupt input", {
  expect_error(
    as_r(structure(user_p, class = c("p_fun", "pdqr_fun"))),
    "f.*proper.*type"
  )
  expect_error(
    as_r(structure(user_d, class = c("d_fun", "pdqr_fun"))),
    "f.*proper.*type"
  )
  expect_error(
    as_r(structure(user_q, class = c("q_fun", "pdqr_fun"))),
    "f.*proper.*type"
  )
})

test_that("as_r asserts extra arguments of methods", {
  # Default method
  expect_error(as_r(1, "smooth", c(0, 1)), "f.*function")
  expect_error(as_r(user_r, 1, c(0, 1)), "type.*string")
  expect_error(as_r(user_r, "a", c(0, 1)), "type.*raw.*smooth")
  expect_error(as_r(user_r, "smooth", "a"), "support.*numeric")
  expect_error(as_r(user_r, "smooth", 1), "support.*length 2")
  expect_error(as_r(user_r, "smooth", c(1, 0)), "support.*bigger")
  expect_error(
    as_r(user_r, "smooth", c(0, 1), adjust_max_iter = "a"), "single number"
  )
  expect_error(
    as_r(user_r, "smooth", c(0, 1), adjust_max_iter = 1:2), "single number"
  )
  expect_error(
    as_r(user_r, "smooth", c(0, 1), warn_not_adjusted = "a"), "TRUE.*FALSE"
  )
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


# adjust_to_support_r -----------------------------------------------------
# Tested in `as_r()`


# adjusting_r_impl --------------------------------------------------------
# Tested in `as_r()`


# na_outside_support ------------------------------------------------------
# Tested in `as_r()`


# adjust_final ------------------------------------------------------------
# Tested in `as_r()`


# num_elems_chr -----------------------------------------------------------
test_that("num_elems_chr works", {
  expect_match(num_elems_chr(1), "1 element")
  expect_match(num_elems_chr(11), "11 elements")
})
