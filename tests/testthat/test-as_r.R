context("test-as_r")

set.seed(4444)


# Custom expectations -----------------------------------------------------
expect_different_r_funs <- function(f_1, f_2, n_sample = 10000,
                                    mean_thres = 0.1, sd_thres = 0.05) {
  smpl_1 <- f_1(n_sample)
  smpl_2 <- f_2(n_sample)

  expect_true(abs(mean(smpl_1) - mean(smpl_2)) >= mean_thres)
  expect_true(abs(sd(smpl_1) - sd(smpl_2)) >= sd_thres)
}


# as_r --------------------------------------------------------------------
test_that("as_r works with user-defined function", {
  expect_distr_fun(as_r(user_r, support = c(0, 1)), "r", "smooth")
  expect_error(as_r(user_r), "r-function.*supply.*support")
})

test_that("as_r rewrites metadata on user-defined function", {
  input <- structure(user_r, meta = list(type = "raw", new = 1), old = 2)
  output <- as_r(input, support = c(0, 1))
  expect_true("old" %in% names(attributes(output)))
  expect_equal(meta(output), list(support = c(0, 1), type = "smooth"))
})

test_that("as_r adjusts user-defined function to be probability distribution", {
  # Adjusted function produces only data inside support
  output_smooth_smpl <- adj_r_smooth(1000)
  expect_true(all((output_smooth_smpl >= 0) & (output_smooth_smpl <= 1)))
})

test_that("as_r adjusts the same way as other `as_*()` functions", {
  expect_equal_r_funs(as_r(adj_p_smooth), adj_r_smooth)
  expect_equal_r_funs(as_r(adj_d_smooth), adj_r_smooth)
  expect_equal_r_funs(as_r(adj_q_smooth), adj_r_smooth)
})

test_that("as_r.default throws errors and warnings", {
  new_user_r_smooth_1 <- as_r(
    user_r, support = c(1.5, 2), warn_not_adjusted = FALSE
  )
  expect_error(new_user_r_smooth_1(1), "[Nn]o.*inside.*support")

  new_user_r_smooth_2 <- as_r(
    user_r, support = c(0.5, 1), adjust_max_iter = 1,
    warn_not_adjusted = TRUE
  )
  expect_warning(new_user_r_smooth_2(100), "there.*are.*outside.*support")
})

test_that('as_r works with "p"', {
  # With "raw" type and low number of unique values errors might be quite big
  expect_equal_r_funs(as_r(p_raw), r_raw, mean_thres = 0.2)
  expect_equal_r_funs(as_r(p_smooth), r_smooth)

  # Custom functions have smooth nature, so error should be lower
  expect_equal_r_funs(
    as_r(p_custom), r_custom,
    mean_thres = 0.05, sd_thres = 0.01
  )
})

test_that('as_r works with "d"', {
  # With "raw" type and low number of unique values errors might be quite big
  expect_equal_r_funs(
    as_r(d_raw), r_raw,
    mean_thres = 0.2, sd_thres = 0.1
  )
  expect_equal_r_funs(as_r(d_smooth), r_smooth)

  # Custom functions have smooth nature, so error should be lower
  expect_equal_r_funs(
    as_r(d_custom), r_custom,
    mean_thres = 0.05, sd_thres = 0.01
  )
})

test_that('as_r works with "q"', {
  expect_equal_r_funs(
    as_r(q_raw), r_raw,
    mean_thres = 0.2, sd_thres = 0.1
  )

  expect_equal_r_funs(as_r(q_smooth), r_smooth)

  # Custom functions have smooth nature, so error should be lower
  expect_equal_r_funs(
    as_r(q_custom), r_custom,
    mean_thres = 0.05, sd_thres = 0.01
  )
})

test_that('as_r returns self in case of "r"', {
  expect_identical(as_r(r_raw), r_raw)
  expect_identical(as_r(r_smooth), r_smooth)
  expect_identical(as_r(r_custom), r_custom)
})

test_that('as_r works with "pdqr" (not adding duplicated class)', {
  input <- structure(
    function(x) {rbeta(x, 1, 2)}, class = c("pdqr", "function")
  )
  output <- as_r(input, support = c(0, 1))
  expect_equal(class(output), c("r", "pdqr", "function"))
})

test_that("as_r respects `n_grid` argument", {
  expect_different_r_funs(
    as_r(p_smooth), as_r(p_smooth, n_grid = 3)
  )
  expect_different_r_funs(
    as_r(d_smooth), as_r(d_smooth, n_grid = 3)
  )
})

test_that("as_r methods throw error with corrupt input", {
  expect_error(
    as_r(structure(user_p, class = c("p", "pdqr"))), "f.*proper.*type"
  )
  expect_error(
    as_r(structure(user_d, class = c("d", "pdqr"))), "f.*proper.*type"
  )
  expect_error(
    as_r(structure(user_q, class = c("q", "pdqr"))), "f.*proper.*type"
  )
})

test_that("as_r asserts extra arguments of methods", {
  # Default method
  expect_error(as_r(1, c(0, 1)), "f.*function")
  expect_error(as_r(user_r, "a"), "support.*numeric")
  expect_error(as_r(user_r, 1), "support.*length 2")
  expect_error(as_r(user_r, c(1, 0)), "support.*bigger")
  expect_error(
    as_r(user_r, c(0, 1), adjust_max_iter = "a"), "single number"
  )
  expect_error(
    as_r(user_r, c(0, 1), adjust_max_iter = 1:2), "single number"
  )
  expect_error(
    as_r(user_r, c(0, 1), warn_not_adjusted = "a"), "TRUE.*FALSE"
  )
})


# as_r.default ------------------------------------------------------------
# Tested in `as_r()`


# as_r.p ------------------------------------------------------------------
# Tested in `as_r()`


# as_r.d ------------------------------------------------------------------
# Tested in `as_r()`


# as_r.q ------------------------------------------------------------------
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
