context("test-utils-summ")


# Custom expectations -----------------------------------------------------
expect_raw_moment_works <- function(stat_data, thres_1 = 1e-6, thres_2 = 1e-6) {
  cur_d <- stat_data[["d_fun"]]
  cur_mean <- stat_data[["mean"]]

  expect_equal(raw_moment(cur_d, 1), cur_mean, tolerance = thres_1)
  expect_equal(
    raw_moment(cur_d, 2), stat_data[["var"]] + cur_mean^2,
    tolerance = thres_2
  )
}


# raw_moment --------------------------------------------------------------
test_that("raw_moment works with 'discrete' functions", {
  # Small thresholds because in case of finite support moments should be exact
  expect_raw_moment_works(
    stat_list[["binom"]], thres_1 = 1e-12, thres_2 = 1e-12
  )
  # Here moments aren't exact because tail trimming is done during `as_d()`
  expect_raw_moment_works(stat_list[["pois"]], thres_1 = 1e-5, thres_2 = 1e-5)
})

test_that("raw_moment works with common 'continuous' functions", {
  expect_raw_moment_works(stat_list[["beta"]])
  # Big thresholds because original density goes to infinity at edges
  expect_raw_moment_works(
    stat_list[["beta_inf"]], thres_1 = 2e-2, thres_2 = 2e-2
  )
  expect_raw_moment_works(stat_list[["chisq"]], thres_1 = 3e-5, thres_2 = 1e-3)
  # Big thresholds because original density goes to infinity at left edge
  expect_raw_moment_works(
    stat_list[["chisq_inf"]], thres_1 = 2e-2, thres_2 = 5e-2
  )
  expect_raw_moment_works(stat_list[["exp"]], thres_1 = 2e-5, thres_2 = 3e-4)
  expect_raw_moment_works(stat_list[["norm"]], thres_2 = 5e-5)
  expect_raw_moment_works(stat_list[["norm_2"]])
  expect_raw_moment_works(stat_list[["unif"]])
})

test_that("raw_moment works with dirac-like 'continuous' functions", {
  d_dirac <- new_d(2, "continuous")
  expect_equal(raw_moment(d_dirac, 1), 2)
  expect_equal(raw_moment(d_dirac, 2), 4)
  expect_equal(raw_moment(d_dirac, 10), 1024)
})

test_that("raw_moment works with 'continuous' functions with few intervals", {
  d_unif_1 <- new_d(data.frame(x = 1:2, y = c(1, 1)), "continuous")
  expect_equal(raw_moment(d_unif_1, 1), 1.5)
  expect_equal(raw_moment(d_unif_1, 2), 1/12 + 1.5^2)

  d_unif_2 <- new_d(data.frame(x = 0:2, y = c(1, 1, 1)/2), "continuous")
  expect_equal(raw_moment(d_unif_2, 1), 1)
  expect_equal(raw_moment(d_unif_2, 2), 1/3 + 1^2)
})


# raw_moment_con ----------------------------------------------------------
# Tested in `raw_moment()`


# compute_density_crossings -----------------------------------------------
test_that("compute_density_crossings works", {
  cur_d_1 <- new_d(data.frame(x = 1:6, y = c(0, 1, 0, 0, 1, 0)), "continuous")
  cur_d_2 <- new_d(
    data.frame(x = 1:6+0.5, y = c(0, 1, 0, 0, 1, 0)), "continuous"
  )
  expect_equal(
    compute_density_crossings(cur_d_1, cur_d_2), c(2.25, 3.5, 4, 5.25)
  )

  # Acceptence of different pdqr-functions
  expect_equal(
    compute_density_crossings(cur_d_1, as_p(cur_d_2)),
    compute_density_crossings(as_q(cur_d_1), as_r(cur_d_2))
  )
})

test_that("compute_density_crossings handles 'intervals of identity'", {
  # All consecutive intervals on which densities are identical should be treated
  # as a single identity interval and be represented in output with its edges.

  # "Partial" identity
  cur_d_1 <- new_d(data.frame(x = 1:5, y = c(0, 0, 0, 1, 0)), "continuous")
  cur_d_2 <- new_d(
    data.frame(x = c(1:4, 4.5, 5.5, 6), y = c(0, 0, 0, 1, 0, 0, 1)),
    "continuous"
  )
  expect_equal(compute_density_crossings(cur_d_1, cur_d_2), c(1, 4, 5))

  # Total identity
  cur_d <- new_d(data.frame(x = 1:6, y = c(0, 1, 1, 0, 1, 0)), "continuous")
  expect_equal(compute_density_crossings(cur_d, cur_d), c(1, 6))
})

test_that("compute_density_crossings handles single intervals in 'x_tbl'", {
  cur_d_1 <- new_d(data.frame(x = 1:2, y = c(1, 0)), "continuous")
  cur_d_2 <- new_d(data.frame(x = 1:2, y = c(0, 1)), "continuous")
  expect_equal(compute_density_crossings(cur_d_1, cur_d_2), 1.5)
})

test_that("compute_density_crossings handles intersection on grid", {
  cur_d_1 <- new_d(data.frame(x = 1:3, y = c(2, 1, 2)/3), "continuous")
  cur_d_2 <- new_d(data.frame(x = 0:4, y = c(2, 0, 1, 0, 2)/3), "continuous")
  expect_equal(compute_density_crossings(cur_d_1, cur_d_2), 2)
})

test_that("compute_density_crossings handles intersection on edge", {
  # Right edge of first argument
  cur_d_1 <- new_d(data.frame(x = 1:2, y = c(1, 0)), "continuous")
  cur_d_2 <- new_d(data.frame(x = 2:3, y = c(0, 1)), "continuous")
  expect_equal(compute_density_crossings(cur_d_1, cur_d_2), 2)

  # Left edge of first argument
  cur_d_1 <- new_d(data.frame(x = 1:2, y = c(1, 0)), "continuous")
  cur_d_2 <- new_d(data.frame(x = 0:1, y = c(0, 1)), "continuous")
  expect_equal(compute_density_crossings(cur_d_1, cur_d_2), 1)
})

test_that("compute_density_crossings handles no intersections", {
  # Non-trivial intersection support
  cur_d_1 <- new_d(data.frame(x = 1:2, y = c(1, 1)), "continuous")
  cur_d_2 <- new_d(data.frame(x = c(0, 3), y = c(1, 1)/3), "continuous")
  expect_equal(compute_density_crossings(cur_d_1, cur_d_2), numeric(0))

  # No intersection support
  cur_d_3 <- new_d(data.frame(x = 11:12, y = c(1, 1)), "continuous")
  expect_equal(compute_density_crossings(cur_d_1, cur_d_3), numeric(0))
  expect_equal(compute_density_crossings(cur_d_3, cur_d_1), numeric(0))

  # Intersection support consists from one value
  cur_d_4 <- new_d(data.frame(x = c(-1, 1), y = c(1, 1)/2), "continuous")
  expect_equal(compute_density_crossings(cur_d_1, cur_d_4), numeric(0))
})

test_that("compute_density_crossings works with real world cases", {
  # Case of crossing
  d_norm <- as_d(dnorm)
  d_norm_2 <- as_d(dnorm, mean = 1, sd = 0.1)
  inters <- compute_density_crossings(d_norm, d_norm_2)
  expect_true(length(inters) == 2)
  expect_equal(d_norm(inters), d_norm_2(inters))

  # Case of no crossing
  d_unif <- as_d(dunif, min = 2, max = 5)
  expect_equal(compute_density_crossings(d_norm, d_unif), numeric(0))
})


# compute_cdf_crossings ---------------------------------------------------
test_that("compute_cdf_crossings works", {
  cur_p_1 <- new_p(data.frame(x = 1:2, y = c(1, 1)), "continuous")
  cur_p_2 <- new_p(data.frame(x = c(0, 4), y = c(1, 1)/4), "continuous")
  expect_equal(compute_cdf_crossings(cur_p_1, cur_p_2), 4/3)

  # Acceptence of different pdqr-functions
  expect_equal(
    compute_density_crossings(cur_p_1, as_p(cur_p_2)),
    compute_density_crossings(as_q(cur_p_1), as_r(cur_p_2))
  )
})

test_that("compute_cdf_crossings handles 'intervals of identity'", {
  # All consecutive intervals on which CDFs are identical should be treated as a
  # single identity interval and be represented in output with its edges.

  # "Partial" identity
  cur_p_1 <- new_p(
    data.frame(x = c(1, 1.5, 2, 3, 3.5, 4), y = c(1, 0, 1, 1, 0, 1)),
    "continuous"
  )
  cur_p_2 <- new_p(data.frame(x = 1:4, y = c(0, 1, 1, 0)), "continuous")
  expect_equal(compute_cdf_crossings(cur_p_1, cur_p_2), c(1, 2, 3, 4))

  # Total identity
  expect_equal(compute_cdf_crossings(d_con, d_con), meta_support(d_con))
})

test_that("compute_cdf_crossings handles no intersections", {
  # No intersection support
  cur_p_1 <- new_p(data.frame(x = 0:1, y = c(1, 1)), "continuous")
  cur_p_2 <- new_p(data.frame(x = 11:12, y = c(1, 1)), "continuous")
  expect_equal(compute_cdf_crossings(cur_p_1, cur_p_2), numeric(0))
  expect_equal(compute_cdf_crossings(cur_p_2, cur_p_1), numeric(0))

  # Intersection support consists from one value
  cur_p_3 <- new_p(data.frame(x = 1:2, y = c(1, 1)), "continuous")
  expect_equal(compute_cdf_crossings(cur_p_1, cur_p_3), numeric(0))
})

test_that("compute_cdf_crossings works with real world cases", {
  # Case of crossing
  p_norm <- as_p(pnorm)
  p_norm_2 <- as_p(pnorm, mean = 1, sd = 0.1)
  inters <- compute_cdf_crossings(p_norm, p_norm_2)
  expect_true(length(inters) == 1)
  expect_equal(p_norm(inters), p_norm_2(inters))

  # Case of no crossing
  p_unif <- as_p(punif, min = 2, max = 5)
  expect_equal(compute_cdf_crossings(p_norm, p_unif), numeric(0))
})


# pair_cdf_data -----------------------------------------------------------
# Tested in `compute_cdf_crossings()`


# na_sqrt -----------------------------------------------------------------
test_that("na_sqrt works", {
  expect_equal(na_sqrt(c(-4, 0, 4)), c(NA, 0, 2))
})


# na_outside --------------------------------------------------------------
test_that("na_outside works", {
  expect_equal(na_outside(1:5, 2, 4), c(NA, 2:4, NA))
  expect_equal(na_outside(1:5, 0, 6), 1:5)
  expect_equal(na_outside(1:5, 2.5, 2.5), rep(NA_integer_, 5))
})


