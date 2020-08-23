context("test-summ_interval")


# summ_interval -----------------------------------------------------------
test_that("summ_interval works with 'discrete' functions", {
  cur_d <- new_d(data.frame(x = 1:10, prob = c(5:0, 1:4) / 25), "discrete")
  cur_q <- as_q(cur_d)
  cur_d_mean <- sum((1:10) * c(5:0, 1:4) / 25)
  cur_d_sd <- sqrt(sum((1:10)^2 * c(5:0, 1:4) / 25) - cur_d_mean^2)

  # Default method is "minwidth"
  expect_equal(
    summ_interval(cur_d, 0.5),
    summ_interval(cur_d, 0.5, method = "minwidth")
  )

  # Method "minwidth"
  expect_equal(
    summ_interval(cur_d, 0.5, method = "minwidth"),
    data.frame(left = 1, right = 4)
  )
  expect_equal(
    summ_interval(cur_d, 0.75, method = "minwidth"),
    data.frame(left = 1, right = 9)
  )

  # Method "percentile"
  expect_equal(
    summ_interval(cur_d, 0.5, method = "percentile"),
    data.frame(left = cur_q(0.25), right = cur_q(0.75))
  )
  expect_equal(
    summ_interval(cur_d, 0.75, method = "percentile"),
    data.frame(left = cur_q(0.125), right = cur_q(0.875))
  )

  # Method "sigma"
  expect_equal(
    summ_interval(cur_d, 0.5, method = "sigma"),
    data.frame(
      left  = cur_d_mean + stats::qnorm(0.25) * cur_d_sd,
      right = cur_d_mean + stats::qnorm(0.75) * cur_d_sd
    )
  )
  expect_equal(
    summ_interval(cur_d, 0.75, method = "sigma"),
    data.frame(
      left  = cur_d_mean + stats::qnorm(0.125) * cur_d_sd,
      right = cur_d_mean + stats::qnorm(0.875) * cur_d_sd
    )
  )
})

test_that("summ_interval works with 'continuous' functions", {
  cur_d <- new_d(
    data.frame(x = c(1, 2, 3, 5), y = c(1, 0, 0, 0.5)), "continuous"
  )
  cur_q <- as_q(cur_d)
  cur_d_mean <- summ_mean(cur_d)
  cur_d_sd <- summ_sd(cur_d)

  # Default method is "minwidth"
  expect_equal(
    summ_interval(cur_d, 0.5),
    summ_interval(cur_d, 0.5, method = "minwidth")
  )

  # Method "minwidth"
  expect_equal(
    summ_interval(cur_d, 0.5, method = "minwidth"),
    data.frame(left = 1, right = 2)
  )
  expect_equal(
    summ_interval(cur_d, 0.75, method = "minwidth"),
    data.frame(left = 1, right = cur_q(0.75))
  )

  # Method "percentile"
  expect_equal(
    summ_interval(cur_d, 0.5, method = "percentile"),
    data.frame(left = cur_q(0.25), right = cur_q(0.75))
  )
  expect_equal(
    summ_interval(cur_d, 0.75, method = "percentile"),
    data.frame(left = cur_q(0.125), right = cur_q(0.875))
  )

  # Method "sigma"
  expect_equal(
    summ_interval(cur_d, 0.5, method = "sigma"),
    data.frame(
      left  = cur_d_mean + stats::qnorm(0.25) * cur_d_sd,
      right = cur_d_mean + stats::qnorm(0.75) * cur_d_sd
    )
  )
  expect_equal(
    summ_interval(cur_d, 0.75, method = "sigma"),
    data.frame(
      left  = cur_d_mean + stats::qnorm(0.125) * cur_d_sd,
      right = cur_d_mean + stats::qnorm(0.875) * cur_d_sd
    )
  )
})

test_that("summ_interval works with dirac-like functions", {
  # Type "discrete"
  d_dirac_dis <- new_d(10, "discrete")
  expect_equal(
    summ_interval(d_dirac_dis, 0.95, method = "minwidth"),
    data.frame(left = 10, right = 10),
    tolerance = 1e-12
  )
  expect_equal(
    summ_interval(d_dirac_dis, 0.95, method = "percentile"),
    data.frame(left = 10, right = 10),
    tolerance = 1e-12
  )
  expect_equal(
    summ_interval(d_dirac_dis, 0.95, method = "sigma"),
    data.frame(left = 10, right = 10),
    tolerance = 1e-12
  )

  # Type "continuous"
  ## Single dirac-like peak
  d_dirac_1 <- new_d(1, "continuous")
  expect_equal(
    summ_interval(d_dirac_1, 0.95, method = "minwidth"),
    data.frame(left = 1, right = 1),
    tolerance = 1e-7
  )
  expect_equal(
    summ_interval(d_dirac_1, 0.95, method = "percentile"),
    data.frame(left = 1, right = 1),
    tolerance = 1e-7
  )
  expect_equal(
    summ_interval(d_dirac_1, 0.95, method = "sigma"),
    data.frame(left = 1, right = 1),
    tolerance = 1e-7
  )

  ## Two dirac-like peaks
  d_dirac_2 <- form_mix(
    list(new_d(1, "continuous"), new_d(2, "continuous")),
    weights = c(0.25, 0.75)
  )
  d_dirac_2_mean <- 1 * 0.25 + 2 * 0.75
  d_dirac_2_sd <- sqrt(1^2 * 0.25 + 2^2 * 0.75 - d_dirac_2_mean^2)
  d_dirac_2_supp <- meta_support(d_dirac_2)

  expect_equal(
    summ_interval(d_dirac_2, 0.75 - 1e-8, method = "minwidth"),
    data.frame(left = 2, right = 2),
    tolerance = 1e-7
  )
  expect_equal(
    summ_interval(d_dirac_2, 0.75 + 1e-8, method = "minwidth"),
    data.frame(left = 1, right = 2),
    tolerance = 1e-7
  )

  expect_equal(
    summ_interval(d_dirac_2, 0.75, method = "percentile"),
    data.frame(left = 1, right = 2),
    tolerance = 1e-7
  )

  expect_equal(
    summ_interval(d_dirac_2, 0.75, method = "sigma"),
    data.frame(
      left = max(
        d_dirac_2_supp[1],
        d_dirac_2_mean + stats::qnorm(0.125) * d_dirac_2_sd
      ),
      right = min(
        d_dirac_2_supp[2],
        d_dirac_2_mean + stats::qnorm(0.875) * d_dirac_2_sd
      )
    ),
    tolerance = 1e-9
  )
})

test_that("summ_interval works with real world cases", {
  d_unif <- as_d(dunif)
  d_unif_mean <- 0.5
  d_unif_sd <- 1 / sqrt(12)
  expect_equal(
    summ_interval(d_unif, 0.5, method = "minwidth"),
    # Output is "shifted" to the left because global mode is considered to be
    # the smallest "x" with maximum density.
    # This was also a test case for correct `which.min()` behavior in presence
    # of numerical precision storage issues.
    data.frame(left = 0, right = 0.5)
  )
  expect_equal(
    summ_interval(d_unif, 0.5, method = "percentile"),
    data.frame(left = 0.25, right = 0.75)
  )
  expect_equal(
    summ_interval(d_unif, 0.5, method = "sigma"),
    data.frame(
      left  = d_unif_mean + stats::qnorm(0.25) * d_unif_sd,
      right = d_unif_mean + stats::qnorm(0.75) * d_unif_sd
    )
  )

  d_norm <- as_d(dnorm)
  d_norm_mean <- 0
  d_norm_sd <- 1
  ## For normal distribution all three methods should give the same result
  ref_interval <- data.frame(left = qnorm(0.25), right = qnorm(0.75))
  expect_equal(
    summ_interval(d_norm, 0.5, method = "minwidth"), ref_interval,
    tolerance = 5e-6
  )
  expect_equal(
    summ_interval(d_norm, 0.5, method = "percentile"), ref_interval,
    tolerance = 5e-6
  )
  expect_equal(
    summ_interval(d_norm, 0.5, method = "sigma"), ref_interval,
    tolerance = 5e-5
  )
})

test_that("summ_interval works with `level` equal to 0 and 1", {
  cur_d <- d_con
  cur_d_mean <- summ_mean(d_con)
  cur_d_median <- summ_median(d_con)
  cur_d_mode <- summ_mode(d_con)
  cur_d_supp <- meta_support(d_con)
  lev_1_output <- data.frame(left = cur_d_supp[1], right = cur_d_supp[2])

  # Method "minwidth"
  expect_equal(
    summ_interval(cur_d, 0, method = "minwidth"),
    data.frame(left = cur_d_mode, right = cur_d_mode)
  )
  expect_equal(
    summ_interval(cur_d, 1, method = "minwidth"),
    lev_1_output
  )
  # Method "percentile"
  expect_equal(
    summ_interval(cur_d, 0, method = "percentile"),
    data.frame(left = cur_d_median, right = cur_d_median)
  )
  expect_equal(
    summ_interval(cur_d, 1, method = "percentile"),
    lev_1_output
  )
  # Method "sigma"
  expect_equal(
    summ_interval(cur_d, 0, method = "sigma"),
    data.frame(left = cur_d_mean, right = cur_d_mean)
  )
  ## This is also a test for output not being outside of input's support,
  ## because `stats::qnorm(c(0, 1))` is `c(-Inf, Inf)`
  expect_equal(
    summ_interval(cur_d, 1, method = "sigma"),
    lev_1_output
  )
})

test_that("summ_interval uses `n_grid` argument", {
  cur_d <- new_d(data.frame(x = 0:1, y = c(0, 1)), "continuous")
  expect_equal(
    summ_interval(cur_d, level = 0.1, method = "minwidth", n_grid = 1),
    # As `n_grid` equals 1, grid of candidate intervals consists from only one
    # (incorrect) element `[0; as_q(cur_d)(0.1)]`
    data.frame(left = 0, right = as_q(cur_d)(0.1))
  )
})

test_that("summ_interval validates input", {
  expect_error(summ_interval("a"), "`f`.*not pdqr-function")
  expect_error(summ_interval(d_dis, level = "a"), "`level`.*number")
  expect_error(summ_interval(d_dis, level = c(0.05, 0.1)), "`level`.*single")
  expect_error(summ_interval(d_dis, level = 1.1), "`level`.*between 0 and 1")
  expect_error(summ_interval(d_dis, level = -0.1), "`level`.*between 0 and 1")
  expect_error(summ_interval(d_dis, 0.95, method = 1), "`method`.*string")
  expect_error(summ_interval(d_dis, 0.95, method = "a"), "`method`.*one of")
  expect_error(summ_interval(d_dis, 0.95, n_grid = "a"), "`n_grid`.*number")
  expect_error(summ_interval(d_dis, 0.95, n_grid = 1:2), "`n_grid`.*single")
  expect_error(summ_interval(d_dis, 0.95, n_grid = 0.5), "`n_grid`.*1")
})


# interval_minwidth -------------------------------------------------------
# Tested in `summ_interval()`
