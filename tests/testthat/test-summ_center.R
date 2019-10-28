context("test-summ_center")


# Custom expectations -----------------------------------------------------
expect_local_mode_equals_global <- function(d_fun) {
  expect_equal(
    summ_mode(d_fun, method = "local"), summ_mode(d_fun, method = "global")
  )
}


# summ_center -------------------------------------------------------------
# More thorough testing is done in other "center" `summ_*()` functions
test_that("summ_center works", {
  # "discrete"
  expect_equal(summ_center(d_dis, "mean"), summ_mean(d_dis))
  expect_equal(summ_center(d_dis, "median"), summ_median(d_dis))
  expect_equal(summ_center(d_dis, "mode"), summ_mode(d_dis))

  # "continuous"
  expect_equal(summ_center(d_con, "mean"), summ_mean(d_con))
  expect_equal(summ_center(d_con, "median"), summ_median(d_con))
  expect_equal(summ_center(d_con, "mode"), summ_mode(d_con))
})

test_that("summ_center validates input", {
  expect_error(summ_center("a"), "`f`.*not pdqr-function")
  expect_error(summ_center(d_dis, method = 1), "`method`.*string")
  expect_error(summ_center(d_dis, method = "a"), "`method`.*one of")
})


# summ_mean ---------------------------------------------------------------
test_that("summ_mean works with 'discrete' functions", {
  expect_equal_stat(summ_mean, stat_list[["binom"]], "mean")

  # Output isn't exact because of tail trimming during `as_d()`
  expect_equal_stat(summ_mean, stat_list[["pois"]], "mean", thres = 1e-5)
})

test_that("summ_mean works with common 'continuous' functions", {
  expect_equal_stat(summ_mean, stat_list[["beta"]], "mean")
  # Big threshold because original density goes to infinity at edges
  expect_equal_stat(summ_mean, stat_list[["beta_inf"]], "mean", thres = 2e-2)
  expect_equal_stat(summ_mean, stat_list[["chisq"]], "mean", thres = 3e-5)
  # Big threshold because original density goes to infinity at left edge
  expect_equal_stat(summ_mean, stat_list[["chisq_inf"]], "mean", thres = 2e-2)
  expect_equal_stat(summ_mean, stat_list[["exp"]], "mean", thres = 2e-5)
  expect_equal_stat(summ_mean, stat_list[["norm"]], "mean")
  expect_equal_stat(summ_mean, stat_list[["norm_2"]], "mean")
  expect_equal_stat(summ_mean, stat_list[["unif"]], "mean")
})

test_that("summ_mean works with dirac-like 'continuous' functions", {
  d_dirac <- new_d(2, "continuous")
  expect_equal(summ_mean(d_dirac), 2)

  d_dirac_2 <- form_mix(
    lapply(1:2, new_d, type = "continuous"), weights = c(0.7, 0.3)
  )
  expect_equal(summ_mean(d_dirac_2), 1.3)
})

test_that("summ_mean works with winsorized 'continuous' functions", {
  d_wins <- form_resupport(
    new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous"),
    support = c(0.25, 0.85),
    method = "winsor"
  )
  expect_equal(summ_mean(d_wins), 0.25*0.25 + 0.55*0.6 + 0.85*0.15)
})

test_that("summ_mean works with zero probability spaces in distribution", {
  expect_equal(
    summ_mean(new_d(data.frame(x = 1:4, prob = c(0.6, 0, 0, 0.4)), "discrete")),
    2.2
  )
  expect_equal(
    summ_mean(
      new_d(data.frame(x = 1:6, y = c(0, 0.5, 0, 0, 0.5, 0)), "continuous")
    ),
    3.5
  )
})

test_that("summ_mean works with 'continuous' functions with few intervals", {
  d_unif_1 <- new_d(data.frame(x = 1:2, y = c(1, 1)), "continuous")
  expect_equal(summ_mean(d_unif_1), 1.5)

  d_unif_2 <- new_d(data.frame(x = 0:2, y = c(1, 1, 1)/2), "continuous")
  expect_equal(summ_mean(d_unif_2), 1)
})

test_that("summ_mean validates input", {
  expect_error(summ_mean("a"), "`f`.*not pdqr-function")
})


# summ_median -------------------------------------------------------------
test_that("summ_median works with 'discrete' functions", {
  expect_equal_stat(summ_median, stat_list[["binom"]], "median")

  # Output isn't exact because of tail trimming during `as_d()`
  expect_equal_stat(summ_median, stat_list[["pois"]], "median")
})

test_that("summ_median not always mimic `median()`", {
  expect_equal(summ_median(new_q(1:10, "discrete")), 5)
  expect_equal(summ_median(new_q(1:10, "continuous")), 5.5)
})

test_that("summ_median works with common 'continuous' functions", {
  expect_equal_stat(summ_median, stat_list[["beta"]], "median")
  # Big threshold because original density goes to infinity at edges
  expect_equal_stat(
    summ_median, stat_list[["beta_inf"]], "median", thres = 2e-2
  )
  expect_equal_stat(summ_median, stat_list[["chisq"]], "median", thres = 5e-6)
  # Big threshold because original density goes to infinity at left edge
  expect_equal_stat(
    summ_median, stat_list[["chisq_inf"]], "median", thres = 2e-2
  )
  expect_equal_stat(summ_median, stat_list[["exp"]], "median", thres = 5e-6)
  expect_equal_stat(summ_median, stat_list[["norm"]], "median")
  expect_equal_stat(summ_median, stat_list[["norm_2"]], "median")
  expect_equal_stat(summ_median, stat_list[["unif"]], "median")
})

test_that("summ_median works with dirac-like 'continuous' functions", {
  d_dirac <- new_d(2, "continuous")
  expect_equal(summ_median(d_dirac), 2)

  # Using equal weights ends up with numerical accuracy representation of
  # `cumprob` field in `meta_x_tbl(d_dirac_2)`. Therefore, output of
  # `summ_median(d_dirac_2)` is 2.
  d_dirac_2 <- form_mix(
    lapply(1:2, new_d, type = "continuous"), weights = c(0.7, 0.3)
  )
  expect_equal(summ_median(d_dirac_2), 1)
})

test_that("summ_median works with winsorized 'continuous' functions", {
  d_wins <- form_resupport(
    new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous"),
    support = c(0.25, 0.85),
    method = "winsor"
  )
  expect_equal(summ_median(d_wins), 0.5)
})

test_that("summ_median works with zero probability spaces in distribution", {
  expect_equal(
    summ_median(
      new_d(data.frame(x = 1:4, prob = c(0.6, 0, 0, 0.4)), "discrete")
    ),
    1
  )
  expect_equal(
    summ_median(
      new_d(data.frame(x = 1:6, y = c(0, 0.5, 0, 0, 0.5, 0)), "continuous")
    ),
    3
  )
})

test_that("summ_median works with 'continuous' functions with few intervals", {
  d_unif_1 <- new_d(data.frame(x = 1:2, y = c(1, 1)), "continuous")
  expect_equal(summ_median(d_unif_1), 1.5)

  d_unif_2 <- new_d(data.frame(x = 0:2, y = c(1, 1, 1)/2), "continuous")
  expect_equal(summ_median(d_unif_2), 1)
})

test_that("summ_median validates input", {
  expect_error(summ_median("a"), "`f`.*not pdqr-function")
})


# summ_mode ---------------------------------------------------------------
test_that("summ_mode works with 'discrete' functions", {
  # Method "global" (default)
  expect_equal_stat(summ_mode, stat_list[["binom"]], "mode", thres = 1e-12)
    # The expected output should be 4, but on low-precision i386 platform it
    # seems that `which.max(dpois(4:5, 5))` returns 2 (instead of 1, which is
    # the first one among two *equal* values). If test is not relaxed, CRAN
    # check fails.
  pois_mode <- summ_mode(as_d(dpois, lambda = 5L))
  expect_true((abs(pois_mode - 4) < 1e-12) || (abs(pois_mode - 5) < 1e-12))

  # Method "local"
  expect_local_mode_equals_global(stat_list[["binom"]][["d_fun"]])
    # Poisson distribution's number of local modes depends on "integerishness"
    # of `labmda` parameter
  expect_equal(summ_mode(as_d(dpois, lambda = 10.1), method = "local"), 10)
  expect_equal(summ_mode(as_d(dpois, lambda = 10), method = "local"), c(9, 10))
})

test_that("summ_mode works with common 'continuous' functions", {
  # Method "global" (default)
  expect_equal_stat(summ_mode, stat_list[["beta"]], "mode")
  expect_equal_stat(summ_mode, stat_list[["beta_inf"]], "mode")
  expect_equal_stat(summ_mode, stat_list[["chisq"]], "mode")
  expect_equal_stat(summ_mode, stat_list[["chisq_inf"]], "mode")
  expect_equal_stat(summ_mode, stat_list[["exp"]], "mode")
  expect_equal_stat(summ_mode, stat_list[["norm"]], "mode")
  expect_equal_stat(summ_mode, stat_list[["norm_2"]], "mode")
  expect_equal_stat(summ_mode, stat_list[["unif"]], "mode")

  # Method "local"
  expect_local_mode_equals_global(stat_list[["beta"]][["d_fun"]])
    # Both "infinities" are local modes
  expect_equal(
    summ_mode(stat_list[["beta_inf"]][["d_fun"]], method = "local"),
    c(0, 1)
  )
  expect_local_mode_equals_global(stat_list[["chisq"]][["d_fun"]])
  expect_local_mode_equals_global(stat_list[["chisq_inf"]][["d_fun"]])
  expect_local_mode_equals_global(stat_list[["exp"]][["d_fun"]])
  expect_local_mode_equals_global(stat_list[["norm"]][["d_fun"]])
  expect_local_mode_equals_global(stat_list[["norm_2"]][["d_fun"]])
    # All points in "x_tbl" are local modes
  expect_equal(
    summ_mode(stat_list[["unif"]][["d_fun"]], method = "local"),
    meta_x_tbl(stat_list[["unif"]][["d_fun"]])[["x"]]
  )
})

test_that("summ_mode works with dirac-like 'continuous' functions", {
  d_dirac <- new_d(2, "continuous")
  d_dirac_2 <- form_mix(
    lapply(1:2, new_d, type = "continuous"), weights = c(0.7, 0.3)
  )

  # Method "global" (default)
  expect_equal(summ_mode(d_dirac), 2)
  expect_equal(summ_mode(d_dirac_2), 1)

  # Method "local"
  expect_equal(summ_mode(d_dirac, method = "local"), 2)
  expect_equal(summ_mode(d_dirac_2, method = "local"), c(1, 2))
})

test_that("summ_mode works with winsorized 'continuous' functions", {
  # Method "global" (default)
  d_wins <- form_resupport(
    new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous"),
    support = c(0.25, 0.85),
    method = "winsor"
  )
  expect_equal(summ_mode(d_wins), 0.25)

  # Method "local"
  d_norm_wins <- form_resupport(as_d(dnorm), c(-1, 2), method = "winsor")
  expect_equal(summ_mode(d_norm_wins, method = "local"), c(-1, 0, 2))
})

test_that("summ_mode works with plateaus in distribution", {
  d_plateau_dis <- new_d(
    data.frame(x = 1:5, prob = c(0.15, 0.2, 0.2, 0.2, 0.25)), "discrete"
  )
  d_plateau_dis_2 <- new_d(
    data.frame(x = 1:5, prob = c(0.1, 0.25, 0.25, 0.25, 0.15)), "discrete"
  )
  # `y` isn't exact and will get renormalized
  d_plateau_con <- new_d(
    data.frame(x = 1:5, y = c(1, 2, 2, 2, 3)), "continuous"
  )
  d_plateau_con_2 <- new_d(
    data.frame(x = 1:5, y = c(1, 2, 2, 2, 1)), "continuous"
  )

  # Method "global" (default)
  expect_equal(summ_mode(d_plateau_dis), 5)
    # Returns the smallest "x" with highest probability
  expect_equal(summ_mode(d_plateau_dis_2), 2)
  expect_equal(summ_mode(d_plateau_con), 5)
    # Returns the smallest "x" with highest probability
  expect_equal(summ_mode(d_plateau_con_2), 2)

  # Method "local"
  expect_equal(summ_mode(d_plateau_dis, method = "local"), c(2, 3, 5))
  expect_equal(summ_mode(d_plateau_dis_2, method = "local"), c(2, 3, 4))
  expect_equal(summ_mode(d_plateau_con, method = "local"), c(2, 3, 5))
  expect_equal(summ_mode(d_plateau_con_2, method = "local"), c(2, 3, 4))
})

test_that("summ_mode validates input", {
  expect_error(summ_mode("a"), "`f`.*not pdqr-function")
  expect_error(summ_mode(d_dis, method = 1), "`method`.*string")
  expect_error(summ_mode(d_dis, method = "a"), "`method`.*one of")
})


# summ_mean_con -----------------------------------------------------------
# Tested in `summ_mean()`
