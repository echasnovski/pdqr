context("test-summ_moment")


# Custom expectations -----------------------------------------------------
expect_all_moment_types_work <- function(f, order) {
  expect_equal(
    summ_moment(f, order, central = TRUE),
    summ_moment(f - summ_mean(f), order)
  )
  expect_equal(
    summ_moment(f, order, standard = TRUE),
    summ_moment(f / summ_sd(f), order)
  )
  expect_equal(
    summ_moment(f, order, absolute = TRUE),
    summ_moment(abs(f), order)
  )
  expect_equal(
    summ_moment(f, order, central = TRUE, standard = TRUE),
    summ_moment((f - summ_mean(f)) / summ_sd(f), order)
  )
  expect_equal(
    summ_moment(f, order, central = TRUE, absolute = TRUE),
    summ_moment(abs(f - summ_mean(f)), order)
  )
  expect_equal(
    summ_moment(f, order, standard = TRUE, absolute = TRUE),
    summ_moment(abs(f / summ_sd(f)), order)
  )
  expect_equal(
    summ_moment(f, order, central = TRUE, standard = TRUE, absolute = TRUE),
    summ_moment(abs((f - summ_mean(f)) / summ_sd(f)), order)
  )
}

expect_common_moments <- function(f) {
  expect_equal(summ_moment(f, 1, central = TRUE), 0)
  expect_equal(summ_moment(f, 2, central = TRUE, standard = TRUE), 1)

  expect_equal(summ_moment(f, 1), summ_mean(f))
  expect_equal(summ_moment(f, 2, central = TRUE), summ_var(f))
  expect_equal(
    summ_moment(f, 3, central = TRUE, standard = TRUE),
    summ_skewness(f)
  )
  expect_equal(
    summ_moment(f, 4, central = TRUE, standard = TRUE),
    summ_kurtosis(f, excess = FALSE)
  )
}


# summ_moment -------------------------------------------------------------
test_that("summ_moment works with 'discrete' functions", {
  expect_equal_stat(summ_moment, stat_list[["binom"]], "mean", order = 1)

  # Output isn't exact because of tail trimming during `as_d()`
  expect_equal_stat(
    summ_moment, stat_list[["pois"]], "mean", thres = 1e-5, order = 1
  )
})

test_that("summ_moment works with 'continuous' functions", {
  expect_equal_stat(summ_moment, stat_list[["beta"]], "mean", order = 1)
  # Big threshold because original density goes to infinity at edges
  expect_equal_stat(
    summ_moment, stat_list[["beta_inf"]], "mean", thres = 2e-2, order = 1
  )
  expect_equal_stat(
    summ_moment, stat_list[["chisq"]], "mean", thres = 3e-5, order = 1
  )
  # Big threshold because original density goes to infinity at left edge
  expect_equal_stat(
    summ_moment, stat_list[["chisq_inf"]], "mean", thres = 2e-2, order = 1
  )
  expect_equal_stat(
    summ_moment, stat_list[["exp"]], "mean", thres = 2e-5, order = 1
  )
  expect_equal_stat(summ_moment, stat_list[["norm"]], "mean", order = 1)
  expect_equal_stat(summ_moment, stat_list[["norm_2"]], "mean", order = 1)
  expect_equal_stat(summ_moment, stat_list[["unif"]], "mean", order = 1)
})

test_that("summ_moment works with dirac-like functions", {
  # Type "discrete"
  d_dirac_dis <- new_d(2, "discrete")
  expect_equal(summ_moment(d_dirac_dis, 1), 2)
  expect_equal(summ_moment(d_dirac_dis, 10), 2^10)

  # Type "continuous"
  d_dirac <- new_d(2, "continuous")
  expect_equal(summ_moment(d_dirac, 1), 2)
  expect_equal(summ_moment(d_dirac, 10), 2^10)

  d_dirac_2 <- form_mix(
    lapply(1:2, new_d, type = "continuous"), weights = c(0.7, 0.3)
  )
  expect_equal(summ_moment(d_dirac_2, 1), 1.3)
  expect_equal(summ_moment(d_dirac_2, 10), 0.7*1^10 + 0.3*2^10)
})

test_that("summ_moment agrees with other `summ_*()` functions", {
  expect_common_moments(d_dis)
  expect_common_moments(d_con)
})

test_that("summ_moment uses all arguments", {
  expect_all_moment_types_work(d_dis, 3)
  expect_all_moment_types_work(d_con, 3)
})

test_that("summ_moment works with fractional and zero `order`", {
  expect_equal(
    summ_moment(d_dis, order = 1.5),
    sum(x_dis_x_tbl[["x"]]^1.5 * x_dis_x_tbl[["prob"]])
  )
  expect_equal(summ_moment(d_con, order = 0), 1)
})

test_that("summ_moment computes infinite standard moment", {
  expect_equal(summ_moment(new_d(1, "discrete"), 1, standard = TRUE), Inf)
  expect_equal(summ_moment(new_d(1, "continuous"), 1, standard = TRUE), Inf)
})

test_that("summ_moment validates input", {
  expect_error(summ_moment("a", 1), "`f`.*not pdqr-function")
  expect_error(summ_moment(d_dis), "`order`.*missing.*order of moment")
  expect_error(summ_moment(d_dis, "a"), "`order`.*number")
  expect_error(summ_moment(d_dis, -1), "`order`.*non-negative")
  expect_error(summ_moment(d_dis, 1:2), "`order`.*single")
  expect_error(summ_moment(d_dis, 1, central = "a"), "`central`.*TRUE.*FALSE")
  expect_error(summ_moment(d_dis, 1, standard = "a"), "`standard`.*TRUE.*FALSE")
  expect_error(summ_moment(d_dis, 1, absolute = "a"), "`absolute`.*TRUE.*FALSE")
})


# summ_skewness -----------------------------------------------------------
test_that("summ_skewness works with 'discrete' functions", {
  expect_equal_stat(summ_skewness, stat_list[["binom"]], "skewness")

  # Output isn't exact because of tail trimming during `as_d()`
  expect_equal_stat(
    summ_skewness, stat_list[["pois"]], "skewness", thres = 2e-4
  )
})

test_that("summ_skewness works with 'continuous' functions", {
  expect_equal_stat(
    summ_skewness, stat_list[["beta"]], "skewness", thres = 4e-5
  )
  # Big threshold because original density goes to infinity at edges
  expect_equal_stat(
    summ_skewness, stat_list[["beta_inf"]], "skewness", thres = 5e-2
  )
  expect_equal_stat(
    summ_skewness, stat_list[["chisq"]], "skewness", thres = 4e-3
  )
  # Big threshold because original density goes to infinity at left edge
  expect_equal_stat(
    summ_skewness, stat_list[["chisq_inf"]], "skewness", thres = 4e-2
  )
  expect_equal_stat(summ_skewness, stat_list[["exp"]], "skewness", thres = 4e-3)
  expect_equal_stat(summ_skewness, stat_list[["norm"]], "skewness")
  expect_equal_stat(summ_skewness, stat_list[["norm_2"]], "skewness")
  expect_equal_stat(summ_skewness, stat_list[["unif"]], "skewness")
})

test_that("summ_skewness works with dirac-like functions", {
  # Type "discrete"
  d_dirac_dis <- new_d(2, "discrete")
  expect_equal(summ_skewness(d_dirac_dis), Inf)

  # Type "continuous"
  d_dirac <- new_d(2, "continuous")
  expect_equal(summ_skewness(d_dirac), Inf)

  d_dirac_2 <- form_mix(
    lapply(1:2, new_d, type = "continuous"), weights = c(0.7, 0.3)
  )
  expect_equal(
    summ_skewness(d_dirac_2),
    (0.7*(1-1.3)^3 + 0.3*(2-1.3)^3) / summ_sd(d_dirac_2)^3
  )
})

test_that("summ_skewness validates input", {
  expect_error(summ_skewness("a"), "`f`.*not pdqr-function")
})


# summ_kurtosis -----------------------------------------------------------
test_that("summ_kurtosis works with 'discrete' functions", {
  expect_equal_stat(summ_kurtosis, stat_list[["binom"]], "ex_kurtosis")

  # Output isn't exact because of tail trimming during `as_d()`
  expect_equal_stat(
    summ_kurtosis, stat_list[["pois"]], "ex_kurtosis", thres = 1e-3
  )
})

test_that("summ_kurtosis works with 'continuous' functions", {
  expect_equal_stat(
    summ_kurtosis, stat_list[["beta"]], "ex_kurtosis", thres = 2e-4
  )
  # Big threshold because original density goes to infinity at edges
  expect_equal_stat(
    summ_kurtosis, stat_list[["beta_inf"]], "ex_kurtosis", thres = 0.1
  )
  expect_equal_stat(
    summ_kurtosis, stat_list[["chisq"]], "ex_kurtosis", thres = 5e-2
  )
  # Big threshold because original density goes to infinity at left edge
  expect_equal_stat(
    summ_kurtosis, stat_list[["chisq_inf"]], "ex_kurtosis", thres = 0.3
  )
  expect_equal_stat(
    summ_kurtosis, stat_list[["exp"]], "ex_kurtosis", thres = 5e-2
  )
  expect_equal_stat(
    summ_kurtosis, stat_list[["norm"]], "ex_kurtosis", thres = 1e-3
  )
  expect_equal_stat(
    summ_kurtosis, stat_list[["norm_2"]], "ex_kurtosis", thres = 1e-3
  )
  expect_equal_stat(summ_kurtosis, stat_list[["unif"]], "ex_kurtosis")
})

test_that("summ_kurtosis works with dirac-like functions", {
  # Type "discrete"
  d_dirac_dis <- new_d(2, "discrete")
  expect_equal(summ_kurtosis(d_dirac_dis), Inf)

  # Type "continuous"
  d_dirac <- new_d(2, "continuous")
  expect_equal(summ_kurtosis(d_dirac), Inf)

  d_dirac_2 <- form_mix(
    lapply(1:2, new_d, type = "continuous"), weights = c(0.7, 0.3)
  )
  expect_equal(
    summ_kurtosis(d_dirac_2),
    (0.7*(1-1.3)^4 + 0.3*(2-1.3)^4) / summ_sd(d_dirac_2)^4 - 3
  )
})

test_that("summ_kurtosis uses `excess` argument", {
  expect_equal(summ_kurtosis(d_dis, excess = FALSE), summ_kurtosis(d_dis) + 3)
  expect_equal(
    summ_kurtosis(d_con, excess = FALSE), summ_kurtosis(d_con) + 3
  )
})

test_that("summ_kurtosis validates input", {
  expect_error(summ_kurtosis("a"), "`f`.*not pdqr-function")
  expect_error(summ_kurtosis(d_dis, excess = "a"), "`excess`.*TRUE.*FALSE")
})
