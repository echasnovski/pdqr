context("test-summ_spread")


# summ_spread -------------------------------------------------------------
# More thorough testing is done in other "spread" `summ_*()` functions
test_that("summ_spread works", {
  # "discrete"
  expect_equal(summ_spread(d_dis, "var"), summ_var(d_dis))
  expect_equal(summ_spread(d_dis, "sd"), summ_sd(d_dis))
  expect_equal(summ_spread(d_dis, "iqr"), summ_iqr(d_dis))
  expect_equal(summ_spread(d_dis, "mad"), summ_mad(d_dis))
  expect_equal(summ_spread(d_dis, "range"), summ_range(d_dis))

  # "continuous"
  expect_equal(summ_spread(d_con, "var"), summ_var(d_con))
  expect_equal(summ_spread(d_con, "sd"), summ_sd(d_con))
  expect_equal(summ_spread(d_con, "iqr"), summ_iqr(d_con))
  expect_equal(summ_spread(d_con, "mad"), summ_mad(d_con))
  expect_equal(summ_spread(d_con, "range"), summ_range(d_con))
})

test_that("summ_spread validates input", {
  expect_error(summ_spread("a"), "`f`.*not pdqr-function")
  expect_error(summ_spread(d_dis, method = 1), "`method`.*string")
  expect_error(summ_spread(d_dis, method = "a"), "`method`.*one of")
})


# summ_sd -----------------------------------------------------------------
test_that("summ_sd works with 'discrete' functions", {
  expect_equal_stat(summ_sd, stat_list[["binom"]], "sd")

  # Output isn't exact because of tail trimming during `as_d()`
  expect_equal_stat(summ_sd, stat_list[["pois"]], "sd", thres = 1e-4)
})

test_that("summ_sd works with common 'continuous' functions", {
  expect_equal_stat(summ_sd, stat_list[["beta"]], "sd", thres = 5e-6)
  expect_equal_stat(summ_sd, stat_list[["beta_inf"]], "sd", thres = 1e-3)
  expect_equal_stat(summ_sd, stat_list[["chisq"]], "sd", thres = 5e-4)
  # Big threshold because original density goes to infinity at left edge
  expect_equal_stat(summ_sd, stat_list[["chisq_inf"]], "sd", thres = 1e-2)
  expect_equal_stat(summ_sd, stat_list[["exp"]], "sd", thres = 2e-4)
  expect_equal_stat(summ_sd, stat_list[["norm"]], "sd", thres = 5e-5)
  expect_equal_stat(summ_sd, stat_list[["norm_2"]], "sd", thres = 5e-6)
  expect_equal_stat(summ_sd, stat_list[["unif"]], "sd")
})

test_that("summ_sd works with dirac-like 'continuous' functions", {
  d_dirac <- new_d(2, "continuous")
  expect_equal(summ_sd(d_dirac), 0)

  d_dirac_2 <- form_mix(
    lapply(1:2, new_d, type = "continuous"), weights = c(0.7, 0.3)
  )
  expect_equal(summ_sd(d_dirac_2), sqrt(1^2 * 0.7 + 2^2 * 0.3 - 1.3^2))
})

test_that("summ_sd works with winsorized 'continuous' functions", {
  d_wins <- form_resupport(
    new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous"),
    support = c(0.25, 0.85),
    method = "winsor"
  )
  expect_equal(summ_sd(d_wins), sqrt(0.0531))
})

test_that("summ_sd works with 'continuous' functions with few intervals", {
  d_unif_1 <- new_d(data.frame(x = 1:2, y = c(1, 1)), "continuous")
  expect_equal(summ_sd(d_unif_1), sqrt(1 / 12))

  d_unif_2 <- new_d(data.frame(x = 0:2, y = c(1, 1, 1) / 2), "continuous")
  expect_equal(summ_sd(d_unif_2), sqrt(1 / 3))
})

test_that("summ_sd validates input", {
  expect_error(summ_sd("a"), "`f`.*not pdqr-function")
})


# summ_var ----------------------------------------------------------------
test_that("summ_var works with 'discrete' functions", {
  expect_equal_stat(summ_var, stat_list[["binom"]], "var")

  # Output isn't exact because of tail trimming during `as_d()`
  expect_equal_stat(summ_var, stat_list[["pois"]], "var", thres = 1e-4)
})

test_that("summ_var works with common 'continuous' functions", {
  expect_equal_stat(summ_var, stat_list[["beta"]], "var")
  expect_equal_stat(summ_var, stat_list[["beta_inf"]], "var", thres = 5e-4)
  expect_equal_stat(summ_var, stat_list[["chisq"]], "var", thres = 1e-3)
  # Big threshold because original density goes to infinity at left edge
  expect_equal_stat(summ_var, stat_list[["chisq_inf"]], "var", thres = 2e-2)
  expect_equal_stat(summ_var, stat_list[["exp"]], "var", thres = 4e-4)
  expect_equal_stat(summ_var, stat_list[["norm"]], "var", thres = 1e-4)
  expect_equal_stat(summ_var, stat_list[["norm_2"]], "var")
  expect_equal_stat(summ_var, stat_list[["unif"]], "var")
})

test_that("summ_var always returns non-negative value", {
  # Due to numerical accuracy representation, variance of dirac-like function
  # is although very small in absolute values, can result in negative value if
  # `summ_var()` isn't carefully implemented.
  expect_true(summ_var(new_d(1, "continuous")) >= 0)
})

test_that("summ_var works with dirac-like 'continuous' functions", {
  d_dirac <- new_d(2, "continuous")
  expect_equal(summ_var(d_dirac), 0, tolerance = 1e-7)

  d_dirac_2 <- form_mix(
    lapply(1:2, new_d, type = "continuous"), weights = c(0.7, 0.3)
  )
  expect_equal(summ_var(d_dirac_2), 1^2 * 0.7 + 2^2 * 0.3 - 1.3^2)
})

test_that("summ_var works with winsorized 'continuous' functions", {
  d_wins <- form_resupport(
    new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous"),
    support = c(0.25, 0.85),
    method = "winsor"
  )
  expect_equal(summ_var(d_wins), 0.0531)
})

test_that("summ_var works with 'continuous' functions with few intervals", {
  d_unif_1 <- new_d(data.frame(x = 1:2, y = c(1, 1)), "continuous")
  expect_equal(summ_var(d_unif_1), 1 / 12)

  d_unif_2 <- new_d(data.frame(x = 0:2, y = c(1, 1, 1) / 2), "continuous")
  expect_equal(summ_var(d_unif_2), 1 / 3)
})

test_that("summ_var validates input", {
  expect_error(summ_var("a"), "`f`.*not pdqr-function")
})


# summ_iqr ----------------------------------------------------------------
test_that("summ_iqr works with 'discrete' functions", {
  expect_equal_stat(summ_iqr, stat_list[["binom"]], "iqr")
  expect_equal_stat(summ_iqr, stat_list[["pois"]], "iqr")
})

test_that("summ_iqr works with common 'continuous' functions", {
  expect_equal_stat(summ_iqr, stat_list[["beta"]], "iqr")
  # Big threshold because original density goes to infinity at edges
  expect_equal_stat(summ_iqr, stat_list[["beta_inf"]], "iqr", thres = 2e-2)
  expect_equal_stat(summ_iqr, stat_list[["chisq"]], "iqr", thres = 1e-5)
  # Big threshold because original density goes to infinity at left edge
  expect_equal_stat(summ_iqr, stat_list[["chisq_inf"]], "iqr", thres = 2e-2)
  expect_equal_stat(summ_iqr, stat_list[["exp"]], "iqr", thres = 5e-6)
  expect_equal_stat(summ_iqr, stat_list[["norm"]], "iqr", thres = 6e-6)
  expect_equal_stat(summ_iqr, stat_list[["norm_2"]], "iqr")
  expect_equal_stat(summ_iqr, stat_list[["unif"]], "iqr")
})

test_that("summ_iqr works with dirac-like 'continuous' functions", {
  d_dirac <- new_d(2, "continuous")
  expect_equal(summ_iqr(d_dirac), 0, tolerance = 5e-8)

  d_dirac_2 <- form_mix(
    lapply(1:2, new_d, type = "continuous"), weights = c(0.7, 0.3)
  )
  expect_equal(summ_iqr(d_dirac_2), 1)
})

test_that("summ_iqr works with winsorized 'continuous' functions", {
  d_wins <- form_resupport(
    new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous"),
    support = c(0.25, 0.85),
    method = "winsor"
  )
  expect_equal(summ_iqr(d_wins), 0.5)
})

test_that("summ_iqr works with 'continuous' functions with few intervals", {
  d_unif_1 <- new_d(data.frame(x = 1:2, y = c(1, 1)), "continuous")
  expect_equal(summ_iqr(d_unif_1), 0.5)

  d_unif_2 <- new_d(data.frame(x = 0:2, y = c(1, 1, 1) / 2), "continuous")
  expect_equal(summ_iqr(d_unif_2), 1)
})

test_that("summ_iqr validates input", {
  expect_error(summ_iqr("a"), "`f`.*not pdqr-function")
})


# summ_mad ----------------------------------------------------------------
test_that("summ_mad works with 'discrete' functions", {
  expect_equal_stat(summ_mad, stat_list[["binom"]], "mad")
  expect_equal_stat(summ_mad, stat_list[["pois"]], "mad")
})

test_that("summ_mad works with common 'continuous' functions", {
  # Many tests are "exact" because in `stat_list` reference was computed
  # numerically
  expect_equal_stat(summ_mad, stat_list[["beta"]], "mad")
  expect_equal_stat(summ_mad, stat_list[["beta_inf"]], "mad")
  expect_equal_stat(summ_mad, stat_list[["chisq"]], "mad")
  expect_equal_stat(summ_mad, stat_list[["chisq_inf"]], "mad")
  expect_equal_stat(summ_mad, stat_list[["exp"]], "mad")
  expect_equal_stat(summ_mad, stat_list[["norm"]], "mad", thres = 4e-6)
  expect_equal_stat(summ_mad, stat_list[["norm_2"]], "mad")
  expect_equal_stat(summ_mad, stat_list[["unif"]], "mad")
})

test_that("summ_mad works with dirac-like 'continuous' functions", {
  d_dirac <- new_d(2, "continuous")
  expect_equal(summ_mad(d_dirac), 0)

  d_dirac_2 <- form_mix(
    lapply(1:2, new_d, type = "continuous"), weights = c(0.7, 0.3)
  )
  expect_equal(summ_mad(d_dirac_2), 0)
})

test_that("summ_mad works with winsorized 'continuous' functions", {
  d_wins <- form_resupport(
    new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous"),
    support = c(0.25, 0.85),
    method = "winsor"
  )
  expect_equal(summ_mad(d_wins), 0.25)
})

test_that("summ_mad works with 'continuous' functions with few intervals", {
  d_unif_1 <- new_d(data.frame(x = 1:2, y = c(1, 1)), "continuous")
  expect_equal(summ_mad(d_unif_1), 0.25)

  d_unif_2 <- new_d(data.frame(x = 0:2, y = c(1, 1, 1) / 2), "continuous")
  expect_equal(summ_mad(d_unif_2), 0.5)
})

test_that("summ_mad validates input", {
  expect_error(summ_mad("a"), "`f`.*not pdqr-function")
})


# summ_range --------------------------------------------------------------
test_that("summ_range works with 'discrete' functions", {
  cur_dis_1 <- new_d(data.frame(x = 1:4, prob = c(0.5, 0, 0, 0.5)), "discrete")
  expect_equal(summ_range(cur_dis_1), 4 - 1)

  cur_dis_2 <- new_d(data.frame(x = 1:4, prob = c(0.5, 0, 0.5, 0)), "discrete")
  expect_equal(summ_range(cur_dis_2), 3 - 1)

  cur_dis_3 <- new_d(data.frame(x = 1:4, prob = c(0, 0.5, 0, 0.5)), "discrete")
  expect_equal(summ_range(cur_dis_3), 4 - 2)

  cur_dis_4 <- new_d(data.frame(x = 1:4, prob = c(0, 0.5, 0.5, 0)), "discrete")
  expect_equal(summ_range(cur_dis_4), 3 - 2)

  cur_dis_5 <- new_d(data.frame(x = 1:4, prob = c(0, 0.5, 0, 0)), "discrete")
  expect_equal(summ_range(cur_dis_5), 0)
})

test_that("summ_range works with 'continuous' functions", {
  cur_con_1 <- new_d(data.frame(x = 1:5, y = c(1, 0, 0, 0, 1)), "continuous")
  expect_equal(summ_range(cur_con_1), 5 - 1)

  cur_con_2 <- new_d(data.frame(x = 1:5, y = c(1, 0, 1, 0, 0)), "continuous")
  ## Here range of positive probability is [1; 4] because of piecewise-linear
  ## nature of density
  expect_equal(summ_range(cur_con_2), 4 - 1)

  cur_con_3 <- new_d(data.frame(x = 1:5, y = c(0, 0, 1, 0, 1)), "continuous")
  ## Here range of positive probability is [2; 5]
  expect_equal(summ_range(cur_con_3), 5 - 2)

  cur_con_4 <- new_d(data.frame(x = 1:5, y = c(0, 0, 1, 1, 0)), "continuous")
  ## Here range of positive probability is [2; 5]
  expect_equal(summ_range(cur_con_4), 5 - 2)

  cur_con_5 <- new_d(data.frame(x = 1:5, y = c(0, 0, 1, 0, 0)), "continuous")
  ## Here range of positive probability is [2; 4]
  expect_equal(summ_range(cur_con_5), 4 - 2)
})

test_that("summ_range validates input", {
  expect_error(summ_range("a"), "`f`.*not pdqr-function")
})
