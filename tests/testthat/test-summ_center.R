context("test-summ_center")


# summ_mean ---------------------------------------------------------------
test_that("summ_mean works with 'fin' functions", {
  expect_equal_stat(summ_mean, stat_list[["binom"]], "mean")

  # Output isn't exact because of tail trimming during `as_d()`
  expect_equal_stat(summ_mean, stat_list[["pois"]], "mean", thres = 1e-5)
})

test_that("summ_mean works with common 'infin' functions", {
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

test_that("summ_mean works with dirac-like 'infin' functions", {
  d_dirac <- new_d(2, "infin")
  expect_equal(summ_mean(d_dirac), 2)
})

test_that("summ_mean works with 'infin' functions with few intervals", {
  d_unif_1 <- new_d(data.frame(x = 1:2, y = c(1, 1)), "infin")
  expect_equal(summ_mean(d_unif_1), 1.5)

  d_unif_2 <- new_d(data.frame(x = 0:2, y = c(1, 1, 1)/2), "infin")
  expect_equal(summ_mean(d_unif_2), 1)
})

test_that("summ_mean asserts bad input", {
  expect_error(summ_mean("a"), "`f`.*function")
  expect_error(summ_mean(function(x) {x}), "`f`.*pdqr")
})


# summ_median -------------------------------------------------------------
test_that("summ_median works with 'fin' functions", {
  expect_equal_stat(summ_median, stat_list[["binom"]], "median")

  # Output isn't exact because of tail trimming during `as_d()`
  expect_equal_stat(summ_median, stat_list[["pois"]], "median")
})

test_that("summ_median not always mimic `median()`", {
  expect_equal(summ_median(new_q(1:10, "fin")), 5)
  expect_equal(summ_median(new_q(1:10, "infin")), 5.5)
})

test_that("summ_median works with common 'infin' functions", {
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

test_that("summ_median works with dirac-like 'infin' functions", {
  d_dirac <- new_d(2, "infin")
  expect_equal(summ_median(d_dirac), 2)
})

test_that("summ_median works with 'infin' functions with few intervals", {
  d_unif_1 <- new_d(data.frame(x = 1:2, y = c(1, 1)), "infin")
  expect_equal(summ_median(d_unif_1), 1.5)

  d_unif_2 <- new_d(data.frame(x = 0:2, y = c(1, 1, 1)/2), "infin")
  expect_equal(summ_median(d_unif_2), 1)
})

test_that("summ_median asserts bad input", {
  expect_error(summ_median("a"), "`f`.*function")
  expect_error(summ_median(function(x) {x}), "`f`.*pdqr")
})


# summ_mean_infin ---------------------------------------------------------
# Tested in `summ_mean()`
