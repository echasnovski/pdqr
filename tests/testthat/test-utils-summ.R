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
test_that("raw_moment works with 'fin' functions", {
  # Small thresholds because in case of finite support moments should be exact
  expect_raw_moment_works(
    stat_list[["binom"]], thres_1 = 1e-12, thres_2 = 1e-12
  )
  # Here moments aren't exact because tail trimming is done during `as_d()`
  expect_raw_moment_works(stat_list[["pois"]], thres_1 = 1e-5, thres_2 = 1e-5)
})

test_that("raw_moment works with common 'infin' functions", {
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

test_that("raw_moment works with dirac-like 'infin' functions", {
  d_dirac <- new_d(2, "infin")
  expect_equal(raw_moment(d_dirac, 1), 2)
  expect_equal(raw_moment(d_dirac, 2), 4)
  expect_equal(raw_moment(d_dirac, 10), 1024)
})

test_that("raw_moment works with 'infin' functions with few intervals", {
  d_unif_1 <- new_d(data.frame(x = 1:2, y = c(1, 1)), "infin")
  expect_equal(raw_moment(d_unif_1, 1), 1.5)
  expect_equal(raw_moment(d_unif_1, 2), 1/12 + 1.5^2)

  d_unif_2 <- new_d(data.frame(x = 0:2, y = c(1, 1, 1)/2), "infin")
  expect_equal(raw_moment(d_unif_2, 1), 1)
  expect_equal(raw_moment(d_unif_2, 2), 1/3 + 1^2)
})


# raw_moment_infin --------------------------------------------------------
# Tested in `raw_moment()`
