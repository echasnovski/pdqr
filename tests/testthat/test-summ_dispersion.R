context("test-summ_dispersion")


# summ_var ----------------------------------------------------------------
test_that("summ_var works with 'fin' functions", {
  expect_equal_stat(summ_var, stat_list[["binom"]], "var")

  # Output isn't exact because of tail trimming during `as_d()`
  expect_equal_stat(summ_var, stat_list[["pois"]], "var", thres = 1e-4)
})

test_that("summ_var works with common 'infin' functions", {
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
  expect_true(summ_var(new_d(1, "infin")) >= 0)
})

test_that("summ_var works with dirac-like 'infin' functions", {
  d_dirac <- new_d(2, "infin")
  expect_equal(summ_var(d_dirac), 0, tolerance = 1e-7)

  d_dirac_2 <- form_mix(
    lapply(1:2, new_d, type = "infin"), weights = c(0.7, 0.3)
  )
  expect_equal(summ_var(d_dirac_2), 1^2*0.7+2^2*0.3 - 1.3^2)
})

test_that("summ_var works with winsorized 'infin' functions", {
  d_wins <- form_resupport(
    new_d(data.frame(x = 0:1, y = c(1, 1)), "infin"),
    support = c(0.25, 0.85),
    method = "winsor"
  )
  expect_equal(summ_var(d_wins), 0.0531)
})

test_that("summ_var works with 'infin' functions with few intervals", {
  d_unif_1 <- new_d(data.frame(x = 1:2, y = c(1, 1)), "infin")
  expect_equal(summ_var(d_unif_1), 1/12)

  d_unif_2 <- new_d(data.frame(x = 0:2, y = c(1, 1, 1)/2), "infin")
  expect_equal(summ_var(d_unif_2), 1/3)
})

test_that("summ_var validates input", {
  expect_error(summ_var("a"), "`f`.*function")
  expect_error(summ_var(function(x) {x}), "`f`.*pdqr")
})


# summ_sd -----------------------------------------------------------------
test_that("summ_sd works with 'fin' functions", {
  expect_equal_stat(summ_sd, stat_list[["binom"]], "sd")

  # Output isn't exact because of tail trimming during `as_d()`
  expect_equal_stat(summ_sd, stat_list[["pois"]], "sd", thres = 1e-4)
})

test_that("summ_sd works with common 'infin' functions", {
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

test_that("summ_sd works with dirac-like 'infin' functions", {
  d_dirac <- new_d(2, "infin")
  expect_equal(summ_sd(d_dirac), 0)

  d_dirac_2 <- form_mix(
    lapply(1:2, new_d, type = "infin"), weights = c(0.7, 0.3)
  )
  expect_equal(summ_sd(d_dirac_2), sqrt(1^2*0.7+2^2*0.3 - 1.3^2))
})

test_that("summ_sd works with winsorized 'infin' functions", {
  d_wins <- form_resupport(
    new_d(data.frame(x = 0:1, y = c(1, 1)), "infin"),
    support = c(0.25, 0.85),
    method = "winsor"
  )
  expect_equal(summ_sd(d_wins), sqrt(0.0531))
})

test_that("summ_sd works with 'infin' functions with few intervals", {
  d_unif_1 <- new_d(data.frame(x = 1:2, y = c(1, 1)), "infin")
  expect_equal(summ_sd(d_unif_1), sqrt(1/12))

  d_unif_2 <- new_d(data.frame(x = 0:2, y = c(1, 1, 1)/2), "infin")
  expect_equal(summ_sd(d_unif_2), sqrt(1/3))
})

test_that("summ_sd validates input", {
  expect_error(summ_sd("a"), "`f`.*function")
  expect_error(summ_sd(function(x) {x}), "`f`.*pdqr")
})

