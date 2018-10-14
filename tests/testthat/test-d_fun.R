context("test-d_fun")


# d_fun -------------------------------------------------------------------
test_that("d_fun works", {
  expect_distr_fun(d_raw, "d_fun", "raw")
  expect_equal(meta(d_raw, "domain_in"), x_raw_domain_in)
  expect_equal(d_raw(1:10), c(x_raw_distr_tbl[["prob"]], 0))

  expect_distr_fun(d_smooth, "d_fun", "smooth")
  expect_equal(
    round(meta(d_smooth, "domain_in"), 2), round(x_smooth_domain_in, 2)
  )
  expect_equal(
    round(d_smooth(seq(from = -1, to = 1, by = 0.1)), 2),
    c(
      0.09, 0.1, 0.12, 0.16, 0.21, 0.27, 0.34,  0.4, 0.45, 0.49, 0.51,
      0.51, 0.5, 0.47, 0.43, 0.39, 0.36, 0.33, 0.31, 0.29, 0.28
    )
  )
})

test_that("d_fun rounds input in case of `type` = 'raw'", {
  near_1 <- 1 + 10^c(-6, -9)
  expect_equal(d_raw(near_1), c(0, 0.1))
})

test_that("d_fun output integrates to 1 in case `type` = 'smooth'", {
  integral <- stats::integrate(d_smooth, -3, 3)
  output_range <- integral[["value"]] + c(-1, 1) * integral[["abs.error"]]
  expect_true((output_range[1] <= 1) && (1 <= output_range[2]))
})

test_that("d_fun equals 0 on edges of input domain in case `type` = 'smooth'", {
  domain_in <- meta(d_smooth, "domain_in")
  expect_equal(d_smooth(domain_in), c(0, 0))
})

test_that("d_fun output works with extreme values", {
  extreme_vec <- c(-1, 1) * 10000
  expect_equal(d_raw(extreme_vec), c(0, 0))
  expect_equal(d_smooth(extreme_vec), c(0, 0))
})

test_that("d_fun asserts", {
  expect_error(d_fun("a"), "x.*numeric")
  expect_error(d_fun(x_raw, type = 1), "type.*string")
  expect_error(d_fun(x_raw, type = "a"), "type.*raw.*smooth")
  expect_error(d_fun(x_raw, attach_x = NULL), "attach_x.*TRUE.*FALSE")
})

test_that("d_fun handles metadata", {
  expect_equal(
    meta(d_raw),
    list(
      domain_in = x_raw_domain_in, type = "raw",
      x = x_raw
    )
  )

  d_smooth_1 <- d_fun(x_smooth, type = "smooth", attach_x = TRUE)
  expect_named(meta(d_smooth_1), c("domain_in", "type", "x"))
  expect_equal(
    round(meta(d_smooth_1, "domain_in"), 2), round(x_smooth_domain_in, 2)
  )
  expect_equal(
    meta(d_smooth_1)[c("x", "type")],
    list(x = x_smooth, type = "smooth")
  )

  d_smooth_2 <- d_fun(x_smooth, type = "smooth", extra = list(a = TRUE))
  expect_named(meta(d_smooth_2), c("domain_in", "extra", "type", "x"))
  expect_equal(meta(d_smooth_2, "extra"), list(a = TRUE))
})

test_that("d_fun uses `...` as arguments for `density()`", {
  d_smooth_cosine <- d_fun(x_smooth, type = "smooth", kernel = "cosine")
  expect_equal(
    round(d_smooth_cosine(seq(from = -1, to = 1, by = 0.1)), 2),
    c(
      0.09, 0.11, 0.13, 0.17, 0.22, 0.28, 0.34, 0.39, 0.44, 0.48, 0.5,
       0.5, 0.49, 0.47, 0.44,  0.4, 0.37, 0.34, 0.32,  0.3, 0.28
    )
  )
})


# d_fun_raw ---------------------------------------------------------------
# Tested in `d_fun()`


# d_fun_smooth ------------------------------------------------------------
# Tested in `d_fun()`


# print.d_fun -------------------------------------------------------------
test_that("print.d_fun works", {
  expect_output(
    print(d_raw),
    "Density function.*raw.*[mM]eta.*domain_in, type.*function"
  )

  d_smooth_extra <- d_fun(x_smooth, type = "smooth", extra = "a")
  expect_output(
    print(d_smooth_extra),
    "Density function.*smoothed.*[mM]eta.*domain_in, extra, type.*function"
  )
})
