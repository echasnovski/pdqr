context("test-d_fun")


# d_fun -------------------------------------------------------------------
test_that("d_fun works", {
  expect_distr_fun(d_raw, "d_fun", "raw")
  expect_equal(meta(d_raw, "support"), x_raw_support)
  expect_equal(d_raw(1:10), c(x_raw_distr_tbl[["prob"]], 0))

  expect_distr_fun(d_smooth, "d_fun", "smooth")
  expect_equal(
    round(meta(d_smooth, "support"), 2), round(x_smooth_support, 2)
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

test_that("d_fun output works with extreme values", {
  extreme_vec <- c(-1, 1) * 10000
  expect_equal(d_raw(extreme_vec), c(0, 0))
  expect_equal(d_smooth(extreme_vec), c(0, 0))
})

test_that("d_fun asserts", {
  expect_warning(d_fun(c(1, 0, NA)), "x.*NA.*removed")
  expect_warning(d_fun(c(1, 0, NaN)), "x.*NaN.*removed")
  expect_warning(d_fun(c(1, 0, Inf)), "x.*infinite.*removed")

  expect_error(d_fun("a"), "x.*numeric")
  expect_error(d_fun(numeric(0)), "x.*empty")
  expect_error(d_fun(x_raw, type = 1), "type.*string")
  expect_error(d_fun(x_raw, type = "a"), "type.*raw.*smooth")
  expect_error(d_fun(x_raw, attach_x = NULL), "attach_x.*TRUE.*FALSE")
})

test_that("d_fun handles metadata", {
  expect_equal(
    meta(d_raw),
    list(
      support = x_raw_support, type = "raw",
      x = x_raw
    )
  )

  d_smooth_1 <- d_fun(x_smooth, type = "smooth", attach_x = TRUE)
  expect_named(meta(d_smooth_1), c("support", "type", "x"))
  expect_equal(
    round(meta(d_smooth_1, "support"), 2), round(x_smooth_support, 2)
  )
  expect_equal(
    meta(d_smooth_1)[c("x", "type")],
    list(x = x_smooth, type = "smooth")
  )

  d_smooth_2 <- d_fun(x_smooth, type = "smooth", extra = list(a = TRUE))
  expect_named(meta(d_smooth_2), c("extra", "support", "type"))
  expect_equal(meta(d_smooth_2, "extra"), list(a = TRUE))
})

test_that("d_fun has correct default for `attach_x`", {
  expect_true("x" %in% names(meta(d_raw)))
  expect_false("x" %in% names(meta(d_smooth)))
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
  expect_pdqr_print(d_fun, "Density")
})
