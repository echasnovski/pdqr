context("test-new_r")


# r_fun -------------------------------------------------------------------
test_that("r_fun works", {
  expect_distr_fun(r_raw, "r_fun", "raw")
  expect_equal(meta(r_raw, "support"), x_raw_support)
  expect_true(all(r_raw(100) %in% x_raw_distr_tbl[["x"]]))

  expect_distr_fun(r_smooth, "r_fun", "smooth")
  expect_equal(
    round(meta(r_smooth, "support"), 2), round(x_smooth_support, 2)
  )
  r_smooth_out <- r_smooth(100)
  expect_true(all(
    (r_smooth_out >= x_smooth_support[1]) &
      (r_smooth_out <= x_smooth_support[2])
  ))
})

test_that("r_fun asserts", {
  expect_warning(r_fun(c(1, 0, NA)), "x.*NA.*removed")
  expect_warning(r_fun(c(1, 0, NaN)), "x.*NaN.*removed")
  expect_warning(r_fun(c(1, 0, Inf)), "x.*infinite.*removed")

  expect_error(r_fun("a"), "x.*numeric")
  expect_error(r_fun(numeric(0)), "x.*empty")
  expect_error(r_fun(x_raw, type = 1), "type.*string")
  expect_error(r_fun(x_raw, type = "a"), "type.*raw.*smooth")
  expect_error(r_fun(x_raw, attach_x = NULL), "attach_x.*TRUE.*FALSE")
})

test_that("r_fun handles metadata", {
  expect_equal(
    meta(r_raw),
    list(
      support = x_raw_support, type = "raw",
      x = x_raw
    )
  )

  r_smooth_1 <- r_fun(x_smooth, type = "smooth", attach_x = TRUE)
  expect_named(meta(r_smooth_1), c("support", "type", "x"))
  expect_equal(
    round(meta(r_smooth_1, "support"), 2), round(x_smooth_support, 2)
  )
  expect_equal(
    meta(r_smooth_1)[c("x", "type")],
    list(x = x_smooth, type = "smooth")
  )

  r_smooth_2 <- r_fun(x_smooth, type = "smooth", extra = list(a = TRUE))
  expect_named(meta(r_smooth_2), c("extra", "support", "type"))
  expect_equal(meta(r_smooth_2, "extra"), list(a = TRUE))
})

test_that("r_fun has correct default for `attach_x`", {
  expect_true("x" %in% names(meta(r_raw)))
  expect_false("x" %in% names(meta(r_smooth)))
})


# r_fun_raw ---------------------------------------------------------------
# Tested in `r_fun()`


# r_fun_smooth ------------------------------------------------------------
# Tested in `r_fun()`


# print.r_fun -------------------------------------------------------------
test_that("print.r_fun works", {
  expect_pdqr_print(r_fun, "Random generation")
})
