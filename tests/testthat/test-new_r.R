context("test-new_r")


# new_r -------------------------------------------------------------------
test_that("new_r works", {
  expect_distr_fun(r_raw, "r", "raw")
  expect_equal(meta(r_raw, "support"), x_raw_support)
  expect_true(all(r_raw(100) %in% x_raw_distr_tbl[["x"]]))

  expect_distr_fun(r_smooth, "r", "smooth")
  expect_equal(
    round(meta(r_smooth, "support"), 2), round(x_smooth_support, 2)
  )
  r_smooth_out <- r_smooth(100)
  expect_true(all(
    (r_smooth_out >= x_smooth_support[1]) &
      (r_smooth_out <= x_smooth_support[2])
  ))
})

test_that("new_r asserts", {
  expect_warning(new_r(c(1, 0, NA)), "x.*NA.*removed")
  expect_warning(new_r(c(1, 0, NaN)), "x.*NaN.*removed")
  expect_warning(new_r(c(1, 0, Inf)), "x.*infinite.*removed")

  expect_error(new_r("a"), "x.*numeric")
  expect_error(new_r(numeric(0)), "x.*empty")
  expect_error(new_r(x_raw, type = 1), "type.*string")
  expect_error(new_r(x_raw, type = "a"), "type.*raw.*smooth")
  expect_error(new_r(x_raw, attach_x = NULL), "attach_x.*TRUE.*FALSE")
})

test_that("new_r handles metadata", {
  expect_equal(
    meta(r_raw),
    list(
      support = x_raw_support, type = "raw",
      x = x_raw
    )
  )

  r_smooth_1 <- new_r(x_smooth, type = "smooth", attach_x = TRUE)
  expect_named(meta(r_smooth_1), c("support", "type", "x"))
  expect_equal(
    round(meta(r_smooth_1, "support"), 2), round(x_smooth_support, 2)
  )
  expect_equal(
    meta(r_smooth_1)[c("x", "type")],
    list(x = x_smooth, type = "smooth")
  )

  r_smooth_2 <- new_r(x_smooth, type = "smooth", extra = list(a = TRUE))
  expect_named(meta(r_smooth_2), c("extra", "support", "type"))
  expect_equal(meta(r_smooth_2, "extra"), list(a = TRUE))
})

test_that("new_r has correct default for `attach_x`", {
  expect_true("x" %in% names(meta(r_raw)))
  expect_false("x" %in% names(meta(r_smooth)))
})


# new_r_raw ---------------------------------------------------------------
# Tested in `new_r()`


# new_r_smooth ------------------------------------------------------------
# Tested in `new_r()`


# print.r -----------------------------------------------------------------
test_that("print.r works", {
  expect_pdqr_print(new_r, "Random generation")
})
