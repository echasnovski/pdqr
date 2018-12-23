context("test-new_r")


# new_r -------------------------------------------------------------------
test_that("new_r works", {
  expect_distr_fun(r_raw, "r", "raw")
  expect_equal(meta(r_raw, "support"), x_raw_support)
  expect_true(all(r_raw(100) %in% x_raw_x_tbl[["x"]]))

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
})

test_that("new_r handles metadata", {
  expect_equal(
    meta(r_raw),
    list(support = x_raw_support, type = "raw", x_tbl = x_raw_x_tbl)
  )

  expect_named(meta(r_smooth), c("support", "type", "x_tbl"))
  expect_equal(meta(r_smooth, "x_tbl"), x_smooth_x_tbl)
  expect_equal(
    round(meta(r_smooth, "support"), 2), round(x_smooth_support, 2)
  )
  expect_equal(meta(r_smooth)["type"], list(type = "smooth"))
})


# new_r_raw ---------------------------------------------------------------
# Tested in `new_r()`


# new_r_smooth ------------------------------------------------------------
# Tested in `new_r()`


# print.r -----------------------------------------------------------------
test_that("print.r works", {
  expect_pdqr_print(new_r, "Random generation")
})
