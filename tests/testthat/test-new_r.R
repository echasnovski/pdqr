context("test-new_r")


# new_r -------------------------------------------------------------------
test_that("new_r works with numeric input", {
  expect_distr_fun(r_fin, "r", "fin")
  expect_equal(meta(r_fin, "support"), x_fin_support)
  expect_equal(meta(r_fin, "x_tbl"), x_fin_x_tbl)
  expect_true(all(r_fin(100) %in% x_fin_x_tbl[["x"]]))

  expect_distr_fun(r_smooth, "r", "smooth")
  expect_equal(
    round(meta(r_smooth, "support"), 2), round(x_smooth_support, 2)
  )
  expect_equal(meta(r_smooth, "x_tbl"), x_smooth_x_tbl)
  r_smooth_out <- r_smooth(100)
  expect_true(all(
    (r_smooth_out >= x_smooth_support[1]) &
      (r_smooth_out <= x_smooth_support[2])
  ))
})

test_that("new_r works with data frame input", {
  expect_equal_r_distr(new_r(x_fin_x_tbl, "fin"), r_fin)
  expect_equal_r_distr(new_r(x_smooth_x_tbl, "smooth"), r_smooth)
})

test_that("new_r imputes data frame input", {
  expect_x_tbl_imputation(new_r)
})

test_that("new_r asserts", {
  expect_warning(new_r(c(1, 0, NA)), "x.*NA.*removed")
  expect_warning(new_r(c(1, 0, NaN)), "x.*NaN.*removed")
  expect_warning(new_r(c(1, 0, Inf)), "x.*infinite.*removed")

  expect_error(new_r("a"), "x.*numeric.*data.*frame")
  expect_error(new_r(numeric(0)), "x.*empty")
  expect_error(new_r(x_fin, type = 1), "type.*string")
  expect_error(new_r(x_fin, type = "a"), "type.*fin.*smooth")
  expect_error(new_r(1, type = "smooth"), "at least 2")
})

test_that("new_r handles metadata", {
  expect_equal(
    meta(r_fin),
    list(support = x_fin_support, type = "fin", x_tbl = x_fin_x_tbl)
  )

  expect_named(meta(r_smooth), c("support", "type", "x_tbl"))
  expect_equal(meta(r_smooth, "x_tbl"), x_smooth_x_tbl)
  expect_equal(
    round(meta(r_smooth, "support"), 2), round(x_smooth_support, 2)
  )
  expect_equal(meta(r_smooth)["type"], list(type = "smooth"))
})


# new_r_fin ---------------------------------------------------------------
# Tested in `new_r()`


# new_r_smooth ------------------------------------------------------------
# Tested in `new_r()`


# print.r -----------------------------------------------------------------
test_that("print.r works", {
  expect_pdqr_print(new_r, "Random generation")
})
