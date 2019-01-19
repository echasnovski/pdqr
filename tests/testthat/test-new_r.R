context("test-new_r")


# new_r -------------------------------------------------------------------
test_that("new_r works with numeric input", {
  expect_distr_fun(r_fin, "r", "fin")
  expect_equal(meta_support(r_fin), x_fin_support)
  expect_equal(meta_x_tbl(r_fin), x_fin_x_tbl)
  expect_true(all(r_fin(100) %in% x_fin_x_tbl[["x"]]))

  expect_distr_fun(r_infin, "r", "infin")
  expect_equal(round(meta_support(r_infin), 2), round(x_infin_support, 2))
  expect_equal(meta_x_tbl(r_infin), x_infin_x_tbl)
  r_infin_out <- r_infin(100)
  expect_true(all(
    (r_infin_out >= x_infin_support[1]) &
      (r_infin_out <= x_infin_support[2])
  ))
})

test_that("new_r works with data frame input", {
  expect_equal_r_distr(new_r(x_fin_x_tbl, "fin"), r_fin)
  expect_equal_r_distr(new_r(x_infin_x_tbl, "infin"), r_infin)
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
  expect_error(new_r(x_fin, type = "a"), "type.*fin.*infin")
  expect_error(new_r(1, type = "infin"), "at least 2")
})

test_that("new_r handles metadata", {
  expect_equal(
    meta(r_fin),
    list(support = x_fin_support, type = "fin", x_tbl = x_fin_x_tbl)
  )

  expect_named(meta(r_infin), c("support", "type", "x_tbl"))
  expect_equal(meta_x_tbl(r_infin), x_infin_x_tbl)
  expect_equal(round(meta_support(r_infin), 2), round(x_infin_support, 2))
  expect_equal(meta(r_infin)["type"], list(type = "infin"))
})


# new_r_fin ---------------------------------------------------------------
# Tested in `new_r()`


# new_r_infin -------------------------------------------------------------
# Tested in `new_r()`


# print.r -----------------------------------------------------------------
test_that("print.r works", {
  expect_pdqr_print(new_r, "Random generation")
})
