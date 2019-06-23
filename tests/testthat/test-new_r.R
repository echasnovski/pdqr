context("test-new_r")


# new_r -------------------------------------------------------------------
test_that("new_r works with numeric input", {
  expect_distr_fun(r_fin, "r", "fin")
  expect_equal(meta_support(r_fin), x_fin_support)
  expect_equal(meta_x_tbl(r_fin), x_fin_x_tbl)
  expect_true(all(r_fin(100) %in% x_fin_x_tbl[["x"]]))

  expect_distr_fun(r_con, "r", "continuous")
  expect_equal(round(meta_support(r_con), 2), round(x_con_support, 2))
  expect_equal(meta_x_tbl(r_con), x_con_x_tbl)
  r_con_out <- r_con(100)
  expect_true(all(
    (r_con_out >= x_con_support[1]) &
      (r_con_out <= x_con_support[2])
  ))
})

test_that("new_r returns dirac-like function with length-one numeric input",  {
  expect_ref_x_tbl(new_r(0.1, "fin"), data.frame(x = 0.1, prob = 1))
  expect_ref_x_tbl(
    new_r(0.1, "continuous"),
    data.frame(x = 0.1 + 1e-8*c(-1, 0, 1), y = 1e8*c(0, 1, 0))
  )

  # With big center value there can be problems with total integral being 1
  expect_silent(assert_pdqr_fun(new_r(1e8, "continuous")))
})

test_that("new_r works with data frame input", {
  expect_equal_r_distr(new_r(x_fin_x_tbl, "fin"), r_fin)
  expect_equal_r_distr(new_r(x_con_x_tbl, "continuous"), r_con)
})

test_that("new_r imputes data frame input", {
  expect_x_tbl_imputation(new_r)
})

test_that("new_r's output works with 'edge case' inputs", {
  expect_equal(r_fin(numeric(0)), numeric(0))
  expect_equal(r_con(numeric(0)), numeric(0))
})

test_that("new_r's output validates input", {
  expect_error(r_fin("a"), "`n`.*number")
  expect_error(r_fin(NA_real_), "`n`.*number")
  expect_error(r_fin(NaN), "`n`.*number")
  expect_error(r_fin(Inf), "`n`.*number")
  expect_error(r_fin(1:10), "`n`.*single")
  expect_error(r_fin(-1), "`n`.*non-negative")

  expect_error(r_con("a"), "`n`.*number")
  expect_error(r_con(NA_real_), "`n`.*number")
  expect_error(r_con(NaN), "`n`.*number")
  expect_error(r_con(Inf), "`n`.*number")
  expect_error(r_con(1:10), "`n`.*single")
  expect_error(r_con(-1), "`n`.*non-negative")
})

test_that("new_r's output handles `n = 0`", {
  expect_equal(r_fin(0), numeric(0))
  expect_equal(r_con(0), numeric(0))
})

test_that("new_r warns about bad `x` elements", {
  expect_warning(new_r(c(1, 0, NA), "continuous"), "x.*NA.*removed")
  expect_warning(new_r(c(1, 0, NaN), "continuous"), "x.*NaN.*removed")
  expect_warning(new_r(c(1, 0, Inf), "continuous"), "x.*infinite.*removed")
})

test_that("new_r validates input", {
  expect_error(new_r(type = "continuous"), "`x`.*missing.*numeric.*data frame")
  expect_error(new_r("a", "continuous"), "x.*numeric.*data.*frame")
  expect_error(new_r(numeric(0), "continuous"), "x.*empty")
  expect_error(new_r(x_fin), "`type`.*missing.*pdqr type")
  expect_error(new_r(x_fin, type = 1), "type.*string")
  expect_error(new_r(x_fin, type = "a"), "type.*fin.*continuous")
})

test_that("new_r handles metadata", {
  expect_equal(
    meta_all(r_fin),
    list(
      class = "r", type = "fin", support = x_fin_support, x_tbl = x_fin_x_tbl
    )
  )

  expect_named(meta_all(r_con), c("class", "type", "support", "x_tbl"))
  expect_equal(meta_x_tbl(r_con), x_con_x_tbl)
  expect_equal(round(meta_support(r_con), 2), round(x_con_support, 2))
  expect_equal(meta_all(r_con)["type"], list(type = "continuous"))
})


# new_r_fin ---------------------------------------------------------------
# Tested in `new_r()`


# new_r_con ---------------------------------------------------------------
# Tested in `new_r()`


# print.r -----------------------------------------------------------------
test_that("print.r works", {
  expect_pdqr_print(new_r, "Random generation")
})
