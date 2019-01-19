context("test-meta")


# meta --------------------------------------------------------------------
test_that("meta throws errors on bad input", {
  expect_error(meta(1), "`f`.*environment")

  f_from_global <- function(x) {x}
  environment(f_from_global) <- globalenv()
  expect_error(meta(f_from_global), "`f`.*[Gg]lobal")
})


# has_meta ----------------------------------------------------------------
test_that("has_meta works", {
  input <- function(x) {x}
  assign("type", "fin", environment(input))

  expect_true(has_meta(input, "type"))
  expect_false(has_meta(input, "support"))
})


# pdqr_type ---------------------------------------------------------------
test_that("pdqr_type works", {
  input <- function(x) {x}
  assign("type", "a", environment(input))
  expect_equal(pdqr_type(input), "a")
})

test_that("pdqr_type throws errors on bad input", {
  expect_error(pdqr_type(1), "`f`.*environment")

  f_from_global <- function(x) {x}
  environment(f_from_global) <- globalenv()
  expect_error(meta(f_from_global), "`f`.*[Gg]lobal")
})


# pdqr_support ------------------------------------------------------------
test_that("pdqr_support works", {
  input <- function(x) {x}
  assign("support", "a", environment(input))
  expect_equal(pdqr_support(input), "a")
})

test_that("pdqr_support throws errors on bad input", {
  expect_error(pdqr_support(1), "`f`.*environment")

  f_from_global <- function(x) {x}
  environment(f_from_global) <- globalenv()
  expect_error(meta(f_from_global), "`f`.*[Gg]lobal")
})


# pdqr_x_tbl --------------------------------------------------------------
test_that("pdqr_x_tbl works", {
  input <- function(x) {x}
  assign("x_tbl", "a", environment(input))
  expect_equal(pdqr_x_tbl(input), "a")
})

test_that("pdqr_x_tbl throws errors on bad input", {
  expect_error(pdqr_x_tbl(1), "`f`.*environment")

  f_from_global <- function(x) {x}
  environment(f_from_global) <- globalenv()
  expect_error(meta(f_from_global), "`f`.*[Gg]lobal")
})


# check_f_envir -----------------------------------------------------------
test_that("check_f_envir works", {
  expect_silent(check_f_envir(p_fin))
  expect_error(check_f_envir(1), "`f`.*environment")

  f_from_global <- function(x) {x}
  environment(f_from_global) <- globalenv()
  expect_error(meta(f_from_global), "`f`.*[Gg]lobal")
})
