context("test-meta")


# meta_all ----------------------------------------------------------------
test_that("meta_all validates input", {
  expect_error(meta_all(1), "`f`.*environment")

  f_from_global <- function(x) {x}
  environment(f_from_global) <- globalenv()
  expect_error(meta_all(f_from_global), "`f`.*[Gg]lobal")
})


# meta_type ---------------------------------------------------------------
test_that("meta_type works", {
  input <- function(x) {x}
  assign("type", "a", environment(input))
  expect_equal(meta_type(input), "a")
})

test_that("meta_type validates input", {
  expect_error(meta_type(1), "`f`.*environment")

  f_from_global <- function(x) {x}
  environment(f_from_global) <- globalenv()
  expect_error(meta_type(f_from_global), "`f`.*[Gg]lobal")
})


# meta_support ------------------------------------------------------------
test_that("meta_support works", {
  input <- function(x) {x}
  assign("support", "a", environment(input))
  expect_equal(meta_support(input), "a")
})

test_that("meta_support validates input", {
  expect_error(meta_support(1), "`f`.*environment")

  f_from_global <- function(x) {x}
  environment(f_from_global) <- globalenv()
  expect_error(meta_support(f_from_global), "`f`.*[Gg]lobal")
})


# meta_x_tbl --------------------------------------------------------------
test_that("meta_x_tbl works", {
  input <- function(x) {x}
  assign("x_tbl", "a", environment(input))
  expect_equal(meta_x_tbl(input), "a")
})

test_that("meta_x_tbl validates input", {
  expect_error(meta_x_tbl(1), "`f`.*environment")

  f_from_global <- function(x) {x}
  environment(f_from_global) <- globalenv()
  expect_error(meta_x_tbl(f_from_global), "`f`.*[Gg]lobal")
})


# has_meta ----------------------------------------------------------------
test_that("has_meta works", {
  input <- function(x) {x}
  assign("type", "fin", environment(input))

  expect_true(has_meta(input, "type"))
  expect_false(has_meta(input, "support"))
})


# check_f_envir -----------------------------------------------------------
test_that("check_f_envir works", {
  expect_silent(check_f_envir(p_fin))
  expect_error(check_f_envir(1), "`f`.*environment")

  f_from_global <- function(x) {x}
  environment(f_from_global) <- globalenv()
  expect_error(check_f_envir(f_from_global), "`f`.*[Gg]lobal")
})
