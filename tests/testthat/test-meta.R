context("test-meta")


# meta_all ----------------------------------------------------------------
test_that("meta_all works", {
  expect_equal(
    meta_all(d_dis),
    list(
      class = meta_class(d_dis),
      type = meta_type(d_dis),
      support = meta_support(d_dis),
      x_tbl = meta_x_tbl(d_dis))
  )
})

test_that("meta_all validates input", {
  expect_error(meta_all(1), "`f`.*environment")

  f_from_global <- function(x) {x}
  environment(f_from_global) <- globalenv()
  expect_error(meta_all(f_from_global), "`f`.*[Gg]lobal")
})


# meta_class --------------------------------------------------------------
test_that("meta_class works", {
  expect_equal(meta_class(d_dis), "d")
  expect_equal(meta_class(p_dis), "p")
  expect_equal(meta_class(q_con), "q")
  expect_equal(meta_class(r_con), "r")

  expect_equal(meta_class(structure("a", class = "p")), "p")
  expect_equal(meta_class(structure("a", class = c("p", "d"))), "p")
  expect_equal(meta_class(structure("a", class = "bbb")), NA_character_)
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
  assign("type", "discrete", environment(input))

  expect_true(has_meta(input, "type"))
  expect_false(has_meta(input, "support"))
})


# assert_f_envir ----------------------------------------------------------
test_that("assert_f_envir works", {
  expect_silent(assert_f_envir(p_dis))
  expect_error(assert_f_envir(1), "`f`.*environment")

  f_from_global <- function(x) {x}
  environment(f_from_global) <- globalenv()
  expect_error(assert_f_envir(f_from_global), "`f`.*[Gg]lobal")
})

test_that("assert_f_envir respects glopal options", {
  op <- options(pdqr.assert_args = FALSE)
  on.exit(options(op))
  expect_silent(assert_f_envir("a"))
})
