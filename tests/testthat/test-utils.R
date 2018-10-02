context("test-utils")


# Input data --------------------------------------------------------------
x <- 1
null <- NULL


# Custom expectations -----------------------------------------------------
expect_density_ext_works <- function(input, ...) {
  output <- density_ext(input, ...)
  out_x <- output[["x"]]
  out_y <- output[["y"]]
  n <- length(out_x)

  expect_true(
    (diff(out_x[1:2]) < 10^(-3)) && (diff(out_x[n - 1:0]) < 10^(-3))
  )
  expect_equal(out_y[c(1, n)], c(0, 0))
}


# stop_glue ---------------------------------------------------------------
test_that("stop_glue works", {
  expect_error(stop_glue("x = {x}", ", null = {null}"), "x = 1, null = NULL")
})


# warning_glue ------------------------------------------------------------
test_that("warning_glue works", {
  expect_warning(
    warning_glue("x = {x}", ", null = {null}"), "x = 1, null = NULL"
  )
})


# message_glue ------------------------------------------------------------
test_that("message_glue works", {
  expect_message(
    message_glue("x = {x}", ", null = {null}"), "x = 1, null = NULL"
  )
})


# glue_null ---------------------------------------------------------------
test_that("glue_null works", {
  expect_equal(
    glue_null("x = {x}", ", null = {null}"), "x = 1, null = NULL"
  )
})


# null_transformer --------------------------------------------------------
# Tested in `glue_null()`


# compute_distr_tbl -------------------------------------------------------
test_that("compute_distr_tbl works", {
  input <- c(rep(-1, 4), 1, rep(1.1, 3), 2, 1000)
  output <- data.frame(
    x = c(-1, 1, 1.1, 2, 1000),
    prob = c(0.4, 0.1, 0.3, 0.1, 0.1)
  )
  expect_equal(compute_distr_tbl(input), output)
})

test_that("compute_distr_tbl does rounding", {
  input <- 1 + 10^(-(9:12))
  output <- data.frame(x = 1, prob = 1)
  expect_equal(compute_distr_tbl(input), output)
})


# density_ext -------------------------------------------------------------
test_that("density_ext works", {
  expect_density_ext_works(x_smooth)
  expect_density_ext_works(x_smooth / 10000)
})

test_that("density_ext works on edge case n = 1", {
  expect_density_ext_works(x_smooth, n = 1)
})


# trapez_integral ---------------------------------------------------------
test_that("trapez_integral works", {
  expect_equal(trapez_integral(1:2, 1:2), 1.5)
  expect_equal(trapez_integral(cumsum(1:10), c(1:5, 5:1)), 174)
})
