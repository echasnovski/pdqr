context("test-utils-distr")


# Custom expectations -----------------------------------------------------
expect_density_ext_works <- function(input, ...) {
  output <- density_ext(input, ...)
  out_x <- output[["x"]]
  out_y <- output[["y"]]
  n <- length(out_x)

  # Expect extension tails have small width
  expect_true(
    (diff(out_x[1:2]) < 10^(-3)) && (diff(out_x[n - 1:0]) < 10^(-3))
  )
  # Expect density start from and end in 0 (for continuity)
  expect_equal(out_y[c(1, n)], c(0, 0))
}


# distr_impl --------------------------------------------------------------
# Tested in `*_fun()` functions


# distr_print -------------------------------------------------------------
# Tested in `print.*_fun()` functions


# assert_common_args ------------------------------------------------------
test_that("assert_common_args works", {
  expect_silent(assert_common_args(1:2, "raw", TRUE))
  expect_silent(assert_common_args(1:2, "smooth", TRUE))

  expect_error(assert_common_args("a", "raw", TRUE), "x.*numeric")
  expect_error(assert_common_args(1:2, 1, TRUE), "type.*string")
  expect_error(assert_common_args(1:2, "a", TRUE), "type.*raw.*smooth")
  expect_error(assert_common_args(1:2, "raw", NA), "attach_x.*TRUE.*FALSE")
})


# add_common_meta ---------------------------------------------------------
test_that("add_common_meta works", {
  input <- "a"
  input_x <- 1:10

  output_1 <- structure(input, meta = list(type = "smooth", x = input_x))
  expect_equal(
    add_common_meta(input, x = input_x, type = "smooth"),
    output_1
  )

  output_2 <- structure(
    input, meta = list(type = "raw", x = input_x)
  )
  expect_equal(
    add_common_meta(
      input, x = input_x, type = "raw", attach_x = TRUE
    ),
    output_2
  )

  output_3 <- structure(
    input, meta = list(type = "smooth", x = input_x)
  )
  expect_equal(
    add_common_meta(
      input, x = input_x, type = "smooth", attach_x = TRUE
    ),
    output_3
  )

  output_4 <- structure(
    input, meta = list(extra = list(a = -1), type = "smooth", x = input_x)
  )
  expect_equal(
    add_common_meta(
      input, x = input_x, type = "smooth", extra = list(a = -1)
    ),
    output_4
  )
})


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
