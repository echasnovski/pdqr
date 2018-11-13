context("test-utils-distr")


# Custom expectations -----------------------------------------------------
expect_density_ext_works <- function(input, ...) {
  output <- density_ext(input, ...)
  out_x <- output[["x"]]
  out_y <- output[["y"]]
  n <- length(out_x)

  # Expect extension tails have small width
  expect_true(
    (diff(out_x[1:2]) < 10^(-5)) && (diff(out_x[n - 1:0]) < 10^(-5))
  )
  # Expect density start from and end in 0 (for continuity)
  expect_equal(out_y[c(1, n)], c(0, 0))
}


# add_pdqr_class ----------------------------------------------------------
test_that("add_pdqr_class works", {
  expect_equal(
    add_pdqr_class(structure(1, class = "a"), "b"),
    structure(1, class = c("b", "pdqr_fun", "a"))
  )
  expect_equal(
    add_pdqr_class(structure(1, class = c("pdqr_fun", "a")), "b"),
    structure(1, class = c("b", "pdqr_fun", "a"))
  )
})


# distr_impl --------------------------------------------------------------
# Tested in `*_fun()` functions


# filter_numbers ----------------------------------------------------------
test_that("filter_numbers works", {
  expect_equal(
    expect_warning(filter_numbers(c(1, 0, NA, NA_real_)), "x.*NA.*removed"),
    c(1, 0)
  )
  expect_equal(
    expect_warning(filter_numbers(c(1, NaN, 0)), "x.*NaN.*removed"),
    c(1, 0)
  )
  expect_equal(
    expect_warning(filter_numbers(c(Inf, 0, -Inf, 1)), "x.*infinite.*removed"),
    c(0, 1)
  )
})


# assert_common_args ------------------------------------------------------
test_that("assert_common_args works", {
  expect_silent(assert_common_args(1:2, "raw", TRUE))
  expect_silent(assert_common_args(1:2, "smooth", TRUE))

  expect_error(assert_common_args("a", "raw", TRUE), "x.*numeric")
  expect_error(assert_common_args(numeric(0), "raw", TRUE), "x.*empty")
  expect_error(assert_common_args(1:2, 1, TRUE), "type.*string")
  expect_error(assert_common_args(1:2, "a", TRUE), "type.*raw.*smooth")
  expect_error(assert_common_args(1:2, "raw", NA), "attach_x.*TRUE.*FALSE")
})


# assert_distr_type -------------------------------------------------------
test_that("assert_distr_type works", {
  expect_silent(assert_distr_type("raw"))
  expect_silent(assert_distr_type("smooth"))

  expect_error(assert_distr_type(1), "type.*string")
  expect_error(assert_distr_type(c("raw", "smooth")), "type.*string")
  expect_error(assert_distr_type("a"), "type.*raw.*smooth")
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


# is_support --------------------------------------------------------------
test_that("is_support works", {
  expect_true(is_support(c(-1, 1)))

  expect_false(is_support("a"))
  expect_false(is_support(1))
  expect_false(is_support(c(1, -1)))
})


# distr_tbl ---------------------------------------------------------------
test_that("distr_tbl works on object with 'x' metadata", {
  input <- structure(x_smooth[1], meta = list(x = x_raw))
  expect_equal(distr_tbl(input), x_raw_distr_tbl)
})

test_that("distr_tbl works on numeric vector", {
  expect_equal(distr_tbl(x_raw), x_raw_distr_tbl)
})

test_that("distr_tbl throws errors", {
  expect_error(distr_tbl("a"), "meta.*numeric")
})

test_that("distr_tbl does rounding", {
  input <- 1 + 10^(-(9:12))
  output <- data.frame(x = 1, prob = 1)
  expect_equal(distr_tbl(input), output)
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
