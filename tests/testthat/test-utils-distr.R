context("test-utils-distr")


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
  expect_false(is_support(c(-Inf, 1)))
  expect_false(is_support(c(-1, Inf)))
  expect_false(is_support(c(-Inf, Inf)))
})


# is_pdqr_fun -------------------------------------------------------------
test_that("is_pdqr_fun works", {
  expect_true(is_pdqr_fun(p_raw))
  expect_false(is_pdqr_fun(user_p))
  expect_false(is_pdqr_fun("a"))
})


# is_pdqr_class -----------------------------------------------------------
test_that("is_pdqr_class works", {
  expect_true(all(is_pdqr_class(c("p_fun", "d_fun", "q_fun", "r_fun"))))

  expect_false(is_pdqr_class("p"))
})


# has_meta_type -----------------------------------------------------------
test_that("has_meta_type works", {
  expect_true(has_meta_type(p_raw))
  expect_false(has_meta_type(1))
  expect_false(has_meta_type(structure(1, meta = list(type = "a"))))
})


# has_meta_support --------------------------------------------------------
test_that("has_meta_support works", {
  expect_true(has_meta_support(p_raw))
  expect_false(has_meta_support(1))
  expect_false(has_meta_support(structure(1, meta = list(support = c(2, 1)))))
})


# has_meta_x --------------------------------------------------------------
test_that("has_meta_x works", {
  expect_true(has_meta_x(p_raw_withx))
  expect_false(has_meta_x(p_raw_nox))
  expect_false(has_meta_x(structure(1, meta = list(x = "a"))))
})


# density_piecelin --------------------------------------------------------
# Tested in `*_fun()`


# trapez_integral ---------------------------------------------------------
test_that("trapez_integral works", {
  expect_equal(trapez_integral(1:2, 1:2), 1.5)
  expect_equal(trapez_integral(cumsum(1:10), c(1:5, 5:1)), 174)
})


# p_from_d_points ---------------------------------------------------------
# Tested in `p_fun` and `as_p`
