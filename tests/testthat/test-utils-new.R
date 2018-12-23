context("test-utils-new")


# add_pdqr_class ----------------------------------------------------------
test_that("add_pdqr_class works", {
  expect_equal(
    add_pdqr_class(structure(1, class = "a"), "b"),
    structure(1, class = c("b", "pdqr", "a"))
  )
  expect_equal(
    add_pdqr_class(structure(1, class = c("pdqr", "a")), "b"),
    structure(1, class = c("b", "pdqr", "a"))
  )
})


# compute_raw_tbl ---------------------------------------------------------
# Tested in `new_*()` functions


# distr_impl --------------------------------------------------------------
# Tested in `new_*()` functions


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
  expect_silent(assert_common_args(1:2, "raw"))
  expect_silent(assert_common_args(1:2, "smooth"))

  expect_error(assert_common_args("a", "raw"), "x.*numeric")
  expect_error(assert_common_args(1:2, 1), "type.*string")
  expect_error(assert_common_args(1:2, "a"), "type.*raw.*smooth")
})


# is_pdqr_fun -------------------------------------------------------------
test_that("is_pdqr_fun works", {
  expect_true(is_pdqr_fun(p_raw))
  expect_false(is_pdqr_fun("a"))
  expect_false(is_pdqr_fun(user_p))
  expect_false(
    is_pdqr_fun(structure(user_p, class = "pdqr", meta = list(type = "a")))
  )
  expect_false(
    is_pdqr_fun(
      structure(
        user_p, class = "pdqr", meta = list(type = "raw", support = c(2, 1))
      )
    )
  )
})


# is_distr_type -----------------------------------------------------------
test_that("is_distr_type works", {
  expect_true(is_distr_type("raw"))
  expect_true(is_distr_type("smooth"))

  expect_false(is_distr_type(1))
  expect_false(is_distr_type(c("raw", "smooth")))
  expect_false(is_distr_type("a"))
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


# is_raw_tbl --------------------------------------------------------------
test_that("is_raw_tbl works", {
  expect_true(is_raw_tbl(x_raw_raw_tbl))
  expect_true(is_raw_tbl(x_raw_raw_tbl[, c("x", "prob")]))
  expect_true(is_raw_tbl(x_raw_raw_tbl[, c("x", "n")]))

  expect_false(is_raw_tbl("a"))
  expect_false(is_raw_tbl(data.frame(a = 1)))
  expect_false(is_raw_tbl(data.frame(x = "a")))
  expect_false(is_raw_tbl(data.frame(x = 1)))
  expect_false(is_raw_tbl(data.frame(x = 1, prob = "a")))
  expect_false(is_raw_tbl(data.frame(x = 1, prob = 0.5)))
  expect_false(is_raw_tbl(data.frame(x = 1, n = "a")))
})


# is_smooth_tbl -----------------------------------------------------------
test_that("is_smooth_tbl works", {
  expect_true(is_smooth_tbl(x_smooth_smooth_tbl))

  input <- "a"
  expect_false(is_smooth_tbl(input))
  expect_false(is_smooth_tbl(data.frame(a = 1)))
  expect_false(is_smooth_tbl(data.frame(x = "a")))
  expect_false(is_smooth_tbl(data.frame(x = 1)))
  expect_false(is_smooth_tbl(data.frame(x = 1, y = "a")))
})


# is_pdqr_class -----------------------------------------------------------
test_that("is_pdqr_class works", {
  expect_true(all(is_pdqr_class(c("p", "d", "q", "r"))))

  expect_false(is_pdqr_class("p_fun"))
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


# density_piecelin --------------------------------------------------------
# Tested in `new_*()`


# trapez_integral ---------------------------------------------------------
test_that("trapez_integral works", {
  expect_equal(trapez_integral(1:2, 1:2), 1.5)
  expect_equal(trapez_integral(cumsum(1:10), c(1:5, 5:1)), 174)
})


# p_from_d_points ---------------------------------------------------------
# Tested in `new_p` and `as_p`
