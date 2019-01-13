context("test-utils-new")


# distr_impl --------------------------------------------------------------
# Tested in `new_*()` functions


# impute_x_tbl ------------------------------------------------------------
# Tested in `new_*()` functions


# impute_x_tbl_impl -------------------------------------------------------
# Main tests are in `new_*()` functions
test_that("impute_x_tbl_impl throws error", {
  expect_error(impute_x_tbl_impl(x_fin_x_tbl, "a"), "type")
})


# impute_prob -------------------------------------------------------------
# Tested in `impute_x_tbl_impl()`


# impute_y ----------------------------------------------------------------
# Tested in `impute_x_tbl_impl()`


# impute_vec --------------------------------------------------------------
# Tested in `impute_x_tbl_impl()`


# compute_x_tbl -----------------------------------------------------------
# Tested in `new_*()` functions


# compute_x_tbl_fin -------------------------------------------------------
# Tested in `new_*()` functions


# compute_x_tbl_smooth ----------------------------------------------------
# Tested in `new_*()` functions


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


# unpdqr ------------------------------------------------------------------
test_that("unpdqr works", {
  out_f <- unpdqr(p_fin)
  expect_null(attr(out_f, "meta"))
  expect_false(inherits(out_f, c("p", "d", "q", "r", "pdqr")))
})


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


# is_pdqr_fun -------------------------------------------------------------
test_that("is_pdqr_fun works", {
  expect_true(is_pdqr_fun(p_fin))
  expect_true(is_pdqr_fun(d_fin))
  expect_true(is_pdqr_fun(q_fin))
  expect_true(is_pdqr_fun(r_fin))

  # Function type
  input <- 1
  expect_false(is_pdqr_fun(input))

  # Classes
  expect_false(is_pdqr_fun(user_p))
  expect_false(is_pdqr_fun(structure(user_p, class = "pdqr")))

  # "type" metadata
  f_with_class <- structure(user_p, class = c("p", "pdqr"))
  expect_false(is_pdqr_fun(f_with_class))
  expect_false(is_pdqr_fun(structure(f_with_class, type = "a")))

  # "support" metadata
  expect_false(
    is_pdqr_fun(structure(f_with_class, meta = list(type = "smooth")))
  )
  f_with_corrupt_support <- structure(
    f_with_class, meta = list(type = "smooth", support = c(2, 1))
  )
  expect_false(is_pdqr_fun(f_with_corrupt_support))

  # "x_tbl" metadata
  expect_true(is_pdqr_fun(p_custom))

    # "x_tbl" is completely missing
  input_bad_x_tbl_1 <- p_smooth
  attr(input_bad_x_tbl_1, "meta")[["x_tbl"]] <- NULL
  expect_false(is_pdqr_fun(input_bad_x_tbl_1), "have.*x_tbl")

    # "x_tbl" has not proper structure
  input_bad_x_tbl_2 <- p_fin
  attr(input_bad_x_tbl_2, "meta")[["x_tbl"]] <- "a"
  expect_false(
    is_pdqr_fun(input_bad_x_tbl_2), 'meta.*x_tbl.*data.*frame'
  )
})

test_that("is_pdqr_fun checks extra properties of 'x_tbl' metadata", {
  # "x" is sorted
  input_bad_x_tbl_1 <- p_fin
  attr(input_bad_x_tbl_1, "meta")[["x_tbl"]][["x"]] <- rev(
    attr(input_bad_x_tbl_1, "meta")[["x_tbl"]][["x"]]
  )
  expect_false(is_pdqr_fun(input_bad_x_tbl_1))

  # "fin" `type`
    # Column "prob" is mandatory
  input_bad_x_tbl_2 <- p_fin
  attr(input_bad_x_tbl_2, "meta")[["x_tbl"]][["prob"]] <- NULL
  expect_false(is_pdqr_fun(input_bad_x_tbl_2))

    # Sum of "prob" is 1
  input_bad_x_tbl_3 <- p_fin
  attr(input_bad_x_tbl_3, "meta")[["x_tbl"]][["prob"]] <- 10 *
    attr(input_bad_x_tbl_3, "meta")[["x_tbl"]][["prob"]]
  expect_false(is_pdqr_fun(input_bad_x_tbl_3))

    # Column "cumprob" is mandatory
  input_bad_x_tbl_4 <- p_fin
  attr(input_bad_x_tbl_4, "meta")[["x_tbl"]][["cumprob"]] <- NULL
  expect_false(is_pdqr_fun(input_bad_x_tbl_4))

  # "smooth" type
    # Total integral is 1
  input_bad_x_tbl_5 <- p_smooth
  attr(input_bad_x_tbl_5, "meta")[["x_tbl"]][["y"]] <- 10 *
    attr(input_bad_x_tbl_5, "meta")[["x_tbl"]][["y"]]
  expect_false(is_pdqr_fun(input_bad_x_tbl_5))

    # Column "cumprob" is mandatory
  input_bad_x_tbl_6 <- p_smooth
  attr(input_bad_x_tbl_6, "meta")[["x_tbl"]][["cumprob"]] <- NULL
  expect_false(is_pdqr_fun(input_bad_x_tbl_6))
})


# is_distr_type -----------------------------------------------------------
test_that("is_distr_type works", {
  expect_true(is_distr_type("fin"))
  expect_true(is_distr_type("smooth"))

  expect_false(is_distr_type(1))
  expect_false(is_distr_type(c("fin", "smooth")))
  expect_false(is_distr_type("a"))
})


# is_support --------------------------------------------------------------
test_that("is_support works", {
  expect_true(is_support(c(-1, 1)))
  expect_true(is_support(c(1, NA), allow_na = TRUE))

  expect_false(is_support("a"))
  expect_false(is_support(1))
  expect_false(is_support(c(1, -1)))
  expect_false(is_support(c(1, NA)))
  expect_false(is_support(c(-Inf, 1)))
  expect_false(is_support(c(-1, Inf)))
  expect_false(is_support(c(-Inf, Inf)))
})


# is_x_tbl ----------------------------------------------------------------
test_that("is_x_tbl works with `type = 'fin'`", {
  expect_true(is_x_tbl(x_fin_x_tbl, type = "fin"))

  # Input type
  input <- "a"
  expect_false(is_x_tbl(input, type = "fin"))

  # Column "x"
  expect_false(is_x_tbl(data.frame(a = 1), type = "fin"))
  expect_false(is_x_tbl(data.frame(x = "a"), type = "fin"))

  # Column "prob"
  expect_false(is_x_tbl(data.frame(x = 1), type = "fin"))
  expect_false(is_x_tbl(data.frame(x = 1, prob = "a"), type = "fin"))
  expect_false(is_x_tbl(data.frame(x = 1, prob = -1), type = "fin"))
  expect_false(is_x_tbl(data.frame(x = 1, prob = 0), type = "fin"))

  # Extra columns are allowed
  expect_true(is_x_tbl(data.frame(x = 1, prob = 1, extra = "a"), type = "fin"))
  # Different column order is allowed
  expect_true(
    is_x_tbl(data.frame(prob = c(0.1, 0.9), x = 1:2), type = "fin")
  )
})

test_that("is_x_tbl works with `type = 'smooth'`", {
  expect_true(is_x_tbl(x_smooth_x_tbl, type = "smooth"))

  # Input type
  input <- "a"
  expect_false(is_x_tbl(input, type = "smooth"), "`input`.*data.*frame")

  # Number of rows
  expect_false(
    is_x_tbl(data.frame(x = 1, y = 1), type = "smooth"), "2.*rows"
  )

  # Column "x"
  expect_false(is_x_tbl(data.frame(a = 1:2), type = "smooth"), "x")
  expect_false(
    is_x_tbl(data.frame(x = c("a", "b")), type = "smooth"), "numeric.*x"
  )

  # Column "y"
  expect_false(is_x_tbl(data.frame(x = 1:2), type = "smooth"), "y")
  expect_false(
    is_x_tbl(data.frame(x = 1:2, y = c("a", "b")), type = "smooth"),
    "numeric.*y"
  )
  expect_false(
    is_x_tbl(data.frame(x = 1:2, y = c(-1, 1)), type = "smooth"),
    '"y".*negative'
  )
  expect_false(
    is_x_tbl(data.frame(x = 1:2, y = c(0, 0)), type = "smooth"),
    '"y".*positive'
  )

  # Extra columns are allowed
  expect_true(
    is_x_tbl(data.frame(x = 1:2, y = c(1, 1), extra = "a"), type = "smooth")
  )
  # Different column order is allowed
  expect_true(is_x_tbl(data.frame(y = c(1, 1), x = 1:2), type = "smooth"))
})


# is_x_tbl_meta -----------------------------------------------------------
test_that("is_x_tbl_meta works", {
  # "x" is sorted
  input_bad_x_tbl_1 <- x_fin_x_tbl
  input_bad_x_tbl_1[["x"]] <- rev(input_bad_x_tbl_1[["x"]])
  expect_false(is_x_tbl_meta(input_bad_x_tbl_1, "fin"))

  # "fin" `type`
    # Column "prob" is mandatory
  input_bad_x_tbl_2 <- x_fin_x_tbl
  input_bad_x_tbl_2[["prob"]] <- NULL
  expect_false(is_x_tbl_meta(input_bad_x_tbl_2, "fin"))

    # Sum of "prob" is 1
  input_bad_x_tbl_3 <- x_fin_x_tbl
  input_bad_x_tbl_3[["prob"]] <- 10 * input_bad_x_tbl_3[["prob"]]
  expect_false(is_x_tbl_meta(input_bad_x_tbl_3, "fin"))

    # Column "cumprob" is mandatory
  input_bad_x_tbl_4 <- x_fin_x_tbl
  input_bad_x_tbl_4[["cumprob"]] <- NULL
  expect_false(is_x_tbl_meta(input_bad_x_tbl_4, "fin"))

  # "smooth" type
    # Total integral is 1
  input_bad_x_tbl_5 <- x_smooth_x_tbl
  input_bad_x_tbl_5[["y"]] <- 10 * input_bad_x_tbl_5[["y"]]
  expect_false(is_x_tbl_meta(input_bad_x_tbl_5, "smooth"))

    # Column "cumprob" is mandatory
  input_bad_x_tbl_5 <- x_smooth_x_tbl
  input_bad_x_tbl_5[["cumprob"]] <- NULL
  expect_false(is_x_tbl_meta(input_bad_x_tbl_5, "smooth"))
})


# is_pdqr_class -----------------------------------------------------------
test_that("is_pdqr_class works", {
  expect_true(all(is_pdqr_class(c("p", "d", "q", "r"))))

  expect_false(is_pdqr_class("p_fun"))
})


# has_meta_type -----------------------------------------------------------
test_that("has_meta_type works", {
  expect_true(has_meta_type(p_fin))
  expect_false(has_meta_type(1))
  expect_false(has_meta_type(structure(1, meta = list(type = "a"))))
})


# has_meta_support --------------------------------------------------------
test_that("has_meta_support works", {
  expect_true(has_meta_support(p_fin))
  expect_false(has_meta_support(1))
  expect_false(has_meta_support(structure(1, meta = list(support = c(2, 1)))))
})


# has_meta_x_tbl ----------------------------------------------------------
test_that("has_meta_x_tbl works", {
  expect_true(has_meta_x_tbl(p_fin, "fin"))
  expect_true(has_meta_x_tbl(p_smooth, "smooth"))

  expect_false(has_meta_x_tbl(p_fin, "smooth"))
  expect_false(has_meta_x_tbl(p_smooth, "fin"))
  expect_false(has_meta_x_tbl(1, "smooth"))
  expect_false(has_meta_x_tbl(structure(1, meta = list(x_tbl = "a")), "fin"))

  # Check for "good" "x_tbl" metadata
  expect_false(
    has_meta_x_tbl(
      structure(1, meta = list(x_tbl = data.frame(x = 1, prob = -1))),
      "fin"
    )
  )
})


# density_piecelin --------------------------------------------------------
# Tested in `new_*()`


# trapez_integral ---------------------------------------------------------
test_that("trapez_integral works", {
  expect_equal(trapez_integral(1:2, 1:2), 1.5)
  expect_equal(trapez_integral(cumsum(1:10), c(1:5, 5:1)), 174)
})


# trapez_part_integral ----------------------------------------------------
# Tested in `p_from_d_points()`


# p_from_d_points ---------------------------------------------------------
# Tested in `new_p` and `as_p`


# compute_cum_quadr_coeffs ------------------------------------------------
# Tested in `p_from_d_points()` and `new_q_smooth()`
