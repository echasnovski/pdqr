context("test-utils-new")


# distr_impl --------------------------------------------------------------
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
  out_f <- unpdqr(p_dis)
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
  expect_true(is_pdqr_fun(p_dis))
  expect_true(is_pdqr_fun(d_dis))
  expect_true(is_pdqr_fun(q_dis))
  expect_true(is_pdqr_fun(r_dis))

  # Function type
  input <- 1
  expect_false(is_pdqr_fun(input))

  # Classes
  expect_false(is_pdqr_fun(user_p))
  expect_false(is_pdqr_fun(structure(user_p, class = "pdqr")))

  # "type" metadata
  f_no_type <- structure(user_p, class = c("p", "pdqr"))
  expect_false(is_pdqr_fun(f_no_type))

  f_bad_type <- f_no_type
  assign("type", "a", environment(f_bad_type))
  expect_false(is_pdqr_fun(structure(f_bad_type, type = "a")))

  # "support" metadata
  f_no_support <- f_no_type
  assign("type", "continuous", environment(f_no_support))
  expect_false(is_pdqr_fun(f_no_support))

  f_bad_support <- f_no_support
  assign("support", c(2, 1), environment(f_bad_support))
  expect_false(is_pdqr_fun(f_bad_support))

  # "x_tbl" metadata
  ## "x_tbl" is completely missing
  f_no_x_tbl <- as_p(p_con)
  rm("x_tbl", envir = environment(f_no_x_tbl))
  expect_false(is_pdqr_fun(f_no_x_tbl))

  ## "x_tbl" has not proper structure
  f_bad_x_tbl <- as_p(p_dis)
  assign("x_tbl", "a", environment(f_bad_x_tbl))
  expect_false(is_pdqr_fun(f_bad_x_tbl))
})

test_that("is_pdqr_fun checks extra properties of 'x_tbl' metadata", {
  # "x" is sorted
  bad_x_tbl_1 <- x_dis_x_tbl
  bad_x_tbl_1[["x"]] <- rev(bad_x_tbl_1[["x"]])
  f_bad_x_tbl_1 <- as_p(p_dis)
  assign("x_tbl", bad_x_tbl_1, environment(f_bad_x_tbl_1))
  expect_false(is_pdqr_fun(f_bad_x_tbl_1))

  # "discrete" `type`
  ## Column "prob" is mandatory
  bad_x_tbl_2 <- x_dis_x_tbl
  bad_x_tbl_2[["prob"]] <- NULL
  f_bad_x_tbl_2 <- as_p(p_dis)
  assign("x_tbl", bad_x_tbl_2, environment(f_bad_x_tbl_2))
  expect_false(is_pdqr_fun(f_bad_x_tbl_2))

  ## Sum of "prob" is 1
  bad_x_tbl_3 <- x_dis_x_tbl
  bad_x_tbl_3[["prob"]] <- 10 * bad_x_tbl_3[["prob"]]
  f_bad_x_tbl_3 <- as_p(p_dis)
  assign("x_tbl", bad_x_tbl_3, environment(f_bad_x_tbl_3))
  expect_false(is_pdqr_fun(f_bad_x_tbl_3))

  ## Column "cumprob" is mandatory
  bad_x_tbl_4 <- x_dis_x_tbl
  bad_x_tbl_4[["cumprob"]] <- NULL
  f_bad_x_tbl_4 <- as_p(p_dis)
  assign("x_tbl", bad_x_tbl_4, environment(f_bad_x_tbl_4))
  expect_false(is_pdqr_fun(f_bad_x_tbl_4))

  ## Column "x" shouldn't have duplicate values
  bad_x_tbl_5 <- x_dis_x_tbl
  bad_x_tbl_5[["x"]] <- 1
  f_bad_x_tbl_5 <- as_p(p_dis)
  assign("x_tbl", bad_x_tbl_5, environment(f_bad_x_tbl_5))
  expect_false(is_pdqr_fun(f_bad_x_tbl_5))

  # "continuous" type
  ## Total integral is 1
  bad_x_tbl_6 <- x_con_x_tbl
  bad_x_tbl_6[["y"]] <- 10 * bad_x_tbl_6[["y"]]
  f_bad_x_tbl_6 <- as_p(p_con)
  assign("x_tbl", bad_x_tbl_6, environment(f_bad_x_tbl_6))
  expect_false(is_pdqr_fun(f_bad_x_tbl_6))

  ## Column "cumprob" is mandatory
  bad_x_tbl_7 <- x_con_x_tbl
  bad_x_tbl_7[["cumprob"]] <- NULL
  f_bad_x_tbl_7 <- as_p(p_con)
  assign("x_tbl", bad_x_tbl_7, environment(f_bad_x_tbl_7))
  expect_false(is_pdqr_fun(f_bad_x_tbl_7))
})

test_that("is_pdqr_fun is not affected by 'pdqr.assert_args'", {
  op <- options(pdqr.assert_args = FALSE)
  on.exit(options(op))
  expect_false(is_pdqr_fun("a"))
})


# is_pdqr_type ------------------------------------------------------------
test_that("is_pdqr_type works", {
  expect_true(is_pdqr_type("discrete"))
  expect_true(is_pdqr_type("continuous"))

  expect_false(is_pdqr_type(1))
  expect_false(is_pdqr_type(c("discrete", "continuous")))
  expect_false(is_pdqr_type("a"))
})

test_that("is_pdqr_type is not affected by 'pdqr.assert_args'", {
  op <- options(pdqr.assert_args = FALSE)
  on.exit(options(op))
  expect_false(is_pdqr_type("a"))
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

test_that("is_support is not affected by 'pdqr.assert_args'", {
  op <- options(pdqr.assert_args = FALSE)
  on.exit(options(op))
  expect_false(is_support("a"))
})


# is_x_tbl ----------------------------------------------------------------
test_that("is_x_tbl works with 'discrete' type", {
  expect_true(is_x_tbl(x_dis_x_tbl, type = "discrete"))

  # Input type
  input <- "a"
  expect_false(is_x_tbl(input, type = "discrete"))

  # Column "x"
  expect_false(is_x_tbl(data.frame(a = 1), type = "discrete"))
  expect_false(is_x_tbl(data.frame(x = "a"), type = "discrete"))

  # Column "prob"
  expect_false(is_x_tbl(data.frame(x = 1), type = "discrete"))
  expect_false(is_x_tbl(data.frame(x = 1, prob = "a"), type = "discrete"))
  expect_false(is_x_tbl(data.frame(x = 1, prob = -1), type = "discrete"))
  expect_false(is_x_tbl(data.frame(x = 1, prob = 0), type = "discrete"))

  # Extra columns are allowed
  expect_true(
    is_x_tbl(data.frame(x = 1, prob = 1, extra = "a"), type = "discrete")
  )
  # Different column order is allowed
  expect_true(
    is_x_tbl(data.frame(prob = c(0.1, 0.9), x = 1:2), type = "discrete")
  )
})

test_that("is_x_tbl works with 'continuous' type", {
  expect_true(is_x_tbl(x_con_x_tbl, type = "continuous"))

  # Input type
  input <- "a"
  expect_false(is_x_tbl(input, type = "continuous"))

  # Number of rows
  expect_false(is_x_tbl(data.frame(x = 1, y = 1), type = "continuous"))

  # Column "x"
  expect_false(is_x_tbl(data.frame(a = 1:2), type = "continuous"), "x")
  expect_false(is_x_tbl(data.frame(x = c("a", "b")), type = "continuous"))
  expect_false(
    is_x_tbl(data.frame(x = c(1, 1, 2), y = c(1, 1, 1)), type = "continuous")
  )

  # Column "y"
  expect_false(is_x_tbl(data.frame(x = 1:2), type = "continuous"), "y")
  expect_false(
    is_x_tbl(data.frame(x = 1:2, y = c("a", "b")), type = "continuous")
  )
  expect_false(is_x_tbl(data.frame(x = 1:2, y = c(-1, 1)), type = "continuous"))
  expect_false(is_x_tbl(data.frame(x = 1:2, y = c(0, 0)), type = "continuous"))

  # Extra columns are allowed
  expect_true(
    is_x_tbl(data.frame(x = 1:2, y = c(1, 1), extra = "a"), type = "continuous")
  )
  # Different column order is allowed
  expect_true(is_x_tbl(data.frame(y = c(1, 1), x = 1:2), type = "continuous"))
})

test_that("is_x_tbl is not affected by 'pdqr.assert_args'", {
  op <- options(pdqr.assert_args = FALSE)
  on.exit(options(op))
  expect_false(is_x_tbl("a", "discrete"))
})


# is_x_tbl_meta -----------------------------------------------------------
test_that("is_x_tbl_meta works", {
  # "x" is sorted
  input_bad_x_tbl_1 <- x_dis_x_tbl
  input_bad_x_tbl_1[["x"]] <- rev(input_bad_x_tbl_1[["x"]])
  expect_false(is_x_tbl_meta(input_bad_x_tbl_1, "discrete"))

  # "discrete" `type`
  ## Column "prob" is mandatory
  input_bad_x_tbl_2 <- x_dis_x_tbl
  input_bad_x_tbl_2[["prob"]] <- NULL
  expect_false(is_x_tbl_meta(input_bad_x_tbl_2, "discrete"))

  ## Sum of "prob" is 1
  input_bad_x_tbl_3 <- x_dis_x_tbl
  input_bad_x_tbl_3[["prob"]] <- 10 * input_bad_x_tbl_3[["prob"]]
  expect_false(is_x_tbl_meta(input_bad_x_tbl_3, "discrete"))

  ## Column "cumprob" is mandatory
  input_bad_x_tbl_4 <- x_dis_x_tbl
  input_bad_x_tbl_4[["cumprob"]] <- NULL
  expect_false(is_x_tbl_meta(input_bad_x_tbl_4, "discrete"))

  ## Column "x" shouldn't have duplicate values
  input_bad_x_tbl_5 <- x_dis_x_tbl
  input_bad_x_tbl_5[["x"]] <- 1
  expect_false(is_pdqr_fun(input_bad_x_tbl_5))

  # "continuous" type
  ## Total integral is 1
  input_bad_x_tbl_6 <- x_con_x_tbl
  input_bad_x_tbl_6[["y"]] <- 10 * input_bad_x_tbl_6[["y"]]
  expect_false(is_x_tbl_meta(input_bad_x_tbl_6, "continuous"))

  ## Column "cumprob" is mandatory
  input_bad_x_tbl_7 <- x_con_x_tbl
  input_bad_x_tbl_7[["cumprob"]] <- NULL
  expect_false(is_x_tbl_meta(input_bad_x_tbl_7, "continuous"))
})


# is_pdqr_class -----------------------------------------------------------
test_that("is_pdqr_class works", {
  expect_true(all(is_pdqr_class(c("p", "d", "q", "r"))))

  expect_false(is_pdqr_class("p_fun"))
})


# is_boolean_pdqr_fun -----------------------------------------------------
test_that("is_boolean_pdqr_fun works", {
  boolean_x_tbl_1 <- data.frame(x = c(0, 1), prob = c(0.3, 0.7))
  expect_true(is_boolean_pdqr_fun(new_d(boolean_x_tbl_1, "discrete")))
  expect_true(is_boolean_pdqr_fun(new_r(boolean_x_tbl_1, "discrete")))

  boolean_x_tbl_2 <- data.frame(x = c(0, 1), prob = c(1, 0))
  expect_true(is_boolean_pdqr_fun(new_d(boolean_x_tbl_2, "discrete")))
  boolean_x_tbl_3 <- data.frame(x = c(0, 1), prob = c(0, 1))
  expect_true(is_boolean_pdqr_fun(new_d(boolean_x_tbl_3, "discrete")))

  not_boolean_x_tbl_1 <- data.frame(x = 0, prob = 1)
  expect_false(is_boolean_pdqr_fun(new_d(not_boolean_x_tbl_1, "discrete")))
  not_boolean_x_tbl_2 <- data.frame(x = 1, prob = 1)
  expect_false(is_boolean_pdqr_fun(new_d(not_boolean_x_tbl_2, "discrete")))
  not_boolean_x_tbl_3 <- data.frame(x = c(0, 1), y = c(1, 1))
  expect_false(is_boolean_pdqr_fun(new_d(not_boolean_x_tbl_3, "continuous")))
})


# has_meta_type -----------------------------------------------------------
test_that("has_meta_type works", {
  expect_true(has_meta_type(p_dis))

  f_no_type <- as_p(p_dis)
  rm("type", envir = environment(f_no_type))
  expect_false(has_meta_type(f_no_type))

  f_bad_type <- as_p(p_dis)
  assign("type", "a", environment(f_bad_type))
  expect_false(has_meta_type(f_bad_type))
})


# has_meta_support --------------------------------------------------------
test_that("has_meta_support works", {
  expect_true(has_meta_support(p_dis))

  f_no_support <- as_p(p_dis)
  rm("support", envir = environment(f_no_support))
  expect_false(has_meta_support(f_no_support))

  f_bad_support <- as_p(p_dis)
  assign("support", c(2, 1), environment(f_bad_support))
  expect_false(has_meta_support(f_bad_support))
})


# has_meta_x_tbl ----------------------------------------------------------
test_that("has_meta_x_tbl works", {
  expect_true(has_meta_x_tbl(p_dis, "discrete"))
  expect_true(has_meta_x_tbl(p_con, "continuous"))

  expect_false(has_meta_x_tbl(p_dis, "continuous"))
  expect_false(has_meta_x_tbl(p_con, "discrete"))

  f_no_x_tbl <- as_p(p_dis)
  rm("x_tbl", envir = environment(f_no_x_tbl))
  expect_false(has_meta_x_tbl(f_no_x_tbl, "discrete"))

  f_bad_x_tbl_1 <- as_p(p_con)
  assign("x_tbl", "a", environment(f_bad_x_tbl_1))
  expect_false(has_meta_x_tbl(f_bad_x_tbl_1, "continuous"))

  # Check for "good" "x_tbl" metadata
  f_bad_x_tbl_2 <- as_p(p_dis)
  assign("x_tbl", data.frame(x = 1, prob = -1), environment(f_bad_x_tbl_2))
  expect_false(has_meta_x_tbl(f_bad_x_tbl_2, "discrete"))
})


# density_piecelin --------------------------------------------------------
# Tested in `new_*()`


# trapez_integral ---------------------------------------------------------
test_that("trapez_integral works", {
  expect_equal(trapez_integral(1:2, 1:2), 1.5)
  expect_equal(trapez_integral(cumsum(1:10), c(1:5, 5:1)), 174)
})


# trapez_piece_integral ---------------------------------------------------
test_that("trapez_piece_integral works", {
  expect_equal(trapez_piece_integral(cumsum(1:4), c(1:2, 2:1)), c(3, 6, 6))
})


# trapez_part_integral ----------------------------------------------------
# Tested in `p_from_d_points()`


# compute_piecelin_density_coeffs -----------------------------------------
# Tested in `new_p()` and `new_q()`
