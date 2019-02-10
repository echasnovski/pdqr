context("test-utils")


# Input data --------------------------------------------------------------
x <- 1
null <- NULL


# is_near -----------------------------------------------------------------
test_that("is_near works", {
  expect_equal(
    is_near(1, 1 + 0.001 * 0:20, tol = 0.01),
    c(rep(TRUE, 10), rep(FALSE, 11))
  )
})


# is_string ---------------------------------------------------------------
test_that("is_string works", {
  expect_true(is_string("a"))
  expect_false(is_string(c("a", "b")))
  expect_false(is_string(1))
})


# is_single_number --------------------------------------------------------
test_that("is_single_number works", {
  expect_true(is_single_number(1))
  expect_true(is_single_number(0L))
  expect_false(is_single_number(1:2))
  expect_false(is_single_number("a"))
  expect_false(is_single_number(Inf))
  expect_false(is_single_number(NA_real_))
})

test_that("is_single_number checks value bounds", {
  expect_true(is_single_number(1, min_val = 0))
  expect_true(is_single_number(1, min_val = 1))
  expect_false(is_single_number(1, min_val = 2))

  expect_true(is_single_number(1, max_val = 2))
  expect_true(is_single_number(1, max_val = 1))
  expect_false(is_single_number(1, max_val = 0))

  expect_true(is_single_number(1, min_val = 0, max_val = 2))
  expect_true(is_single_number(1, min_val = 1, max_val = 1))
  expect_false(is_single_number(NA_real_, min_val = 1, max_val = 1))
})


# is_truefalse ------------------------------------------------------------
test_that("is_truefalse worksq", {
  expect_true(is_truefalse(TRUE))
  expect_true(is_truefalse(FALSE))
  expect_false(is_truefalse(NA))
  expect_false(is_truefalse(c(TRUE, TRUE)))
  expect_false(is_truefalse(1))
})


# neigh_dist --------------------------------------------------------------
test_that("neigh_dist works",  {
  vec      <- c(  2,   1, 2.5,  2.6, -100,  100)

  min_dist <- c(0.5,   1, 0.1,  0.1,  101, 97.4)
  max_dist <- c(  1, 101, 0.5, 97.4,  101, 97.4)

  expect_equal(neigh_dist(vec, "min"), min_dist)
  expect_equal(neigh_dist(vec, "max"), max_dist)

  # Output doesn't depend on order of input
  vec_ord <- order(vec)
  expect_equal(neigh_dist(vec[vec_ord], "min"), min_dist[vec_ord])
  expect_equal(neigh_dist(vec[vec_ord], "max"), max_dist[vec_ord])

  # Default distance is used
  expect_equal(
    neigh_dist(vec, "max", def_dist = 1e5),
    c(1, 101, 0.5, 97.4, 1e5, 1e5)
  )
})


# inversing ---------------------------------------------------------------
test_that("inversing works", {
  square <- function(x) {x^2}
  inv_square <- inversing(square, c(0.5, 10))
  x_vec <- sample(seq(0.5, 10, by = 0.01))

  max_error <- max(abs(inv_square(x_vec) - sqrt(x_vec)))
  expect_true(max_error <= 10^(-4))
})

test_that("inversing linearly imputes infinite values", {
  f_inv <- inversing(function(x) {1 / x}, c(0, 1), n_grid = 11)
  expect_equal(f_inv(10:15), seq(0.1, 0, by = -0.02))
})

test_that("inversing accepts extra arguments for input function", {
  f_inv <- inversing(qunif, c(0, 1), min = 10, max = 11)
  f_ref <- function(q) {punif(q, min = 10, max = 11)}
  expect_close_f(f_inv, f_ref, seq(-9, 12, by = 0.001))
})

test_that("inversing accepts extra arguments for `approxfun()`", {
  f_inv <- inversing(
    qunif, c(0, 1), .approxfun_args = list(yleft = -100, yright = 100)
  )
  expect_equal(
    f_inv(c(-1, -0.001, 0, 1, 1.001, 2)), c(-100, -100, 0, 1, 100, 100)
  )
})


# impute_inf --------------------------------------------------------------
test_that("impute_inf works", {
  expect_equal(
    # All equations are y = x
    impute_inf(1:6, c(1, Inf, 3, 4, Inf, 6), "y"), 1:6
  )
  expect_equal(
    # All equations are y = x
    impute_inf(1:7, c(Inf, 2, Inf, 4, Inf, 6, Inf), "y"), 1:7
  )
  expect_equal(
    # Left: y = x, right: y = -2x + 10
    impute_inf(1:5, c(1, 2, Inf, 2, 0), "y"), c(1, 2, 4, 2, 0)
  )
  expect_equal(
    # Left: y = x, right: y = -x + 9
    impute_inf(c(1, 2, 3, 7, 9), c(1, 2, Inf, 2, 0), "y"), c(1, 2, 6, 2, 0)
  )
  expect_equal(
    # Left: y = x, right: y = x + 7
    impute_inf(c(1, 2, 3, 7, 9), c(1, 2, Inf, 0, 2), "y"), c(1, 2, 3, 0, 2)
  )

  expect_error(impute_inf(1:3, c(NA, 1, 3), "`a`"), "[Aa]ll.*`a`.*number.*NA")
  expect_error(impute_inf(1:3, c(1, 2, Inf), "`a`"), "`a`.*3 finite values")
})


# impute_linearly ---------------------------------------------------------
# Tested in `impute_inf()`


# extrap_lin --------------------------------------------------------------
test_that("extrap_lin works", {
  # True are: y = -x, y = 10, y = 2x + 2
  expect_equal(
    extrap_lin(
      x_1 = c(-1,  0, 1), x_2 = c( 2,  3,  4),
      y_1 = c( 1, 10, 4), y_2 = c(-2, 10, 10),
      x_target = c(-10, 0, 10)
    ),
    c(10, 10, 22)
  )
})


# stretch_range -----------------------------------------------------------
test_that("stretch_range works", {
  expect_equal(stretch_range(c(0, 1)), c(-10^(-6), 1 + 10^(-6)))
  expect_equal(stretch_range(c(0, 1), 1), c(-1, 2))
  expect_equal(stretch_range(c(0, 1), c(1, 2)), c(-1, 3))
})


# seq_between -------------------------------------------------------------
test_that("seq_between works",  {
  expect_equal(
    seq_between(c(-1, 10), length.out = 101),
    seq(from = -1, to = 10, length.out = 101)
  )
  expect_equal(
    seq_between(c(-1, 10), by = 0.03),
    seq(from = -1, to = 10, by = 0.03)
  )
})


# coalesce_pair -----------------------------------------------------------
test_that("coalesce_pair works", {
  expect_equal(
    coalesce_pair(c(1, NA, NA, 4), c(NA, 2, NA, 3)),
    c(1, 2, NA, 4)
  )
})


# integrate_safely --------------------------------------------------------
test_that("integrate_safely integrates when `stats::integrate()` can't", {
  expect_silent(integrate_safely(fam_beta_inf$d, -1e8, 1e8))
})

test_that("integrate_safely throws informative error", {
  f <- function(x) {rep(Inf, length.out = length(x))}
  expect_error(integrate_safely(f, -1, 1), "Can't")
})


# dedupl_list -------------------------------------------------------------
test_that("dedupl_list works", {
  input_1 <- list(1, 2)
  expect_equal(dedupl_list(input_1), input_1)

  input_2 <- list(1, 2, c = 3)
  expect_equal(dedupl_list(input_2), input_2)

  input_3 <- list(a = 1, 2, c = 3, a = 4)
  expect_equal(dedupl_list(input_3), input_3[-4])
})


# swap --------------------------------------------------------------------
test_that("swap works", {
  expect_equal(
    swap(list(a = 1, b = 2, c = 3), "a", "b"), list(a = 2, b = 1, c = 3)
  )
})


# add_class ---------------------------------------------------------------
test_that("add_class works", {
  input <- structure(1, class = c("a", "b"))
  expect_equal(add_class(input, "c"), structure(1, class = c("c", "a", "b")))
  expect_equal(add_class(input, "a"), input)
  expect_equal(
    add_class(input, c("c", "a")), structure(1, class = c("c", "a", "b"))
  )
  expect_equal(
    add_class(input, c("c", "a", "b")),
    structure(1, class = c("c", "a", "b", "a", "b"))
  )
})


# copy_attrs --------------------------------------------------------------
test_that("copy_attrs works", {
  input <- structure(1, a = 2)
  output <- copy_attrs(input, structure("a", b = 3))
  expect_equal(attributes(output), list(b = 3))
})


# stop_collapse -----------------------------------------------------------
test_that("stop_collapse works", {
  expect_error(
    stop_collapse("x = ", x, ", null = ", null),
    "x = 1, null = NULL"
  )
})


# warning_collapse --------------------------------------------------------
test_that("warning_collapse works", {
  expect_warning(
    warning_collapse("x = ", x, ", null = ", null),
    "x = 1, null = NULL"
  )
})


# message_collapse --------------------------------------------------------
test_that("message_collapse works", {
  expect_message(
    message_collapse("x = ", x, ", null = ", null),
    "x = 1, null = NULL"
  )
})


# collapse_nullable -------------------------------------------------------
test_that("collapse_nullable works", {
  expect_equal(
    collapse_nullable("x = ", x, ", null = ", null),
    "x = 1, null = NULL"
  )

  expect_error(collapse_nullable(1:2, c("a", "b")), "length 1")
})


# capture_null ------------------------------------------------------------
test_that("capture_null works", {
  expect_equal(capture_null(NULL), "NULL")
  expect_equal(capture_null(1), 1)
  expect_equal(capture_null("a"), "a")
})
