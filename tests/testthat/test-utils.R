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


# inverse -----------------------------------------------------------------
test_that("inverse works", {
  square <- function(x) {x^2}
  inv_square <- inverse(square, c(0.5, 10))
  x_vec <- sample(seq(0.5, 10, by = 0.01))

  max_error <- max(abs(inv_square(x_vec) - sqrt(x_vec)))
  expect_true(max_error <= 10^(-4))
})


# stretch_range -----------------------------------------------------------
test_that("stretch_range works", {
  expect_equal(stretch_range(c(0, 1)), c(-10^(-6), 1 + 10^(-6)))
  expect_equal(stretch_range(c(0, 1), 1), c(-1, 2))
  expect_equal(stretch_range(c(0, 1), c(1, 2)), c(-1, 3))
})


# add_class ---------------------------------------------------------------
test_that("add_class works", {
  input <- structure(1, class = c("a", "b"))
  expect_equal(add_class(input, "c"), structure(1, class = c("c", "a", "b")))
  expect_equal(add_class(input, "a"), input)
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
