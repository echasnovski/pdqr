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


# is_truefalse ------------------------------------------------------------
test_that("is_truefalse worksq", {
  expect_true(is_truefalse(TRUE))
  expect_true(is_truefalse(FALSE))
  expect_false(is_truefalse(NA))
  expect_false(is_truefalse(c(TRUE, TRUE)))
  expect_false(is_truefalse(1))
})


# inversing ---------------------------------------------------------------
test_that("inversing works", {
  square <- function(x) {x^2}
  inv_square <- inversing(square, c(0.5, 10), f_type = "smooth")
  x_vec <- sample(seq(0.5, 10, by = 0.01))

  max_error <- max(abs(inv_square(x_vec) - sqrt(x_vec)))
  expect_true(max_error <= 10^(-4))
})

test_that("inversing works with constant correctly in case `type` is 'raw'", {
  const_inv <- inversing(
    function(x) {rep(10, length(x))}, c(0, 20), f_type = "raw"
  )
  expect_equal(const_inv(c(0, 10, 20)), rep(20, 3))
})

test_that("inversing removes infinite values", {
  f_inv <- inversing(
    function(x) {1 / x}, c(0, 1), f_type = "smooth"
  )
  expect_true(is.finite(f_inv(10^7)))
})


# approx_method_from_type -------------------------------------------------
test_that("approx_method_from_type works", {
  expect_equal(approx_method_from_type("raw"), "constant")
  expect_equal(approx_method_from_type("smooth"), "linear")

  expect_error(approx_method_from_type("a"), "Invalid")
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


# dedupl_list -------------------------------------------------------------
test_that("dedupl_list works", {
  input_1 <- list(1, 2)
  expect_equal(dedupl_list(input_1), input_1)

  input_2 <- list(1, 2, c = 3)
  expect_equal(dedupl_list(input_2), input_2)

  input_3 <- list(a = 1, 2, c = 3, a = 4)
  expect_equal(dedupl_list(input_3), input_3[-4])
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
