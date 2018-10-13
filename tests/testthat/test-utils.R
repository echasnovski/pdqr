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
