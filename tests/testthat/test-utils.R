context("test-utils")


# Input data --------------------------------------------------------------
x <- 1
null <- NULL


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
