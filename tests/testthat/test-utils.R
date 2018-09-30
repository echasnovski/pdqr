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


# compute_distr_tbl -------------------------------------------------------
test_that("compute_distr_tbl works", {
  input <- c(rep(-1, 4), 1, rep(1.1, 3), 2, 1000)
  output <- data.frame(
    x = c(-1, 1, 1.1, 2, 1000),
    prob = c(0.4, 0.1, 0.3, 0.1, 0.1)
  )
  expect_equal(compute_distr_tbl(input), output)
})

test_that("compute_distr_tbl does rounding", {
  input <- 1 + 10^(-(9:12))
  output <- data.frame(x = 1, prob = 1)
  expect_equal(compute_distr_tbl(input), output)
})
