context("test-assertions")


# assert_type -------------------------------------------------------------
test_that("assert_type works", {
  x_var <- 1L

  expect_equal(assert_type(x_var, is.integer), x_var)
  expect_silent(assert_type(x_var, is.integer))

  expect_error(assert_type(x_var, is.character), "x_var.*character.*integer")
  expect_error(
    assert_type(x_var, is.character, "symbolic"), "x_var.*symbolic.*integer"
  )

  x_df <- data.frame(x = TRUE)
  expect_silent(assert_type(x_df, is.data.frame))
  expect_error(
    assert_type(x_df, is.logical), "x_df.*logical.*data\\.frame"
  )
})


# get_type ----------------------------------------------------------------
test_that("get_type works", {
  expect_equal(get_type(1L), "integer")
  expect_equal(get_type(1), "double")
  expect_equal(get_type(data.frame(x = 1)), "data.frame")
})


# parse_type --------------------------------------------------------------
test_that("parse_type works", {
  expect_equal(parse_type("is.numeric"), "numeric")
  expect_equal(parse_type("is_number"), "number")
  expect_equal(parse_type("is_is1_logical"), "is1_logical")
})


# assert_common_args ------------------------------------------------------
test_that("assert_common_args works", {
  expect_silent(assert_common_args(1:2, "raw", TRUE))
  expect_silent(assert_common_args(1:2, "smooth", TRUE))

  expect_error(assert_common_args("a", "raw", TRUE), "x.*numeric")
  expect_error(assert_common_args(1:2, 1, TRUE), "type.*string")
  expect_error(assert_common_args(1:2, "a", TRUE), "type.*raw.*smooth")
  expect_error(assert_common_args(1:2, "raw", NA), "attach_sample.*TRUE.*FALSE")
})
