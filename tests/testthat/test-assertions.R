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


# assert_pdqr_ftype -------------------------------------------------------
test_that("assert_pdqr_ftype works", {
  expect_error(assert_pdqr_ftype(1), "p_fun.*d_fun.*q_fun.*r_fun")
  expect_error(assert_pdqr_ftype(user_p), "p_fun.*d_fun.*q_fun.*r_fun")

  expect_silent(assert_pdqr_ftype(p_raw))
  expect_silent(assert_pdqr_ftype(d_raw))
  expect_silent(assert_pdqr_ftype(q_raw))
  expect_silent(assert_pdqr_ftype(r_raw))
})
