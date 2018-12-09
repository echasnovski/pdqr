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

test_that("assert_type allows `NULL`", {
  input <- NULL
  expect_silent(assert_type(input, is.numeric, allow_null = TRUE))
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


# assert_missing_args -----------------------------------------------------
# Tested in tests for construction of `*_fun` from user-defined function


# assert_pdqr_fun ---------------------------------------------------------
test_that("assert_pdqr_fun works", {
  input <- 1
  expect_error(assert_pdqr_fun(input), "`input`.*function")
  expect_error(assert_pdqr_fun(user_p), "inherit.*pdqr_fun")
  expect_error(
    assert_pdqr_fun(structure(user_p, class = "pdqr_fun")),
    "inherit.*p_fun.*d_fun.*q_fun.*r_fun"
  )

  f_with_class <- structure(user_p, class = c("p_fun", "pdqr_fun"))
  expect_error(assert_pdqr_fun(f_with_class), "proper.*type")
  expect_error(
    assert_pdqr_fun(structure(f_with_class, type = "a")),
    "proper.*type"
  )

  expect_error(
    assert_pdqr_fun(structure(f_with_class, meta = list(type = "smooth"))),
    "proper.*support"
  )
  f_with_corrupt_support <- structure(
    f_with_class, meta = list(type = "smooth", support = c(2, 1))
  )
  expect_error(assert_pdqr_fun(f_with_corrupt_support), "proper.*support")

  f_with_corrupt_x <- p_raw_withx
  attr(f_with_corrupt_x, "meta")[["x"]] <- "a"
  expect_error(assert_pdqr_fun(f_with_corrupt_x), "x.*metadata.*numeric")

  expect_silent(assert_pdqr_fun(p_raw_withx))
  expect_silent(assert_pdqr_fun(d_raw_withx))
  expect_silent(assert_pdqr_fun(q_raw_withx))
  expect_silent(assert_pdqr_fun(r_raw_withx))
})


# assert_distr_type -------------------------------------------------------
test_that("assert_distr_type works", {
  expect_silent(assert_distr_type("raw"))
  expect_silent(assert_distr_type("smooth"))

  expect_error(assert_distr_type(1), "string")
  expect_error(assert_distr_type(c("raw", "smooth")), "string")
  expect_error(assert_distr_type("a"), "raw.*smooth")
})


# assert_support ----------------------------------------------------------
test_that("assert_support works", {
  expect_silent(assert_support(c(0, 1)))
  expect_silent(assert_support(c(1, 1)))

  expect_error(assert_support("a"), "numeric")
  expect_error(assert_support(1), "length 2")
  expect_error(assert_support(c(1, NA)), "missing value")
  expect_error(assert_support(c(2, 1)), "[fF]irst.*not bigger")
  expect_error(assert_support(c(-Inf, 1)), "finite")
  expect_error(assert_support(c(1, Inf)), "finite")
  expect_error(assert_support(c(-Inf, Inf)), "finite")
})


# assert_tot_prob ---------------------------------------------------------
test_that("assert_tot_prob works", {
  expect_error(assert_tot_prob(0), "probability.*zero")
  expect_silent(assert_tot_prob(0.1))
})
