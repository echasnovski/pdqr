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
# Tested in tests for conversion of pdqr-functions from user-defined function


# assert_pdqr_fun ---------------------------------------------------------
test_that("assert_pdqr_fun works", {
  expect_silent(assert_pdqr_fun(p_raw))
  expect_silent(assert_pdqr_fun(d_raw))
  expect_silent(assert_pdqr_fun(q_raw))
  expect_silent(assert_pdqr_fun(r_raw))

  # Function type
  input <- 1
  expect_error(assert_pdqr_fun(input), "`input`.*function")

  # Classes
  expect_error(assert_pdqr_fun(user_p), "inherit.*pdqr")
  expect_error(
    assert_pdqr_fun(structure(user_p, class = "pdqr")),
    "inherit.*p.*d.*q.*r"
  )

  # "type" metadata
  f_with_class <- structure(user_p, class = c("p", "pdqr"))
  expect_error(assert_pdqr_fun(f_with_class), "proper.*type")
  expect_error(
    assert_pdqr_fun(structure(f_with_class, type = "a")),
    "proper.*type"
  )

  # "support" metadata
  expect_error(
    assert_pdqr_fun(structure(f_with_class, meta = list(type = "smooth"))),
    "proper.*support"
  )
  f_with_corrupt_support <- structure(
    f_with_class, meta = list(type = "smooth", support = c(2, 1))
  )
  expect_error(assert_pdqr_fun(f_with_corrupt_support), "proper.*support")

  # "x_tbl" metadata
  expect_silent(assert_pdqr_fun(p_custom))

    # "x_tbl" is completely missing
  input_bad_x_tbl_1 <- p_smooth
  attr(input_bad_x_tbl_1, "meta")[["x_tbl"]] <- NULL
  expect_error(assert_pdqr_fun(input_bad_x_tbl_1), "have.*x_tbl")

    # "x_tbl" has not proper structure
  input_bad_x_tbl_2 <- p_raw
  attr(input_bad_x_tbl_2, "meta")[["x_tbl"]] <- "a"
  expect_error(
    assert_pdqr_fun(input_bad_x_tbl_2), 'meta.*x_tbl.*data.*frame'
  )

    # "x_tbl" is present but equals `NULL` in case `type` is "raw"
  input_bad_x_tbl_3 <- p_raw
  attr(input_bad_x_tbl_3, "meta") <- c(
    meta(input_bad_x_tbl_3)[c("support", "type")], list(x_tbl = NULL)
  )
  expect_error(assert_pdqr_fun(input_bad_x_tbl_3), 'no.*NULL.*x_tbl.*"raw"')
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


# assert_x_tbl ------------------------------------------------------------
test_that("assert_x_tbl works with `type = 'raw'`", {
  expect_silent(assert_x_tbl(x_raw_x_tbl, type = "raw"))
  expect_silent(assert_x_tbl(x_raw_x_tbl[, c("x", "prob")], type = "raw"))
  expect_silent(assert_x_tbl(x_raw_x_tbl[, c("x", "n")], type = "raw"))

  # Input type
  input <- "a"
  expect_error(assert_x_tbl(input, type = "raw"), "`input`.*data.*frame")

  # Column "x"
  expect_error(assert_x_tbl(data.frame(a = 1), type = "raw"), '"x"')
  expect_error(assert_x_tbl(data.frame(x = "a"), type = "raw"), 'numeric.*"x"')
  expect_error(
    assert_x_tbl(data.frame(x = NA_real_), type = "raw"), '"x".*`NA`'
  )

  # Presense of at least one of "prob" or "n"
  expect_error(assert_x_tbl(data.frame(x = 1), type = "raw"), '"prob".*"n"')

  # Column "n"
  expect_silent(assert_x_tbl(data.frame(x = 1, n = 2), type = "raw"))

  expect_error(
    assert_x_tbl(data.frame(x = 1, n = "a"), type = "raw"), '"n".*numeric'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1, n = NA_real_), type = "raw"), '"n".*`NA`'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1, n = -1), type = "raw"), '"n".*negative'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1, n = 0), type = "raw"),
    '"n".*positive sum'
  )

  # Test that "prob" is ignored if "n" is OK
  expect_silent(assert_x_tbl(data.frame(x = 1, prob = -1, n = 1), type = "raw"))

  # Column "prob"
  expect_error(
    assert_x_tbl(data.frame(x = 1, prob = "a"), type = "raw"), '"prob".*numeric'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1, prob = NA_real_), type = "raw"),
    '"prob".*`NA`'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1, prob = -1), type = "raw"), '"prob".*negative'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1, prob = 0), type = "raw"),
    '"prob".*positive sum'
  )

  # Extra columns are allowed
  expect_silent(
    assert_x_tbl(data.frame(x = 1, prob = 1, extra = "a"), type = "raw")
  )
  # Different column order is allowed
  expect_silent(
    assert_x_tbl(
      data.frame(prob = c(0.1, 0.9), x = 1:2, n = c(1, 9)),
      type = "raw"
    )
  )
})

test_that("assert_x_tbl works with `type = 'smooth'`", {
  expect_silent(assert_x_tbl(x_smooth_x_tbl, type = "smooth"))

  # Input type
  input <- "a"
  expect_error(assert_x_tbl(input, type = "smooth"), "`input`.*data.*frame")

  # Number of rows
  expect_error(
    assert_x_tbl(data.frame(x = 1, y = 1), type = "smooth"), "2.*rows"
  )

  # Column "x"
  expect_error(assert_x_tbl(data.frame(a = 1:2), type = "smooth"), '"x"')
  expect_error(
    assert_x_tbl(data.frame(x = c("a", "b")), type = "smooth"), 'numeric.*"x"'
  )

  # Column "y"
  expect_error(assert_x_tbl(data.frame(x = 1:2), type = "smooth"), '"y"')
  expect_error(
    assert_x_tbl(data.frame(x = 1:2, y = c("a", "b")), type = "smooth"),
    'numeric.*"y"'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1:2, y = c(1, NA_real_)), type = "smooth"),
    '"y".*`NA`'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1:2, y = c(-1, 1)), type = "smooth"),
    '"y".*negative'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1:2, y = c(0, 0)), type = "smooth"),
    '"y".*positive'
  )

  # Extra columns are allowed
  expect_silent(
    assert_x_tbl(data.frame(x = 1:2, y = c(1, 1), extra = "a"), type = "smooth")
  )
  # Different column order is allowed
  expect_silent(
    assert_x_tbl(data.frame(y = c(1, 1), x = 1:2), type = "smooth")
  )
})


# assert_x_tbl_raw --------------------------------------------------------
# Tested in `assert_x_tbl()`


# assert_probish ----------------------------------------------------------
# Tested in `assert_x_tbl()`


# assert_x_tbl_smooth -----------------------------------------------------
# Tested in `assert_x_tbl()`


# assert_tot_prob ---------------------------------------------------------
test_that("assert_tot_prob works", {
  expect_error(assert_tot_prob(0), "probability.*zero")
  expect_silent(assert_tot_prob(0.1))
})
