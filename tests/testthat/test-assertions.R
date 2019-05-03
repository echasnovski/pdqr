context("test-assertions")


# Notes -------------------------------------------------------------------
# Usage of `as_*()` functions withoub seamingly apparent reason (for example,
# `as_p(p_infin)`) ensures that output object has different enclosing
# environement. This is needed for caregul test behavior with "bad" metadat.


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

test_that("assert_type allows extra arguments for `predicate`", {
  is_geq <- function(x, min_val) {x >= min_val}
  expect_silent(assert_type(1, is_geq, min_val = 0))
  expect_error(assert_type(1, is_geq, min_val = 2))
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


# assert_in_set -----------------------------------------------------------
test_that("assert_in_set works",  {
  expect_silent(assert_in_set(1, 1:3))
  expect_silent(assert_in_set("a", c("b", "a")))

  input <- "a"
  expect_error(
    assert_in_set(input, c("b", "c")),
    '`input` should be one of: "b", "c" \\(instead of "a"\\)\\.'
  )
  expect_error(assert_in_set(input, "b"), 'one of: "b" \\(instead of "a"\\)\\.')
  expect_error(
    assert_in_set(1, 2:3), 'one of: "2", "3" \\(instead of "1"\\)\\.'
  )
  expect_error(
    assert_in_set(1, 2:3, quote_set = FALSE),
    'one of: 2, 3 \\(instead of 1\\)\\.'
  )
})


# assert_missing_args -----------------------------------------------------
test_that("assert_missing_args works", {
  expect_silent(assert_missing_args("d"))
  expect_silent(assert_missing_args("d", support = FALSE))
  expect_error(
    assert_missing_args("p", type = TRUE, support = FALSE),
    'p-function.*supply.*`type`'
  )
})


# assert_pdqr_fun ---------------------------------------------------------
test_that("assert_pdqr_fun works", {
  expect_silent(assert_pdqr_fun(p_fin))
  expect_silent(assert_pdqr_fun(d_fin))
  expect_silent(assert_pdqr_fun(q_fin))
  expect_silent(assert_pdqr_fun(r_fin))

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
  f_no_type <- structure(function(q) {user_p(q)}, class = c("p", "pdqr"))
  environment(f_no_type) <- new.env(parent = emptyenv())
  expect_error(assert_pdqr_fun(f_no_type), "proper.*type")

  f_bad_type <- f_no_type
  assign("type", "a", environment(f_bad_type))
  expect_error(
    assert_pdqr_fun(structure(f_bad_type, type = "a")),
    "proper.*type"
  )

  # "support" metadata
  f_no_support <- f_no_type
  assign("type", "infin", environment(f_no_support))
  expect_error(assert_pdqr_fun(f_no_support), "proper.*support")

  f_bad_support <- f_no_support
  assign("support", c(2, 1), environment(f_bad_support))
  expect_error(assert_pdqr_fun(f_bad_support), "proper.*support")

  # "x_tbl" metadata
    # "x_tbl" is completely missing
  f_no_x_tbl <- as_p(p_infin)
  rm("x_tbl", envir = environment(f_no_x_tbl))
  expect_error(assert_pdqr_fun(f_no_x_tbl), "have.*x_tbl")

    # "x_tbl" has not proper structure
  f_bad_x_tbl <- as_p(p_fin)
  assign("x_tbl", "a", environment(f_bad_x_tbl))
  expect_error(assert_pdqr_fun(f_bad_x_tbl), 'x_tbl.*data.*frame')
})

test_that("assert_pdqr_fun checks extra properties of 'x_tbl' metadata", {
  # "x" is sorted
  bad_x_tbl_1 <- x_fin_x_tbl
  bad_x_tbl_1[["x"]] <- rev(bad_x_tbl_1[["x"]])
  f_bad_x_tbl_1 <- as_p(p_fin)
  assign("x_tbl", bad_x_tbl_1, environment(f_bad_x_tbl_1))
  expect_error(assert_pdqr_fun(f_bad_x_tbl_1), '"x".*"x_tbl".*sorted')

  # "fin" `type`
    # Column "prob" is mandatory
  bad_x_tbl_2 <- x_fin_x_tbl
  bad_x_tbl_2[["prob"]] <- NULL
  f_bad_x_tbl_2 <- as_p(p_fin)
  assign("x_tbl", bad_x_tbl_2, environment(f_bad_x_tbl_2))
  expect_error(assert_pdqr_fun(f_bad_x_tbl_2), 'x_tbl.*have.*"prob"')

    # Sum of "prob" is 1
  bad_x_tbl_3 <- x_fin_x_tbl
  bad_x_tbl_3[["prob"]] <- 10 * bad_x_tbl_3[["prob"]]
  f_bad_x_tbl_3 <- as_p(p_fin)
  assign("x_tbl", bad_x_tbl_3, environment(f_bad_x_tbl_3))
  expect_error(assert_pdqr_fun(f_bad_x_tbl_3), '"prob".*"x_tbl".*sum.*1')

    # Column "cumprob" is mandatory
  bad_x_tbl_4 <- x_fin_x_tbl
  bad_x_tbl_4[["cumprob"]] <- NULL
  f_bad_x_tbl_4 <- as_p(p_fin)
  assign("x_tbl", bad_x_tbl_4, environment(f_bad_x_tbl_4))
  expect_error(assert_pdqr_fun(f_bad_x_tbl_4), '"x_tbl".*have.*"cumprob"')

    # Column "x" shouldn't have duplicate values
  bad_x_tbl_5 <- x_fin_x_tbl
  bad_x_tbl_5[["x"]] <- 1
  f_bad_x_tbl_5 <- as_p(p_fin)
  assign("x_tbl", bad_x_tbl_5, environment(f_bad_x_tbl_5))
  expect_error(assert_pdqr_fun(f_bad_x_tbl_5), '"x".*"x_tbl".*duplicate')

  # "infin" type
    # Total integral is 1
  bad_x_tbl_6 <- x_infin_x_tbl
  bad_x_tbl_6[["y"]] <- 10 * bad_x_tbl_6[["y"]]
  f_bad_x_tbl_6 <- as_p(p_infin)
  assign("x_tbl", bad_x_tbl_6, environment(f_bad_x_tbl_6))
  expect_error(assert_pdqr_fun(f_bad_x_tbl_6), '[Tt]otal integral.*"x_tbl".*1')

    # Column "cumprob" is mandatory
  bad_x_tbl_7 <- x_infin_x_tbl
  bad_x_tbl_7[["cumprob"]] <- NULL
  f_bad_x_tbl_7 <- as_p(p_infin)
  assign("x_tbl", bad_x_tbl_7, environment(f_bad_x_tbl_7))
  expect_error(assert_pdqr_fun(f_bad_x_tbl_7), '"x_tbl".*have.*"cumprob"')
})


# assert_distr_type -------------------------------------------------------
test_that("assert_distr_type works", {
  expect_silent(assert_distr_type("fin"))
  expect_silent(assert_distr_type("infin"))

  expect_error(assert_distr_type(1), "string")
  expect_error(assert_distr_type(c("fin", "infin")), "string")
  expect_error(assert_distr_type("a"), "fin.*infin")
})


# assert_support ----------------------------------------------------------
test_that("assert_support works", {
  expect_silent(assert_support(c(0, 1)))
  expect_silent(assert_support(c(1, 1)))
  expect_silent(assert_support(c(1, NA), allow_na = TRUE))

  expect_error(assert_support("a"), "numeric")
  expect_error(assert_support(1), "length 2")
  expect_error(assert_support(c(1, NA)), "missing value")
  expect_error(assert_support(c(2, 1)), "[fF]irst.*not bigger")
  expect_error(assert_support(c(-Inf, 1)), "finite")
  expect_error(assert_support(c(1, Inf)), "finite")
  expect_error(assert_support(c(-Inf, Inf)), "finite")
})


# assert_x_tbl ------------------------------------------------------------
test_that("assert_x_tbl works with 'fin' type", {
  expect_silent(assert_x_tbl(x_fin_x_tbl, type = "fin"))
  expect_silent(
    assert_x_tbl(data.frame(x = 1:3, prob = c(0, 1, 0)), type = "fin")
  )

  # Input type
  input <- "a"
  expect_error(assert_x_tbl(input, type = "fin"), "`input`.*data.*frame")

  # Column "x"
  expect_error(assert_x_tbl(data.frame(a = 1), type = "fin"), '"x"')
  expect_error(assert_x_tbl(data.frame(x = "a"), type = "fin"), '"x".*numeric')
  expect_error(
    assert_x_tbl(data.frame(x = NA_real_), type = "fin"), '"x".*`NA`'
  )
  expect_error(
    assert_x_tbl(data.frame(x = Inf), type = "fin"), '"x".*finite'
  )

  # Column "prob"
  expect_error(assert_x_tbl(data.frame(x = 1), type = "fin"), '"prob"')
  expect_error(
    assert_x_tbl(data.frame(x = 1, prob = "a"), type = "fin"), '"prob".*numeric'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1, prob = NA_real_), type = "fin"),
    '"prob".*`NA`'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1, prob = Inf), type = "fin"),
    '"prob".*finite'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1, prob = -1), type = "fin"), '"prob".*negative'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1, prob = 0), type = "fin"),
    '"prob".*positive sum'
  )

  # Extra columns are allowed
  expect_silent(
    assert_x_tbl(data.frame(x = 1, prob = 1, extra = "a"), type = "fin")
  )
  # Different column order is allowed
  expect_silent(
    assert_x_tbl(data.frame(prob = c(0.1, 0.9), x = 1:2), type = "fin")
  )
})

test_that("assert_x_tbl works with 'infin' type", {
  expect_silent(assert_x_tbl(x_infin_x_tbl, type = "infin"))

  # Input type
  input <- "a"
  expect_error(assert_x_tbl(input, type = "infin"), "`input`.*data.*frame")

  # Number of rows
  expect_error(
    assert_x_tbl(data.frame(x = 1, y = 1), type = "infin"), "2.*rows"
  )

  # Column "x"
  expect_error(assert_x_tbl(data.frame(a = 1:2), type = "infin"), '"x"')
  expect_error(
    assert_x_tbl(data.frame(x = c("a", "b")), type = "infin"), '"x".*numeric'
  )
  expect_error(
    assert_x_tbl(data.frame(x = c(1, NA_real_)), type = "infin"), '"x".*`NA`'
  )
  expect_error(
    assert_x_tbl(data.frame(x = c(1, Inf)), type = "infin"), '"x".*finite'
  )
  expect_error(
    assert_x_tbl(data.frame(x = c(1, 1, 2), y = c(1, 1, 1)), type = "infin"),
    '"x".*duplicate'
  )

  # Column "y"
  expect_error(assert_x_tbl(data.frame(x = 1:2), type = "infin"), '"y"')
  expect_error(
    assert_x_tbl(data.frame(x = 1:2, y = c("a", "b")), type = "infin"),
    '"y".*numeric'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1:2, y = c(1, NA_real_)), type = "infin"),
    '"y".*`NA`'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1:2, y = c(-1, 1)), type = "infin"),
    '"y".*negative'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1:2, y = c(0, 0)), type = "infin"),
    '"y".*positive'
  )

  # Extra columns are allowed
  expect_silent(
    assert_x_tbl(data.frame(x = 1:2, y = c(1, 1), extra = "a"), type = "infin")
  )
  # Different column order is allowed
  expect_silent(
    assert_x_tbl(data.frame(y = c(1, 1), x = 1:2), type = "infin")
  )
})


# assert_x_tbl_fin --------------------------------------------------------
# Tested in `assert_x_tbl()`


# assert_x_tbl_infin ------------------------------------------------------
# Tested in `assert_x_tbl()`


# assert_x_tbl_meta -------------------------------------------------------
# Tested in `assert_pdqr_fun()`


# assert_num_col ----------------------------------------------------------
# Tested in `assert_x_tbl()`
