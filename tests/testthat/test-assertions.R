context("test-assertions")


# Notes -------------------------------------------------------------------
# Usage of `as_*()` functions withoub seamingly apparent reason (for example,
# `as_p(p_con)`) ensures that output object has different enclosing
# environement. This is needed for careful test behavior with "bad" metadat.


# assert_type -------------------------------------------------------------
test_that("assert_type works", {
  x_var <- 1L

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

test_that("assert_type allows custom name for `x`", {
  input <- "a"
  expect_error(assert_type(input, is.numeric, x_name = "aaa"), "^`aaa`")
})

test_that("assert_type allows extra arguments for `predicate`", {
  is_geq <- function(x, min_val) {
    x >= min_val
  }
  expect_silent(assert_type(1, is_geq, min_val = 0))
  expect_error(assert_type(1, is_geq, min_val = 2))
})

test_that("assert_type respects global options", {
  op <- options(pdqr.assert_args = FALSE)
  on.exit(options(op))
  expect_silent(assert_type("a", "b"))
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
    "one of: 2, 3 \\(instead of 1\\)\\."
  )
})

test_that("assert_in_set suggests correct value for character values", {
  expect_error(
    assert_in_set("ks", c("KS", "PPV", "FPV")), 'Did you mean "KS"\\?'
  )
  expect_error(
    assert_in_set("pPV", c("KS", "PPV", "FPV")), 'Did you mean "PPV"\\?'
  )
  expect_error(
    assert_in_set("medin", c("mean", "median")), 'Did you mean "median"\\?'
  )
})

test_that("assert_in_set uses partial match first", {
  expect_error(
    assert_in_set("con", c("discrete", "continuous")),
    'Did you mean "continuous"\\?'
  )
})

test_that("assert_in_set allows custom name for `x`", {
  input <- "a"
  expect_error(assert_in_set(input, c("b"), x_name = "aaa"), "^`aaa`")
})

test_that("assert_in_set uses `allow_null` argument", {
  expect_silent(assert_in_set(NULL, c("a", "b"), allow_null = TRUE))
  expect_error(assert_in_set(NULL, c("a", "b")), "(instead of NULL)")
})

test_that("assert_in_set respects global options", {
  op <- options(pdqr.assert_args = FALSE)
  on.exit(options(op))
  expect_silent(assert_in_set("a", "b"))
})


# is_in_set ---------------------------------------------------------------
test_that("is_in_set works", {
  expect_true(is_in_set(1, 1:3))
  expect_false(is_in_set(0, 1:3))

  expect_true(is_in_set(NULL, 1:3, allow_null = TRUE))
  expect_false(is_in_set(NULL, 1:3))
})


# match_in_set ------------------------------------------------------------
# Tested in `assert_in_set()`


# assert_missing ----------------------------------------------------------
test_that("assert_missing works", {
  f <- function(y) {
    assert_missing(y, value_name = "aaa")
  }
  expect_error(f(), "^Argument `y` is missing. Supply aaa.$")
})

test_that("assert_missing respects global options", {
  op <- options(pdqr.assert_args = FALSE)
  on.exit(options(op))

  f <- function(y) {
    assert_missing(y, value_name = "aaa")
  }
  expect_silent(assert_missing(f()))
})


# error_missing -----------------------------------------------------------
# Tested in `assert_missing()` and some other `assert_*()` functions


# assert_pdqr_fun ---------------------------------------------------------
test_that("assert_pdqr_fun works", {
  expect_silent(assert_pdqr_fun(p_dis))
  expect_silent(assert_pdqr_fun(d_dis))
  expect_silent(assert_pdqr_fun(q_dis))
  expect_silent(assert_pdqr_fun(r_dis))

  # Missing argument
  expect_error(assert_pdqr_fun(), "missing. Supply pdqr-function.")

  # Function type
  input <- 1
  expect_error(assert_pdqr_fun(input), "`input`.*not pdqr-function.*function")

  # Classes
  expect_error(assert_pdqr_fun(user_p), "not pdqr-function.*inherit.*pdqr")
  expect_error(
    assert_pdqr_fun(structure(user_p, class = "pdqr")),
    "not pdqr-function.*inherit.*p.*d.*q.*r"
  )

  # "type" metadata
  f_no_type <- structure(function(q) {
    user_p(q)
  }, class = c("p", "pdqr"))
  environment(f_no_type) <- new.env(parent = emptyenv())
  expect_error(assert_pdqr_fun(f_no_type), "not pdqr-function.*proper.*type")

  f_bad_type <- f_no_type
  assign("type", "a", environment(f_bad_type))
  expect_error(
    assert_pdqr_fun(structure(f_bad_type, type = "a")),
    "not pdqr-function.*proper.*type"
  )

  # "support" metadata
  f_no_support <- f_no_type
  assign("type", "continuous", environment(f_no_support))
  expect_error(
    assert_pdqr_fun(f_no_support), "not pdqr-function.*proper.*support"
  )

  f_bad_support <- f_no_support
  assign("support", c(2, 1), environment(f_bad_support))
  expect_error(
    assert_pdqr_fun(f_bad_support), "not pdqr-function.*proper.*support"
  )

  # "x_tbl" metadata
  ## "x_tbl" is completely missing
  f_no_x_tbl <- as_p(p_con)
  rm("x_tbl", envir = environment(f_no_x_tbl))
  expect_error(assert_pdqr_fun(f_no_x_tbl), "not pdqr-function.*have.*x_tbl")

  ## "x_tbl" has not proper structure
  f_bad_x_tbl <- as_p(p_dis)
  assign("x_tbl", "a", environment(f_bad_x_tbl))
  expect_error(
    assert_pdqr_fun(f_bad_x_tbl), "not pdqr-function.*x_tbl.*data.*frame"
  )
})

test_that("assert_pdqr_fun checks extra properties of 'x_tbl' metadata", {
  # "x" is sorted
  bad_x_tbl_1 <- x_dis_x_tbl
  bad_x_tbl_1[["x"]] <- rev(bad_x_tbl_1[["x"]])
  f_bad_x_tbl_1 <- as_p(p_dis)
  assign("x_tbl", bad_x_tbl_1, environment(f_bad_x_tbl_1))
  expect_error(assert_pdqr_fun(f_bad_x_tbl_1), '"x".*"x_tbl".*sorted')

  # "discrete" `type`
  ## Column "prob" is mandatory
  bad_x_tbl_2 <- x_dis_x_tbl
  bad_x_tbl_2[["prob"]] <- NULL
  f_bad_x_tbl_2 <- as_p(p_dis)
  assign("x_tbl", bad_x_tbl_2, environment(f_bad_x_tbl_2))
  expect_error(assert_pdqr_fun(f_bad_x_tbl_2), 'x_tbl.*have.*"prob"')

  ## Sum of "prob" is 1
  bad_x_tbl_3 <- x_dis_x_tbl
  bad_x_tbl_3[["prob"]] <- 10 * bad_x_tbl_3[["prob"]]
  f_bad_x_tbl_3 <- as_p(p_dis)
  assign("x_tbl", bad_x_tbl_3, environment(f_bad_x_tbl_3))
  expect_error(assert_pdqr_fun(f_bad_x_tbl_3), '"prob".*"x_tbl".*sum.*1')

  ## Column "cumprob" is mandatory
  bad_x_tbl_4 <- x_dis_x_tbl
  bad_x_tbl_4[["cumprob"]] <- NULL
  f_bad_x_tbl_4 <- as_p(p_dis)
  assign("x_tbl", bad_x_tbl_4, environment(f_bad_x_tbl_4))
  expect_error(assert_pdqr_fun(f_bad_x_tbl_4), '"x_tbl".*have.*"cumprob"')

  ## Column "x" shouldn't have duplicate values
  bad_x_tbl_5 <- x_dis_x_tbl
  bad_x_tbl_5[["x"]] <- 1
  f_bad_x_tbl_5 <- as_p(p_dis)
  assign("x_tbl", bad_x_tbl_5, environment(f_bad_x_tbl_5))
  expect_error(assert_pdqr_fun(f_bad_x_tbl_5), '"x".*"x_tbl".*duplicate')

  # "continuous" type
  ## Total integral is 1
  bad_x_tbl_6 <- x_con_x_tbl
  bad_x_tbl_6[["y"]] <- 10 * bad_x_tbl_6[["y"]]
  f_bad_x_tbl_6 <- as_p(p_con)
  assign("x_tbl", bad_x_tbl_6, environment(f_bad_x_tbl_6))
  expect_error(assert_pdqr_fun(f_bad_x_tbl_6), '[Tt]otal integral.*"x_tbl".*1')

  ## Column "cumprob" is mandatory
  bad_x_tbl_7 <- x_con_x_tbl
  bad_x_tbl_7[["cumprob"]] <- NULL
  f_bad_x_tbl_7 <- as_p(p_con)
  assign("x_tbl", bad_x_tbl_7, environment(f_bad_x_tbl_7))
  expect_error(assert_pdqr_fun(f_bad_x_tbl_7), '"x_tbl".*have.*"cumprob"')
})

test_that("assert_pdqr_fun allows custom name for `f`", {
  input <- "a"
  expect_error(assert_pdqr_fun(input, f_name = "aaa"), "^`aaa`")
})

test_that("assert_pdqr_fun respects global options", {
  op <- options(pdqr.assert_args = FALSE)
  on.exit(options(op))
  expect_silent(assert_pdqr_fun("a"))
})


# assert_pdqr_type --------------------------------------------------------
test_that("assert_pdqr_type works", {
  expect_silent(assert_pdqr_type("discrete"))
  expect_silent(assert_pdqr_type("continuous"))
  expect_silent(assert_pdqr_type(NULL, allow_null = TRUE))

  expect_error(assert_pdqr_type(1), "string")
  expect_error(assert_pdqr_type(c("discrete", "continuous")), "string")
  expect_error(assert_pdqr_type("a"), "discrete.*continuous")
  expect_error(assert_pdqr_type(NULL), "NULL")
})

test_that("assert_pdqr_type suggests correctly", {
  expect_error(assert_pdqr_type("dis"), 'mean "discrete"\\?')
  expect_error(assert_pdqr_type("con"), 'mean "continuous"\\?')
})

test_that("assert_pdqr_type respects global options", {
  op <- options(pdqr.assert_args = FALSE)
  on.exit(options(op))
  expect_silent(assert_pdqr_type("a"))
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

test_that("assert_support respects global options", {
  op <- options(pdqr.assert_args = FALSE)
  on.exit(options(op))
  expect_silent(assert_support("a"))
})


# assert_x_tbl ------------------------------------------------------------
test_that("assert_x_tbl works with 'discrete' type", {
  expect_silent(assert_x_tbl(x_dis_x_tbl, type = "discrete"))
  expect_silent(
    assert_x_tbl(data.frame(x = 1:3, prob = c(0, 1, 0)), type = "discrete")
  )

  # Input type
  input <- "a"
  expect_error(assert_x_tbl(input, type = "discrete"), "`input`.*data.*frame")

  # Column "x"
  expect_error(assert_x_tbl(data.frame(a = 1), type = "discrete"), '"x"')
  expect_error(
    assert_x_tbl(data.frame(x = "a"), type = "discrete"), '"x".*numeric'
  )
  expect_error(
    assert_x_tbl(data.frame(x = NA_real_), type = "discrete"), '"x".*`NA`'
  )
  expect_error(
    assert_x_tbl(data.frame(x = Inf), type = "discrete"), '"x".*finite'
  )

  # Column "prob"
  expect_error(assert_x_tbl(data.frame(x = 1), type = "discrete"), '"prob"')
  expect_error(
    assert_x_tbl(data.frame(x = 1, prob = "a"), type = "discrete"),
    '"prob".*numeric'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1, prob = NA_real_), type = "discrete"),
    '"prob".*`NA`'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1, prob = Inf), type = "discrete"),
    '"prob".*finite'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1, prob = -1), type = "discrete"),
    '"prob".*negative'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1, prob = 0), type = "discrete"),
    '"prob".*positive sum'
  )

  # Extra columns are allowed
  expect_silent(
    assert_x_tbl(data.frame(x = 1, prob = 1, extra = "a"), type = "discrete")
  )
  # Different column order is allowed
  expect_silent(
    assert_x_tbl(data.frame(prob = c(0.1, 0.9), x = 1:2), type = "discrete")
  )
})

test_that("assert_x_tbl works with 'continuous' type", {
  expect_silent(assert_x_tbl(x_con_x_tbl, type = "continuous"))

  # Input type
  input <- "a"
  expect_error(assert_x_tbl(input, type = "continuous"), "`input`.*data.*frame")

  # Number of rows
  expect_error(
    assert_x_tbl(data.frame(x = 1, y = 1), type = "continuous"), "2.*rows"
  )

  # Column "x"
  expect_error(assert_x_tbl(data.frame(a = 1:2), type = "continuous"), '"x"')
  expect_error(
    assert_x_tbl(data.frame(x = c("a", "b")), type = "continuous"),
    '"x".*numeric'
  )
  expect_error(
    assert_x_tbl(data.frame(x = c(1, NA_real_)), type = "continuous"),
    '"x".*`NA`'
  )
  expect_error(
    assert_x_tbl(data.frame(x = c(1, Inf)), type = "continuous"), '"x".*finite'
  )
  expect_error(
    assert_x_tbl(
      data.frame(x = c(1, 1, 2), y = c(1, 1, 1)), type = "continuous"
    ),
    '"x".*duplicate'
  )

  # Column "y"
  expect_error(assert_x_tbl(data.frame(x = 1:2), type = "continuous"), '"y"')
  expect_error(
    assert_x_tbl(data.frame(x = 1:2, y = c("a", "b")), type = "continuous"),
    '"y".*numeric'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1:2, y = c(1, NA_real_)), type = "continuous"),
    '"y".*`NA`'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1:2, y = c(-1, 1)), type = "continuous"),
    '"y".*negative'
  )
  expect_error(
    assert_x_tbl(data.frame(x = 1:2, y = c(0, 0)), type = "continuous"),
    '"y".*positive'
  )

  # Extra columns are allowed
  expect_silent(
    assert_x_tbl(
      data.frame(x = 1:2, y = c(1, 1), extra = "a"), type = "continuous"
    )
  )
  # Different column order is allowed
  expect_silent(
    assert_x_tbl(data.frame(y = c(1, 1), x = 1:2), type = "continuous")
  )
})

test_that("assert_x_tbl respects global options", {
  op <- options(pdqr.assert_args = FALSE)
  on.exit(options(op))
  expect_silent(assert_x_tbl("a", "b"))
})


# assert_x_tbl_dis --------------------------------------------------------
# Tested in `assert_x_tbl()`


# assert_x_tbl_con --------------------------------------------------------
# Tested in `assert_x_tbl()`


# assert_x_tbl_meta -------------------------------------------------------
# Tested in `assert_pdqr_fun()`


# assert_num_col ----------------------------------------------------------
# Tested in `assert_x_tbl()`


# assert_method -----------------------------------------------------------
test_that("assert_method works", {
  expect_silent(assert_method("a", c("a", "b")))

  x <- 1
  expect_error(assert_method(x, c("a", "b")), "`x`.*string")
  y <- "c"
  expect_error(assert_method(y, c("a", "b")), '`y`.*one of.*"a", "b"')
})

test_that("assert_method respects global options", {
  op <- options(pdqr.assert_args = FALSE)
  on.exit(options(op))
  expect_silent(assert_method(1, "a"))
})


# warning_boolean_pdqr_fun ------------------------------------------------
test_that("warning_boolean_pdqr_fun works", {
  my_f <- new_d(data.frame(x = c(-1, 0, 1), prob = 1:3 / 6), "discrete")
  expect_warning(warning_boolean_pdqr_fun(my_f), "`my_f`.*not.*boolean")

  bool_f <- boolean_pdqr(0.5, "d")
  # This still throws warning because there is no check for "booleanness"
  expect_warning(
    warning_boolean_pdqr_fun(bool_f, f_name = "my_bool"),
    "my_bool.*not.*boolean"
  )
})


# dont_assert -------------------------------------------------------------
test_that("dont_assert works", {
  op <- options(pdqr.assert_args = TRUE)
  on.exit(options(op))
  expect_false(dont_assert())

  options(pdqr.assert_args = FALSE)
  expect_true(dont_assert())
})
