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
    # "x_tbl" is completely missing
  input_bad_x_tbl_1 <- p_smooth
  attr(input_bad_x_tbl_1, "meta")[["x_tbl"]] <- NULL
  expect_error(assert_pdqr_fun(input_bad_x_tbl_1), "have.*x_tbl")

    # "x_tbl" has not proper structure
  input_bad_x_tbl_2 <- p_fin
  attr(input_bad_x_tbl_2, "meta")[["x_tbl"]] <- "a"
  expect_error(
    assert_pdqr_fun(input_bad_x_tbl_2), 'meta.*x_tbl.*data.*frame'
  )
})

test_that("assert_pdqr_fun checks extra properties of 'x_tbl' metadata", {
  # "x" is sorted
  input_bad_x_tbl_1 <- p_fin
  attr(input_bad_x_tbl_1, "meta")[["x_tbl"]][["x"]] <- rev(
    attr(input_bad_x_tbl_1, "meta")[["x_tbl"]][["x"]]
  )
  expect_error(assert_pdqr_fun(input_bad_x_tbl_1), '"x".*"x_tbl".*sorted')

  # "fin" `type`
    # Column "prob" is mandatory
  input_bad_x_tbl_2 <- p_fin
  attr(input_bad_x_tbl_2, "meta")[["x_tbl"]][["prob"]] <- NULL
  expect_error(assert_pdqr_fun(input_bad_x_tbl_2), '"x_tbl".*have.*"prob"')

    # Sum of "prob" is 1
  input_bad_x_tbl_3 <- p_fin
  attr(input_bad_x_tbl_3, "meta")[["x_tbl"]][["prob"]] <- 10 *
    attr(input_bad_x_tbl_3, "meta")[["x_tbl"]][["prob"]]
  expect_error(assert_pdqr_fun(input_bad_x_tbl_3), '"prob".*"x_tbl".*sum.*1')

    # Column "cumprob" is mandatory
  input_bad_x_tbl_4 <- p_fin
  attr(input_bad_x_tbl_4, "meta")[["x_tbl"]][["cumprob"]] <- NULL
  expect_error(assert_pdqr_fun(input_bad_x_tbl_4), '"x_tbl".*have.*"cumprob"')

  # "smooth" type
    # Total integral is 1
  input_bad_x_tbl_5 <- p_smooth
  attr(input_bad_x_tbl_5, "meta")[["x_tbl"]][["y"]] <- 10 *
    attr(input_bad_x_tbl_5, "meta")[["x_tbl"]][["y"]]
  expect_error(
    assert_pdqr_fun(input_bad_x_tbl_5), '[Tt]otal integral.*"x_tbl".*1'
  )

    # Column "cumprob" is mandatory
  input_bad_x_tbl_6 <- p_smooth
  attr(input_bad_x_tbl_6, "meta")[["x_tbl"]][["cumprob"]] <- NULL
  expect_error(assert_pdqr_fun(input_bad_x_tbl_6), '"x_tbl".*have.*"cumprob"')
})


# assert_distr_type -------------------------------------------------------
test_that("assert_distr_type works", {
  expect_silent(assert_distr_type("fin"))
  expect_silent(assert_distr_type("smooth"))

  expect_error(assert_distr_type(1), "string")
  expect_error(assert_distr_type(c("fin", "smooth")), "string")
  expect_error(assert_distr_type("a"), "fin.*smooth")
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
test_that("assert_x_tbl works with `type = 'fin'`", {
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
    assert_x_tbl(data.frame(x = c("a", "b")), type = "smooth"), '"x".*numeric'
  )
  expect_error(
    assert_x_tbl(data.frame(x = c(1, NA_real_)), type = "smooth"), '"x".*`NA`'
  )
  expect_error(
    assert_x_tbl(data.frame(x = c(1, Inf)), type = "smooth"), '"x".*finite'
  )

  # Column "y"
  expect_error(assert_x_tbl(data.frame(x = 1:2), type = "smooth"), '"y"')
  expect_error(
    assert_x_tbl(data.frame(x = 1:2, y = c("a", "b")), type = "smooth"),
    '"y".*numeric'
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


# assert_x_tbl_fin --------------------------------------------------------
# Tested in `assert_x_tbl()`


# assert_x_tbl_smooth -----------------------------------------------------
# Tested in `assert_x_tbl()`


# assert_x_tbl_meta -------------------------------------------------------
# Tested in `assert_pdqr_fun()`


# assert_num_col ----------------------------------------------------------
# Tested in `assert_x_tbl()`
