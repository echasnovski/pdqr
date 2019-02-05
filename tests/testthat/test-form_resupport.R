context("test-form_resupport")


# form_resupport ----------------------------------------------------------
test_that("form_resupport works with `method = 'trim'` and `type`='fin'", {
  x_tbl <- data.frame(
    x    = c(0,   1, 2,   3,   4,   5,   6, 7),
    prob = c(0, 0.2, 0, 0.4, 0.1, 0.1, 0.2, 0)
  )
  p_f <- new_p(x_tbl, "fin")

  out_x_tbl_1 <- meta_x_tbl(form_resupport(p_f, c(1.5, 4.5), "trim"))
  expect_equal(
    out_x_tbl_1[, c("x", "prob")], data.frame(x = 2:4, prob = c(0, 0.8, 0.2))
  )

  out_x_tbl_2 <- meta_x_tbl(form_resupport(p_f, c(-1, 1), "trim"))
  expect_equal(
    out_x_tbl_2[, c("x", "prob")], data.frame(x = 0:1, prob = c(0, 1))
  )

  out_x_tbl_3 <- meta_x_tbl(form_resupport(p_f, c(-100, 100), "trim"))
  expect_equal(out_x_tbl_3[, c("x", "prob")], x_tbl)

  expect_error(form_resupport(p_f, c(0.5, 0.75), "trim"), "not.*positive.*prob")
  expect_error(form_resupport(p_f, c(-1, 0.5), "trim"), "not.*positive.*prob")
  expect_error(form_resupport(p_f, c(6.5, 7), "trim"), "not.*positive.*prob")
})

test_that("form_resupport works with `method = 'trim'` and `type`='infin'", {
  x_tbl <- data.frame(
    x = c(0, 1,   2, 3, 4, 5,   6, 7,   8),
    y = c(0, 0, 0.4, 0, 0, 0, 0.4, 0, 0.4)
  )
  d_f <- new_d(x_tbl, "infin")

  out_x_tbl_1 <- meta_x_tbl(form_resupport(d_f, c(0.5, 3.5), "trim"))
  expect_equal(
    out_x_tbl_1[, c("x", "y")],
    data.frame(x = c(0.5, 1, 2, 3, 3.5), y = c(0, 0, 1, 0, 0))
  )

  out_x_tbl_2 <- meta_x_tbl(form_resupport(d_f, c(-1, 1.5), "trim"))
  expect_equal(
    out_x_tbl_2[, c("x", "y")],
    data.frame(x = c(0, 1, 1.5), y = c(0, 0, 4))
  )

  out_x_tbl_3 <- meta_x_tbl(form_resupport(d_f, c(6.5, 7.5), "trim"))
  expect_equal(
    out_x_tbl_3[, c("x", "y")],
    data.frame(x = c(6.5, 7, 7.5), y = c(2, 0, 2))
  )

  out_x_tbl_4 <- meta_x_tbl(form_resupport(d_f, c(-100, 100), "trim"))
  expect_equal(out_x_tbl_4[, c("x", "y")], x_tbl)

  # Check that output doesn't increase support
  expect_equal(
    meta_support(d_f),
    meta_support(form_resupport(d_f, c(-100, 100), "trim"))
  )

  # Single point support has total zero probability
  expect_error(form_resupport(d_f, c(2, 2), "trim"), "not.*positive.*prob")
  # Inside present support but with total zero probability
  expect_error(form_resupport(d_f, c(3, 5), "trim"), "not.*positive.*prob")

  expect_error(form_resupport(d_f, c(0.5, 0.75), "trim"), "not.*positive.*prob")
  expect_error(form_resupport(d_f, c(-1, 0.5), "trim"), "not.*positive.*prob")
})

test_that("form_resupport works with `method = 'linear'` and `type`='fin'", {
  p_f <- new_p(data.frame(x = c(-1, -0.25, 2), prob = c(0, 0.1, 0.9)), "fin")

  out_x_tbl_1 <- meta_x_tbl(form_resupport(p_f, c(-0.5, 3), "linear"))
  expect_equal(
    out_x_tbl_1[, c("x", "prob")],
    data.frame(x = c(-0.5, 0.375, 3), prob = meta_x_tbl(p_f)[["prob"]])
  )

  out_x_tbl_2 <- meta_x_tbl(form_resupport(p_f, c(15, 15), "linear"))
  expect_equal(out_x_tbl_2[, c("x", "prob")], data.frame(x = 15, prob = 1))

  # Can't resupport from single point support to interval one
  expect_error(
    form_resupport(new_p(1, "fin"), c(0, 1), "linear"), "single.*interval"
  )
})

test_that("form_resupport works with `method = 'linear'` and `type`='infin'", {
  x_tbl <- data.frame(
    x = c(0, 1,   2, 5),
    y = c(0, 0, 0.5, 0)
  )
  p_f <- new_p(x_tbl, "infin")

  out_x_tbl_1 <- meta_x_tbl(form_resupport(p_f, c(1, 3.5), "linear"))
  expect_equal(
    out_x_tbl_1[, c("x", "y")],
    data.frame(x = c(1, 1.5, 2, 3.5), y = c(0, 0, 1, 0))
  )

  # For now collapsing "infin" pdqr-function into singular "fin" one is
  # considered correct behaviour
  out_x_tbl_2 <- meta_x_tbl(form_resupport(p_f, c(15, 15), "linear"))
  expect_equal(out_x_tbl_2[, c("x", "prob")], data.frame(x = 15, prob = 1))
})

test_that("form_resupport works with `method = 'reflect'` and `type`='fin'", {
  p_f <- new_p(data.frame(x = 1:4, prob = (1:4)/10), "fin")
  p_f_x_tbl <- meta_x_tbl(p_f)

  # Returns self when supplied support equals `f`'s support or wider
  expect_equal(meta_x_tbl(form_resupport(p_f, c(1, 4), "reflect")), p_f_x_tbl)
  expect_equal(meta_x_tbl(form_resupport(p_f, c(0, 5), "reflect")), p_f_x_tbl)

  # Left reflection
  expect_equal(
    meta_x_tbl(form_resupport(p_f, c(1.1, 5), "reflect"))[, c("x", "prob")],
    data.frame(x = c(1.2, 2:4), prob = (1:4)/10)
  )

  # Right reflection
  expect_equal(
    meta_x_tbl(form_resupport(p_f, c(0, 2.3), "reflect"))[, c("x", "prob")],
    data.frame(x = c(0.6, 1, 1.6, 2), prob = c(0.4, 0.1, 0.3, 0.2))
  )

  # Reflection from both sides
  # There are 3 numbers instead of four because 4 got reflected to 0.6 which is
  # outside of supplied support
  expect_equal(
    meta_x_tbl(form_resupport(p_f, c(1.1, 2.3), "reflect"))[, c("x", "prob")],
    data.frame(x = c(1.2, 1.6, 2), prob = c(0.1, 0.3, 0.2) / 0.6)
  )

  # Collapsing into single element is possible
  expect_equal(
    meta_x_tbl(form_resupport(p_f, c(1.1, 1.3), "reflect"))[, c("x", "prob")],
    data.frame(x = 1.2, prob = 1)
  )

  # If there is an element on edge of supplied support then it is reflected into
  # itself
  expect_equal(
    meta_x_tbl(form_resupport(p_f, c(2, 5), "reflect"))[, c("x", "prob")],
    data.frame(x = 2:4, prob = c(0.2+0.2, 0.3+0.1, 0.4)/1.2)
  )
})

test_that("form_resupport works with `method='reflect'` and `type`='infin'",  {
  p_f <- new_p(data.frame(x = c(0, 1), y = c(1, 1)), "infin")
  p_f_x_tbl <- meta_x_tbl(p_f)

  # Returns self when supplied support equals `f`'s support or wider
  expect_equal(meta_x_tbl(form_resupport(p_f, c(0, 1), "reflect")), p_f_x_tbl)
  expect_equal(meta_x_tbl(form_resupport(p_f, c(-1, 2), "reflect")), p_f_x_tbl)

  # Left reflection
  expect_equal(
    meta_x_tbl(form_resupport(p_f, c(0.2, 1), "reflect"))[, c("x", "y")],
    data.frame(x = c(0.2, 0.4, 0.4+1e-8, 1), y = c(2, 2, 1, 1))
  )

  # Right reflection
  expect_equal(
    meta_x_tbl(form_resupport(p_f, c(0, 0.6), "reflect"))[, c("x", "y")],
    data.frame(x = c(0, 0.2-1e-8, 0.2, 0.6), y = c(1, 1, 2, 2))
  )

  # Reflection from both sides
  expect_equal(
    meta_x_tbl(form_resupport(p_f, c(0.1, 0.9), "reflect"))[, c("x", "y")],
    data.frame(
      x = c(0.1, 0.2, 0.2+1e-8, 0.8-1e-8, 0.8, 0.9),
      y = c(  2,   2,        1,        1,   2,   2)
    )
  )
})

test_that("form_resupport returns correct pdqr-function", {
  p_f_fin <- new_p(data.frame(x = 1:2, prob = c(0.3, 0.7)), "fin")
  p_f_infin <- new_p(data.frame(x = 1:3, y = c(0, 1, 0)), "infin")

  # Method "trim"
  expect_is(form_resupport(p_f_fin, c(1, 2), "trim"), "p")
  expect_is(form_resupport(as_d(p_f_fin), c(1, 2), "trim"), "d")
  expect_is(form_resupport(as_q(p_f_fin), c(1, 2), "trim"), "q")
  expect_is(form_resupport(as_r(p_f_fin), c(1, 2), "trim"), "r")

  expect_is(form_resupport(p_f_infin, c(1, 2), "trim"), "p")
  expect_is(form_resupport(as_d(p_f_infin), c(1, 2), "trim"), "d")
  expect_is(form_resupport(as_q(p_f_infin), c(1, 2), "trim"), "q")
  expect_is(form_resupport(as_r(p_f_infin), c(1, 2), "trim"), "r")

  # Method "linear"
  expect_is(form_resupport(p_f_fin, c(1, 2), "linear"), "p")
  expect_is(form_resupport(as_d(p_f_fin), c(1, 2), "linear"), "d")
  expect_is(form_resupport(as_q(p_f_fin), c(1, 2), "linear"), "q")
  expect_is(form_resupport(as_r(p_f_fin), c(1, 2), "linear"), "r")

  expect_is(form_resupport(p_f_infin, c(1, 2), "linear"), "p")
  expect_is(form_resupport(as_d(p_f_infin), c(1, 2), "linear"), "d")
  expect_is(form_resupport(as_q(p_f_infin), c(1, 2), "linear"), "q")
  expect_is(form_resupport(as_r(p_f_infin), c(1, 2), "linear"), "r")

  # Method "reflect"
  expect_is(form_resupport(p_f_fin, c(1, 2), "reflect"), "p")
  expect_is(form_resupport(as_d(p_f_fin), c(1, 2), "reflect"), "d")
  expect_is(form_resupport(as_q(p_f_fin), c(1, 2), "reflect"), "q")
  expect_is(form_resupport(as_r(p_f_fin), c(1, 2), "reflect"), "r")

  expect_is(form_resupport(p_f_infin, c(1, 2), "reflect"), "p")
  expect_is(form_resupport(as_d(p_f_infin), c(1, 2), "reflect"), "d")
  expect_is(form_resupport(as_q(p_f_infin), c(1, 2), "reflect"), "q")
  expect_is(form_resupport(as_r(p_f_infin), c(1, 2), "reflect"), "r")
})

test_that("form_resupport handles `NA`s in `support`", {
  p_f <- new_p(1:3, "fin")
  expect_close_f(
    f_1 = form_resupport(p_f, c(1.5, NA), "trim"),
    f_2 = form_resupport(p_f, c(1.5,  3), "trim"),
    grid = 0:4 + 0.5
  )
  expect_close_f(
    f_1 = form_resupport(p_f, c(NA, 1.5), "trim"),
    f_2 = form_resupport(p_f, c(1,  1.5), "trim"),
    grid = 0:4 + 0.5
  )
})

test_that("form_resupport returns self when appropriate", {
  output_1 <- form_resupport(p_fin)
  expect_close_f(p_fin, output_1, x_fin_vec)

  output_2 <- form_resupport(p_fin, support = NULL)
  expect_close_f(p_fin, output_2, x_fin_vec)

  output_3 <- form_resupport(p_fin, c(NA_real_, NA_real_))
  expect_close_f(p_fin, output_3, x_fin_vec)
})

test_that("form_resupport throws errors on bad input", {
  expect_error(form_resupport(1, c(0, 1), "trim"), "`f`.*function")
  expect_error(form_resupport(function(x) {x}, c(0, 1), "trim"), "`f`.*pdqr")
  expect_error(form_resupport(p_fin, "a", "trim"), "`support`.*numeric")
  expect_error(form_resupport(p_fin, c(2, 1), "trim"), "`support`")
  expect_error(form_resupport(p_fin, c(1, 3), 1), "`method`.*string")
  expect_error(form_resupport(p_fin, c(1, 3), "a"), "`method`.*one of")
  expect_error(
    form_resupport(p_fin, c(x_fin_support[2] + 1, NA), "trim"),
    "`NA`.*support.*(10, 9).*not proper"
  )
})


# resupport_trim ----------------------------------------------------------
# Tested in `form_resupport()`


# resupport_trim_fin ------------------------------------------------------
# Tested in `resupport_trim()`


# resupport_trim_infin ----------------------------------------------------
# Tested in `resupport_trim()`


# resupport_move ----------------------------------------------------------
# Tested in `form_resupport()`


# resupport_reflect -------------------------------------------------------
# Tested in `form_resupport()`


# resupport_winsor --------------------------------------------------------
# Tested in `form_resupport()`


# stop_resupport_zero_tot_prob --------------------------------------------
# Tested in `form_resupport()`
