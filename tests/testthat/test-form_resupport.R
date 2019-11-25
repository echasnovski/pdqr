context("test-form_resupport")


# form_resupport ----------------------------------------------------------
test_that("form_resupport works with `method='reflect'` and 'discrete' type", {
  p_f <- new_p(data.frame(x = 1:4, prob = (1:4)/10), "discrete")
  p_f_x_tbl <- meta_x_tbl(p_f)

  # Returns self when supplied support equals `f`'s support or wider
  expect_ref_x_tbl(form_resupport(p_f, c(1, 4), "reflect"), p_f_x_tbl)
  expect_ref_x_tbl(form_resupport(p_f, c(0, 5), "reflect"), p_f_x_tbl)

  # Left reflection
  expect_ref_x_tbl(
    form_resupport(p_f, c(1.1, 5), "reflect"),
    data.frame(x = c(1.2, 2:4), prob = (1:4)/10)
  )

  # Right reflection
  expect_ref_x_tbl(
    form_resupport(p_f, c(0, 2.3), "reflect"),
    data.frame(x = c(0.6, 1, 1.6, 2), prob = c(0.4, 0.1, 0.3, 0.2))
  )

  # Reflection from both sides
  # There are 3 numbers instead of four because 4 got reflected to 0.6 which is
  # outside of supplied support
  expect_ref_x_tbl(
    form_resupport(p_f, c(1.1, 2.3), "reflect"),
    data.frame(x = c(1.2, 1.6, 2), prob = c(0.1, 0.3, 0.2) / 0.6)
  )

  # Collapsing into single element is possible
  expect_ref_x_tbl(
    form_resupport(p_f, c(1.1, 1.3), "reflect"),
    data.frame(x = 1.2, prob = 1)
  )

  # If there is an element on edge of supplied support then it is reflected into
  # itself
  expect_ref_x_tbl(
    form_resupport(p_f, c(2, 5), "reflect"),
    data.frame(x = 2:4, prob = c(0.2+0.2, 0.3+0.1, 0.4)/1.2)
  )
})

test_that("form_resupport works with `method='reflect'`, 'continuous' type",  {
  p_f <- new_p(data.frame(x = c(0, 1), y = c(1, 1)), "continuous")
  p_f_x_tbl <- meta_x_tbl(p_f)
  h <- 1e-8
  tol <- 1e-9

  # Returns self when supplied support equals `f`'s support or wider
  expect_ref_x_tbl(form_resupport(p_f, c(0, 1), "reflect"), p_f_x_tbl)
  expect_ref_x_tbl(form_resupport(p_f, c(-1, 2), "reflect"), p_f_x_tbl)

  # Left reflection
  expect_ref_x_tbl(
    form_resupport(p_f, c(0.2, 1), "reflect"),
    data.frame(x = c(0.2, 0.4-h, 0.4, 0.4+h, 1), y = c(2, 2, 1.5, 1, 1))
  )

  # Right reflection
  expect_ref_x_tbl(
    form_resupport(p_f, c(0, 0.6), "reflect"),
    data.frame(x = c(0, 0.2-h, 0.2, 0.2+h, 0.6), y = c(1, 1, 1.5, 2, 2))
  )

  # Reflection from both sides
  expect_ref_x_tbl(
    form_resupport(p_f, c(0.1, 0.9), "reflect"),
    data.frame(
      x = c(0.1, 0.2-h, 0.2, 0.2+h, 0.8-h, 0.8, 0.8+h, 0.9),
      y = c(  2,     2, 1.5,     1,     1, 1.5,     2,   2)
    )
  )
})

test_that("form_resupport works with `method = 'trim'` and 'discrete' type", {
  x_tbl <- data.frame(
    x    = c(0,   1, 2,   3,   4,   5,   6, 7),
    prob = c(0, 0.2, 0, 0.4, 0.1, 0.1, 0.2, 0)
  )
  p_f <- new_p(x_tbl, "discrete")

  expect_ref_x_tbl(
    form_resupport(p_f, c(1.5, 4.5), "trim"),
    data.frame(x = 2:4, prob = c(0, 0.8, 0.2))
  )

  expect_ref_x_tbl(
    form_resupport(p_f, c(-1, 1), "trim"),
    data.frame(x = 0:1, prob = c(0, 1))
  )

  expect_ref_x_tbl(form_resupport(p_f, c(-100, 100), "trim"), x_tbl)

  expect_error(form_resupport(p_f, c(0.5, 0.75), "trim"), "not.*positive.*prob")
  expect_error(form_resupport(p_f, c(-1, 0.5), "trim"), "not.*positive.*prob")
  expect_error(form_resupport(p_f, c(6.5, 7), "trim"), "not.*positive.*prob")
})

test_that("form_resupport works with `method = 'trim'` and 'continuous' type", {
  x_tbl <- data.frame(
    x = c(0, 1,   2, 3, 4, 5,   6, 7,   8),
    y = c(0, 0, 0.4, 0, 0, 0, 0.4, 0, 0.4)
  )
  d_f <- new_d(x_tbl, "continuous")

  expect_ref_x_tbl(
    form_resupport(d_f, c(0.5, 3.5), "trim"),
    data.frame(x = c(0.5, 1, 2, 3, 3.5), y = c(0, 0, 1, 0, 0))
  )

  expect_ref_x_tbl(
    form_resupport(d_f, c(-1, 1.5), "trim"),
    data.frame(x = c(0, 1, 1.5), y = c(0, 0, 4))
  )

  expect_ref_x_tbl(
    form_resupport(d_f, c(6.5, 7.5), "trim"),
    data.frame(x = c(6.5, 7, 7.5), y = c(2, 0, 2))
  )

  expect_ref_x_tbl(form_resupport(d_f, c(-100, 100), "trim"), x_tbl)

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

test_that("form_resupport works with `method='winsor'` and 'discrete' type",  {
  d_f <- new_d(data.frame(x = 1:4, prob = (1:4) / 10), "discrete")

  # Collapsing into single element
  expect_equal_x_tbl(
    form_resupport(d_f, c(5, 6), "winsor"),
    new_d(5, "discrete")
  )
  expect_equal_x_tbl(
    form_resupport(d_f, c(0, 0.5), "winsor"),
    new_d(0.5, "discrete")
  )
  expect_equal_x_tbl(
    form_resupport(d_f, c(3.14, 3.14), "winsor"),
    new_d(3.14, "discrete")
  )

  # Left
  expect_ref_x_tbl(
    form_resupport(d_f, c(2.5, 6), "winsor"),
    data.frame(x = c(2.5, 3, 4), prob = c(0.3, 0.3, 0.4))
  )

  # Right
  expect_ref_x_tbl(
    form_resupport(d_f, c(0, 2.5), "winsor"),
    data.frame(x = c(1, 2, 2.5), prob = c(0.1, 0.2, 0.7))
  )

  # Both
  expect_ref_x_tbl(
    form_resupport(d_f, c(2.5, 2.75), "winsor"),
    data.frame(x = c(2.5, 2.75), prob = c(0.3, 0.7))
  )
})

test_that("form_resupport works with `method='winsor'`, 'continuous' type",  {
  d_f <- new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous")

  # Collapsing into single element
  expect_equal_x_tbl(
    form_resupport(d_f, c(2, 3), "winsor"),
    new_d(2, "continuous")
  )
  expect_equal_x_tbl(
    form_resupport(d_f, c(-1, 0), "winsor"),
    new_d(0, "continuous")
  )
  expect_equal_x_tbl(
    form_resupport(d_f, c(0.7, 0.7), "winsor"),
    new_d(0.7, "continuous")
  )

  # Left
  expect_ref_x_tbl(
    form_resupport(d_f, c(0.3, 1), "winsor"),
    # Here `+1.032` in `y[1]` seems to be because of numerical representation
    # accuracy.
    data.frame(x = c(0.3, 0.3+1e-8, 1), y = c(6e7+1.032, 1, 1))
  )

  # Right
  expect_ref_x_tbl(
    form_resupport(d_f, c(0, 0.7), "winsor"),
    # Here `+0.696` in `y[3]` seems to be because of numerical representation
    # accuracy.
    data.frame(x = c(0, 0.7-1e-8, 0.7), y = c(1, 1, 6e7+0.696))
  )

  # Both
  expect_ref_x_tbl(
    form_resupport(d_f, c(0.3, 0.7), "winsor"),
    # Adding of "small" values to edges explained in tests for left and right
    # winsoring
    data.frame(
      x = c(0.3, 0.3+1e-8, 0.7-1e-8, 0.7),
      y = c(6e7+1.03, 1, 1, 6e7+0.696)
    )
  )
})

test_that("form_resupport works with `method = 'linear'` and 'discrete' type", {
  p_f <- new_p(
    data.frame(x = c(-1, -0.25, 2), prob = c(0, 0.1, 0.9)), "discrete"
  )

  expect_ref_x_tbl(
    form_resupport(p_f, c(-0.5, 3), "linear"),
    data.frame(x = c(-0.5, 0.375, 3), prob = meta_x_tbl(p_f)[["prob"]])
  )

  expect_equal_x_tbl(
    form_resupport(p_f, c(15, 15), "linear"),
    new_p(15, "discrete")
  )

  # Can't resupport from single point support to interval one
  expect_error(
    form_resupport(new_p(1, "discrete"), c(0, 1), "linear"), "single.*interval"
  )
})

test_that("form_resupport works with `method='linear'` and 'continuous' type", {
  x_tbl <- data.frame(
    x = c(0, 1,   2, 5),
    y = c(0, 0, 0.5, 0)
  )
  p_f <- new_p(x_tbl, "continuous")

  expect_ref_x_tbl(
    form_resupport(p_f, c(1, 3.5), "linear"),
    data.frame(x = c(1, 1.5, 2, 3.5), y = c(0, 0, 1, 0))
  )

  expect_equal_x_tbl(
    form_resupport(p_f, c(15, 15), "linear"),
    new_p(15, "continuous")
  )
})

test_that("form_resupport returns correct class of pdqr-function", {
  p_f_dis <- new_p(data.frame(x = 1:2, prob = c(0.3, 0.7)), "discrete")
  p_f_con <- new_p(data.frame(x = 1:3, y = c(0, 1, 0)), "continuous")

  # Method "trim"
  expect_is(form_resupport(p_f_dis, c(1, 2), "trim"), "p")
  expect_is(form_resupport(as_d(p_f_dis), c(1, 2), "trim"), "d")
  expect_is(form_resupport(as_q(p_f_dis), c(1, 2), "trim"), "q")
  expect_is(form_resupport(as_r(p_f_dis), c(1, 2), "trim"), "r")

  expect_is(form_resupport(p_f_con, c(1, 2), "trim"), "p")
  expect_is(form_resupport(as_d(p_f_con), c(1, 2), "trim"), "d")
  expect_is(form_resupport(as_q(p_f_con), c(1, 2), "trim"), "q")
  expect_is(form_resupport(as_r(p_f_con), c(1, 2), "trim"), "r")

  # Method "linear"
  expect_is(form_resupport(p_f_dis, c(1, 2), "linear"), "p")
  expect_is(form_resupport(as_d(p_f_dis), c(1, 2), "linear"), "d")
  expect_is(form_resupport(as_q(p_f_dis), c(1, 2), "linear"), "q")
  expect_is(form_resupport(as_r(p_f_dis), c(1, 2), "linear"), "r")

  expect_is(form_resupport(p_f_con, c(1, 2), "linear"), "p")
  expect_is(form_resupport(as_d(p_f_con), c(1, 2), "linear"), "d")
  expect_is(form_resupport(as_q(p_f_con), c(1, 2), "linear"), "q")
  expect_is(form_resupport(as_r(p_f_con), c(1, 2), "linear"), "r")

  # Method "reflect"
  expect_is(form_resupport(p_f_dis, c(1, 2), "reflect"), "p")
  expect_is(form_resupport(as_d(p_f_dis), c(1, 2), "reflect"), "d")
  expect_is(form_resupport(as_q(p_f_dis), c(1, 2), "reflect"), "q")
  expect_is(form_resupport(as_r(p_f_dis), c(1, 2), "reflect"), "r")

  expect_is(form_resupport(p_f_con, c(1, 2), "reflect"), "p")
  expect_is(form_resupport(as_d(p_f_con), c(1, 2), "reflect"), "d")
  expect_is(form_resupport(as_q(p_f_con), c(1, 2), "reflect"), "q")
  expect_is(form_resupport(as_r(p_f_con), c(1, 2), "reflect"), "r")

  # Method "winsor"
  expect_is(form_resupport(p_f_dis, c(1, 2), "winsor"), "p")
  expect_is(form_resupport(as_d(p_f_dis), c(1, 2), "winsor"), "d")
  expect_is(form_resupport(as_q(p_f_dis), c(1, 2), "winsor"), "q")
  expect_is(form_resupport(as_r(p_f_dis), c(1, 2), "winsor"), "r")

  expect_is(form_resupport(p_f_con, c(1, 2), "winsor"), "p")
  expect_is(form_resupport(as_d(p_f_con), c(1, 2), "winsor"), "d")
  expect_is(form_resupport(as_q(p_f_con), c(1, 2), "winsor"), "q")
  expect_is(form_resupport(as_r(p_f_con), c(1, 2), "winsor"), "r")
})

test_that("form_resupport handles `NA`s in `support`", {
  p_f <- new_p(1:3, "discrete")
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
  output_1 <- form_resupport(p_dis, c(NA_real_, NA_real_))
  expect_close_f(p_dis, output_1, x_dis_vec)

  for (method in c("trim", "linear", "reflect", "winsor")) {
    output_dis <- form_resupport(p_dis, meta_support(p_dis), method)
    expect_close_f(p_dis, output_dis, x_dis_vec)

    output_con <- form_resupport(p_con, meta_support(p_con), method)
    expect_close_f(p_con, output_con, x_con_vec)
  }
})

test_that("form_resupport validates input", {
  expect_error(form_resupport(1, c(0, 1), "trim"), "`f`.*not pdqr-function")
  expect_error(form_resupport(p_dis), "`support`.*missing.*vector for support")
  expect_error(form_resupport(p_dis, "a", "trim"), "`support`.*numeric")
  expect_error(form_resupport(p_dis, c(2, 1), "trim"), "`support`")
  expect_error(form_resupport(p_dis, c(1, 3), 1), "`method`.*string")
  expect_error(form_resupport(p_dis, c(1, 3), "a"), "`method`.*one of")
  expect_error(
    form_resupport(p_dis, c(x_dis_support[2] + 1, NA), "trim"),
    "`NA`.*support.*(10, 9).*not proper"
  )
})


# resupport_reflect -------------------------------------------------------
# Tested in `form_resupport()`


# resupport_trim ----------------------------------------------------------
# Tested in `form_resupport()`


# resupport_trim_dis ------------------------------------------------------
# Tested in `resupport_trim()`


# resupport_trim_con ------------------------------------------------------
# Tested in `resupport_trim()`


# resupport_winsor --------------------------------------------------------
# Tested in `form_resupport()`


# resupport_winsor_dis ----------------------------------------------------
# Tested in `form_resupport()`


# resupport_winsor_con ----------------------------------------------------
# Tested in `form_resupport()`


# increase_tail_weight ----------------------------------------------------
# Tested in `form_resupport()`


# resupport_linear --------------------------------------------------------
# Tested in `form_resupport()`


# stop_resupport_zero_tot_prob --------------------------------------------
# Tested in `form_resupport()`
