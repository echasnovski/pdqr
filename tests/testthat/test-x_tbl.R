context("test-x_tbl")


# compute_x_tbl -----------------------------------------------------------
# Tested in `new_*()` functions


# compute_x_tbl_dis -------------------------------------------------------
# Tested in `new_*()` functions


# compute_x_tbl_con -------------------------------------------------------
# Tested in `new_*()` functions


# dirac_x_tbl -------------------------------------------------------------
# Main functionality is tested in `new_*()` functions
test_that("dirac_x_tbl ensures that total integral is 1", {
  d_dirac <- new_d(1e8, "continuous")
  x_tbl <- meta_x_tbl(d_dirac)
  expect_equal(trapez_part_integral(x_tbl[["x"]], x_tbl[["y"]]), c(0, 0.5, 1))
  # This was the indicator of problem: error was returned because total integral
  # wasn't equal to 1.
  expect_silent(assert_pdqr_fun(d_dirac))
})


# impute_x_tbl ------------------------------------------------------------
# Tested in `new_*()` functions


# impute_x_tbl_impl -------------------------------------------------------
# Main tests are in `new_*()` functions
test_that("impute_x_tbl_impl throws error", {
  expect_error(impute_x_tbl_impl(x_dis_x_tbl, "a"), "type")
})


# impute_x_tbl_impl_dis ---------------------------------------------------
# Main functionality is tested in `impute_x_tbl_impl()`
test_that("impute_x_tbl_impl_dis correctly collapses duplicate 'x'",  {
  expect_equal(
    impute_x_tbl_impl_dis(data.frame(x = c(1, 2, 1), prob = c(0.3, 0.2, 0.5))),
    data.frame(x = c(1, 2), prob = c(0.8, 0.2), cumprob = c(0.8, 1))
  )
})


# impute_x_tbl_impl_con ---------------------------------------------------
# Tested in `impute_x_tbl_impl()`


# impute_prob -------------------------------------------------------------
# Tested in `impute_x_tbl_impl()`


# impute_y ----------------------------------------------------------------
# Tested in `impute_x_tbl_impl()`


# impute_vec --------------------------------------------------------------
# Tested in `impute_x_tbl_impl()`


# get_x_tbl_sec_col -------------------------------------------------------
test_that("get_x_tbl_sec_col works", {
  expect_equal(get_x_tbl_sec_col(x_dis_x_tbl), "prob")
  expect_equal(get_x_tbl_sec_col(x_con_x_tbl), "y")
})


# get_type_from_x_tbl -----------------------------------------------------
test_that("get_type_from_x_tbl works", {
  expect_equal(get_type_from_x_tbl(x_dis_x_tbl), "discrete")
  expect_equal(get_type_from_x_tbl(x_con_x_tbl), "continuous")
})


# filter_x_tbl ------------------------------------------------------------
test_that("filter_x_tbl works", {
  x_tbl_dis <- data.frame(x = 1:5, prob = (1:5) / 15)
  expect_equal(
    filter_x_tbl(x_tbl_dis, c(-10, 1))[, c("x", "prob")],
    x_tbl_dis[1, ]
  )

  x_tbl_con <- data.frame(x = 1:5, y = (1:5) / 12)
  expect_equal(
    filter_x_tbl(x_tbl_con, c(2.5, 5))[, c("x", "y")],
    x_tbl_con[3:5, ]
  )
})


# union_inside_x_tbl ------------------------------------------------------
test_that("union_inside_x_tbl works", {
  x_tbl_dis_1 <- data.frame(x = 1:3, prob = c(0, 0.3, 0.7))
  x_tbl_dis_2 <- data.frame(x = c(0, 1, 1.5, 3.1), prob = rep(0.5, 4))

  expect_equal(
    union_inside_x_tbl(x_tbl_dis_1, x_tbl_dis_2),
    data.frame(x = c(1, 1.5, 2, 3), prob = c(0, 0.5, 0.3, 0.7))
  )

  x_tbl_con_1 <- data.frame(x = 1:3, y = c(0, 1, 0))
  x_tbl_con_2 <- data.frame(x = c(0, 1, 1.5, 3.1), y = rep(0.5, 4))

  expect_equal(
    union_inside_x_tbl(x_tbl_con_1, x_tbl_con_2),
    data.frame(x = c(1, 1.5, 2, 3), y = c(0, 0.5, 1, 0))
  )
})


# reflect_x_tbl -----------------------------------------------------------
test_that("reflect_x_tbl works with 'discrete' type", {
  x_tbl_dis <- data.frame(
    x = c(1, 2, 4), prob = c(0.1, 0, 0.9), cumprob = c(0.1, 0.1, 1)
  )
  expect_equal(
    reflect_x_tbl(x_tbl_dis, 0),
    data.frame(
      x = c(-4, -2, -1), prob = c(0.9, 0, 0.1), cumprob = c(0.9, 0.9, 1)
    )
  )
  expect_equal(
    reflect_x_tbl(x_tbl_dis, 2),
    data.frame(
      x = c(0, 2, 3), prob = c(0.9, 0, 0.1), cumprob = c(0.9, 0.9, 1)
    )
  )
})

test_that("reflect_x_tbl works with 'continuous' type", {
  x_tbl_con <- data.frame(
    x = c( -2, -1, 0, 0.5, 4),
    y = c(0.5,  0, 1,   0, 0),
    cumprob = c(0, 0.25, 0.75, 1, 1)
  )
  expect_equal(
    reflect_x_tbl(x_tbl_con, 0),
    data.frame(
      x = c(-4, -0.5, 0, 1,   2),
      y = c( 0,    0, 1, 0, 0.5),
      cumprob = c(0, 0, 0.25, 0.75, 1)
    )
  )
  expect_equal(
    reflect_x_tbl(x_tbl_con, 10),
    data.frame(
      x = c(16, 19.5, 20, 21,  22),
      y = c( 0,    0,  1,  0, 0.5),
      cumprob = c(0, 0, 0.25, 0.75, 1)
    )
  )
})


# ground_x_tbl ------------------------------------------------------------
test_that("ground_x_tbl works", {
  expect_equal(ground_x_tbl(x_dis_x_tbl), x_dis_x_tbl)

  x_tbl <- data.frame(
    x = c(-1, 0.25, 2), y = c(1/1.25, 0, 1/1.75), cumprob = c(0, 0.5, 1)
  )
  x <- x_tbl[["x"]]
  y <- x_tbl[["y"]]
  n <- nrow(x_tbl)

  out_left <- ground_x_tbl(x_tbl, "left")
  expect_equal(out_left[["x"]], c(x[1]-1e-8, x))
  expect_equal(out_left[["y"]], c(        0, y))

  out_right <- ground_x_tbl(x_tbl, "right")
  expect_equal(out_right[["x"]], c(x, x[n]+1e-8))
  expect_equal(out_right[["y"]], c(y,         0))

  out_both <- ground_x_tbl(x_tbl, "both")
  expect_equal(out_both[["x"]], c(x[1]-1e-8, x, x[n]+1e-8))
  expect_equal(out_both[["y"]], c(        0, y,         0))
})

test_that("ground_x_tbl doesn't add new zeros to 'y'",  {
  x_tbl <- data.frame(x = c(1, 2, 3), y = c(0, 1, 0), cumprob = c(0, 0.5, 1))

  expect_equal(ground_x_tbl(x_tbl, "left"), x_tbl)
  expect_equal(ground_x_tbl(x_tbl, "right"), x_tbl)
  expect_equal(ground_x_tbl(x_tbl, "both"), x_tbl)
})

test_that("ground_x_tbl works without column 'cumprob' present",  {
  output <- ground_x_tbl(data.frame(x = 0:1, y = c(1, 1)), "both")
  expect_named(output, c("x", "y"))
})


# add_x_tbl_knots ---------------------------------------------------------
test_that("add_x_tbl_knots works",  {
  x_tbl <- data.frame(x = 1:3, y = c(1, 2, 1))

  expect_equal(
    add_x_tbl_knots(x_tbl, c(1.5, 1, -1, 10), only_inside = TRUE),
    data.frame(x = c(1, 1.5, 2, 3), y = c(1, 1.5, 2, 1))
  )
  expect_equal(
    add_x_tbl_knots(x_tbl, c(1.5, 1, -1, 10), only_inside = FALSE),
    data.frame(x = c(-1, 1, 1.5, 2, 3, 10), y = c(0, 1, 1.5, 2, 1, 0))
  )

  # `only_inside` is `TRUE` by default
  expect_equal(add_x_tbl_knots(x_tbl, c(-100, 100)), x_tbl)

  # Present knots aren't get duplicated
  expect_equal(add_x_tbl_knots(x_tbl, x_tbl[["x"]]), x_tbl)
})


# enfun_x_tbl -------------------------------------------------------------
test_that("enfun_x_tbl works",  {
  out_f <- enfun_x_tbl(data.frame(x = c(1, 2, 5), y = c(0, 10, 2)))
  expect_equal(
    out_f(c(0, 1, 1.5, 2.75, 5, 1000)), c(0, 0, 5, 8, 2, 0)
  )
})


# stack_x_tbl -------------------------------------------------------------
test_that("stack_x_tbl works with 'discrete' type",  {
  x_tbl_dis_1 <- data.frame(x = 1, prob = 1)
  x_tbl_dis_2 <- data.frame(x = 2:4, prob = c(0.2, 0.5, 0.3))
  x_tbl_dis_3 <- data.frame(x = c(-1, 1, 4, 5), prob = c(0.1, 0.2, 0.3, 0.4))

  expect_equal(
    stack_x_tbl(list(x_tbl_dis_1, x_tbl_dis_2, x_tbl_dis_3)),
    data.frame(x = c(-1, 1, 2, 3, 4, 5), prob = c(0.1, 1.2, 0.2, 0.5, 0.6, 0.4))
  )
  expect_equal(stack_x_tbl(list(x_tbl_dis_3)), x_tbl_dis_3)
})

test_that("stack_x_tbl works with 'continuous' type",  {
  x_tbl_con_1 <- data.frame(x = c(1, 3), y = c(0.5, 0.5))
  x_tbl_con_2 <- data.frame(x = c(2, 6), y = c(0.25, 0.25))
  x_tbl_con_3 <- data.frame(x = c(7, 8), y = c(1, 1))

  expect_equal(
    data.frame(
      x = c(  1, 2-1e-8,    2,    3, 3+1e-8,    6, 6+1e-8, 7-1e-8, 7, 8),
      y = c(0.5,    0.5, 0.75, 0.75,   0.25, 0.25,      0,      0, 1, 1)
    ),
    stack_x_tbl(list(x_tbl_con_1, x_tbl_con_2, x_tbl_con_3))
  )
})

test_that("stack_x_tbl handles zero density edges",  {
  x_tbl_1 <- data.frame(x = 1:3, y = c(0, 1, 0))
  x_tbl_2 <- data.frame(x = 2:4, y = c(0, 1, 0))
  expect_equal(
    stack_x_tbl(list(x_tbl_1, x_tbl_2)), data.frame(x = 1:4, y = c(0, 1, 1, 0))
  )
})


# stack_x_tbl_dis ---------------------------------------------------------
# Tested in `stack_x_tbl()`


# stack_x_tbl_con ---------------------------------------------------------
# Tested in `stack_x_tbl()`


# remove_extra_edges ------------------------------------------------------
# Tested in `stack_x_tbl()`


# is_x_extra --------------------------------------------------------------
# Tested in `stack_x_tbl()`
