context("test-x_tbl")


# compute_x_tbl -----------------------------------------------------------
# Tested in `new_*()` functions


# compute_x_tbl_fin -------------------------------------------------------
# Tested in `new_*()` functions


# compute_x_tbl_infin -----------------------------------------------------
# Tested in `new_*()` functions


# impute_x_tbl ------------------------------------------------------------
# Tested in `new_*()` functions


# impute_x_tbl_impl -------------------------------------------------------
# Main tests are in `new_*()` functions
test_that("impute_x_tbl_impl throws error", {
  expect_error(impute_x_tbl_impl(x_fin_x_tbl, "a"), "type")
})


# impute_x_tbl_impl_fin ---------------------------------------------------
# Tested in `impute_x_tbl_impl()`


# impute_x_tbl_impl_infin -------------------------------------------------
# Tested in `impute_x_tbl_impl()`


# impute_prob -------------------------------------------------------------
# Tested in `impute_x_tbl_impl()`


# impute_y ----------------------------------------------------------------
# Tested in `impute_x_tbl_impl()`


# impute_vec --------------------------------------------------------------
# Tested in `impute_x_tbl_impl()`


# get_x_tbl_sec_col -------------------------------------------------------
test_that("get_x_tbl_sec_col works", {
  expect_equal(get_x_tbl_sec_col(x_fin_x_tbl), "prob")
  expect_equal(get_x_tbl_sec_col(x_infin_x_tbl), "y")
})


# get_type_from_x_tbl -----------------------------------------------------
test_that("get_type_from_x_tbl works", {
  expect_equal(get_type_from_x_tbl(x_fin_x_tbl), "fin")
  expect_equal(get_type_from_x_tbl(x_infin_x_tbl), "infin")
})


# filter_x_tbl ------------------------------------------------------------
test_that("filter_x_tbl works", {
  x_tbl_fin <- data.frame(x = 1:5, prob = (1:5) / 15)
  expect_equal(
    filter_x_tbl(x_tbl_fin, c(-10, 1))[, c("x", "prob")],
    x_tbl_fin[1, ]
  )

  x_tbl_infin <- data.frame(x = 1:5, y = (1:5) / 12)
  expect_equal(
    filter_x_tbl(x_tbl_infin, c(2.5, 5))[, c("x", "y")],
    x_tbl_infin[3:5, ]
  )
})


# union_inside_x_tbl ------------------------------------------------------
test_that("union_inside_x_tbl works", {
  x_tbl_fin_1 <- data.frame(x = 1:3, prob = c(0, 0.3, 0.7))
  x_tbl_fin_2 <- data.frame(x = c(0, 1, 1.5, 3.1), prob = rep(0.5, 4))

  expect_equal(
    union_inside_x_tbl(x_tbl_fin_1, x_tbl_fin_2),
    data.frame(x = c(1, 1.5, 2, 3), prob = c(0, 0.5, 0.3, 0.7))
  )

  x_tbl_infin_1 <- data.frame(x = 1:3, y = c(0, 1, 0))
  x_tbl_infin_2 <- data.frame(x = c(0, 1, 1.5, 3.1), y = rep(0.5, 4))

  expect_equal(
    union_inside_x_tbl(x_tbl_infin_1, x_tbl_infin_2),
    data.frame(x = c(1, 1.5, 2, 3), y = c(0, 0.5, 1, 0))
  )
})


# reflect_x_tbl -----------------------------------------------------------
test_that("reflect_x_tbl works when `type` 'fin'", {
  x_tbl_fin <- data.frame(
    x = c(1, 2, 4), prob = c(0.1, 0, 0.9), cumprob = c(0.1, 0.1, 1)
  )
  expect_equal(
    reflect_x_tbl(x_tbl_fin, 0),
    data.frame(
      x = c(-4, -2, -1), prob = c(0.9, 0, 0.1), cumprob = c(0.9, 0.9, 1)
    )
  )
  expect_equal(
    reflect_x_tbl(x_tbl_fin, 2),
    data.frame(
      x = c(0, 2, 3), prob = c(0.9, 0, 0.1), cumprob = c(0.9, 0.9, 1)
    )
  )
})

test_that("reflect_x_tbl works when `type` 'infin'", {
  x_tbl_infin <- data.frame(
    x = c( -2, -1, 0, 0.5, 4),
    y = c(0.5,  0, 1,   0, 0),
    cumprob = c(0, 0.25, 0.75, 1, 1)
  )
  expect_equal(
    reflect_x_tbl(x_tbl_infin, 0),
    data.frame(
      x = c(-4, -0.5, 0, 1,   2),
      y = c( 0,    0, 1, 0, 0.5),
      cumprob = c(0, 0, 0.25, 0.75, 1)
    )
  )
  expect_equal(
    reflect_x_tbl(x_tbl_infin, 10),
    data.frame(
      x = c(16, 19.5, 20, 21,  22),
      y = c( 0,    0,  1,  0, 0.5),
      cumprob = c(0, 0, 0.25, 0.75, 1)
    )
  )
})


# ground_x_tbl ------------------------------------------------------------
test_that("ground_x_tbl works", {
  expect_equal(ground_x_tbl(x_fin_x_tbl), x_fin_x_tbl)
  expect_error(ground_x_tbl(x_infin_x_tbl, "a"), "Corrupt.*dir")

  x_tbl <- data.frame(
    x = c(-1, 0.25, 2), y = c(1.6, 0, 0), cumprob = c(0, 1, 1)
  )
  n <- nrow(x_tbl)

  out_left <- ground_x_tbl(x_tbl, "left")
  expect_true(
    (x_tbl[["x"]][1] - out_left[["x"]][1] <= 1e-8) &&
      (x_tbl[["x"]][1] >= out_left[["x"]][1])
  )
  expect_equal(out_left[["y"]][1], 0)

  out_right <- ground_x_tbl(x_tbl, "right")
  expect_true(
    (out_right[["x"]][n+1] - x_tbl[["x"]][n] <= 1e-8) &&
      (out_right[["x"]][n+1] >= x_tbl[["x"]][n])
  )
  expect_equal(out_right[["y"]][n+1], 0)

  out_both <- ground_x_tbl(x_tbl, "both")
  expect_true(
    (x_tbl[["x"]][1] - out_both[["x"]][1] <= 1e-8) &&
      (x_tbl[["x"]][1] >= out_both[["x"]][1]) &&
      (out_both[["x"]][n+1] - x_tbl[["x"]][n] <= 1e-8) &&
      (out_both[["x"]][n+1] >= x_tbl[["x"]][n])
  )
  expect_equal(out_both[["y"]][c(1, n+1)], c(0, 0))
})
