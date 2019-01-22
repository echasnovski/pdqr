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
