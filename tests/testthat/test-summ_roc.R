context("test-summ_roc")

library(grDevices)

# All tested plot calls are wrapped in `grDevices::recordPlot` to avoid printing
# of the result of their calls (`NULL`). This is a result of {vdiffr} approach:
# it `print()`s its input (resulting here with `NULL`), unless wrapped in
# function.


# summ_roc ----------------------------------------------------------------
test_that("summ_roc works", {
  output <- summ_roc(d_dis, d_con)
  expect_named(output, c("threshold", "fpr", "tpr"))

  expect_decreasing <- function(x) {expect_false(is.unsorted(-x))}
  expect_decreasing(output[["threshold"]])

  # "fpr" column should be sorted non-decreasingly
  expect_decreasing(-output[["fpr"]])
  expect_equal(range(output[["fpr"]]), c(0, 1))

  expect_equal(range(output[["tpr"]]), c(0, 1))
})

test_that("summm_roc covers [0; 1] on both axis in case of 'discrete' input", {
  output <- summ_roc(d_dis, d_dis + 1)
  expect_equal(range(output[["fpr"]]), c(0, 1))
  expect_equal(range(output[["tpr"]]), c(0, 1))
})

test_that("summ_roc uses `n_grid` argument", {
  expect_equal(nrow(summ_roc(d_dis, d_con, n_grid = 3)), 3)
})

test_that("summ_roc works with different pdqr classes", {
  expect_equal(summ_roc(p_dis, d_con), summ_roc(r_dis, q_con))
})

test_that("summ_roc validates input", {
  expect_error(summ_roc("a", d_dis), "`f`.*not pdqr-function")
  expect_error(summ_roc(d_dis, "a"), "`g`.*not pdqr-function")
  expect_error(summ_roc(d_dis, d_con, n_grid = "a"), "`n_grid`.*number")
  expect_error(summ_roc(d_dis, d_con, n_grid = 10:11), "`n_grid`.*single")
  expect_error(summ_roc(d_dis, d_con, n_grid = 0.5), "`n_grid`.*more than 1")
})


# summ_rocauc -------------------------------------------------------------
test_that("summ_rocauc works", {
  cur_dis_1 <- new_d(1:2, "discrete")
  cur_dis_2 <- new_d(2:3, "discrete")
  expect_equal(summ_rocauc(cur_dis_1, cur_dis_2, method = "expected"), 0.875)
  expect_equal(summ_rocauc(cur_dis_1, cur_dis_2, method = "pessimistic"), 0.75)
  expect_equal(summ_rocauc(cur_dis_1, cur_dis_2, method = "optimistic"), 1)

  mixed_out <- summ_prob_true(d_con > d_dis)
  expect_equal(summ_rocauc(d_dis, d_con, method = "expected"), mixed_out)
  expect_equal(summ_rocauc(d_dis, d_con, method = "pessimistic"), mixed_out)
  expect_equal(summ_rocauc(d_dis, d_con, method = "optimistic"), mixed_out)

  g <- q_con + 1
  con_out <- summ_prob_true(g > p_con)
  expect_equal(summ_rocauc(p_con, g, method = "expected"), con_out)
  expect_equal(summ_rocauc(p_con, g, method = "pessimistic"), con_out)
  expect_equal(summ_rocauc(p_con, g, method = "optimistic"), con_out)
})

test_that("summ_rocauc validates input", {
  expect_error(summ_rocauc("a", d_dis), "`f`.*not pdqr-function")
  expect_error(summ_rocauc(d_dis, "a"), "`g`.*not pdqr-function")
  expect_error(summ_rocauc(d_dis, d_con, method = 1), "`method`.*string")
  expect_error(summ_rocauc(d_dis, d_con, method = "a"), "`method`.*one of")
})


# roc_plot ----------------------------------------------------------------
test_that("roc_plot works", {
  roc_1 <- summ_roc(new_d(1:4, "continuous"), new_d(3:7, "continuous"))
  roc_2 <- summ_roc(new_d(3:7, "continuous"), new_d(2:5, "continuous"))

  # Basic usage of `roc_plot()` and `roc_lines()`
  vdiffr::expect_doppelganger(
    "roc-basic-1", recordPlot({
      roc_plot(roc_1)
      roc_lines(roc_2, col = "blue")
    })
  )

  vdiffr::expect_doppelganger(
    "roc-basic-2", recordPlot({
      cur_dis_1 <- new_d(1:2, "discrete")
      cur_dis_2 <- new_d(2:3, "discrete")
      roc_plot(summ_roc(cur_dis_1, cur_dis_2))
      roc_lines(summ_roc(cur_dis_2, cur_dis_1), col = "blue")
    })
  )

  # Usage of `...` argument
  vdiffr::expect_doppelganger(
    "roc-ellipsis", recordPlot({
      roc_plot(
        roc_1, xlab = "Other text", main = "Other title",
        type = "b"
      )
      roc_lines(roc_2, col = "red", type = "p")
    })
  )

  # Usage of `add_bisector` argument
  vdiffr::expect_doppelganger(
    "roc-bisector", recordPlot({
      roc_plot(roc_1, add_bisector = FALSE)
    })
  )

  # Ordering of ROC curve points
  vdiffr::expect_doppelganger(
    "roc-ordering", recordPlot({
      roc_1_reordered <- roc_1[sample(seq_len(nrow(roc_1))), ]
      roc_2_reordered <- roc_2[sample(seq_len(nrow(roc_1))), ]

      roc_plot(roc_1_reordered, main = "Reordered ROC data frames")
      roc_lines(roc_2_reordered, col = "blue")
    })
  )
})

test_that("roc_plot validates input", {
  expect_error(roc_plot(), "`roc`.*missing.*data frame for ROC curve")
  expect_error(roc_plot(list(fpr = 1, tpr = 1)), "`roc`.*ROC")
  expect_error(
    roc_plot(data.frame(threshold = 1, fpr = 1, tpr = 1), add_bisector = "a"),
    "`add_bisector`.*`TRUE` or `FALSE`"
  )
})


# roc_lines ---------------------------------------------------------------
# Main functionality is tested in `roc_plot()`
test_that("roc_lines validates input", {
  expect_error(roc_lines(), "`roc`.*missing.*data frame for ROC curve")
  expect_error(roc_lines(list(fpr = 1, tpr = 1)), "`roc`.*ROC")
})


# is_roc ------------------------------------------------------------------
test_that("is_roc works", {
  expect_true(is_roc(summ_roc(d_dis, d_con)))

  expect_false(is_roc(list(fpr = 1, tpr = 1)))
  expect_false(is_roc(data.frame(tpr = 1)))
  expect_false(is_roc(data.frame(fpr = "a", tpr = 1)))
})


# assert_roc --------------------------------------------------------------
# Tested in `roc_plot()` and `roc_lines()`
