context("test-summ-classification")

library(grDevices)

# All tested plot calls are wrapped in `grDevices::recordPlot` to avoid printing
# of the result of their calls (`NULL`). This is a result of {vdiffr} approach:
# it `print()`s its input (resulting here with `NULL`), unless wrapped in
# function.


# summ_roc ----------------------------------------------------------------
test_that("summ_roc works", {
  output <- summ_roc(d_fin, d_infin)
  expect_named(output, c("threshold", "fpr", "tpr"))

  expect_sorted <- function(x) {expect_false(is.unsorted(x))}
  expect_sorted(output[["threshold"]])
  expect_equal(range(output[["fpr"]]), c(0, 1))
  expect_equal(range(output[["tpr"]]), c(0, 1))
    # "fpr" and "tpr" columns should be sorted decreasingly
  expect_sorted(-output[["fpr"]])
  expect_sorted(-output[["tpr"]])
})

test_that("summm_roc covers [0; 1] range on both axis in case of 'fin' input", {
  output <- summ_roc(d_fin, d_fin + 1)
  expect_equal(range(output[["fpr"]]), c(0, 1))
  expect_equal(range(output[["tpr"]]), c(0, 1))
})

test_that("summ_roc uses `n_grid` argument", {
  expect_equal(nrow(summ_roc(d_fin, d_infin, n_grid = 3)), 3)
})

test_that("summ_roc works with different pdqr classes", {
  expect_equal(summ_roc(p_fin, d_infin), summ_roc(r_fin, q_infin))
})

test_that("summ_roc validates input", {
  expect_error(summ_roc("a", d_fin), "`f`.*not pdqr-function")
  expect_error(summ_roc(d_fin, "a"), "`g`.*not pdqr-function")
  expect_error(summ_roc(d_fin, d_infin, n_grid = "a"), "`n_grid`.*number")
  expect_error(summ_roc(d_fin, d_infin, n_grid = 10:11), "`n_grid`.*single")
  expect_error(summ_roc(d_fin, d_infin, n_grid = 0.5), "`n_grid`.*more than 1")
})


# summ_rocauc -------------------------------------------------------------
test_that("summ_rocauc works", {
  expect_equal(summ_rocauc(new_d(1:2, "fin"), new_d(2:3, "fin")), 0.75)
  expect_equal(summ_rocauc(d_fin, d_infin), summ_prob_true(d_infin > d_fin))

  g <- q_infin + 1
  expect_equal(summ_rocauc(p_infin, g), summ_prob_true(g > p_infin))
})

test_that("summ_rocauc validates input", {
  expect_error(summ_rocauc("a", d_fin), "`f`.*not pdqr-function")
  expect_error(summ_rocauc(d_fin, "a"), "`g`.*not pdqr-function")
})


# roc_plot ----------------------------------------------------------------
test_that("roc_plot works", {
  roc_1 <- summ_roc(new_d(1:4, "infin"), new_d(3:7, "infin"))
  roc_2 <- summ_roc(new_d(3:7, "infin"), new_d(2:5, "infin"))

  # Basic usage of `roc_plot()` and `roc_lines()`
  vdiffr::expect_doppelganger(
    "roc-basic", recordPlot({
      roc_plot(roc_1)
      roc_lines(roc_2, col = "blue")
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

  # Usage of `plot_bisector` argument
  vdiffr::expect_doppelganger(
    "roc-bisector", recordPlot({
      roc_plot(roc_1, plot_bisector = FALSE)
    })
  )
})

test_that("roc_plot validates input", {
  expect_error(roc_plot(list(fpr = 1, tpr = 1)), "`roc`.*ROC")
  expect_error(
    roc_plot(data.frame(fpr = 1, tpr = 1), plot_bisector = "a"),
    "`plot_bisector`.*`TRUE` or `FALSE`"
  )
})


# roc_lines ---------------------------------------------------------------
# Main functionality is tested in `roc_plot()`
test_that("roc_lines validates input", {
  expect_error(roc_lines(list(fpr = 1, tpr = 1)), "`roc`.*ROC")
})


# is_roc ------------------------------------------------------------------
test_that("is_roc works", {
  expect_true(is_roc(summ_roc(d_fin, d_infin)))

  expect_false(is_roc(list(fpr = 1, tpr = 1)))
  expect_false(is_roc(data.frame(tpr = 1)))
  expect_false(is_roc(data.frame(fpr = "a", tpr = 1)))
})


# assert_roc --------------------------------------------------------------
# Tested in `roc_plot()` and `roc_lines()`
