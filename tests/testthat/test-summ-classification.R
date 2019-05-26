context("test-summ-classification")


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
