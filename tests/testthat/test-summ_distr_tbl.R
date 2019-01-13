context("test-summ_distr_tbl")


# summ_distr_tbl ----------------------------------------------------------
test_that("summ_distr_tbl works", {
  expect_equal(summ_distr_tbl(p_fin), x_fin_x_tbl[, c("x", "prob")])
  expect_equal(summ_distr_tbl(q_fin), x_fin_x_tbl[, c("x", "prob")])

  output_infin_distr_tbl <- summ_distr_tbl(p_infin, n_discrete = 1000)
  # Output has 999 rows instead of 1000 because the first discrete element has
  # probability zero.
  expect_equal(nrow(output_infin_distr_tbl), 999)
  expect_true(sum(output_infin_distr_tbl[["prob"]]) == 1)
})

test_that("summ_distr_tbl throws errors", {
  expect_error(summ_distr_tbl("a"), "pdqr.*function")

  corrupt_d <- d_infin
  attr(corrupt_d, "meta")[["support"]] <- c(2, 1)
  expect_error(summ_distr_tbl(corrupt_d), "pdqr.*function")
})


# p_summ_distr_tbl --------------------------------------------------------
# Tested in `summ_distr_tbl()`
