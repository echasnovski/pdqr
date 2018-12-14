context("test-summ_distr_tbl")


# distr_tbl ---------------------------------------------------------------
test_that("distr_tbl works with 'x' in metadata", {
  expect_equal(distr_tbl(p_raw_withx), x_raw_distr_tbl)
})

test_that("distr_tbl removes `NA` from 'x' metadata", {
  input <- structure(1, meta = list(x = c(1, 1, 2, 2, NA)))
  output_ref <- data.frame(x = c(1, 2), prob = c(0.5, 0.5))
  expect_equal(distr_tbl(input), output_ref)
})

test_that("distr_tbl ignores corrupt 'x' metadata", {
  corrupt_p <- p_raw_nox
  attr(corrupt_p, "meta")[["x"]] <- "a"
  expect_equal(distr_tbl(corrupt_p), x_raw_distr_tbl)
})

test_that("distr_tbl works with 'pdqr' function", {
  expect_equal(distr_tbl(p_raw_nox), x_raw_distr_tbl)
  expect_equal(distr_tbl(q_raw_nox), x_raw_distr_tbl)

  output_smooth_distr_tbl <- distr_tbl(p_smooth_nox, n_discrete = 1000)
  # Output has 999 rows instead of 1000 because the first discrete element has
  # probability zero.
  expect_equal(nrow(output_smooth_distr_tbl), 999)
  expect_true(sum(output_smooth_distr_tbl[["prob"]]) == 1)
})

test_that("distr_tbl throws errors", {
  expect_error(distr_tbl("a"), "x.*metadata.*pdqr.*function")

  corrupt_d <- d_smooth_nox
  attr(corrupt_d, "meta")[["support"]] <- c(2, 1)
  expect_error(distr_tbl(corrupt_d), "x.*metadata.*pdqr.*function")
})


# vec_distr_tbl -----------------------------------------------------------
# Main functionality is tested in `distr_tbl()`
test_that("vec_distr_tbl removes `NA`s", {
  input <- c(1, 1, 2, 2, NA)
  output_ref <- data.frame(x = c(1, 2), prob = c(0.5, 0.5))
  expect_equal(vec_distr_tbl(input), output_ref)
})


# p_fun_distr_tbl ---------------------------------------------------------
# Tested in `distr_tbl()`
