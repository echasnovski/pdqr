context("test-distr-tbl")


# distr_tbl ---------------------------------------------------------------
test_that("distr_tbl works with numeric input", {
  input <- c(10, 8, rep(2, 5), 4, 6, 5)
  output_ref <- data.frame(x = c(2, 4, 5, 6, 8, 10), prob = c(0.5, rep(0.1, 5)))
  expect_equal(distr_tbl(input), output_ref)
})

test_that("distr_tbl works with 'x' in metadata", {
  expect_equal(distr_tbl(p_raw_withx), x_raw_distr_tbl)
})

test_that("distr_tbl works with 'pdqr' function", {
  expect_equal(distr_tbl(p_raw_nox), x_raw_distr_tbl)
  expect_equal(distr_tbl(q_raw_nox), x_raw_distr_tbl)

  # Output has 999 rows instead of 1000 because the first discrete element has
  # probability zero.
  expect_equal(nrow(distr_tbl(p_smooth_nox, n_discrete = 1000)), 999)
})

test_that("distr_tbl throws errors", {
  expect_error(distr_tbl("a"), "x.*metadata.*numeric.*pdqr.*function")

  corrupt_r <- r_smooth_nox
  attr(corrupt_r, "meta")[["x"]] <- "a"
  expect_error(distr_tbl(corrupt_r), "x.*metadata.*numeric")

  corrupt_d <- d_smooth_nox
  attr(corrupt_d, "meta")[["support"]] <- c(2, 1)
  expect_error(distr_tbl(corrupt_d), "proper.*support")
})


# vec_distr_tbl -----------------------------------------------------------
# Tested in `distr_tbl()`


# p_fun_distr_tbl ---------------------------------------------------------
# Tested in `distr_tbl()`
