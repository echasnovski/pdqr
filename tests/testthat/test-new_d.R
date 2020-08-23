context("test-new_d")


# new_d -------------------------------------------------------------------
test_that("new_d works with numeric input", {
  expect_distr_fun(d_dis, "d", "discrete")
  expect_equal(meta_support(d_dis), x_dis_support)
  expect_equal(d_dis(1:10), c(x_dis_x_tbl[["prob"]], 0))

  expect_distr_fun(d_con, "d", "continuous")
  expect_equal(round(meta_support(d_con), 2), round(x_con_support, 2))
  expect_equal(
    round(d_con(seq(from = -1, to = 1, by = 0.1)), 2),
    c(
      0.09, 0.1, 0.12, 0.16, 0.21, 0.27, 0.34,  0.4, 0.45, 0.49, 0.51,
      0.51, 0.5, 0.47, 0.43, 0.39, 0.36, 0.33, 0.31, 0.29, 0.28
    )
  )
})

test_that("new_d returns dirac-like function with length-one numeric input",  {
  expect_ref_x_tbl(new_d(0.1, "discrete"), data.frame(x = 0.1, prob = 1))
  expect_ref_x_tbl(
    new_d(0.1, "continuous"),
    data.frame(x = 0.1 + 1e-8 * c(-1, 0, 1), y = 1e8 * c(0, 1, 0))
  )

  # With big center value there can be problems with total integral being 1
  expect_silent(assert_pdqr_fun(new_d(1e8, "continuous")))
})

test_that("new_d works with data frame input", {
  expect_equal_distr(new_d(x_dis_x_tbl, "discrete"), d_dis, x_dis_vec_ext)
  expect_equal_distr(
    new_d(x_con_x_tbl, "continuous"), d_con, x_con_vec_ext
  )
})

test_that("new_d imputes data frame input", {
  expect_x_tbl_imputation(new_d)
})

test_that("new_d rounds input in case of `type = 'discrete'`", {
  # Some values in this sequence show numerical representation issues after
  # `round(x_seq, digits = 10)` (see `dput(x_seq - round(x_seq, digits = 10))`).
  # If input is not rounded during `x_tbl` creation/imputation then output
  # function will behave incorrectly.
  x_seq <- seq(5e-4, 1, by = 5e-4)
  x_df <- data.frame(x = x_seq, prob = rep(1, length(x_seq)) / length(x_seq))

  # Testing correctness of `compute_x_tbl()`
  cur_d_1 <- new_d(x_seq, "discrete")
  expect_equal(cur_d_1(0.0045), 1 / length(x_seq))

  # Testing correctness of `impute_x_tbl()`
  cur_d_2 <- new_d(x_df, "discrete")
  expect_equal(cur_d_2(0.0045), 1 / length(x_seq))
})

test_that("new_d's output rounds input in case of `type` = 'discrete'", {
  near_1 <- 1 + 10^c(-6, -11)
  expect_equal(d_dis(near_1), c(0, 0.1))
})

test_that("new_d's output works with 'edge case' inputs", {
  expect_equal(d_dis(c(NA_real_, NaN, -Inf, Inf)), c(NA, NA, 0, 0))
  expect_equal(d_dis(numeric(0)), numeric(0))
  # Exact edges of support shouldn't be treated as "out of support"
  expect_true(all(d_dis(meta_support(d_dis)) != 0))
  expect_equal(d_dis(meta_support(d_dis) + c(-1, 1)), c(0, 0))

  expect_equal(d_con(c(NA_real_, NaN, -Inf, Inf)), c(NA, NA, 0, 0))
  expect_equal(d_con(numeric(0)), numeric(0))
  # Exact edges of support shouldn't be treated as "out of support"
  expect_true(all(d_dis(meta_support(d_dis)) != 0))
  expect_equal(d_dis(meta_support(d_con) + c(-1, 1)), c(0, 0))
})

test_that("new_d's output works with extreme values", {
  extreme_vec <- c(-1, 1) * 10000
  expect_equal(d_dis(extreme_vec), c(0, 0))
  expect_equal(d_con(extreme_vec), c(0, 0))
})

test_that("new_d's output validates input", {
  expect_error(d_dis("a"), "`x`.*numeric")
  expect_error(d_con("a"), "`x`.*numeric")
})

test_that("new_d's output integrates to 1 in case `type = 'continuous'`", {
  integral <- stats::integrate(d_con, -3, 3)
  output_range <- integral[["value"]] + c(-1, 1) * integral[["abs.error"]]
  expect_true((output_range[1] <= 1) && (1 <= output_range[2]))
})

test_that("new_d warns about bad `x` elements", {
  expect_warning(new_d(c(1, 0, NA), "continuous"), "x.*NA.*removed")
  expect_warning(new_d(c(1, 0, NaN), "continuous"), "x.*NaN.*removed")
  expect_warning(new_d(c(1, 0, Inf), "continuous"), "x.*infinite.*removed")
})

test_that("new_d validates input", {
  expect_error(new_d(type = "continuous"), "`x`.*missing.*numeric.*data frame")
  expect_error(new_d("a", "continuous"), "x.*numeric.*data.*frame")
  expect_error(new_d(numeric(0), "continuous"), "x.*empty")
  expect_error(new_d(x_dis), "`type`.*missing.*pdqr type")
  expect_error(new_d(x_dis, type = 1), "type.*string")
  expect_error(new_d(x_dis, type = "a"), "type.*discrete.*continuous")
})

test_that("new_d handles metadata", {
  expect_equal(
    meta_all(d_dis),
    list(
      class = "d", type = "discrete", support = x_dis_support,
      x_tbl = x_dis_x_tbl
    )
  )

  expect_named(meta_all(d_con), c("class", "type", "support", "x_tbl"))
  expect_equal(meta_x_tbl(d_con), x_con_x_tbl)
  expect_equal(round(meta_support(d_con), 2), round(x_con_support, 2))
  expect_equal(meta_all(d_con)["type"], list(type = "continuous"))
})

test_that("new_d uses `...` as arguments for `density()`", {
  d_con_cosine <- new_d(x_con, type = "continuous", kernel = "cosine")
  expect_equal(
    round(d_con_cosine(seq(from = -1, to = 1, by = 0.1)), 2),
    c(
      0.09, 0.11, 0.13, 0.17, 0.22, 0.28, 0.34, 0.39, 0.44, 0.48, 0.5,
      0.5,  0.49, 0.47, 0.44, 0.4,  0.37, 0.34, 0.32, 0.3,  0.28
    )
  )
})


# new_d_dis ---------------------------------------------------------------
# Tested in `new_d()`


# new_d_con ---------------------------------------------------------------
# Tested in `new_d()`


# print.d -----------------------------------------------------------------
test_that("print.d works", {
  expect_pdqr_print(new_d, "Probability mass", "Density")
})
