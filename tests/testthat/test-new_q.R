context("test-new_q")


# new_q -------------------------------------------------------------------
test_that("new_q works with numeric input", {
  expect_distr_fun(q_dis, "q", "discrete")
  expect_equal(meta_support(q_dis), x_dis_support)
  expect_equal(
    q_dis(cumsum(x_dis_x_tbl[["prob"]])), x_dis_x_tbl[["x"]]
  )

  expect_distr_fun(q_con, "q", "continuous")
  expect_equal(round(meta_support(q_con), 2), round(x_con_support, 2))
  expect_equal(
    round(q_con(0:20/20), 3),
    c(
      -2.919, -1.816, -1.434, -0.922, -0.579, -0.399, -0.267, -0.154,
      -0.052,  0.047,  0.144,  0.244,  0.351,  0.467,  0.598,  0.745,
       0.909,  1.087,  1.285,  1.533,  2.519
    )
  )
})

test_that("new_q returns dirac-like function with length-one numeric input",  {
  expect_ref_x_tbl(new_q(0.1, "discrete"), data.frame(x = 0.1, prob = 1))
  expect_ref_x_tbl(
    new_q(0.1, "continuous"),
    data.frame(x = 0.1 + 1e-8*c(-1, 0, 1), y = 1e8*c(0, 1, 0))
  )

  # With big center value there can be problems with total integral being 1
  expect_silent(assert_pdqr_fun(new_q(1e8, "continuous")))
})

test_that("new_q works with data frame input", {
  expect_equal_distr(new_q(x_dis_x_tbl, "discrete"), q_dis, p_vec)
  expect_equal_distr(new_q(x_con_x_tbl, "continuous"), q_con, p_vec)
})

test_that("new_q imputes data frame input", {
  expect_x_tbl_imputation(new_q)
})

test_that("new_q's output works with 'edge case' inputs", {
  expect_equal(q_dis(c(NA_real_, NaN, -Inf, Inf)), c(NA, NA, NaN, NaN))
  expect_equal(q_dis(numeric(0)), numeric(0))
  expect_equal(q_dis(c(-0.1, 1.1)), c(NaN, NaN))
  expect_equal(q_dis(c(0, 1)), x_dis_support)

  expect_equal(q_con(c(NA_real_, NaN, -Inf, Inf)), c(NA, NA, NaN, NaN))
  expect_equal(q_con(numeric(0)), numeric(0))
  expect_equal(q_con(c(-0.1, 1.1)), c(NaN, NaN))
  expect_equal(q_con(c(0, 1)), x_con_support)
})

test_that("new_q's output validates input", {
  expect_error(q_dis("a"), "`p`.*numeric")
  expect_error(q_con("a"), "`p`.*numeric")
})

test_that("new_q's output behaves like ecdf() inverse if `type='discrete'`", {
  inv_ecdf <- quantile(x_dis, probs = p_vec, type = 1)
  names(inv_ecdf) <- NULL

  expect_equal(q_dis(p_vec), inv_ecdf)
})

test_that("new_q's output is inverse of new_p's output", {
  expect_equal(x_dis_vec, q_dis(p_dis(x_dis_vec)))
  # There is not test `p_vec == p_dis(q_dis(p_vec))` because it shouldn't be
  # true in "discrete" case. This is tested in "behaves like inverse of ecdf()"
  # test.
  expect_equal(x_con_vec, q_con(p_con(x_con_vec)))
  expect_equal(p_vec, p_con(q_con(p_vec)))
})

test_that("new_q's output returns the smallest `x` with not exceeding `p`", {
  # Here values 1 and 2 correspond to cumulative probability of 0 and
  # values 4, 5, and 6 - to 1. Quantile function should return the smallest from
  # those sets.
  cur_x_tbl_dis <- data.frame(x = 1:6, prob = c(0, 0, 0.5, 0.5, 0, 0))
  cur_q_dis <- new_q(cur_x_tbl_dis, "discrete")
  expect_equal(cur_q_dis(c(0, 1)), c(1, 4))

  # Here values 1 and 2 correspond to cumulative probability of 0 and
  # values 5 and 6 - to 1. Quantile function should return the smallest from
  # pairs.
  cur_x_tbl_con <- data.frame(x = 1:6, y = c(0, 0, 1, 1, 0, 0))
  cur_q_con <- new_q(cur_x_tbl_con, "continuous")
  expect_equal(cur_q_con(c(0, 1)), c(1, 5))
})

test_that("new_q's dirac-like output works with close to 0.5 values", {
  q_dirac <- new_q(1000, "continuous")

  # In case of not careful implementation, these will be not accuracte values
  # due to numerical representation issues
  close_vals <- q_dirac(0:10/10)
  expect_equal(order(close_vals), 1:11)
  expect_equal(close_vals[c(1, 6, 11)], 1000 + c(-1e-8, 0, 1e-8))
})

test_that("new_q warns about bad `x` elements", {
  expect_warning(new_q(c(1, 0, NA), "continuous"), "x.*NA.*removed")
  expect_warning(new_q(c(1, 0, NaN), "continuous"), "x.*NaN.*removed")
  expect_warning(new_q(c(1, 0, Inf), "continuous"), "x.*infinite.*removed")
})

test_that("new_q validates input", {
  expect_error(new_q(type = "continuous"), "`x`.*missing.*numeric.*data frame")
  expect_error(new_q("a", "continuous"), "x.*numeric.*data.*frame")
  expect_error(new_q(numeric(0), "continuous"), "x.*empty")
  expect_error(new_q(x_dis), "`type`.*missing.*pdqr type")
  expect_error(new_q(x_dis, type = 1), "type.*string")
  expect_error(new_q(x_dis, type = "a"), "type.*discrete.*continuous")
})

test_that("new_q handles metadata", {
  expect_equal(
    meta_all(q_dis),
    list(
      class = "q", type = "discrete", support = x_dis_support,
      x_tbl = x_dis_x_tbl
    )
  )

  expect_named(meta_all(q_con), c("class", "type", "support", "x_tbl"))
  expect_equal(meta_x_tbl(q_con), x_con_x_tbl)
  expect_equal(round(meta_support(q_con), 2), round(x_con_support, 2))
  expect_equal(meta_all(q_con)["type"], list(type = "continuous"))
})

test_that("new_q uses `...` as arguments for `density()`", {
  q_con_cosine <- new_q(x_con, type = "continuous", kernel = "cosine")
  expect_equal(
    round(q_con_cosine(0:20/20), 3),
    c(
      -2.919, -1.82, -1.427, -0.91, -0.589, -0.408, -0.273, -0.158,
      -0.053, 0.048,  0.148,  0.25,  0.357,  0.473,    0.6,  0.743,
       0.903, 1.083,  1.286,  1.54,  2.476
    )
  )
})


# new_q_dis ---------------------------------------------------------------
# Tested in `new_q()`


# new_q_con ---------------------------------------------------------------
# Tested in `new_q()`


# find_quant --------------------------------------------------------------
# Tested in `new_q()`


# print.q -----------------------------------------------------------------
test_that("print.q works", {
  expect_pdqr_print(new_q, "Quantile")
})
