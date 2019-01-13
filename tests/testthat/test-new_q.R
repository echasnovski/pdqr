context("test-new_q")


# new_q -------------------------------------------------------------------
test_that("new_q works with numeric input", {
  expect_distr_fun(q_fin, "q", "fin")
  expect_equal(meta(q_fin, "support"), x_fin_support)
  expect_equal(
    q_fin(cumsum(x_fin_x_tbl[["prob"]])), x_fin_x_tbl[["x"]]
  )

  expect_distr_fun(q_infin, "q", "infin")
  expect_equal(
    round(meta(q_infin, "support"), 2), round(x_infin_support, 2)
  )
  expect_equal(
    round(q_infin(0:20/20), 3),
    c(
      -2.919, -1.816, -1.434, -0.922, -0.579, -0.399, -0.267, -0.154,
      -0.052,  0.047,  0.144,  0.244,  0.351,  0.467,  0.598,  0.745,
       0.909,  1.087,  1.285,  1.533,  2.519
    )
  )
})

test_that("new_q works with data frame input", {
  expect_equal_distr(new_q(x_fin_x_tbl, "fin"), q_fin, p_vec)
  expect_equal_distr(new_q(x_infin_x_tbl, "infin"), q_infin, p_vec)
})

test_that("new_q imputes data frame input", {
  expect_x_tbl_imputation(new_q)
})

test_that("new_q rounds input in case of `type` = 'fin'", {
  near_1 <- 0.1 + 10^c(-6, -9)
  expect_equal(q_fin(near_1), c(2, 1))
})

test_that("new_q behaves like inverse of ecdf() in case of `type` = 'fin'", {
  inv_ecdf <- quantile(x_fin, probs = p_vec, type = 1)
  names(inv_ecdf) <- NULL

  expect_equal(q_fin(p_vec), inv_ecdf)
})

test_that("new_q output is inverse of new_p output", {
  expect_equal(x_fin_vec, q_fin(p_fin(x_fin_vec)))
  # There is not test `p_vec == p_fin(q_fin(p_vec))` because it shouldn't be
  # true in "fin" case. This is tested in "behaves like inverse of ecdf()" test.
  expect_equal(x_infin_vec, q_infin(p_infin(x_infin_vec)))
  expect_equal(p_vec, p_infin(q_infin(p_vec)))
})

test_that("new_q output works with extreme values", {
  expect_equal(q_fin(c(0, 1)), x_fin_support)
  expect_equal(q_infin(c(0, 1)), x_infin_support)
})

test_that("new_q returns `NaN` for out of range probabilities", {
  expect_true(all(is.nan(q_fin(c(-1, 2)))))
  expect_true(all(is.nan(q_infin(c(-1, 2)))))
})

test_that("new_q returns the smallest `x` with not exceeding `p`", {
  # Here values 1 and 2 correspond to cumulative probability of 0 and
  # values 4, 5, and 6 - to 1. Quantile function should return the smallest from
  # those sets.
  cur_x_tbl_fin <- data.frame(x = 1:6, prob = c(0, 0, 0.5, 0.5, 0, 0))
  cur_q_fin <- new_q(cur_x_tbl_fin, "fin")
  expect_equal(cur_q_fin(c(0, 1)), c(1, 4))

  # Here values 1 and 2 correspond to cumulative probability of 0 and
  # values 5 and 6 - to 1. Quantile function should return the smallest from
  # pairs.
  cur_x_tbl_infin <- data.frame(x = 1:6, y = c(0, 0, 1, 1, 0, 0))
  cur_q_infin <- new_q(cur_x_tbl_infin, "infin")
  expect_equal(cur_q_infin(c(0, 1)), c(1, 5))
})

test_that("new_q asserts", {
  expect_warning(new_q(c(1, 0, NA)), "x.*NA.*removed")
  expect_warning(new_q(c(1, 0, NaN)), "x.*NaN.*removed")
  expect_warning(new_q(c(1, 0, Inf)), "x.*infinite.*removed")

  expect_error(new_q("a"), "x.*numeric.*data.*frame")
  expect_error(new_q(numeric(0)), "x.*empty")
  expect_error(new_q(x_fin, type = 1), "type.*string")
  expect_error(new_q(x_fin, type = "a"), "type.*fin.*infin")
  expect_error(new_q(1, type = "infin"), "at least 2")
})

test_that("new_q handles metadata", {
  expect_equal(
    meta(q_fin),
    list(support = x_fin_support, type = "fin", x_tbl = x_fin_x_tbl)
  )

  expect_named(meta(q_infin), c("support", "type", "x_tbl"))
  expect_equal(meta(q_infin, "x_tbl"), x_infin_x_tbl)
  expect_equal(
    round(meta(q_infin, "support"), 2), round(x_infin_support, 2)
  )
  expect_equal(meta(q_infin)["type"], list(type = "infin"))
})

test_that("new_q uses `...` as arguments for `density()`", {
  q_infin_cosine <- new_q(x_infin, type = "infin", kernel = "cosine")
  expect_equal(
    round(q_infin_cosine(0:20/20), 3),
    c(
      -2.919, -1.82, -1.427, -0.91, -0.589, -0.408, -0.273, -0.158,
      -0.053, 0.048,  0.148,  0.25,  0.357,  0.473,    0.6,  0.743,
       0.903, 1.083,  1.286,  1.54,  2.476
    )
  )
})


# new_q_fin ---------------------------------------------------------------
# Tested in `new_q()`


# new_q_infin -------------------------------------------------------------
# Tested in `new_q()`


# find_quant --------------------------------------------------------------
# Tested in `new_q()`


# print.q -----------------------------------------------------------------
test_that("print.q works", {
  expect_pdqr_print(new_q, "Quantile")
})
