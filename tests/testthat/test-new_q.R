context("test-new_q")


# new_q -------------------------------------------------------------------
test_that("new_q works", {
  expect_distr_fun(q_raw, "q", "raw")
  expect_equal(meta(q_raw, "support"), x_raw_support)
  expect_equal(
    q_raw(cumsum(x_raw_distr_tbl[["prob"]])), x_raw_distr_tbl[["x"]]
  )

  expect_distr_fun(q_smooth, "q", "smooth")
  expect_equal(
    round(meta(q_smooth, "support"), 2), round(x_smooth_support, 2)
  )
  expect_equal(
    round(q_smooth(0:20/20), 3),
    c(
      -2.919, -1.816, -1.434, -0.922, -0.579, -0.399, -0.267, -0.154,
      -0.052,  0.047,  0.144,  0.244,  0.351,  0.467,  0.598,  0.745,
       0.909,  1.087,  1.285,  1.533,  2.519
    )
  )
})

test_that("new_q rounds input in case of `type` = 'raw'", {
  near_1 <- 0.1 + 10^c(-6, -9)
  expect_equal(q_raw(near_1), c(2, 1))
})

test_that("new_q behaves like inverse of ecdf() in case of `type` = 'raw'", {
  inv_ecdf <- quantile(x_raw, probs = p_vec, type = 1)
  names(inv_ecdf) <- NULL

  expect_equal(q_raw(p_vec), inv_ecdf)
})

test_that("new_q output is inverse of new_p output", {
  expect_equal(x_raw_vec, q_raw(p_raw(x_raw_vec)))
  # There is not test `p_vec == p_raw(q_raw(p_vec))` because it shouldn't be
  # true in "raw" case. This is tested in "behaves like inverse of ecdf()" test.
  expect_equal(x_smooth_vec, q_smooth(p_smooth(x_smooth_vec)))
  expect_equal(p_vec, p_smooth(q_smooth(p_vec)))
})

test_that("new_q output works with extreme values", {
  expect_equal(q_raw(c(0, 1)), x_raw_support)
  expect_equal(q_smooth(c(0, 1)), x_smooth_support)
})

test_that("new_q returns `NaN` for out of range probabilities", {
  expect_true(all(is.nan(q_raw(c(-1, 2)))))
  expect_true(all(is.nan(q_smooth(c(-1, 2)))))
})

test_that("new_q asserts", {
  expect_warning(new_q(c(1, 0, NA)), "x.*NA.*removed")
  expect_warning(new_q(c(1, 0, NaN)), "x.*NaN.*removed")
  expect_warning(new_q(c(1, 0, Inf)), "x.*infinite.*removed")

  expect_error(new_q("a"), "x.*numeric")
  expect_error(new_q(numeric(0)), "x.*empty")
  expect_error(new_q(x_raw, type = 1), "type.*string")
  expect_error(new_q(x_raw, type = "a"), "type.*raw.*smooth")
  expect_error(new_q(x_raw, attach_x = NULL), "attach_x.*TRUE.*FALSE")
})

test_that("new_q handles metadata", {
  expect_equal(
    meta(q_raw),
    list(
      support = x_raw_support, type = "raw",
      x = x_raw
    )
  )

  q_smooth_1 <- new_q(x_smooth, type = "smooth", attach_x = TRUE)
  expect_named(meta(q_smooth_1), c("support", "type", "x"))
  expect_equal(
    round(meta(q_smooth_1, "support"), 2), round(x_smooth_support, 2)
  )
  expect_equal(
    meta(q_smooth_1)[c("x", "type")],
    list(x = x_smooth, type = "smooth")
  )

  q_smooth_2 <- new_q(x_smooth, type = "smooth", extra = list(a = TRUE))
  expect_named(meta(q_smooth_2), c("extra", "support", "type"))
  expect_equal(meta(q_smooth_2, "extra"), list(a = TRUE))
})

test_that("new_q has correct default for `attach_x`", {
  expect_true("x" %in% names(meta(q_raw)))
  expect_false("x" %in% names(meta(q_smooth)))
})

test_that("new_q uses `...` as arguments for `density()`", {
  q_smooth_cosine <- new_q(x_smooth, type = "smooth", kernel = "cosine")
  expect_equal(
    round(q_smooth_cosine(0:20/20), 3),
    c(
      -2.919, -1.82, -1.427, -0.91, -0.589, -0.408, -0.273, -0.158,
      -0.053, 0.048,  0.148,  0.25,  0.357,  0.473,    0.6,  0.743,
       0.903, 1.083,  1.286,  1.54,  2.519
    )
  )
})


# new_q_raw ---------------------------------------------------------------
# Tested in `new_q()`


# new_q_smooth ------------------------------------------------------------
# Tested in `new_q()`


# find_quant --------------------------------------------------------------
# Tested in `new_q()`


# print.q -----------------------------------------------------------------
test_that("print.q works", {
  expect_pdqr_print(new_q, "Quantile")
})
