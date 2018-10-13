context("test-q_fun")

# Input data --------------------------------------------------------------
q_raw <- q_fun(x_raw, type = "raw")
q_smooth <- q_fun(x_smooth, type = "smooth")

x_smooth_vec <- sample(
  seq(x_smooth_domain_in[1], x_smooth_domain_in[2], length.out = 1000)
)
x_raw_vec <- sample(unique(x_raw))
p_vec <- sample(0:1000 / 1000)


# q_fun -------------------------------------------------------------------
test_that("q_fun works", {
  expect_distr_fun(q_raw, "q_fun", "raw")
  expect_equal(meta(q_raw, "domain_out"), x_raw_domain_in)
  expect_equal(
    q_raw(cumsum(x_raw_distr_tbl[["prob"]])), x_raw_distr_tbl[["x"]]
  )

  expect_distr_fun(q_smooth, "q_fun", "smooth")
  expect_equal(
    round(meta(q_smooth, "domain_out"), 2), round(x_smooth_domain_in, 2)
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

test_that("q_fun rounds input in case of `type` = 'raw'", {
  near_1 <- 0.1 + 10^c(-6, -9)
  expect_equal(q_raw(near_1), c(2, 1))
})

test_that("q_fun behaves like inverse of ecdf() in case of `type` = 'raw'", {
  inv_ecdf <- quantile(x_raw, probs = p_vec, type = 1)
  names(inv_ecdf) <- NULL

  expect_equal(q_raw(p_vec), inv_ecdf)
})

test_that("q_fun output is inverse of p_fun output", {
  p_raw <- p_fun(x_raw, type = "raw")
  p_smooth <- p_fun(x_smooth, type = "smooth")

  expect_equal(x_raw_vec, q_raw(p_raw(x_raw_vec)))
  # There is not test `p_vec == p_raw(q_raw(p_vec))` because it shouldn't be
  # true in "raw" case. This is tested in "behaves like inverse of ecdf()" test.
  expect_equal(x_smooth_vec, q_smooth(p_smooth(x_smooth_vec)))
  expect_equal(p_vec, p_smooth(q_smooth(p_vec)))
})

test_that("q_fun output works with extreme values", {
  expect_equal(q_raw(c(0, 1)), x_raw_domain_in)
  expect_equal(q_smooth(c(0, 1)), x_smooth_domain_in)
})

test_that("q_fun returns `NaN` for out of range probabilities", {
  expect_true(all(is.nan(q_raw(c(-1, 2)))))
  expect_true(all(is.nan(q_smooth(c(-1, 2)))))
})

test_that("q_fun asserts", {
  expect_error(q_fun("a"), "x.*numeric")
  expect_error(q_fun(x_raw, type = 1), "type.*string")
  expect_error(q_fun(x_raw, type = "a"), "type.*raw.*smooth")
  expect_error(q_fun(x_raw, attach_x = NULL), "attach_x.*TRUE.*FALSE")
})

test_that("q_fun handles meta data", {
  expect_equal(
    meta(q_raw),
    list(
      distr_tbl = x_raw_distr_tbl, domain_out = x_raw_domain_in, type = "raw",
      x = x_raw
    )
  )

  q_smooth_1 <- q_fun(x_smooth, type = "smooth", attach_x = TRUE)
  expect_named(meta(q_smooth_1), c("domain_out", "type", "x"))
  expect_equal(
    round(meta(q_smooth_1, "domain_out"), 2), round(x_smooth_domain_in, 2)
  )
  expect_equal(
    meta(q_smooth_1)[c("x", "type")],
    list(x = x_smooth, type = "smooth")
  )

  q_smooth_2 <- q_fun(x_smooth, type = "smooth", extra = list(a = TRUE))
  expect_named(meta(q_smooth_2), c("domain_out", "extra", "type", "x"))
  expect_equal(meta(q_smooth_2, "extra"), list(a = TRUE))
})

test_that("q_fun uses `...` as arguments for `density()`", {
  q_smooth_cosine <- q_fun(x_smooth, type = "smooth", kernel = "cosine")
  expect_equal(
    round(q_smooth_cosine(0:20/20), 3),
    c(
      -2.919, -1.82, -1.427, -0.91, -0.589, -0.408, -0.273, -0.158,
      -0.053, 0.048,  0.148,  0.25,  0.357,  0.473,    0.6,  0.743,
       0.903, 1.083,  1.286,  1.54,  2.519
    )
  )
})


# q_fun_raw ---------------------------------------------------------------
# Tested in `q_fun()`


# q_fun_smooth ------------------------------------------------------------
# Tested in `q_fun()`


# find_quant --------------------------------------------------------------
# Tested in `q_fun()`


# print.q_fun -------------------------------------------------------------
test_that("print.q_fun works", {
  expect_output(
    print(q_raw),
    "Quantile function.*raw.*[mM]eta.*distr_tbl, domain_out, type.*function"
  )

  q_smooth_extra <- q_fun(x_smooth, type = "smooth", extra = "a")
  expect_output(
    print(q_smooth_extra),
    "Quantile function.*smoothed.*[mM]eta.*domain_out, extra, type.*function"
  )
})
