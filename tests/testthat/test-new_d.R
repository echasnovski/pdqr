context("test-new_d")


# new_d -------------------------------------------------------------------
test_that("new_d works with numeric input", {
  expect_distr_fun(d_fin, "d", "fin")
  expect_equal(meta_support(d_fin), x_fin_support)
  expect_equal(d_fin(1:10), c(x_fin_x_tbl[["prob"]], 0))

  expect_distr_fun(d_infin, "d", "infin")
  expect_equal(round(meta_support(d_infin), 2), round(x_infin_support, 2))
  expect_equal(
    round(d_infin(seq(from = -1, to = 1, by = 0.1)), 2),
    c(
      0.09, 0.1, 0.12, 0.16, 0.21, 0.27, 0.34,  0.4, 0.45, 0.49, 0.51,
      0.51, 0.5, 0.47, 0.43, 0.39, 0.36, 0.33, 0.31, 0.29, 0.28
    )
  )
})

test_that("new_d returns dirac-like function with length-one numeric input",  {
  expect_ref_x_tbl(new_d(0.1, "fin"), data.frame(x = 0.1, prob = 1))
  expect_ref_x_tbl(
    new_d(0.1, "infin"),
    data.frame(x = 0.1 + 1e-8*c(-1, 0, 1), y = 1e8*c(0, 1, 0))
  )
})

test_that("new_d works with data frame input", {
  expect_equal_distr(new_d(x_fin_x_tbl, "fin"), d_fin, x_fin_vec_ext)
  expect_equal_distr(
    new_d(x_infin_x_tbl, "infin"), d_infin, x_infin_vec_ext
  )
})

test_that("new_d imputes data frame input", {
  expect_x_tbl_imputation(new_d)
})

test_that("new_d's output rounds input in case of `type` = 'fin'", {
  near_1 <- 1 + 10^c(-6, -11)
  expect_equal(d_fin(near_1), c(0, 0.1))
})

test_that("new_d's output works with 'edge case' inputs", {
  expect_equal(d_fin(c(NA_real_, NaN, -Inf, Inf)), c(NA, NA, 0, 0))
  expect_equal(d_fin(numeric(0)), numeric(0))
  # Exact edges of support shouldn't be treated as "out of support"
  expect_true(all(d_fin(meta_support(d_fin)) != 0))
  expect_equal(d_fin(meta_support(d_fin) + c(-1, 1)), c(0, 0))

  expect_equal(d_infin(c(NA_real_, NaN, -Inf, Inf)), c(NA, NA, 0, 0))
  expect_equal(d_infin(numeric(0)), numeric(0))
  # Exact edges of support shouldn't be treated as "out of support"
  expect_true(all(d_fin(meta_support(d_fin)) != 0))
  expect_equal(d_fin(meta_support(d_infin) + c(-1, 1)), c(0, 0))
})

test_that("new_d's output works with extreme values", {
  extreme_vec <- c(-1, 1) * 10000
  expect_equal(d_fin(extreme_vec), c(0, 0))
  expect_equal(d_infin(extreme_vec), c(0, 0))
})

test_that("new_d's output asserts bad input", {
  expect_error(d_fin("a"), "`x`.*numeric")
  expect_error(d_infin("a"), "`x`.*numeric")
})

test_that("new_d's output integrates to 1 in case `type` = 'infin'", {
  integral <- stats::integrate(d_infin, -3, 3)
  output_range <- integral[["value"]] + c(-1, 1) * integral[["abs.error"]]
  expect_true((output_range[1] <= 1) && (1 <= output_range[2]))
})

test_that("new_d asserts", {
  expect_warning(new_d(c(1, 0, NA), "infin"), "x.*NA.*removed")
  expect_warning(new_d(c(1, 0, NaN), "infin"), "x.*NaN.*removed")
  expect_warning(new_d(c(1, 0, Inf), "infin"), "x.*infinite.*removed")

  expect_error(new_d("a", "infin"), "x.*numeric.*data.*frame")
  expect_error(new_d(numeric(0), "infin"), "x.*empty")
  expect_error(new_d(x_fin, type = 1), "type.*string")
  expect_error(new_d(x_fin, type = "a"), "type.*fin.*infin")
})

test_that("new_d handles metadata", {
  expect_equal(
    meta_all(d_fin),
    list(type = "fin", support = x_fin_support, x_tbl = x_fin_x_tbl)
  )

  expect_named(meta_all(d_infin), c("type", "support", "x_tbl"))
  expect_equal(meta_x_tbl(d_infin), x_infin_x_tbl)
  expect_equal(round(meta_support(d_infin), 2), round(x_infin_support, 2))
  expect_equal(meta_all(d_infin)["type"], list(type = "infin"))
})

test_that("new_d uses `...` as arguments for `density()`", {
  d_infin_cosine <- new_d(x_infin, type = "infin", kernel = "cosine")
  expect_equal(
    round(d_infin_cosine(seq(from = -1, to = 1, by = 0.1)), 2),
    c(
      0.09, 0.11, 0.13, 0.17, 0.22, 0.28, 0.34, 0.39, 0.44, 0.48, 0.5,
       0.5, 0.49, 0.47, 0.44,  0.4, 0.37, 0.34, 0.32,  0.3, 0.28
    )
  )
})


# new_d_fin ---------------------------------------------------------------
# Tested in `new_d()`


# new_d_infin -------------------------------------------------------------
# Tested in `new_d()`


# print.d -----------------------------------------------------------------
test_that("print.d works", {
  expect_pdqr_print(new_d, "Probability mass", "Density")
})
