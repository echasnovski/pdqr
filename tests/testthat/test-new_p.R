context("test-new_p")


# new_p -------------------------------------------------------------------
test_that("new_p works with numeric input", {
  expect_distr_fun(p_fin, "p", "fin")
  expect_equal(meta_support(p_fin), x_fin_support)
  expect_equal(p_fin(1:10), c(cumsum(x_fin_x_tbl[["prob"]]), 1))

  expect_distr_fun(p_infin, "p", "infin")
  expect_equal(round(meta_support(p_infin), 2), round(x_infin_support, 2))
  expect_equal(
    round(p_infin(seq(from = -1, to = 1, by = 0.1)), 3),
    c(
      0.143, 0.152, 0.163, 0.177, 0.195, 0.219,  0.25, 0.286, 0.329,
      0.376, 0.426, 0.477, 0.528, 0.577, 0.622, 0.663, 0.701, 0.735,
      0.767, 0.797, 0.826
    )
  )
})

test_that("new_p returns dirac-like function with length-one numeric input",  {
  expect_ref_x_tbl(new_p(0.1, "fin"), data.frame(x = 0.1, prob = 1))
  expect_ref_x_tbl(
    new_p(0.1, "infin"),
    data.frame(x = 0.1 + 1e-8*c(-1, 0, 1), y = 1e8*c(0, 1, 0))
  )

  # With big center value there can be problems with total integral being 1
  expect_silent(assert_pdqr_fun(new_p(1e8, "infin")))
})

test_that("new_p works with data frame input", {
  expect_equal_distr(new_p(x_fin_x_tbl, "fin"), p_fin, x_fin_vec_ext)
  expect_equal_distr(
    new_p(x_infin_x_tbl, "infin"), p_infin, x_infin_vec_ext
  )
})

test_that("new_p imputes data frame input", {
  expect_x_tbl_imputation(new_p)
})

test_that("new_p rounds input in case of `type` = 'fin'", {
  # Some values in this sequence show numerical representation issues after
  # `round(x_seq, digits = 10)` (see `dput(x_seq - round(x_seq, digits = 10))`).
  # If input is not rounded during `x_tbl` creation/imputation then output
  # function will behave incorrectly.
  x_seq <- seq(5e-4, 1, by = 5e-4)
  x_df <- data.frame(x = x_seq, prob = rep(1, length(x_seq)) / length(x_seq))

  # Testing correctness of `compute_x_tbl()`
  cur_p_1 <- new_p(x_seq, "fin")
  expect_equal(cur_p_1(0.0045), 0.0045)

  # Testing correctness of `impute_x_tbl()`
  cur_p_2 <- new_p(x_df, "fin")
  expect_equal(cur_p_2(0.0045), 0.0045)
})

test_that("new_p's output rounds input in case of `type` = 'fin'", {
  near_1 <- 1 - 10^c(-6, -11)
  expect_equal(p_fin(near_1), c(0, 0.1))
})

test_that("new_p's output works with 'edge case' inputs", {
  expect_equal(p_fin(c(NA_real_, NaN, -Inf, Inf)), c(NA, NA, 0, 1))
  expect_equal(p_fin(numeric(0)), numeric(0))
  expect_equal(p_fin(meta_support(p_fin)[1] - 1:2), c(0, 0))

  expect_equal(p_infin(c(NA_real_, NaN, -Inf, Inf)), c(NA, NA, 0, 1))
  expect_equal(p_infin(numeric(0)), numeric(0))
  expect_equal(p_infin(meta_support(p_infin)[1] - 1:2), c(0, 0))
})

test_that("new_p's output works with extreme values", {
  extreme_vec <- c(-1, 1) * 10000
  expect_equal(p_fin(extreme_vec), c(0, 1))
  expect_equal(p_infin(extreme_vec), c(0, 1))
})

test_that("new_p's output validates input", {
  expect_error(p_fin("a"), "`q`.*numeric")
  expect_error(p_infin("a"), "`q`.*numeric")
})

test_that("new_p's output behaves like ecdf() in case of `type` = 'fin'", {
  x_fin_grid <- seq(from = min(x_fin) - 1, to = max(x_fin) + 1, by = 0.01)
  expect_equal(p_fin(x_fin_grid), ecdf(x_fin)(x_fin_grid))
})

test_that("new_p's output is integration of new_d's if `type` = 'infin'", {
  d_support <- meta_support(d_infin)
  x_infin_grid <- seq(d_support[1] - 1, d_support[2] + 1, by = 0.01)

  p_infin_int <- vapply(
    x_infin_grid,
    function(q) {
      integrate(d_infin, x_infin_grid[1], q)[["value"]]
    },
    numeric(1)
  )
  p_infin_out <- p_infin(x_infin_grid)


  # `p_infin()` differs insignificantly from `integrate()` output
  # (due to approximate nature of `integrate()`)
  expect_true(
    all(abs(p_infin_out - p_infin_int) <= 10^(-4))
  )
})

test_that("new_p warns about bad `x` elements", {
  expect_warning(new_p(c(1, 0, NA), "infin"), "x.*NA.*removed")
  expect_warning(new_p(c(1, 0, NaN), "infin"), "x.*NaN.*removed")
  expect_warning(new_p(c(1, 0, Inf), "infin"), "x.*infinite.*removed")
})

test_that("new_p validates input", {
  expect_error(new_p("a", "infin"), "x.*numeric.*data.*frame")
  expect_error(new_p(numeric(0), "infin"), "x.*empty")
  expect_error(new_p(x_fin, type = 1), "type.*string")
  expect_error(new_p(x_fin, type = "a"), "type.*fin.*infin")
})

test_that("new_p handles metadata", {
  expect_equal(
    meta_all(p_fin),
    list(type = "fin", support = x_fin_support, x_tbl = x_fin_x_tbl)
  )

  expect_named(meta_all(p_infin), c("type", "support", "x_tbl"))
  expect_equal(meta_x_tbl(p_infin), x_infin_x_tbl)
  expect_equal(round(meta_support(p_infin), 2), round(x_infin_support, 2))
  expect_equal(meta_all(p_infin)["type"], list(type = "infin"))
})

test_that("new_p uses `...` as arguments for `density()`", {
  p_infin_cosine <- new_p(x_infin, type = "infin", kernel = "cosine")
  expect_equal(
    round(p_infin_cosine(seq(from = -1, to = 1, by = 0.1)), 3),
    c(
      0.141, 0.151, 0.163, 0.178, 0.197, 0.222, 0.253, 0.289, 0.331,
      0.377, 0.426, 0.476, 0.526, 0.574, 0.619, 0.661,   0.7, 0.735,
      0.768, 0.799, 0.828
    )
  )
})


# new_p_fin ---------------------------------------------------------------
# Tested in `new_p()`


# new_p_infin -------------------------------------------------------------
# Tested in `new_p()`


# print.p -----------------------------------------------------------------
test_that("print.p works", {
  expect_pdqr_print(new_p, "Cumulative distribution")
})
