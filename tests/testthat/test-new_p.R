context("test-new_p")


# new_p -------------------------------------------------------------------
test_that("new_p works with numeric input", {
  expect_distr_fun(p_dis, "p", "discrete")
  expect_equal(meta_support(p_dis), x_dis_support)
  expect_equal(p_dis(1:10), c(cumsum(x_dis_x_tbl[["prob"]]), 1))

  expect_distr_fun(p_con, "p", "continuous")
  expect_equal(round(meta_support(p_con), 2), round(x_con_support, 2))
  expect_equal(
    round(p_con(seq(from = -1, to = 1, by = 0.1)), 3),
    c(
      0.143, 0.152, 0.163, 0.177, 0.195, 0.219,  0.25, 0.286, 0.329,
      0.376, 0.426, 0.477, 0.528, 0.577, 0.622, 0.663, 0.701, 0.735,
      0.767, 0.797, 0.826
    )
  )
})

test_that("new_p returns dirac-like function with length-one numeric input",  {
  expect_ref_x_tbl(new_p(0.1, "discrete"), data.frame(x = 0.1, prob = 1))
  expect_ref_x_tbl(
    new_p(0.1, "continuous"),
    data.frame(x = 0.1 + 1e-8*c(-1, 0, 1), y = 1e8*c(0, 1, 0))
  )

  # With big center value there can be problems with total integral being 1
  expect_silent(assert_pdqr_fun(new_p(1e8, "continuous")))
})

test_that("new_p works with data frame input", {
  expect_equal_distr(new_p(x_dis_x_tbl, "discrete"), p_dis, x_dis_vec_ext)
  expect_equal_distr(
    new_p(x_con_x_tbl, "continuous"), p_con, x_con_vec_ext
  )
})

test_that("new_p imputes data frame input", {
  expect_x_tbl_imputation(new_p)
})

test_that("new_p rounds input in case of `type = 'discrete'`", {
  # Some values in this sequence show numerical representation issues after
  # `round(x_seq, digits = 10)` (see `dput(x_seq - round(x_seq, digits = 10))`).
  # If input is not rounded during `x_tbl` creation/imputation then output
  # function will behave incorrectly.
  x_seq <- seq(5e-4, 1, by = 5e-4)
  x_df <- data.frame(x = x_seq, prob = rep(1, length(x_seq)) / length(x_seq))

  # Testing correctness of `compute_x_tbl()`
  cur_p_1 <- new_p(x_seq, "discrete")
  expect_equal(cur_p_1(0.0045), 0.0045)

  # Testing correctness of `impute_x_tbl()`
  cur_p_2 <- new_p(x_df, "discrete")
  expect_equal(cur_p_2(0.0045), 0.0045)
})

test_that("new_p's output rounds input in case of `type = 'discrete'`", {
  near_1 <- 1 - 10^c(-6, -11)
  expect_equal(p_dis(near_1), c(0, 0.1))
})

test_that("new_p's output works with 'edge case' inputs", {
  expect_equal(p_dis(c(NA_real_, NaN, -Inf, Inf)), c(NA, NA, 0, 1))
  expect_equal(p_dis(numeric(0)), numeric(0))
  expect_equal(p_dis(meta_support(p_dis)[1] - 1:2), c(0, 0))

  expect_equal(p_con(c(NA_real_, NaN, -Inf, Inf)), c(NA, NA, 0, 1))
  expect_equal(p_con(numeric(0)), numeric(0))
  expect_equal(p_con(meta_support(p_con)[1] - 1:2), c(0, 0))
})

test_that("new_p's output works with extreme values", {
  extreme_vec <- c(-1, 1) * 10000
  expect_equal(p_dis(extreme_vec), c(0, 1))
  expect_equal(p_con(extreme_vec), c(0, 1))
})

test_that("new_p's output validates input", {
  expect_error(p_dis("a"), "`q`.*numeric")
  expect_error(p_con("a"), "`q`.*numeric")
})

test_that("new_p's output behaves like ecdf() in case of `type = 'discrete'`", {
  x_dis_grid <- seq(from = min(x_dis) - 1, to = max(x_dis) + 1, by = 0.01)
  expect_equal(p_dis(x_dis_grid), ecdf(x_dis)(x_dis_grid))
})

test_that("new_p's output is integration of new_d's if `type = 'continuous'`", {
  d_support <- meta_support(d_con)
  x_con_grid <- seq(d_support[1] - 1, d_support[2] + 1, by = 0.01)

  p_con_int <- vapply(
    x_con_grid,
    function(q) {
      integrate(d_con, x_con_grid[1], q)[["value"]]
    },
    numeric(1)
  )
  p_con_out <- p_con(x_con_grid)


  # `p_con()` differs insignificantly from `integrate()` output
  # (due to approximate nature of `integrate()`)
  expect_true(
    all(abs(p_con_out - p_con_int) <= 10^(-4))
  )
})

test_that("new_p warns about bad `x` elements", {
  expect_warning(new_p(c(1, 0, NA), "continuous"), "x.*NA.*removed")
  expect_warning(new_p(c(1, 0, NaN), "continuous"), "x.*NaN.*removed")
  expect_warning(new_p(c(1, 0, Inf), "continuous"), "x.*infinite.*removed")
})

test_that("new_p validates input", {
  expect_error(new_p(type = "continuous"), "`x`.*missing.*numeric.*data frame")
  expect_error(new_p("a", "continuous"), "x.*numeric.*data.*frame")
  expect_error(new_p(numeric(0), "continuous"), "x.*empty")
  expect_error(new_p(x_dis), "`type`.*missing.*pdqr type")
  expect_error(new_p(x_dis, type = 1), "type.*string")
  expect_error(new_p(x_dis, type = "a"), "type.*discrete.*continuous")
})

test_that("new_p handles metadata", {
  expect_equal(
    meta_all(p_dis),
    list(
      class = "p", type = "discrete", support = x_dis_support,
      x_tbl = x_dis_x_tbl
    )
  )

  expect_named(meta_all(p_con), c("class", "type", "support", "x_tbl"))
  expect_equal(meta_x_tbl(p_con), x_con_x_tbl)
  expect_equal(round(meta_support(p_con), 2), round(x_con_support, 2))
  expect_equal(meta_all(p_con)["type"], list(type = "continuous"))
})

test_that("new_p uses `...` as arguments for `density()`", {
  p_con_cosine <- new_p(x_con, type = "continuous", kernel = "cosine")
  expect_equal(
    round(p_con_cosine(seq(from = -1, to = 1, by = 0.1)), 3),
    c(
      0.141, 0.151, 0.163, 0.178, 0.197, 0.222, 0.253, 0.289, 0.331,
      0.377, 0.426, 0.476, 0.526, 0.574, 0.619, 0.661,   0.7, 0.735,
      0.768, 0.799, 0.828
    )
  )
})


# new_p_dis ---------------------------------------------------------------
# Tested in `new_p()`


# new_p_con ---------------------------------------------------------------
# Tested in `new_p()`


# print.p -----------------------------------------------------------------
test_that("print.p works", {
  expect_pdqr_print(new_p, "Cumulative distribution")
})
