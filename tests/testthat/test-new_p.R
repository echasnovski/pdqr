context("test-new_p")


# new_p -------------------------------------------------------------------
test_that("new_p works", {
  expect_distr_fun(p_raw, "p", "raw")
  expect_equal(meta(p_raw, "support"), x_raw_support)
  expect_equal(p_raw(1:10), c(cumsum(x_raw_raw_tbl[["prob"]]), 1))

  expect_distr_fun(p_smooth, "p", "smooth")
  expect_equal(
    round(meta(p_smooth, "support"), 2), round(x_smooth_support, 2)
  )
  expect_equal(
    round(p_smooth(seq(from = -1, to = 1, by = 0.1)), 3),
    c(
      0.143, 0.152, 0.163, 0.177, 0.195, 0.219,  0.25, 0.286, 0.329,
      0.376, 0.426, 0.477, 0.528, 0.577, 0.622, 0.663, 0.701, 0.735,
      0.767, 0.797, 0.826
    )
  )
})

test_that("new_p rounds input in case of `type` = 'raw'", {
  near_1 <- 1 - 10^c(-6, -9)
  expect_equal(p_raw(near_1), c(0, 0.1))
})

test_that("new_p behaves like ecdf() in case of `type` = 'raw'", {
  x_raw_grid <- seq(from = min(x_raw) - 1, to = max(x_raw) + 1, by = 0.01)
  expect_equal(p_raw(x_raw_grid), ecdf(x_raw)(x_raw_grid))
})

test_that("new_p output is integration of new_d in case of `type` = 'smooth'", {
  d_support <- meta(d_smooth, "support")
  x_smooth_grid <- seq(d_support[1] - 1, d_support[2] + 1, by = 0.01)

  p_smooth_int <- vapply(
    x_smooth_grid,
    function(q) {
      integrate(d_smooth, x_smooth_grid[1], q)[["value"]]
    },
    numeric(1)
  )
  p_smooth_out <- p_smooth(x_smooth_grid)


  # `p_smooth()` differs insignificantly from `integrate()` output
  # (due to approximate nature of `integrate()`)
  expect_true(
    all(abs(p_smooth_out - p_smooth_int) <= 10^(-4))
  )
})

test_that("new_p output works with extreme values", {
  extreme_vec <- c(-1, 1) * 10000
  expect_equal(p_raw(extreme_vec), c(0, 1))
  expect_equal(p_smooth(extreme_vec), c(0, 1))
})

test_that("new_p asserts", {
  expect_warning(new_p(c(1, 0, NA)), "x.*NA.*removed")
  expect_warning(new_p(c(1, 0, NaN)), "x.*NaN.*removed")
  expect_warning(new_p(c(1, 0, Inf)), "x.*infinite.*removed")

  expect_error(new_p("a"), "x.*numeric")
  expect_error(new_p(numeric(0)), "x.*empty")
  expect_error(new_p(x_raw, type = 1), "type.*string")
  expect_error(new_p(x_raw, type = "a"), "type.*raw.*smooth")
})

test_that("new_p handles metadata", {
  expect_equal(
    meta(p_raw),
    list(raw_tbl = x_raw_raw_tbl, support = x_raw_support, type = "raw")
  )

  expect_named(meta(p_smooth), c("support", "type"))
  expect_equal(
    round(meta(p_smooth, "support"), 2), round(x_smooth_support, 2)
  )
  expect_equal(meta(p_smooth)["type"], list(type = "smooth"))
})

test_that("new_p uses `...` as arguments for `density()`", {
  p_smooth_cosine <- new_p(x_smooth, type = "smooth", kernel = "cosine")
  expect_equal(
    round(p_smooth_cosine(seq(from = -1, to = 1, by = 0.1)), 3),
    c(
      0.141, 0.151, 0.163, 0.178, 0.197, 0.222, 0.253, 0.289, 0.331,
      0.377, 0.426, 0.476, 0.526, 0.574, 0.619, 0.661,   0.7, 0.735,
      0.768, 0.799, 0.828
    )
  )
})


# new_p_raw ---------------------------------------------------------------
# Tested in `new_p()`


# new_p_smooth ------------------------------------------------------------
# Tested in `new_p()`


# print.p -----------------------------------------------------------------
test_that("print.p works", {
  expect_pdqr_print(new_p, "Cumulative distribution")
})
