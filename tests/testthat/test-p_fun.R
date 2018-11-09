context("test-p_fun")


# p_fun -------------------------------------------------------------------
test_that("p_fun works", {
  expect_distr_fun(p_raw, "p_fun", "raw")
  expect_equal(meta(p_raw, "domain_in"), x_raw_domain_in)
  expect_equal(p_raw(1:10), c(cumsum(x_raw_distr_tbl[["prob"]]), 1))

  expect_distr_fun(p_smooth, "p_fun", "smooth")
  expect_equal(
    round(meta(p_smooth, "domain_in"), 2), round(x_smooth_domain_in, 2)
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

test_that("p_fun rounds input in case of `type` = 'raw'", {
  near_1 <- 1 - 10^c(-6, -9)
  expect_equal(p_raw(near_1), c(0, 0.1))
})

test_that("p_fun behaves like ecdf() in case of `type` = 'raw'", {
  x_raw_grid <- seq(from = min(x_raw) - 1, to = max(x_raw) + 1, by = 0.01)
  expect_equal(p_raw(x_raw_grid), ecdf(x_raw)(x_raw_grid))
})

test_that("p_fun output is integration of d_fun in case of `type` = 'smooth'", {
  d_domain <- meta(d_smooth, "domain_in")
  x_smooth_grid <- seq(from = d_domain[1] - 1, to = d_domain[2] + 1, by = 0.01)

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

test_that("p_fun output works with extreme values", {
  extreme_vec <- c(-1, 1) * 10000
  expect_equal(p_raw(extreme_vec), c(0, 1))
  expect_equal(p_smooth(extreme_vec), c(0, 1))
})

test_that("p_fun asserts", {
  expect_warning(p_fun(c(1, 0, NA)), "x.*NA.*removed")
  expect_warning(p_fun(c(1, 0, NaN)), "x.*NaN.*removed")
  expect_warning(p_fun(c(1, 0, Inf)), "x.*infinite.*removed")

  expect_error(p_fun("a"), "x.*numeric")
  expect_error(p_fun(numeric(0)), "x.*empty")
  expect_error(p_fun(x_raw, type = 1), "type.*string")
  expect_error(p_fun(x_raw, type = "a"), "type.*raw.*smooth")
  expect_error(p_fun(x_raw, attach_x = NULL), "attach_x.*TRUE.*FALSE")
})

test_that("p_fun handles metadata", {
  expect_equal(
    meta(p_raw),
    list(
      domain_in = x_raw_domain_in, type = "raw",
      x = x_raw
    )
  )

  p_smooth_1 <- p_fun(x_smooth, type = "smooth", attach_x = TRUE)
  expect_named(meta(p_smooth_1), c("domain_in", "type", "x"))
  expect_equal(
    round(meta(p_smooth_1, "domain_in"), 2), round(x_smooth_domain_in, 2)
  )
  expect_equal(
    meta(p_smooth_1)[c("x", "type")],
    list(x = x_smooth, type = "smooth")
  )

  p_smooth_2 <- p_fun(x_smooth, type = "smooth", extra = list(a = TRUE))
  expect_named(meta(p_smooth_2), c("domain_in", "extra", "type", "x"))
  expect_equal(meta(p_smooth_2, "extra"), list(a = TRUE))
})

test_that("p_fun uses `...` as arguments for `density()`", {
  p_smooth_cosine <- p_fun(x_smooth, type = "smooth", kernel = "cosine")
  expect_equal(
    round(p_smooth_cosine(seq(from = -1, to = 1, by = 0.1)), 3),
    c(
      0.141, 0.151, 0.163, 0.178, 0.197, 0.222, 0.253, 0.289, 0.331,
      0.377, 0.426, 0.476, 0.526, 0.574, 0.619, 0.661,   0.7, 0.735,
      0.768, 0.799, 0.828
    )
  )
})


# p_fun_raw ---------------------------------------------------------------
# Tested in `p_fun()`


# p_fun_smooth ------------------------------------------------------------
# Tested in `p_fun()`


# print.p_fun -------------------------------------------------------------
test_that("print.p_fun works", {
  expect_output(
    print(p_raw),
    paste0(
      "Cumulative distribution function.*raw.*",
      "[mM]eta.*domain_in, type.*function"
    )
  )

  p_smooth_extra <- p_fun(x_smooth, type = "smooth", extra = "a")
  expect_output(
    print(p_smooth_extra),
    paste0(
      "Cumulative distribution function.*smoothed.*",
      "[mM]eta.*domain_in, extra, type.*function"
    )
  )
})
