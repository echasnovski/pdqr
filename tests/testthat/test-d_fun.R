context("test-d_fun")


# Input data --------------------------------------------------------------
x_raw <- c(4, 1, 2, 6, 8, 6, 7, 3, 2, 3, 2, 6, 7, 8, 1, 9, 2, 9, 5, 7)
x_raw_distr_tbl <- data.frame(
  x = as.numeric(1:9), prob = c(0.1, 0.2, 0.1, 0.05, 0.05, 0.15, 0.15, 0.1, 0.1)
)
x_raw_domain_in <- c(1, 9)

x_smooth <- c(
   1.47, 0.11, 0.04,  0.37, -0.43, 0.17, -0.18, 0.85, -0.05, 0.17,
  -1.27, 1.04, 1.06, -1.87, -1.71, -0.3, -0.26, 0.62,  1.42, 0.36
)
x_smooth_domain_in <- c(-2.91865392160928, 2.51865392160928)


# d_fun -------------------------------------------------------------------
test_that("d_fun works", {
  d_raw <- d_fun(x_raw, type = "raw")
  expect_distr_fun(d_raw, "d_fun", "raw")
  expect_equal(meta(d_raw, "domain_in"), x_raw_domain_in)
  expect_equal(d_raw(1:10), c(x_raw_distr_tbl[["prob"]], 0))

  d_smooth <- d_fun(x_smooth, type = "smooth")
  expect_distr_fun(d_smooth, "d_fun", "smooth")
  expect_equal(
    round(meta(d_smooth, "domain_in"), 2), round(x_smooth_domain_in, 2)
  )
  expect_equal(
    round(d_smooth(seq(from = -1, to = 1, by = 0.1)), 2),
    c(
      0.09, 0.1, 0.12, 0.16, 0.21, 0.27, 0.34,  0.4, 0.45, 0.49, 0.51,
      0.51, 0.5, 0.47, 0.43, 0.39, 0.36, 0.33, 0.31, 0.29, 0.28
    )
  )
})

test_that("d_fun rounds input in case of `type` = 'raw'", {
  d_raw <- d_fun(x_raw, type = "raw")
  near_1 <- 1 + 10^c(-6, -9)
  expect_equal(d_raw(near_1), c(0, 0.1))
})

test_that("d_fun asserts", {
  expect_error(d_fun("a"), "x.*numeric")
  expect_error(d_fun(x_raw, type = 1), "type.*string")
  expect_error(d_fun(x_raw, type = "a"), "type.*raw.*smooth")
  expect_error(d_fun(x_raw, attach_sample = NULL), "attach_sample.*TRUE.*FALSE")
})

test_that("d_fun handles meta data", {
  d_raw <- d_fun(x_raw, type = "raw")
  expect_equal(
    meta(d_raw),
    list(distr_tbl = x_raw_distr_tbl, domain_in = x_raw_domain_in, type = "raw")
  )

  d_smooth_1 <- d_fun(x_smooth, type = "smooth", attach_sample = TRUE)
  expect_named(meta(d_smooth_1), c("domain_in", "sample", "type"))
  expect_equal(
    round(meta(d_smooth_1, "domain_in"), 2), round(x_smooth_domain_in, 2)
  )
  expect_equal(
    meta(d_smooth_1)[c("sample", "type")],
    list(sample = x_smooth, type = "smooth")
  )

  d_smooth_2 <- d_fun(x_smooth, type = "smooth", extra = list(a = TRUE))
  expect_named(meta(d_smooth_2), c("domain_in", "extra", "type"))
  expect_equal(meta(d_smooth_2, "extra"), list(a = TRUE))
})

test_that("d_fun uses `...` as arguments for `density()`", {
  d_smooth <- d_fun(x_smooth, type = "smooth", kernel = "cosine")
  expect_equal(
    round(d_smooth(seq(from = -1, to = 1, by = 0.1)), 2),
    c(
      0.09, 0.11, 0.13, 0.17, 0.22, 0.28, 0.34, 0.39, 0.44, 0.48, 0.5,
       0.5, 0.49, 0.47, 0.44,  0.4, 0.37, 0.34, 0.32,  0.3, 0.28
    )
  )
})


# d_fun_raw ---------------------------------------------------------------
# Tested in `d_fun()`


# d_fun_smooth ------------------------------------------------------------
# Tested in `d_fun()`


# print.d_fun -------------------------------------------------------------
test_that("print.d_fun works", {
  d_raw <- d_fun(x_raw, type = "raw")
  expect_output(
    print(d_raw),
    "Density function.*raw.*[mM]eta.*distr_tbl, domain_in, type.*function"
  )

  d_smooth <- d_fun(x_smooth, type = "smooth", extra = "a")
  expect_output(
    print(d_smooth),
    "Density function.*smooth.*[mM]eta.*domain_in, extra, type.*function"
  )
})
