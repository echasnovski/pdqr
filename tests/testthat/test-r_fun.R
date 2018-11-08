context("test-r_fun")


# r_fun -------------------------------------------------------------------
test_that("r_fun works", {
  expect_distr_fun(r_raw, "r_fun", "raw")
  expect_equal(meta(r_raw, "domain_out"), x_raw_domain_in)
  expect_true(all(r_raw(100) %in% x_raw_distr_tbl[["x"]]))

  expect_distr_fun(r_smooth, "r_fun", "smooth")
  expect_equal(
    round(meta(r_smooth, "domain_out"), 2), round(x_smooth_domain_in, 2)
  )
  r_smooth_out <- r_smooth(100)
  expect_true(all(
    (r_smooth_out >= x_smooth_domain_in[1]) &
      (r_smooth_out <= x_smooth_domain_in[2])
  ))
})

test_that("r_fun asserts", {
  expect_error(r_fun("a"), "x.*numeric")
  expect_error(r_fun(numeric(0)), "x.*empty")
  expect_error(r_fun(x_raw, type = 1), "type.*string")
  expect_error(r_fun(x_raw, type = "a"), "type.*raw.*smooth")
  expect_error(r_fun(x_raw, attach_x = NULL), "attach_x.*TRUE.*FALSE")
})

test_that("r_fun handles metadata", {
  expect_equal(
    meta(r_raw),
    list(
      domain_out = x_raw_domain_in, type = "raw",
      x = x_raw
    )
  )

  r_smooth_1 <- r_fun(x_smooth, type = "smooth", attach_x = TRUE)
  expect_named(meta(r_smooth_1), c("domain_out", "type", "x"))
  expect_equal(
    round(meta(r_smooth_1, "domain_out"), 2), round(x_smooth_domain_in, 2)
  )
  expect_equal(
    meta(r_smooth_1)[c("x", "type")],
    list(x = x_smooth, type = "smooth")
  )

  r_smooth_2 <- r_fun(x_smooth, type = "smooth", extra = list(a = TRUE))
  expect_named(meta(r_smooth_2), c("domain_out", "extra", "type", "x"))
  expect_equal(meta(r_smooth_2, "extra"), list(a = TRUE))
})


# r_fun_raw ---------------------------------------------------------------
# Tested in `r_fun()`


# r_fun_smooth ------------------------------------------------------------
# Tested in `r_fun()`


# print.r_fun -------------------------------------------------------------
test_that("print.r_fun works", {
  expect_output(
    print(r_raw),
    paste0(
      "Random generation function.*raw.*",
      "[mM]eta.*domain_out, type.*function"
    )
  )

  r_smooth_extra <- r_fun(x_smooth, type = "smooth", extra = "a")
  expect_output(
    print(r_smooth_extra),
    paste0(
      "Random generation function.*smoothed.*",
      "[mM]eta.*domain_out, extra, type.*function"
    )
  )
})
