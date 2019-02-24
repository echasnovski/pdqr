context("test-group-generics")


# Input data --------------------------------------------------------------
x_norm_seq <- seq(-10, 10, by = 0.01)


# Math.pdqr ---------------------------------------------------------------
test_that("Math.pdqr works", {
  lnorm_x <- seq(0, 100, by = 0.001)
  d_lnorm <- new_d(data.frame(x = lnorm_x, y = dlnorm(lnorm_x)))

  d_norm_ref <- new_d(data.frame(x = x_norm_seq, y = dnorm(x_norm_seq)))
  d_norm_out <- log(d_lnorm)

  expect_distr_fun(d_norm_out, "d", "infin")
  expect_equal_distr(
    d_norm_out, d_norm_ref,
    grid = x_norm_seq, thres = 0.05,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )
})


# Ops.pdqr ----------------------------------------------------------------
test_that("Ops.pdqr works", {
  unif_x <- seq(0, 1, by = 0.001)
  p_unif <- new_p(data.frame(x = unif_x, y = dunif(unif_x)))

  p_norm_ref <- new_p(data.frame(x = x_norm_seq, y = dnorm(x_norm_seq)))
  # Approximation of standard normal as centered sum of 12 uniform
  p_norm_out <- p_unif + p_unif + p_unif + p_unif + p_unif + p_unif +
    p_unif + p_unif + p_unif + p_unif + p_unif + p_unif - 6

  expect_distr_fun(p_norm_out, "p", "infin")
  expect_equal_distr(
    p_norm_out, p_norm_ref,
    grid = x_norm_seq, thres = 0.05,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )
})

test_that("Ops.pdqr works with logical functions", {
  d_leq <- d_fin <= 3
  expect_true(abs(d_leq(1) - p_fin(3)) <= 0.1)
})

test_that("Ops.pdqr works with generics which take one argument", {
  # `+`
  expect_equal(+d_fin, d_fin)
  expect_equal(+d_infin, d_infin)

  # `-`
  d_fin_minus <- -d_fin
  expect_equal(-meta_support(d_fin)[2:1], meta_support(d_fin_minus))
  expect_equal(d_fin(x_fin_vec_ext), d_fin_minus(-x_fin_vec_ext))

  d_infin_minus <- -d_infin
  expect_equal(-meta_support(d_infin)[2:1], meta_support(d_infin_minus))
  expect_equal(d_infin(x_infin_vec_ext), d_infin_minus(-x_infin_vec_ext))

  # `!`
  d_negate <- !new_d(
    data.frame(x = c(-1, 0, 1), prob = c(0.1, 0.2, 0.7)), "fin"
  )
  expect_equal(d_negate(1), 0.2)

    # Probability of type "infin" pdqr-function being exactly 0 is equal to zero
  expect_equal((!d_infin)(1), 0)
})


# Summary.pdqr ------------------------------------------------------------
test_that("Summary.pdqr works", {
  expect_distr_fun(min(p_fin), "p", "fin")
  expect_distr_fun(max(p_fin, p_fin), "p", "fin")
  expect_distr_fun(sum(q_custom, q_infin, q_infin), "q", "infin")
  expect_distr_fun(prod(r_custom, r_custom, na.rm = TRUE), "r", "infin")
})

test_that("Summary.pdqr throws error on `range()`", {
  expect_error(range(p_fin, p_fin), "range.*two.*numbers")
})


# reflect_pdqr_around_zero ------------------------------------------------
# Tested in `Ops.pdqr`


# negate_pdqr -------------------------------------------------------------
# Tested in `Ops.pdqr`
