context("test-group-generics")

set.seed(654321)


# Input data --------------------------------------------------------------
x_norm_seq <- seq(-10, 10, by = 0.01)

bad_pdqr <- structure(function(x) {x}, class = c("p", "pdqr", "function"))
bad_pdqr_2 <- structure(function(x) {x+1}, class = c("p", "pdqr", "function"))


# Custom functions --------------------------------------------------------
make_unif <- function(min, max) {
  new_d(data.frame(x = c(min, max), y = c(1, 1) / c(max - min)), "infin")
}


# Custom expactations -----------------------------------------------------
expect_lin_trans <- function(f_out, f_in, ref_supp) {
  output_ref <- form_resupport(f_in, ref_supp, method = "linear")

  expect_equal_x_tbl(f_out, output_ref)
}

expect_prob_true <- function(f, val) {
  expect_equal(as_d(f)(1), val)
}

expect_op_support <- function(op, in_supp, ref_supp, inds = 1:2) {
  f <- new_d(data.frame(x = in_supp, y = c(1, 1)/diff(in_supp)), "infin")
  out_supp <- suppressWarnings(meta_support(op(f)))

  expect_equal(out_supp[inds], ref_supp)
}

expect_close_op_support <- function(op, in_supp, ref_supp, thres) {
  f <- new_d(data.frame(x = in_supp, y = c(1, 1)/diff(in_supp)), "infin")
  out_supp <- meta_support(op(f))

  expect_true(all(abs(out_supp - ref_supp) < thres))
}


# Math.pdqr ---------------------------------------------------------------
test_that("Math.pdqr works", {
  # Test 1
  lnorm_x <- seq(0, 100, by = 0.001)
  d_lnorm <- new_d(data.frame(x = lnorm_x, y = dlnorm(lnorm_x)), "infin")

  d_norm_ref <- new_d(
    data.frame(x = x_norm_seq, y = dnorm(x_norm_seq)), "infin"
  )
  d_norm_out <- log(d_lnorm)

  expect_distr_fun(d_norm_out, "d", "infin")
  expect_close_f(
    d_norm_out, d_norm_ref,
    grid = x_norm_seq, thres = 0.05
  )

  # Test 2
  x_chisq_seq <- seq(0.001, 20, by = 0.001)
  d_chisq_ref <- new_d(
    data.frame(x = x_chisq_seq, y = dchisq(x_chisq_seq, df = 1)), "infin"
  )
  d_chisq_out <- d_norm_ref^2

  expect_distr_fun(d_chisq_out, "d", "infin")
  expect_close_f(
    d_chisq_out, d_chisq_ref,
    # At the edge (here 0) output will differ because `dchisq` goes to infinity
    # when `df = 1`.
    grid = x_chisq_seq[x_chisq_seq > 0.4], thres = 0.05
  )
})

test_that("Math.pdqr uses options", {
  # Option for `n_sample`
  op <- options(pdqr.group_gen.n_sample = 1)
  sqrt_fin <- sqrt(d_fin)
  expect_true(nrow(meta_x_tbl(sqrt_fin)) == 1)
  options(op)

  # Options for `args_new` and support repair method
  op <- options(
    pdqr.group_gen.args_new = list(n = 2),
    pdqr.group_gen.repair_supp_method = NULL
  )
  sqrt_infin <- sqrt(new_d(data.frame(x = 0:1, y = c(1, 1)), "infin"))
  expect_true(nrow(meta_x_tbl(sqrt_infin)) == 2)
  options(op)
})

test_that("Math.pdqr repairs support in general cases", {
  # skip_on_cran()

  d_unif <- make_unif(0, 1)

  # General
  expect_equal(meta_support(sqrt(d_unif)), c(0, 1))
  expect_equal(meta_support(floor(d_unif))[1], 0)
  expect_equal(meta_support(ceiling(d_unif))[2], 1)
  expect_equal(meta_support(trunc(d_unif))[1], 0)
  expect_equal(meta_support(round(d_unif)), c(0, 1))
  expect_equal(meta_support(signif(d_unif)), c(0, 1))

  # Exponential
  expect_equal(meta_support(exp(d_unif)), exp(c(0, 1)))
  expect_equal(meta_support(log(d_unif))[2], 0)
  expect_equal(meta_support(expm1(d_unif)), expm1(c(0, 1)))
  expect_equal(meta_support(log1p(d_unif))[1],  0)

  # Trigonometric
    # cos()
  expect_op_support(cos, in_supp = c(0, 1), ref_supp = range(cos(c(0, 1))))
  expect_op_support(cos, in_supp = c(-1, 1)*0.5*pi, ref_supp = c(0, 1))
  expect_op_support(cos, in_supp = c(1, 3)*0.5*pi, ref_supp = c(-1, 0))
  expect_op_support(cos, in_supp = c(0, 10), ref_supp = c(-1, 1))

    # sin()
  expect_op_support(sin, in_supp = c(0, 1), ref_supp = range(sin(c(0, 1))))
  expect_op_support(sin, in_supp = c(0, 1)*pi, ref_supp = c(0, 1))
  expect_op_support(sin, in_supp = c(1, 2)*pi, ref_supp = c(-1, 0))
  expect_op_support(sin, in_supp = c(0, 10), ref_supp = c(-1, 1))

    # tan()
  expect_op_support(tan, in_supp = c(0, 1), ref_supp = range(tan(c(0, 1))))
  # If `0.5*pi + k*pi` family is inside input support, then output should have
  # very big support (based on actual points generated inside `form_trans()`)
  tan_supp <- meta_support(tan(make_unif(0, pi)))
  expect_true(all(abs(tan_supp) > 100))

    # cospi()
  expect_op_support(cospi, in_supp = c(0, 0.5), ref_supp = c(0, 1))
  expect_op_support(cospi, in_supp = c(-1, 1)*0.5, ref_supp = c(0, 1))
  expect_op_support(cospi, in_supp = c(1, 3)*0.5, ref_supp = c(-1, 0))
  expect_op_support(cospi, in_supp = c(0, 10), ref_supp = c(-1, 1))

    # sinpi()
  expect_op_support(sinpi, in_supp = c(0, 0.5), ref_supp = c(0, 1))
  expect_op_support(sinpi, in_supp = c(0, 1), ref_supp = c(0, 1))
  expect_op_support(sinpi, in_supp = c(1, 2), ref_supp = c(-1, 0))
  expect_op_support(sinpi, in_supp = c(0, 10), ref_supp = c(-1, 1))

    # tanpi()
  expect_op_support(tanpi, in_supp = c(0, 0.25), ref_supp = c(0, 1))
  # If `0.5 + k` family is inside input support, then output should have very
  # big support (based on actual points generated inside `form_trans()`)
  tanpi_supp <- meta_support(tanpi(make_unif(0, 1)))
  expect_true(all(abs(tanpi_supp) > 100))

    # acos()
  expect_op_support(acos, in_supp = c(-0.5, 0.5), ref_supp = acos(c(0.5, -0.5)))
  expect_op_support(acos, in_supp = c(-1, 1), ref_supp = acos(c(1, -1)))
      # Applying `acos()` to this uniform gives warning which is suppressed
      # inside `expect_op_support()`
  expect_op_support(acos, in_supp = c(-10, 10), ref_supp = acos(c(1, -1)))

    # asin()
  expect_op_support(asin, in_supp = c(-0.5, 0.5), ref_supp = asin(c(-0.5, 0.5)))
  expect_op_support(asin, in_supp = c(-1, 1), ref_supp = asin(c(-1, 1)))
      # Applying `asin()` to this uniform gives warning which is suppressed
      # inside `expect_op_support()`
  expect_op_support(asin, in_supp = c(-10, 10), ref_supp = asin(c(-1, 1)))

    # atan()
  expect_op_support(atan, in_supp = c(0, 1), ref_supp = atan(c(0, 1)))

  # Hyperbolic
    # cosh()
  expect_op_support(cosh, in_supp = c(1, 2), ref_supp = cosh(c(1, 2)))
  expect_op_support(cosh, in_supp = c(-2, 1), ref_supp = c(1, cosh(-2)))

    # sinh()
  expect_op_support(sinh, in_supp = c(1, 2), ref_supp = sinh(c(1, 2)))

    # tanh()
  expect_op_support(tanh, in_supp = c(1, 2), ref_supp = tanh(c(1, 2)))

    # acosh()
  expect_op_support(acosh, in_supp = c(1, 2), ref_supp = acosh(c(1, 2)))
  expect_op_support(acosh, in_supp = c(-1, 2), ref_supp = c(0, acosh(2)))

    # asinh()
  expect_op_support(asinh, in_supp = c(1, 2), ref_supp = asinh(c(1, 2)))

    # atanh()
  expect_op_support(
    atanh, in_supp = c(-0.5, 0.5), ref_supp = atanh(c(-0.5, 0.5))
  )
  atanh_supp <- suppressWarnings(meta_support(atanh(make_unif(-2, 2))))
      # Output should have large support. Here modulus `3` is "large" because of
      # slow nature of `atanh()`
  expect_true(all(abs(atanh_supp) > 3))

  # Gamma. All supports are repaired with simulation, so tests aren't exact
  expect_close_op_support(
    lgamma, in_supp = c(0.1, 0.2), ref_supp = lgamma(c(0.2, 0.1)), thres = 1e-3
  )
  expect_close_op_support(
    gamma, in_supp = c(0.1, 0.2), ref_supp = gamma(c(0.2, 0.1)), thres = 1e-2
  )
  expect_close_op_support(
    digamma, in_supp = c(0.1, 0.2), ref_supp = digamma(c(0.1, 0.2)),
    thres = 1e-3
  )
  expect_close_op_support(
    trigamma, in_supp = c(0.1, 0.2), ref_supp = trigamma(c(0.2, 0.1)),
    thres = 2e-2
  )
})

test_that("Math.pdqr method for `abs()` works", {
  # Type "fin"
  cur_d_fin <- new_d(
    data.frame(
      x    = c(-1.5,  -1,   0,    1, 1.25),
      prob = c( 0.1, 0.2, 0.3, 0.25, 0.15)
    ),
    "fin"
  )
  expect_ref_x_tbl(
    abs(cur_d_fin),
    data.frame(x = c(0, 1, 1.25, 1.5), prob = c(0.3, 0.2+0.25, 0.15, 0.1))
  )

  # Type "infin"
  d_abs_ref <- function(x) {
    res <- numeric(length(x))
    res[x >= 0] <- d_infin(x[x >= 0]) + d_infin(-x[x >= 0])

    res
  }
  expect_close_f(abs(d_infin), d_abs_ref, c(x_infin_vec_ext, -x_infin_vec_ext))

    # Case with discontinuity
  d_unif <- new_d(data.frame(x = c(-3, 1), y = c(0.25, 0.25)), "infin")
  expect_ref_x_tbl(
    abs(d_unif),
    data.frame(x = c(0, 1, 1+1e-8, 3), y = c(0.5, 0.5, 0.25, 0.25))
  )
})

test_that("Math.pdqr method for `sign()` works", {
  # Type "fin"
  cur_d_fin <- new_d(
    data.frame(
      x    = c(-1.5,  -1,   0,    1, 1.25),
      prob = c( 0.1, 0.2, 0.3, 0.25, 0.15)
    ),
    "fin"
  )
  expect_ref_x_tbl(
    sign(cur_d_fin),
    data.frame(x = c(-1, 0, 1), prob = c(0.1+0.2, 0.3, 0.25+0.15))
  )

  cur_d_fin_2 <- new_d(
    data.frame(x = c(0, 0.5, 1), prob = c(0.1, 0.3, 0.6)), "fin"
  )
  expect_ref_x_tbl(
    sign(cur_d_fin_2),
    data.frame(x = c(-1, 0, 1), prob = c(0, 0.1, 0.3+0.6))
  )

  # Type "infin"
  d_unif <- new_d(data.frame(x = c(-3, 1), y = c(0.25, 0.25)), "infin")
  expect_ref_x_tbl(
    sign(d_unif),
    data.frame(x = c(-1, 0, 1), prob = c(0.75, 0, 0.25))
  )

  d_unif_2 <- new_d(data.frame(x = c(0.5, 1), y = c(2, 2)), "infin")
  expect_ref_x_tbl(
    sign(d_unif_2),
    data.frame(x = c(-1, 0, 1), prob = c(0, 0, 1))
  )
})

test_that("Math.pdqr validates input", {
  expect_error(log(bad_pdqr), "`x`")
})


# Ops.pdqr ----------------------------------------------------------------
test_that("Ops.pdqr works", {
  unif_x <- seq(0, 1, by = 0.001)
  p_unif <- new_p(data.frame(x = unif_x, y = dunif(unif_x)), "infin")

  p_norm_ref <- new_p(
    data.frame(x = x_norm_seq, y = dnorm(x_norm_seq)), "infin"
  )
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

test_that("Ops.pdqr uses options", {
  # Option for `n_sample`
  op <- options(pdqr.group_gen.n_sample = 1)
  plus_fin <- d_fin + d_fin
  expect_true(nrow(meta_x_tbl(plus_fin)) == 1)
  options(op)

  # Options for `args_new` and support repair method
  op <- options(
    pdqr.group_gen.args_new = list(n = 2),
    pdqr.group_gen.repair_supp_method = NULL
  )
  plus_infin <- d_infin + d_infin
  expect_true(nrow(meta_x_tbl(plus_infin)) == 2)
  options(op)

  # Option only for support repair method
  op <- options(pdqr.group_gen.repair_supp_method = NULL)
  mod_infin <- d_infin %% 1
  # `mod_infin` should have values outside of (0, 1) because of default
  # "extending" property of `density()`
  expect_true(mod_infin(-0.01) > 0)
  options(op)
})

test_that("Ops.pdqr repairs support in general cases", {
  # skip_on_cran()

  d_unif <- make_unif(0, 1)
  d_unif_1 <- make_unif(-100, 1)
  d_unif_2 <- make_unif(50, 51)

  # `+`
  expect_equal(meta_support(d_unif_1 + d_unif_2), c(-50, 52))

  # `-`
  expect_equal(meta_support(d_unif_1 - d_unif_2), c(-151, -49))

  # `*`
  expect_equal(meta_support(d_unif_1 * d_unif_2), c(-5100, 51))
  expect_equal(
    meta_support(make_unif(-2, -0.5) * make_unif(-10, -9)), c(4.5, 20)
  )

  # `/`
  expect_equal(meta_support(d_unif_1 / d_unif_2), c(-2, 1/50))
  division_supp_2 <- meta_support(d_unif_2 / d_unif_1)
  expect_true(all(abs(division_supp_2) > 100))
  division_supp_3 <- meta_support(d_unif / d_unif)
  expect_true((division_supp_3[1] == 0) && (division_supp_3[2] > 100))
  division_supp_4 <- meta_support(d_unif / make_unif(-1, 0))
  expect_true((division_supp_4[1] < -100) && (division_supp_4[2] == 0))

  # `^`. Tests aren't exact because support repair uses simulation
  power_supp_1 <- meta_support(make_unif(1, 2)^make_unif(1, 2))
  expect_true(all(abs(power_supp_1 - c(1, 4)) < 0.05))
  power_supp_2 <- meta_support(make_unif(1, 2)^make_unif(-2, -1))
  expect_true(all(abs(power_supp_2 - c(0.25, 1)) < 0.005))
  power_supp_3 <- suppressWarnings(
    meta_support(make_unif(-2, 1.1)^make_unif(0.5, 2))
  )
  expect_true(
    (abs(power_supp_3[1]) < 1e-4) && (abs(power_supp_3[2] - 1.21) < 2e-2)
  )

  # `%%`. Tests aren't exact because support repair uses simulation
  mod_supp_1 <- meta_support(make_unif(-10, 10) %% make_unif(1, 3))
  expect_true(all(abs(mod_supp_1 - c(0, 3)) < 0.05))
  mod_supp_2 <- meta_support(make_unif(-10, 10) %% make_unif(-2, -1))
  expect_true(all(abs(mod_supp_2 - c(-2, 0)) < 0.05))

  # `%/%`
  expect_equal(meta_support(make_unif(1, 4.5) %/% make_unif(1, 4)), c(0, 4))
  expect_equal(
    meta_support(d_unif_1 %/% d_unif_2),
    floor(meta_support(d_unif_1 / d_unif_2))
  )
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

test_that("Ops.pdqr validates input with generics which take one argument", {
  expect_error(+bad_pdqr, "`e1`")
  expect_error(-bad_pdqr, "`e1`")
  expect_error(!bad_pdqr, "`e1`")
})

test_that("Ops.pdqr works in case of linear operation", {
  d_fin_supp <- meta_support(d_fin)
  d_infin_supp <- meta_support(d_infin)
  d_dirac <- form_retype(d_fin, "infin", method = "dirac")
  d_dirac_supp <- meta_support(d_dirac)

  # `+`
  expect_lin_trans(d_fin + 10, f_in = d_fin, ref_supp = d_fin_supp + 10)
  expect_lin_trans(10 + d_fin, f_in = d_fin, ref_supp = d_fin_supp + 10)
  expect_lin_trans(d_infin + 10, f_in = d_infin, ref_supp = d_infin_supp + 10)
  expect_lin_trans(10 + d_infin, f_in = d_infin, ref_supp = d_infin_supp + 10)
  expect_lin_trans(d_dirac + 10, f_in = d_dirac, ref_supp = d_dirac_supp + 10)
  expect_lin_trans(10 + d_dirac, f_in = d_dirac, ref_supp = d_dirac_supp + 10)

  # `-`
  expect_lin_trans(d_fin - 10, f_in = d_fin, ref_supp = d_fin_supp - 10)
  expect_lin_trans(10 - d_fin, f_in = -d_fin, ref_supp = 10 - d_fin_supp[2:1])
  expect_lin_trans(d_infin - 10, f_in = d_infin, ref_supp = d_infin_supp - 10)
  expect_lin_trans(
    10 - d_infin, f_in = -d_infin, ref_supp = 10 - d_infin_supp[2:1]
  )
  expect_lin_trans(d_dirac - 10, f_in = d_dirac, ref_supp = d_dirac_supp - 10)
  expect_lin_trans(
    10 - d_dirac, f_in = -d_dirac, ref_supp = 10 - d_dirac_supp[2:1]
  )

  # `*`
  expect_lin_trans(d_fin * 10, f_in = d_fin, ref_supp = d_fin_supp * 10)
  expect_lin_trans(10 * d_fin, f_in = d_fin, ref_supp = d_fin_supp * 10)
  expect_lin_trans(d_infin * 10, f_in = d_infin, ref_supp = d_infin_supp * 10)
  expect_lin_trans(10 * d_infin, f_in = d_infin, ref_supp = d_infin_supp * 10)
  expect_lin_trans(d_dirac * 10, f_in = d_dirac, ref_supp = d_dirac_supp * 10)
  expect_lin_trans(10 * d_dirac, f_in = d_dirac, ref_supp = d_dirac_supp * 10)

  # `/`
  expect_lin_trans(d_fin / 10, f_in = d_fin, ref_supp = d_fin_supp / 10)
  expect_lin_trans(d_infin / 10, f_in = d_infin, ref_supp = d_infin_supp / 10)
  expect_lin_trans(d_dirac / 10, f_in = d_dirac, ref_supp = d_dirac_supp / 10)
})

test_that("Ops.pdqr validates input in case of linear operation", {
  expect_error(bad_pdqr + 1, "`e1`")
  expect_error(1 + bad_pdqr, "`e2`")
  expect_error(bad_pdqr - 1, "`e1`")
  expect_error(1 - bad_pdqr, "`e2`")
  expect_error(bad_pdqr * 1, "`e1`")
  expect_error(1 * bad_pdqr, "`e2`")
  expect_error(bad_pdqr / 1, "`e1`")
})

test_that("Ops.pdqr works in case of comparison", {
  d_dirac <- form_retype(d_fin, "infin", method = "dirac")
  d_infin_2 <- new_d(data.frame(x = 0:1, y = c(1, 1)), "infin")
  num <- 3
  num_d <- new_d(num, "fin")

  # `>=`
  expect_equal_x_tbl(d_fin >= num, form_geq(d_fin, num_d))
  expect_equal_x_tbl(num >= d_infin, form_geq(num_d, d_infin))
  expect_equal_x_tbl(d_infin >= d_infin_2, form_geq(d_infin, d_infin_2))
  expect_equal_x_tbl(d_infin >= d_dirac, form_geq(d_infin, d_dirac))

  # `>`
  expect_equal_x_tbl(d_fin > num, form_greater(d_fin, num_d))
  expect_equal_x_tbl(num > d_infin, form_greater(num_d, d_infin))
  expect_equal_x_tbl(d_infin > d_infin_2, form_greater(d_infin, d_infin_2))
  expect_equal_x_tbl(d_infin > d_dirac, form_greater(d_infin, d_dirac))

  # `<=`
  expect_equal_x_tbl(d_fin <= num, form_leq(d_fin, num_d))
  expect_equal_x_tbl(num <= d_infin, form_leq(num_d, d_infin))
  expect_equal_x_tbl(d_infin <= d_infin_2, form_leq(d_infin, d_infin_2))
  expect_equal_x_tbl(d_infin <= d_dirac, form_leq(d_infin, d_dirac))

  # `<`
  expect_equal_x_tbl(d_fin < num, form_less(d_fin, num_d))
  expect_equal_x_tbl(num < d_infin, form_less(num_d, d_infin))
  expect_equal_x_tbl(d_infin < d_infin_2, form_less(d_infin, d_infin_2))
  expect_equal_x_tbl(d_infin < d_dirac, form_less(d_infin, d_dirac))

  # `==`
  expect_equal_x_tbl(d_fin == num, form_equal(d_fin, num_d))
  expect_equal_x_tbl(num == d_infin, form_equal(num_d, d_infin))
  expect_equal_x_tbl(d_infin == d_infin_2, form_equal(d_infin, d_infin_2))
  expect_equal_x_tbl(d_infin == d_dirac, form_equal(d_infin, d_dirac))

  # `!=`
  expect_equal_x_tbl(d_fin != num, form_not_equal(d_fin, num_d))
  expect_equal_x_tbl(num != d_infin, form_not_equal(num_d, d_infin))
  expect_equal_x_tbl(d_infin != d_infin_2, form_not_equal(d_infin, d_infin_2))
  expect_equal_x_tbl(d_infin != d_dirac, form_not_equal(d_infin, d_dirac))
})

test_that("Ops.pdqr validates input in case of comparison", {
  # `>=`
  expect_error(bad_pdqr >= 1, "`e1`")
  expect_error(1 >= bad_pdqr, "`e2`")
  expect_error(bad_pdqr >= bad_pdqr_2, "`e1`.*pdqr-function.*number")
  expect_error(d_fin >= bad_pdqr_2, "`e2`.*pdqr-function.*number")
  expect_error("a" >= d_fin, "`e1`.*pdqr-function.*number")
  expect_error(d_fin >= "a", "`e2`.*pdqr-function.*number")

  `>`
  expect_error(bad_pdqr > 1, "`e1`")
  expect_error(1 > bad_pdqr, "`e2`")
  expect_error(bad_pdqr > bad_pdqr_2, "`e1`.*pdqr-function.*number")
  expect_error(d_fin > bad_pdqr_2, "`e2`.*pdqr-function.*number")
  expect_error("a" > d_fin, "`e1`.*pdqr-function.*number")
  expect_error(d_fin > "a", "`e2`.*pdqr-function.*number")

  `<=`
  expect_error(bad_pdqr <= 1, "`e1`")
  expect_error(1 <= bad_pdqr, "`e2`")
  expect_error(bad_pdqr <= bad_pdqr_2, "`e1`.*pdqr-function.*number")
  expect_error(d_fin <= bad_pdqr_2, "`e2`.*pdqr-function.*number")
  expect_error("a" <= d_fin, "`e1`.*pdqr-function.*number")
  expect_error(d_fin <= "a", "`e2`.*pdqr-function.*number")

  `<`
  expect_error(bad_pdqr < 1, "`e1`")
  expect_error(1 < bad_pdqr, "`e2`")
  expect_error(bad_pdqr < bad_pdqr_2, "`e1`.*pdqr-function.*number")
  expect_error(d_fin < bad_pdqr_2, "`e2`.*pdqr-function.*number")
  expect_error("a" < d_fin, "`e1`.*pdqr-function.*number")
  expect_error(d_fin < "a", "`e2`.*pdqr-function.*number")

  `==`
  expect_error(bad_pdqr == 1, "`e1`")
  expect_error(1 == bad_pdqr, "`e2`")
  expect_error(bad_pdqr == bad_pdqr_2, "`e1`.*pdqr-function.*number")
  expect_error(d_fin == bad_pdqr_2, "`e2`.*pdqr-function.*number")
  expect_error("a" == d_fin, "`e1`.*pdqr-function.*number")
  expect_error(d_fin == "a", "`e2`.*pdqr-function.*number")

  `!=`
  expect_error(bad_pdqr != 1, "`e1`")
  expect_error(1 != bad_pdqr, "`e2`")
  expect_error(bad_pdqr != bad_pdqr_2, "`e1`.*pdqr-function.*number")
  expect_error(d_fin != bad_pdqr_2, "`e2`.*pdqr-function.*number")
  expect_error("a" != d_fin, "`e1`.*pdqr-function.*number")
  expect_error(d_fin != "a", "`e2`.*pdqr-function.*number")
})

test_that("Ops.pdqr works in case of logical AND/OR", {
  cur_d_fin <- new_d(
    data.frame(x = c(-1, 0, 1), prob = c(0.3, 0.2, 0.5)), "fin"
  )

  # `&`
  expect_prob_true(cur_d_fin & cur_d_fin, 0.5*0.5)
  expect_prob_true(cur_d_fin & 1, 0.5)
  expect_prob_true(2 & cur_d_fin, 0)
  expect_prob_true(cur_d_fin & 0, 0)

    # Probability of `d_infin` being 1 is always 0 (i.e. it is always
    # "`FALSE`"), so presence of "infin" terms always results into 0
  expect_prob_true(d_infin & cur_d_fin, 0)
  expect_prob_true(d_infin & d_infin, 0)

  # `|`
  expect_prob_true(cur_d_fin | cur_d_fin, 1 - (1-0.5)*(1-0.5))
  expect_prob_true(cur_d_fin | 0, 0.5)
  expect_prob_true(0 | cur_d_fin, 0.5)
  expect_prob_true(cur_d_fin | 2, 0.5)

    # Probability of `d_infin` being 1 is always 0 (i.e. it is always
    # "`FALSE`"), so presence of "infin" terms is irrelevant
  expect_prob_true(d_infin | cur_d_fin, 0.5)
  expect_prob_true(d_infin | d_infin, 0)
})

test_that("Ops.pdqr validates input in case of logical AND/OR", {
  # `&`
  expect_error(bad_pdqr & 1, "`e1`")
  expect_error(1 & bad_pdqr, "`e2`")
  expect_error(bad_pdqr & bad_pdqr_2, "`e1`.*pdqr-function.*number")
  expect_error(d_fin & bad_pdqr_2, "`e2`.*pdqr-function.*number")
  expect_error("a" & d_fin, "`e1`.*pdqr-function.*number")
  expect_error(d_fin & "a", "`e2`.*pdqr-function.*number")

  # `|`
  expect_error(bad_pdqr | 1, "`e1`")
  expect_error(1 | bad_pdqr, "`e2`")
  expect_error(bad_pdqr | bad_pdqr_2, "`e1`.*pdqr-function.*number")
  expect_error(d_fin | bad_pdqr_2, "`e2`.*pdqr-function.*number")
  expect_error("a" | d_fin, "`e1`.*pdqr-function.*number")
  expect_error(d_fin | "a", "`e2`.*pdqr-function.*number")
})

test_that("Ops.pdqr validates input", {
  expect_error(bad_pdqr + d_fin, "`e1`")
  expect_error(d_fin - bad_pdqr_2, "`e2`")
  expect_error(bad_pdqr * bad_pdqr_2, "`e1`")
  expect_error(bad_pdqr %% bad_pdqr_2, "`e1`")
})


# Summary.pdqr ------------------------------------------------------------
test_that("Summary.pdqr works", {
  expect_distr_fun(min(p_fin), "p", "fin")
  expect_distr_fun(max(p_fin, p_fin), "p", "fin")
  expect_distr_fun(sum(q_custom, q_infin, q_infin), "q", "infin")
  expect_distr_fun(prod(r_custom, r_custom, na.rm = TRUE), "r", "infin")
})

test_that("Summary.pdqr uses options", {
  # Option for `n_sample`
  op <- options(pdqr.group_gen.n_sample = 1)
  min_fin <- min(d_fin, d_fin)
  expect_true(nrow(meta_x_tbl(min_fin)) == 1)
  options(op)

  # Options for `args_new` and support repair method
  op <- options(
    pdqr.group_gen.args_new = list(n = 2),
    pdqr.group_gen.repair_supp_method = NULL
  )
  min_infin <- min(d_infin, d_infin)
  expect_true(nrow(meta_x_tbl(min_infin)) == 2)
  options(op)
})

test_that("Summary.pdqr repairs support in general cases", {
  # skip_on_cran()

  d_unif <- make_unif(0, 1)
  d_unif_1 <- make_unif(-1, 1.5)

  # `sum`
  expect_equal(meta_support(sum(d_unif, d_unif, d_unif_1)), c(-1, 3.5))

  # `prod`
  expect_equal(meta_support(prod(d_unif, d_unif_1)), c(-1, 1.5))
  expect_equal(meta_support(prod(d_unif)), c(0, 1))

  # `min`
  expect_equal(meta_support(min(d_unif, d_unif_1)), c(-1, 1))

  # `max`
  expect_equal(meta_support(max(d_unif, d_unif_1)), c(0, 1.5))
})

test_that("Summary.pdqr throws error on `range()`", {
  expect_error(range(p_fin, p_fin), "range.*two.*numbers")
})


# math_pdqr_impl ----------------------------------------------------------
# Tested in `Math.pdqr`


# math_abs ----------------------------------------------------------------
# Tested in `Math.pdqr`


# math_sign ---------------------------------------------------------------
# Tested in `Math.pdqr`


# reflect_pdqr_around_zero ------------------------------------------------
# Tested in `Ops.pdqr`


# negate_pdqr -------------------------------------------------------------
# Tested in `Ops.pdqr`


# is_ops_linear -----------------------------------------------------------
# Tested in `Ops.pdqr`


# ops_linear --------------------------------------------------------------
# Tested in `Ops.pdqr`


# ops_compare -------------------------------------------------------------
# Tested in `Ops.pdqr`


# ensure_pdqr_functions ---------------------------------------------------
# Tested in `Ops.pdqr`


# repair_group_gen_support ------------------------------------------------
# Tested in methods for group generics


# repair_supp_periodic ----------------------------------------------------
# Tested in `repair_group_gen_support()`


# is_periodically_inside --------------------------------------------------
test_that("is_periodically_inside works", {
  expect_true(is_periodically_inside(1, interval = c(0, 1.5), period = 2))
  expect_true(is_periodically_inside(1, interval = c(0, 1), period = 2))
  expect_true(is_periodically_inside(-0.5, interval = c(0, 1.5), period = 2))
  expect_false(is_periodically_inside(-0.5, interval = c(0, 1.5), period = 20))
  expect_false(is_periodically_inside(1.51, interval = c(0, 1.5), period = 1.6))
})


# repair_supp_monotone ----------------------------------------------------
# Tested in `repair_group_gen_support()`


# repair_supp_cosh --------------------------------------------------------
# Tested in `repair_group_gen_support()`


# repair_supp_subtraction -------------------------------------------------
# Tested in `repair_group_gen_support()


# repair_supp_multiplication ----------------------------------------------
# Tested in `repair_group_gen_support()


# repair_supp_division ----------------------------------------------------
# Tested in `repair_group_gen_support()


# repair_supp_inverse -----------------------------------------------------
# Tested in `repair_group_gen_support()


# repair_supp_sum ---------------------------------------------------------
# Tested in `repair_group_gen_support()


# repair_supp_prod --------------------------------------------------------
# Tested in `repair_group_gen_support()


# repair_supp_min ---------------------------------------------------------
# Tested in `repair_group_gen_support()


# repair_supp_max ---------------------------------------------------------
# Tested in `repair_group_gen_support()


# simulate_repair_supp ----------------------------------------------------
# Tested in `repair_group_gen_support()
