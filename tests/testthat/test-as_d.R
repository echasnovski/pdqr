context("test-as_d")

set.seed(2222)


# Computation of d-functions ----------------------------------------------
d_norm <-        as_d(fam_norm$d,        fam_norm$support)
d_norm_2 <-      as_d(fam_norm_2$d,      fam_norm_2$support)
d_exp <-         as_d(fam_exp$d,         fam_exp$support)
d_exp_rev <-     as_d(fam_exp_rev$d,     fam_exp_rev$support)
d_beta <-        as_d(fam_beta$d,        fam_beta$support)
d_beta_inf <-    as_d(fam_beta_inf$d,    fam_beta_inf$support)
d_beta_midinf <- as_d(fam_beta_midinf$d, fam_beta_midinf$support)
d_chisq <-       as_d(fam_chisq$d,       fam_chisq$support)
d_chisq_inf <-   as_d(fam_chisq_inf$d,   fam_chisq_inf$support)
d_mix_norm <-    as_d(fam_mix_norm$d,    fam_mix_norm$support)
d_mix_unif <-    as_d(fam_mix_unif$d,    fam_mix_unif$support)
d_unif <-        as_d(fam_unif$d,        fam_unif$support)


# as_d --------------------------------------------------------------------
# Tested in its methods


# as_d.default ------------------------------------------------------------
test_that("as_d.default results in good approximations of input", {
  expect_close_f(d_norm, fam_norm$d, fam_norm$grid)
  expect_close_f(d_norm_2, fam_norm_2$d, fam_norm_2$grid)
  expect_close_f(d_exp, fam_exp$d, fam_exp$grid)
  expect_close_f(d_exp_rev, fam_exp_rev$d, fam_exp_rev$grid)
  expect_close_f(d_beta, fam_beta$d, fam_beta$grid)

  # `max()` isn't used because of infinite density
  # Overall accuracy isn't great because infinite density is cut which decreases
  # total probability. This causes overall density to increase.
  expect_close_f(
    d_beta_inf, fam_beta_inf$d, fam_beta_inf$grid,
    stat_f = quan90, thres = 5e-2
  )
  expect_not_close_f(
    d_beta_inf, fam_beta_inf$d, fam_beta_inf$grid,
    stat_f = min, thres = 1e-2
  )
  expect_close_f(
    d_beta_midinf, fam_beta_midinf$d, fam_beta_midinf$grid,
    stat_f = quan90, thres = 4e-2
  )
  expect_not_close_f(
    d_beta_inf, fam_beta_inf$d, fam_beta_inf$grid,
    stat_f = min, thres = 1e-2
  )

  expect_close_f(d_chisq, fam_chisq$d, fam_chisq$grid)

  # `max()` isn't used because of infinite density
  expect_close_f(
    d_chisq_inf, fam_chisq_inf$d, fam_chisq_inf$grid,
    stat_f = quan999, thres = 5e-2
  )

  expect_close_f(d_mix_norm, fam_mix_norm$d, fam_mix_norm$grid, thres = 2e-6)

  # `max()` isn't used because of density discontinuity
  expect_close_f(
    d_mix_unif, fam_mix_unif$d, fam_mix_unif$grid,
    stat_f = quan999, thres = 1e-4
  )
  expect_close_f(
    d_unif, fam_unif$d, fam_unif$grid,
    stat_f = quan999, thres = 1e-4
  )
})

test_that("as_d.default uses `n_grid` argument", {
  expect_not_close_f(
    as_d(fam_norm_2$d, fam_norm_2$support, n_grid = 10),
    fam_norm_2$d, fam_norm_2$grid,
    thres = 1e-2
  )
})

test_that("as_d.default properly adjusts to support", {
  supp <- c(-0.5, 1.5)
  out_d <- as_d(fam_norm[["d"]], supp)
  ref_d <- function(x) {fam_norm[["d"]](x) / diff(fam_norm[["p"]](supp))}
  expect_close_f(
    out_d, ref_d, seq(supp[1], supp[2], length.out = 1e5)
  )

  # Output integrates to 1
  integral <- stats::integrate(out_d, supp[1], supp[2])
  expect_true(abs(round(integral[["value"]], 8) - 1) <= integral[["abs.error"]])
})

test_that("as.d.default properly imputes infinity values", {
  # Imputed value should be a maximum of linear extrapolations based on two
  # nearest left and two nearest right non-infinite values

  d_beta <- as_d(fam_beta_inf[["d"]], c(-1, 1))
  d_x_tbl <- meta(d_beta, "x_tbl")
  d_x <- d_x_tbl[["x"]]
  d_y <- d_x_tbl[["y"]]

  # Density goes to infinity at 0 and 1
  zero_ind <- which(is_near(d_x_tbl[["x"]], 0))
  expect_equal(
    d_y[zero_ind],
    extrap_lin(
      x_1 = d_x[zero_ind+1], x_2 = d_x[zero_ind+2],
      y_1 = d_y[zero_ind+1], y_2 = d_y[zero_ind+2],
      x_target = d_x[zero_ind]
    )
  )

  # Imputation should deal with value being on edge of support
  one_ind <- which(is_near(d_x_tbl[["x"]], 1))
  expect_equal(
    d_y[one_ind],
    extrap_lin(
      x_1 = d_x[one_ind-2], x_2 = d_x[one_ind-1],
      y_1 = d_y[one_ind-2], y_2 = d_y[one_ind-1],
      x_target = d_x[one_ind]
    )
  )
})

test_that("as_d.default throws errors on bad input", {
  expect_error(as_d(fam_norm$d), "supply.*support")
  expect_error(as_d("a", c(0, 1)), "`f`.*function")
  expect_error(as_d(fam_norm$d, c(2, 1)), "`support`")
  expect_error(
    as_d(fam_norm$d, fam_norm$support, n_grid = "a"), "`n_grid`.*number"
  )
  expect_error(
    as_d(fam_norm$d, fam_norm$support, n_grid = 2), "`n_grid`.*more.*2"
  )
})


# as_d.pdqr ---------------------------------------------------------------
test_that('as_d.pdqr works with "p"', {
  expect_equal_distr(as_d(p_raw), d_raw, grid = c(x_raw_vec_ext, x_raw_vec))
  expect_equal_distr(as_d(p_smooth), d_smooth, x_smooth_vec_ext)
})

test_that('as_d.pdqr works with "q"', {
  expect_equal_distr(as_d(q_raw), d_raw, grid = c(x_raw_vec_ext, x_raw_vec))
  expect_equal_distr(as_d(q_smooth), d_smooth, grid = x_smooth_vec_ext)
})

test_that('as_d.pdqr works with "r"', {
  expect_equal_distr(as_d(r_raw), d_raw, grid = c(x_raw_vec_ext, x_raw_vec))
  expect_equal_distr(as_d(r_smooth), d_smooth, grid = x_smooth_vec_ext)
})

test_that("as_d.pdqr throws errors on bad input", {
  expect_error(as_d(structure(user_d, class = c("p", "pdqr"))), "`f`")
})
