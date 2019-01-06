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

d_list <- list(
  d_norm = d_norm,
  d_norm_2 = d_norm_2,
  d_exp = d_exp,
  d_exp_rev = d_exp_rev,
  d_beta = d_beta,
  d_beta_inf = d_beta_inf,
  d_beta_midinf = d_beta_midinf,
  d_chisq = d_chisq,
  d_chisq_inf = d_chisq_inf,
  d_mix_norm = d_mix_norm,
  d_mix_unif = d_mix_unif,
  d_unif = d_unif
)


# as_d --------------------------------------------------------------------
# Tested in its methods


# as_d.default ------------------------------------------------------------
test_that("as_d.default output approximates CDF after `as_p()`", {
  expect_close_f(as_p(d_norm), fam_norm$p, fam_norm$grid)
  expect_close_f(as_p(d_norm_2), fam_norm_2$p, fam_norm_2$grid)
  expect_close_f(as_p(d_exp), fam_exp$p, fam_exp$grid)
  expect_close_f(as_p(d_exp_rev), fam_exp_rev$p, fam_exp_rev$grid)
  expect_close_f(as_p(d_beta), fam_beta$p, fam_beta$grid)
  expect_close_f(
    as_p(d_beta_inf), fam_beta_inf$p, fam_beta_inf$grid,
    thres = 4e-2
  )
  expect_close_f(
    as_p(d_beta_midinf), fam_beta_midinf$p, fam_beta_midinf$grid,
    thres = 2e-2
  )
  expect_close_f(as_p(d_chisq), fam_chisq$p, fam_chisq$grid)
  expect_close_f(
    as_p(d_chisq_inf), fam_chisq_inf$p, fam_chisq_inf$grid,
    thres = 2e-2
  )
  expect_close_f(as_p(d_mix_norm), fam_mix_norm$p, fam_mix_norm$grid)
  expect_close_f(
    as_p(d_mix_unif), fam_mix_unif$p, fam_mix_unif$grid,
    thres = 1e-4
  )
  expect_close_f(as_p(d_unif), fam_unif$p, fam_unif$grid, thres = 1e-4)
})

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

test_that("as_d.default output approximates quantile function after `as_q()`", {
  expect_close_f(as_q(d_norm), fam_norm$q, p_seq_btail, thres = 6e-5)
  expect_close_f(as_q(d_norm_2), fam_norm_2$q, p_seq_btail, thres = 1e-5)
  expect_close_f(as_q(d_exp), fam_exp$q, p_seq_rtail, thres = 5e-6)
  expect_close_f(as_q(d_exp_rev), fam_exp_rev$q, p_seq_ltail, thres = 5e-6)
  expect_close_f(as_q(d_beta), fam_beta$q, p_seq, thres = 2e-3)
  expect_close_f(as_q(d_beta_inf), fam_beta_inf$q, p_seq, thres = 2.5e-2)
  expect_close_f(as_q(d_beta_midinf), fam_beta_midinf$q, p_seq, thres = 1e-2)
  expect_close_f(as_q(d_chisq), fam_chisq$q, p_seq_rtail, thres = 5e-5)
  expect_close_f(
    as_q(d_chisq_inf), fam_chisq_inf$q, p_seq_rtail,
    thres = 4e-2
  )
  expect_close_f(as_q(d_mix_norm), fam_mix_norm$q, p_seq_btail, thres = 5e-2)

  # `max()` isn't used because of zero density segment (q-function
  # discontinuity)
  expect_close_f(as_q(d_mix_unif), fam_mix_unif$q, p_seq, thres = 4e-4)

  expect_close_f(as_q(d_unif), fam_unif$q, p_seq, thres = 9e-5)
})

test_that("as_d.default output approximates random-gen-func after `as_r()`", {
  expect_close_r_f(
    as_r(d_norm), fam_norm$r,
    mean_thres = 1e-2, sd_thres = 4e-2
  )
  expect_close_r_f(
    as_r(d_norm_2), fam_norm_2$r,
    mean_thres = 1e-3, sd_thres = 5e-3
  )
  expect_close_r_f(
    as_r(d_exp), fam_exp$r,
    mean_thres = 3e-2, sd_thres = 1e-2
  )
  expect_close_r_f(
    as_r(d_exp_rev), fam_exp_rev$r,
    mean_thres = 1e-2, sd_thres = 5e-2
  )
  expect_close_r_f(
    as_r(d_beta), fam_beta$r,
    mean_thres = 4e-3, sd_thres = 4e-3
  )
  expect_close_r_f(
    as_r(d_beta_inf), fam_beta_inf$r,
    mean_thres = 5e-2, sd_thres = 5e-3
  )
  expect_close_r_f(
    as_r(d_beta_midinf), fam_beta_midinf$r,
    mean_thres = 2e-3, sd_thres = 7e-3
  )
  expect_close_r_f(
    as_r(d_chisq), fam_chisq$r,
    mean_thres = 8e-2, sd_thres = 8e-2
  )
  expect_close_r_f(
    as_r(d_chisq_inf), fam_chisq_inf$r,
    mean_thres = 8e-2, sd_thres = 8e-2
  )
  expect_close_r_f(
    as_r(d_mix_norm), fam_mix_norm$r,
    mean_thres = 5e-2, sd_thres = 6e-2
  )
  expect_close_r_f(
    as_r(d_mix_unif), fam_mix_unif$r,
    mean_thres = 5e-3, sd_thres = 2e-2
  )
  expect_close_r_f(as_r(d_unif), fam_unif$r, mean_thres = 1e-2, sd_thres = 2e-3)
})

test_that("as_d.default output has the same support as was in input", {
  is_equal_supp <- vapply(
    seq_along(d_list), function(i) {
      isTRUE(all.equal(
        meta(d_list[[i]], "support"), fam_list[[i]]$support
      ))
    },
    logical(1)
  )

  expect_equal(is_equal_supp, rep(TRUE, length(d_list)))
})

test_that("as_d.default removes edge `y` with zero density", {
  x_tbl <- meta(d_unif, "x_tbl")
  expect_true(all(x_tbl$y[c(2, nrow(x_tbl)-1)] != 0))
})

test_that("as_d.default uses `n_grid` argument", {
  expect_not_close_f(
    as_d(fam_norm$d, fam_norm$support, n_grid = 10),
    fam_norm$d, fam_norm$grid,
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

test_that("as_d.default throws error if total probability on support is zero", {
  expect_error(as_d(fam_beta$d, c(1.5, 2)), "probability.*positive")
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

test_that('as_d.pdqr works with "d"', {
  expect_equal_distr(as_d(d_raw), d_raw, grid = c(x_raw_vec_ext, x_raw_vec))
  expect_equal_distr(as_d(d_smooth), d_smooth, x_smooth_vec_ext)
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

test_that("as_d.pdqr ensures maximum proper support", {
  input <- p_raw
  attr(input, "meta")[["support"]] <- c(-100, 100)

  expect_equal(meta(as_d(input), "support"), c(-100, 100))
})
