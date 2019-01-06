context("test-as_p")

set.seed(1111)


# Computation of p-functions ----------------------------------------------
p_norm <-        as_p(fam_norm$p,        fam_norm$support)
p_norm_2 <-      as_p(fam_norm_2$p,      fam_norm_2$support)
p_exp <-         as_p(fam_exp$p,         fam_exp$support)
p_exp_rev <-     as_p(fam_exp_rev$p,     fam_exp_rev$support)
p_beta <-        as_p(fam_beta$p,        fam_beta$support)
p_beta_inf <-    as_p(fam_beta_inf$p,    fam_beta_inf$support)
p_beta_midinf <- as_p(fam_beta_midinf$p, fam_beta_midinf$support)
p_chisq <-       as_p(fam_chisq$p,       fam_chisq$support)
p_chisq_inf <-   as_p(fam_chisq_inf$p,   fam_chisq_inf$support)
p_mix_norm <-    as_p(fam_mix_norm$p,    fam_mix_norm$support)
p_mix_unif <-    as_p(fam_mix_unif$p,    fam_mix_unif$support)
p_unif <-        as_p(fam_unif$p,        fam_unif$support)


# as_p --------------------------------------------------------------------
# Tested in its methods


# as_p.default ------------------------------------------------------------
test_that("as_p.default results in good approximations of input", {
  expect_close_f(p_norm, fam_norm$p, fam_norm$grid)
  expect_close_f(p_norm_2, fam_norm_2$p, fam_norm_2$grid)
  expect_close_f(p_exp, fam_exp$p, fam_exp$grid)
  expect_close_f(p_exp_rev, fam_exp_rev$p, fam_exp_rev$grid)
  expect_close_f(p_beta, fam_beta$p, fam_beta$grid)
  expect_close_f(p_beta_inf, fam_beta_inf$p, fam_beta_inf$grid, thres = 1e-2)
  expect_close_f(
    p_beta_midinf, fam_beta_midinf$p, fam_beta_midinf$grid,
    thres = 1e-4
  )
  expect_close_f(p_chisq, fam_chisq$p, fam_chisq$grid)
  expect_close_f(p_chisq_inf, fam_chisq_inf$p, fam_chisq_inf$grid, thres = 1e-2)
  expect_close_f(p_mix_norm, fam_mix_norm$p, fam_mix_norm$grid, thres = 2e-6)
  expect_close_f(p_mix_unif, fam_mix_unif$p, fam_mix_unif$grid, thres = 1e-4)
  expect_close_f(p_unif, fam_unif$p, fam_unif$grid, thres = 1e-4)
})

test_that("as_p.default output approximates density after `as_d()`", {
  expect_close_f(as_d(p_norm), fam_norm$d, fam_norm$grid)
  expect_close_f(as_d(p_norm_2), fam_norm_2$d, fam_norm_2$grid, thres = 2e-6)
  expect_close_f(as_d(p_exp), fam_exp$d, fam_exp$grid, thres = 3e-6)
  expect_close_f(as_d(p_exp_rev), fam_exp_rev$d, fam_exp_rev$grid, thres = 3e-6)
  expect_close_f(as_d(p_beta), fam_beta$d, fam_beta$grid)
  expect_close_f(
    as_d(p_beta_inf), fam_beta_inf$d, fam_beta_inf$grid,
    stat_f = quan90, thres = 2e-2
  )
  expect_close_f(
    as_d(p_beta_midinf), fam_beta_midinf$d, fam_beta_midinf$grid,
    stat_f = quan99, thres = 3e-4
  )
  expect_close_f(as_d(p_chisq), fam_chisq$d, fam_chisq$grid)
  expect_close_f(
    as_d(p_chisq_inf), fam_chisq_inf$d, fam_chisq_inf$grid,
    stat_f = quan999, thres = 2e-2
  )
  expect_close_f(
    as_d(p_mix_norm), fam_mix_norm$d, fam_mix_norm$grid,
    thres = 5e-6
  )
  expect_close_f(
    as_d(p_mix_unif), fam_mix_unif$d, fam_mix_unif$grid,
    stat_f = quan999
  )
  expect_close_f(
    as_d(p_unif), fam_unif$d, fam_unif$grid,
    stat_f = quan999
  )
})

test_that("as_p.default output approximates quantile function after `as_q()`", {
  expect_close_f(as_q(p_norm), fam_norm$q, p_seq_btail, thres = 4e-6)
  expect_close_f(as_q(p_norm_2), fam_norm_2$q, p_seq_btail, thres = 9e-6)
  expect_close_f(as_q(p_exp), fam_exp$q, p_seq_rtail, thres = 3e-6)
  expect_close_f(as_q(p_exp_rev), fam_exp_rev$q, p_seq_ltail, thres = 3e-6)
  expect_close_f(as_q(p_beta), fam_beta$q, p_seq)
  expect_close_f(as_q(p_beta_inf), fam_beta_inf$q, p_seq, thres = 6e-3)
  expect_close_f(as_q(p_beta_midinf), fam_beta_midinf$q, p_seq, thres = 9e-5)
  expect_close_f(as_q(p_chisq), fam_chisq$q, p_seq_rtail, thres = 2e-5)
  expect_close_f(
    as_q(p_chisq_inf), fam_chisq_inf$q, p_seq_rtail,
    thres = 2e-2
  )
  expect_close_f(as_q(p_mix_norm), fam_mix_norm$q, p_seq_btail, thres = 5e-2)

  # `max()` isn't used because of zero density segment (q-function
  # discontinuity)
  expect_close_f(as_q(p_mix_unif), fam_mix_unif$q, p_seq, stat_f = quan999)

  # Too big threshold because "x_tbl" metadata contains too small `y` values
  # with cumulative probability 0.
  expect_close_f(as_q(p_unif), fam_unif$q, p_seq, thres = 1)
})

test_that("as_p.default uses `n_grid` argument", {
  expect_not_close_f(
    as_p(fam_norm_2$p, fam_norm_2$support, n_grid = 10),
    fam_norm_2$p, fam_norm_2$grid,
    thres = 1e-2
  )
})

test_that("as_p.default properly adjusts to support", {
  supp <- c(-0.5, 1.5)
  out_p <- as_p(fam_norm[["p"]], supp)
  # This assumes that `as_d()` correctly adjusts to support and uses
  # `as_p.pdqr()`.
  ref_p <- as_p(as_d(fam_norm[["d"]], supp))
  expect_close_f(
    out_p, ref_p, seq(supp[1], supp[2], length.out = 1e5)
  )
})

test_that("as_p.default throws errors on bad input", {
  expect_error(as_p(fam_norm$p), "supply.*support")
  expect_error(as_p("a", c(0, 1)), "`f`.*function")
  expect_error(as_p(fam_norm$p, c(2, 1)), "`support`")
  expect_error(
    as_p(fam_norm$p, fam_norm$support, n_grid = "a"), "`n_grid`.*number"
  )
  expect_error(
    as_p(fam_norm$p, fam_norm$support, n_grid = 2), "`n_grid`.*more.*2"
  )
})


# as_p.pdqr ---------------------------------------------------------------
test_that('as_p.pdqr works with "p"', {
  expect_equal_distr(as_p(p_raw), p_raw, grid = c(x_raw_vec_ext, x_raw_vec))
  expect_equal_distr(as_p(p_smooth), p_smooth, grid = x_smooth_vec_ext)
})

test_that('as_p.pdqr works with "d"', {
  expect_equal_distr(as_p(d_raw), p_raw, grid = c(x_raw_vec_ext, x_raw_vec))
  expect_equal_distr(as_p(d_smooth), p_smooth, grid = x_smooth_vec_ext)
})

test_that('as_p.pdqr works with "q"', {
  expect_equal_distr(as_p(q_raw), p_raw, grid = c(x_raw_vec_ext, x_raw_vec))
  expect_equal_distr(as_p(q_smooth), p_smooth, grid = x_smooth_vec_ext)
})

test_that('as_p.pdqr works with "r"', {
  expect_equal_distr(as_p(r_raw), p_raw, grid = c(x_raw_vec_ext, x_raw_vec))
  expect_equal_distr(as_p(r_smooth), p_smooth, grid = x_smooth_vec_ext)
})

test_that("as_p.pdqr throws errors on bad input", {
  expect_error(as_p(structure(user_p, class = c("d", "pdqr"))), "`f`")
})
