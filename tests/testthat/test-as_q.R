context("test-as_q")

set.seed(3333)


# Computation of q-functions ----------------------------------------------
q_norm <-        as_q(fam_norm$q,        fam_norm$support)
q_norm_2 <-      as_q(fam_norm_2$q,      fam_norm_2$support)
q_exp <-         as_q(fam_exp$q,         fam_exp$support)
q_exp_rev <-     as_q(fam_exp_rev$q,     fam_exp_rev$support)
q_beta <-        as_q(fam_beta$q,        fam_beta$support)
q_beta_inf <-    as_q(fam_beta_inf$q,    fam_beta_inf$support)
q_beta_midinf <- as_q(fam_beta_midinf$q, fam_beta_midinf$support)
q_chisq <-       as_q(fam_chisq$q,       fam_chisq$support)
q_chisq_inf <-   as_q(fam_chisq_inf$q,   fam_chisq_inf$support)
q_mix_norm <-    as_q(fam_mix_norm$q,    fam_mix_norm$support)
q_mix_unif <-    as_q(fam_mix_unif$q,    fam_mix_unif$support)
q_unif <-        as_q(fam_unif$q,        fam_unif$support)


# as_q --------------------------------------------------------------------
# Tested in its methods


# as_q.default ------------------------------------------------------------
test_that("as_q.default results in good approximations of input", {
  # General tendency: maximum error is rather big but overall is quite good
  # To test it, run it with higher `n_grid` value
  expect_close_f(q_norm, fam_norm$q, p_seq_btail, thres = 6e-5)
  expect_close_f(q_norm_2, fam_norm_2$q, p_seq_btail, thres = 5e-6)
  expect_close_f(q_exp, fam_exp$q, p_seq_rtail, thres = 6e-5)
  expect_close_f(q_exp_rev, fam_exp_rev$q, p_seq_ltail, thres = 6e-5)
  expect_close_f(q_beta, fam_beta$q, p_seq, thres = 4e-5)
  expect_close_f(q_beta_inf, fam_beta_inf$q, p_seq, thres = 6e-3)
  expect_close_f(q_beta_midinf, fam_beta_midinf$q, p_seq, thres = 1e-4)
  expect_close_f(q_chisq, fam_chisq$q, p_seq_rtail, thres = 2e-4)
  expect_close_f(q_chisq_inf, fam_chisq_inf$q, p_seq_rtail, thres = 1.5e-2)
  expect_close_f(q_mix_norm, fam_mix_norm$q, p_seq_btail, thres = 2e-3)

  # `max()` isn't used because of zero density segment (q-function
  # discontinuity)
  expect_close_f(q_mix_unif, fam_mix_unif$q, p_seq, stat_f = quan999)

  expect_close_f(q_unif, fam_unif$q, p_seq, thres = 5e-4)
})

test_that("as_q.default uses `n_grid` argument", {
  expect_not_close_f(
    as_q(fam_norm_2$q, fam_norm_2$support, n_grid = 10),
    fam_norm_2$q, p_seq,
    thres = 1e-2
  )
})

test_that("as_q.default properly adjusts to support", {
  supp <- c(-0.5, 1.5)
  out_q <- as_q(fam_norm[["q"]], supp)
  # This assumes that `as_d()` correctly adjusts to support and uses
  # `as_q.pdqr()`.
  ref_q <- as_q(as_d(fam_norm[["d"]], supp))
  expect_close_f(out_q, ref_q, p_seq, thres = 1e-7)
})

test_that("as_q.default throws errors on bad input", {
  expect_error(as_q(fam_norm$q), "supply.*support")
  expect_error(as_q("a", c(0, 1)), "`f`.*function")
  expect_error(as_q(fam_norm$q, c(2, 1)), "`support`")
  expect_error(
    as_q(fam_norm$q, fam_norm$support, n_grid = "a"), "`n_grid`.*number"
  )
  expect_error(
    as_q(fam_norm$q, fam_norm$support, n_grid = 2), "`n_grid`.*more.*2"
  )
})


# as_q.pdqr ---------------------------------------------------------------
test_that('as_q.pdqr works with "p"', {
  expect_equal_distr(as_q(p_raw), q_raw, grid = p_vec)
  expect_equal_distr(as_q(p_smooth), q_smooth, grid = p_vec)
})

test_that('as_q.pdqr works with "d"', {
  expect_equal_distr(as_q(d_raw), q_raw, grid = p_vec)
  expect_equal_distr(as_q(d_smooth), q_smooth, grid = p_vec)
})

test_that('as_q.pdqr works with "r"', {
  expect_equal_distr(as_q(r_raw), q_raw, grid = p_vec)
  expect_equal_distr(as_q(r_smooth), q_smooth, grid = p_vec)
})
