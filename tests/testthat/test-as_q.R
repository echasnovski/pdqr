context("test-as_q")

set.seed(3333)


# Input data --------------------------------------------------------------
# **Note** that step is ~1e-4, but in `as_d()` and `as_p()` tests it is ~1e-5.
# This is done because of slower `as_q.default()` performance.
p_seq <- seq(0, 1, length.out = 1e4)

# Sequences of probabilities used to test quantile functions that go to infinity
# on one or both edges. Corresponds to unbounded (from left and/or right)
# support, i.e. left, right, or both tails.
p_isnt_small <- p_seq >= 0.001
p_isnt_big <- p_seq <= 0.999
p_seq_ltail <- p_seq[p_isnt_small]
p_seq_rtail <- p_seq[p_isnt_big]
p_seq_btail <- p_seq[p_isnt_small & p_isnt_big]


# as_q --------------------------------------------------------------------
# Tested in its methods


# as_q.default ------------------------------------------------------------
test_that("as_q.default results in good approximations of input", {
  # General tendency: maximum error is rather big but overall is quite good
  # To test it, run it with higher `n_grid` value
  expect_approx(as_q, fam_norm, "q", grid = p_seq_btail, thres = 6e-5)
  expect_approx(as_q, fam_norm_2, "q", grid = p_seq_btail, thres = 5e-6)
  expect_approx(as_q, fam_exp, "q", grid = p_seq_rtail, thres = 6e-5)
  expect_approx(as_q, fam_exp_rev, "q", grid = p_seq_ltail, thres = 6e-5)
  expect_approx(as_q, fam_beta, "q", grid = p_seq, thres = 4e-5)
  expect_approx(as_q, fam_beta_inf, "q", grid = p_seq, thres = 6e-3)
  expect_approx(as_q, fam_chisq, "q", grid = p_seq_rtail, thres = 2e-4)
  expect_approx(as_q, fam_chisq_inf, "q", grid = p_seq_rtail, thres = 1.5e-2)
  expect_approx(as_q, fam_mix_norm, "q", grid = p_seq_btail, thres = 2e-3)

  # `max()` isn't used because of density discontinuity
  expect_approx(as_q, fam_mix_unif, "q", grid = p_seq, stat_f = quan999)

  expect_approx(as_q, fam_unif, "q", grid = p_seq, thres = 5e-4)
})

test_that("as_q.default uses `n_grid` argument", {
  expect_not_approx(
    as_q, fam_norm_2, "q",
    grid = p_seq, thres = 1e-2, n_grid = 10
  )
})

test_that("as_q.default properly adjusts to support", {
  supp <- c(-0.5, 1.5)
  out_q <- as_q(fam_norm[["q"]], supp)
  # This assumes that `as_d()` correctly adjusts to support and uses
  # `as_q.pdqr()`.
  ref_q <- as_q(as_d(fam_norm[["d"]], supp))
  expect_equal_on_grid(out_q, ref_q, p_seq, thres = 1e-7)
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
