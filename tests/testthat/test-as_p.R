context("test-as_p")

set.seed(1111)


# as_p --------------------------------------------------------------------
# Tested in its methods


# as_p.default ------------------------------------------------------------
test_that("as_p.default results in good approximations of input", {
  expect_approx(as_p, fam_norm, "p")
  expect_approx(as_p, fam_norm_2, "p")
  expect_approx(as_p, fam_exp, "p")
  expect_approx(as_p, fam_exp_rev, "p")
  expect_approx(as_p, fam_beta, "p")
  expect_approx(as_p, fam_beta_inf, "p", thres = 1e-2)
  expect_approx(as_p, fam_chisq, "p")
  expect_approx(as_p, fam_chisq_inf, "p", thres = 1e-2)
  expect_approx(as_p, fam_mix_norm, "p", thres = 2e-6)
  expect_approx(as_p, fam_mix_unif, "p", thres = 1e-4)
})

test_that("as_p.default uses `n_grid` argument", {
  expect_not_approx(as_p, fam_norm_2, "p", thres = 1e-2, n_grid = 10)
})

test_that("as_p.default properly adjusts to support", {
  supp <- c(-0.5, 1.5)
  out_p <- as_p(fam_norm[["p"]], supp)
  # This assumes that `as_d()` correctly adjusts to support and uses
  # `as_p.pdqr()`.
  ref_p <- as_p(as_d(fam_norm[["d"]], supp))
  expect_equal_on_grid(
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
    as_p(fam_norm$p, fam_norm$support, n_grid = 1), "`n_grid`.*more.*2"
  )
})


# as_p.pdqr ---------------------------------------------------------------
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
