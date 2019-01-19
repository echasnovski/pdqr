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

p_list <- list(
  p_norm = p_norm,
  p_norm_2 = p_norm_2,
  p_exp = p_exp,
  p_exp_rev = p_exp_rev,
  p_beta = p_beta,
  p_beta_inf = p_beta_inf,
  p_beta_midinf = p_beta_midinf,
  p_chisq = p_chisq,
  p_chisq_inf = p_chisq_inf,
  p_mix_norm = p_mix_norm,
  p_mix_unif = p_mix_unif,
  p_unif = p_unif
)


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
  skip_on_cran()

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
  skip_on_cran()

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

  expect_close_f(as_q(p_unif), fam_unif$q, p_seq, thres = 3e-4)
})

test_that("as_p.default output approximates random-gen-func after `as_r()`", {
  skip_on_cran()

  expect_close_r_f(
    as_r(p_norm), fam_norm$r,
    mean_thres = 2e-2, sd_thres = 4e-2
  )
  expect_close_r_f(
    as_r(p_norm_2), fam_norm_2$r,
    mean_thres = 1e-3, sd_thres = 1e-3
  )
  expect_close_r_f(
    as_r(p_exp), fam_exp$r,
    mean_thres = 7e-3, sd_thres = 5e-2
  )
  expect_close_r_f(
    as_r(p_exp_rev), fam_exp_rev$r,
    mean_thres = 3e-2, sd_thres = 3e-2
  )
  expect_close_r_f(
    as_r(p_beta), fam_beta$r,
    mean_thres = 1e-3, sd_thres = 5e-3
  )
  expect_close_r_f(
    as_r(p_beta_inf), fam_beta_inf$r,
    mean_thres = 1e-3, sd_thres = 5e-3
  )
  expect_close_r_f(
    as_r(p_beta_midinf), fam_beta_midinf$r,
    mean_thres = 1e-2, sd_thres = 6e-3
  )
  expect_close_r_f(
    as_r(p_chisq), fam_chisq$r,
    mean_thres = 2e-2, sd_thres = 8e-2
  )
  expect_close_r_f(
    as_r(p_chisq_inf), fam_chisq_inf$r,
    mean_thres = 8e-2, sd_thres = 0.1
  )
  expect_close_r_f(
    as_r(p_mix_norm), fam_mix_norm$r,
    mean_thres = 1e-2, sd_thres = 6e-2
  )
  expect_close_r_f(
    as_r(p_mix_unif), fam_mix_unif$r,
    mean_thres = 1e-3, sd_thres = 2e-2
  )
  expect_close_r_f(as_r(p_unif), fam_unif$r, mean_thres = 1e-2, sd_thres = 1e-2)
})

test_that("as_p.default output has minimum support according to 'x_tbl'", {
  is_equal_supp <- vapply(
    seq_along(p_list), function(i) {
      isTRUE(all.equal(
        pdqr_support(p_list[[i]]), range(pdqr_x_tbl(p_list[[i]])[["x"]])
      ))
    },
    logical(1)
  )

  expect_equal(is_equal_supp, rep(TRUE, length(p_list)))
})

test_that("as_p.default detects support", {
  # Much more tests are done in `detect_support_p`
  p_beta_both <- as_p(fam_beta$p)
  support_both <- pdqr_support(p_beta_both)
  expect_equal(fam_beta$p(support_both), c(0, 1), tolerance = 1e-7)

  p_beta_left <- as_p(fam_beta$p, support = c(NA, 0.7))
  support_left <- pdqr_support(p_beta_left)
  expect_equal(fam_beta$p(support_left[1]), 0, tolerance = 1e-7)

  p_beta_right <- as_p(fam_beta$p, support = c(0.3, NA))
  support_right <- pdqr_support(p_beta_right)
  expect_equal(fam_beta$p(support_right[2]), 1, tolerance = 1e-7)
})

test_that("as_p.default removes edge `y` with zero density", {
  x_tbl <- pdqr_x_tbl(p_unif)
  expect_true(all(x_tbl$y[c(2, nrow(x_tbl)-1)] != 0))
})

test_that("as_p.default uses `n_grid` argument", {
  expect_not_close_f(
    as_p(fam_norm$p, fam_norm$support, n_grid = 10),
    fam_norm$p, fam_norm$grid,
    thres = 1e-2
  )
})

test_that("as_p.default uses `...` to forward arguments to `f`", {
  output_1 <- as_p(punif, support = c(0, 10), max = 10)
  expect_true(output_1(9) > 0)
  output_2 <- as_p(punif, support = NULL, max = 10)
  expect_true(output_2(9) > 0)
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

test_that("as_p.default throws error if total probability on support is zero", {
  expect_error(as_p(fam_beta$p, c(1.5, 2)), "probability.*positive")
})

test_that("as_p.default throws errors on bad input", {
  expect_error(as_p("a", c(0, 1)), "`f`.*function")
  expect_error(as_p(fam_norm$p, c(2, 1)), "`support`")
  expect_error(
    as_p(fam_norm$p, fam_norm$support, n_grid = "a"), "`n_grid`.*number"
  )
  expect_error(
    as_p(fam_norm$p, fam_norm$support, n_grid = 2), "`n_grid`.*more.*2"
  )
})

test_that("as_p.default throws error if detected support isn't proper", {
  expect_error(as_p(fam_beta$p, c(1.5, NA)), "support.*proper")
  expect_error(as_p(fam_beta$p, c(NA, -0.2)), "support.*proper")
})


# as_p.pdqr ---------------------------------------------------------------
test_that('as_p.pdqr works with "p"', {
  expect_equal_distr(as_p(p_fin), p_fin, grid = c(x_fin_vec_ext, x_fin_vec))
  expect_equal_distr(as_p(p_infin), p_infin, grid = x_infin_vec_ext)
})

test_that('as_p.pdqr works with "d"', {
  expect_equal_distr(as_p(d_fin), p_fin, grid = c(x_fin_vec_ext, x_fin_vec))
  expect_equal_distr(as_p(d_infin), p_infin, grid = x_infin_vec_ext)
})

test_that('as_p.pdqr works with "q"', {
  expect_equal_distr(as_p(q_fin), p_fin, grid = c(x_fin_vec_ext, x_fin_vec))
  expect_equal_distr(as_p(q_infin), p_infin, grid = x_infin_vec_ext)
})

test_that('as_p.pdqr works with "r"', {
  expect_equal_distr(as_p(r_fin), p_fin, grid = c(x_fin_vec_ext, x_fin_vec))
  expect_equal_distr(as_p(r_infin), p_infin, grid = x_infin_vec_ext)
})

test_that("as_p.pdqr throws errors on bad input", {
  expect_error(as_p(structure(user_p, class = c("d", "pdqr"))), "`f`")
})


# detect_support_p --------------------------------------------------------
test_that("detect_support_p detects both edges of support", {
  skip_on_cran()

  edges_are_detected <- vapply(fam_list, function(fam) {
    supp <- detect_support_p(fam$p, c(NA_real_, NA_real_))
    p_on_supp <- fam$p(supp)

    (p_on_supp[1] <= 1e-7) && (p_on_supp[2] >= 1 - 1e-7)
  }, logical(1))

  expect_true(all(edges_are_detected))
})

test_that("detect_support_p detects left edge of support", {
  skip_on_cran()

  left_edge_is_detected <- vapply(fam_list, function(fam) {
    supp <- detect_support_p(fam$p, c(NA, fam$support[2]))
    p_on_supp <- fam$p(supp)

    p_on_supp[1] <= 1e-7
  }, logical(1))

  expect_true(all(left_edge_is_detected))
})

test_that("detect_support_p detects right edge of support", {
  skip_on_cran()

  right_edge_is_detected <- vapply(fam_list, function(fam) {
    supp <- detect_support_p(fam$p, c(fam$support[1], NA))
    p_on_supp <- fam$p(supp)

    p_on_supp[2] >= 1 - 1e-7
  }, logical(1))

  expect_true(all(right_edge_is_detected))
})

test_that("detect_support_p returns input support if it's proper all numeric", {
  input_is_returned <- vapply(fam_list, function(fam) {
    supp <- detect_support_p(fam$p, fam$support)

    isTRUE(all.equal(supp, fam$support))
  }, logical(1))

  expect_true(all(input_is_returned))
})

test_that("detect_support_p throws informative error on bad input function", {
  bad_p_f <- function(q) {fam_norm$p(q) - 10}
  expect_error(
    detect_support_p(bad_p_f, c(NA_real_, NA_real_)), "[Cc]an't find"
  )
})


# solve_for_quan ----------------------------------------------------------
# Main functionality tested in `detect_support_p()` functions
test_that("solve_for_quan throws informative error", {
  expect_error(solve_for_quan(fam_norm$p, -0.1), "[Cc]an't find")
})
