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
test_that("as_p.default honors special 'discrete' distributions", {
  # Originally finite support
  expect_ref_x_tbl(
    as_p(pbinom, size = 10, prob = 0.1),
    data.frame(x = 0:10, prob = dbinom(0:10, size = 10, prob = 0.1))
  )

  # Artificially finite support
  p_pois <- as_p(ppois, lambda = 100)
  pois_x_tbl <- meta_x_tbl(p_pois)
  pois_supp <- qpois(c(1e-6, 1 - 1e-6), lambda = 100)
  pois_x_vec <- pois_supp[1]:pois_supp[2]
  expect_equal(pois_x_tbl[["x"]], pois_x_vec)
  # Probability isn't exact because tail trimming is done
  expect_equal(
    pois_x_tbl[["prob"]], dpois(pois_x_vec, lambda = 100), tolerance = 1e-7
  )
})

test_that("as_p.default honors special 'continuous' distributions", {
  # Standard uniform
  out_unif <- as_p(punif)
  out_unif_ref <- as_p(function(x) {
    punif(x)
  }, c(0, 1))
  expect_equal_x_tbl(out_unif, out_unif_ref)

  # Partially set support is used
  out_unif_2 <- as_p(punif, support = c(0.5, NA))
  expect_equal(meta_support(out_unif_2), c(0.5, 1))
  expect_true(all(meta_x_tbl(out_unif_2)[["y"]] == 2))

  # Hard case of detecting support. Also test for allowing call with `package::`
  # prefix.
  out_norm <- as_p(stats::pnorm, mean = 100, sd = 0.1)
  expect_equal(
    meta_support(out_norm), qnorm(c(1e-6, 1 - 1e-6), mean = 100, sd = 0.1)
  )

  # Distribution function of other "p-d-q-r" type is repaired with warning
  expect_warning(
    out_beta <- as_p(rbeta, shape1 = 2, shape2 = 2),
    "r-function.*`rbeta`.*`as_p\\(\\)`.*p-function"
  )
  out_beta_ref <- as_p(pbeta, shape1 = 2, shape2 = 2)
  expect_is(out_beta, "p")
  expect_equal_x_tbl(out_beta, out_beta_ref)

  # Function environment is used to not pick "honored" function when another
  # object with the same name is found "earlier"
  pgamma <- function(q) {
    punif(q)
  }
  out_bad_gamma <- as_p(pgamma)
  out_bad_gamma_ref <- as_p(function(q) {
    punif(q)
  })
  expect_equal_x_tbl(out_bad_gamma, out_bad_gamma_ref)
})

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
        meta_support(p_list[[i]]), range(meta_x_tbl(p_list[[i]])[["x"]])
      ))
    },
    logical(1)
  )

  expect_equal(is_equal_supp, rep(TRUE, length(p_list)))
})

test_that("as_p.default detects support", {
  # Much more tests are done in `detect_support_p`
  p_beta_both <- as_p(fam_beta$p)
  support_both <- meta_support(p_beta_both)
  expect_equal(fam_beta$p(support_both), c(0, 1), tolerance = 2e-6)

  p_beta_left <- as_p(fam_beta$p, support = c(NA, 0.7))
  support_left <- meta_support(p_beta_left)
  expect_equal(fam_beta$p(support_left[1]), 0, tolerance = 2e-6)

  p_beta_right <- as_p(fam_beta$p, support = c(0.3, NA))
  support_right <- meta_support(p_beta_right)
  expect_equal(fam_beta$p(support_right[2]), 1, tolerance = 2e-6)
})

test_that("as_p.default removes edge `y` with zero density", {
  x_tbl <- meta_x_tbl(p_unif)
  expect_true(all(x_tbl$y[c(2, nrow(x_tbl) - 1)] != 0))
})

test_that("as_p.default uses `n_grid` argument", {
  expect_not_close_f(
    as_p(fam_norm$p, fam_norm$support, n_grid = 10),
    fam_norm$p, fam_norm$grid,
    thres = 1e-2
  )
})

test_that("as_p.default uses `...` to forward arguments to `f`", {
  # This function is used to workaround the "honored" special distribution
  # functions
  my_punif <- function(q, ...) {
    punif(q, ...)
  }

  output_1 <- as_p(my_punif, support = c(0, 10), max = 10)
  expect_true(output_1(9) > 0)
  output_2 <- as_p(my_punif, support = NULL, max = 10)
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

test_that("as_p.default validates input", {
  expect_error(as_p(), "`f`.*missing.*distribution function")
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
test_that("as_p.pdqr works with 'p'", {
  expect_equal_distr(as_p(p_dis), p_dis, grid = c(x_dis_vec_ext, x_dis_vec))
  expect_equal_distr(as_p(p_con), p_con, grid = x_con_vec_ext)
})

test_that("as_p.pdqr works with 'd'", {
  expect_equal_distr(as_p(d_dis), p_dis, grid = c(x_dis_vec_ext, x_dis_vec))
  expect_equal_distr(as_p(d_con), p_con, grid = x_con_vec_ext)
})

test_that("as_p.pdqr works with 'q'", {
  expect_equal_distr(as_p(q_dis), p_dis, grid = c(x_dis_vec_ext, x_dis_vec))
  expect_equal_distr(as_p(q_con), p_con, grid = x_con_vec_ext)
})

test_that("as_p.pdqr works with 'r'", {
  expect_equal_distr(as_p(r_dis), p_dis, grid = c(x_dis_vec_ext, x_dis_vec))
  expect_equal_distr(as_p(r_con), p_con, grid = x_con_vec_ext)
})

test_that("as_p.pdqr validates input", {
  expect_error(
    as_p(structure(user_p, class = c("d", "pdqr"))), "`f`.*not pdqr-function"
  )
})


# detect_support_p --------------------------------------------------------
test_that("detect_support_p detects both edges of support", {
  skip_on_cran()

  edges_are_detected <- vapply(fam_list, function(fam) {
    supp <- detect_support_p(fam$p, c(NA_real_, NA_real_))
    p_on_supp <- fam$p(supp)

    (p_on_supp[1] <= 2e-6) && (p_on_supp[2] >= 1 - 2e-6)
  }, logical(1))

  expect_true(all(edges_are_detected))
})

test_that("detect_support_p detects left edge of support", {
  skip_on_cran()

  left_edge_is_detected <- vapply(fam_list, function(fam) {
    supp <- detect_support_p(fam$p, c(NA, fam$support[2]))
    p_on_supp <- fam$p(supp)

    p_on_supp[1] <= 2e-6
  }, logical(1))

  expect_true(all(left_edge_is_detected))
})

test_that("detect_support_p detects right edge of support", {
  skip_on_cran()

  right_edge_is_detected <- vapply(fam_list, function(fam) {
    supp <- detect_support_p(fam$p, c(fam$support[1], NA))
    p_on_supp <- fam$p(supp)

    p_on_supp[2] >= 1 - 2e-6
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
  bad_p_f <- function(q) {
    fam_norm$p(q) - 10
  }
  expect_error(
    detect_support_p(bad_p_f, c(NA_real_, NA_real_)), "[Cc]an't find"
  )
})


# solve_for_quan ----------------------------------------------------------
# Main functionality tested in `detect_support_p()` functions
test_that("solve_for_quan throws informative error", {
  expect_error(solve_for_quan(fam_norm$p, -0.1), "[Cc]an't find")
})
