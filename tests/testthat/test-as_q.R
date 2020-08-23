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

q_list <- list(
  q_norm = q_norm,
  q_norm_2 = q_norm_2,
  q_exp = q_exp,
  q_exp_rev = q_exp_rev,
  q_beta = q_beta,
  q_beta_inf = q_beta_inf,
  q_beta_midinf = q_beta_midinf,
  q_chisq = q_chisq,
  q_chisq_inf = q_chisq_inf,
  q_mix_norm = q_mix_norm,
  q_mix_unif = q_mix_unif,
  q_unif = q_unif
)


# as_q --------------------------------------------------------------------
# Tested in its methods


# as_q.default ------------------------------------------------------------
test_that("as_q.default honors special 'discrete' distributions", {
  # Originally finite support
  expect_ref_x_tbl(
    as_q(qbinom, size = 10, prob = 0.1),
    data.frame(x = 0:10, prob = dbinom(0:10, size = 10, prob = 0.1))
  )

  # Artificially finite support
  q_pois <- as_q(qpois, lambda = 100)
  pois_x_tbl <- meta_x_tbl(q_pois)
  pois_supp <- qpois(c(1e-6, 1 - 1e-6), lambda = 100)
  pois_x_vec <- pois_supp[1]:pois_supp[2]
  expect_equal(pois_x_tbl[["x"]], pois_x_vec)
  # Probability isn't exact because tail trimming is done
  expect_equal(
    pois_x_tbl[["prob"]], dpois(pois_x_vec, lambda = 100), tolerance = 1e-7
  )
})

test_that("as_q.default honors special 'continuous' distributions", {
  # Standard uniform
  out_unif <- as_q(qunif)
  out_unif_ref <- as_q(function(x) {
    qunif(x)
  }, c(0, 1))
  expect_equal_x_tbl(out_unif, out_unif_ref)

  # Partially set support is used
  out_unif_2 <- as_q(qunif, support = c(0.5, NA))
  expect_equal(meta_support(out_unif_2), c(0.5, 1))
  expect_true(all(meta_x_tbl(out_unif_2)[["y"]] == 2))

  # Hard case of detecting support. Also test for allowing call with `package::`
  # prefix.
  out_norm <- as_q(stats::qnorm, mean = 100, sd = 0.1)
  expect_equal(
    meta_support(out_norm), qnorm(c(1e-6, 1 - 1e-6), mean = 100, sd = 0.1)
  )

  # Distribution function of other "p-d-q-r" type is repaired with warning
  expect_warning(
    out_beta <- as_q(rbeta, shape1 = 2, shape2 = 2),
    "r-function.*`rbeta`.*`as_q\\(\\)`.*q-function"
  )
  out_beta_ref <- as_q(qbeta, shape1 = 2, shape2 = 2)
  expect_is(out_beta, "q")
  expect_equal_x_tbl(out_beta, out_beta_ref)

  # Function environment is used to not pick "honored" function when another
  # object with the same name is found "earlier"
  qgamma <- function(p) {
    qunif(p)
  }
  out_bad_gamma <- as_q(qgamma)
  out_bad_gamma_ref <- as_q(function(p) {
    qunif(p)
  })
  expect_equal_x_tbl(out_bad_gamma, out_bad_gamma_ref)
})

test_that("as_q.default output approximates CDF after `as_p()`", {
  skip_on_cran()

  expect_close_f(as_p(q_norm), fam_norm$p, fam_norm$grid, thres = 5e-5)
  expect_close_f(as_p(q_norm_2), fam_norm_2$p, fam_norm_2$grid, thres = 5e-5)
  expect_close_f(as_p(q_exp), fam_exp$p, fam_exp$grid, thres = 6e-5)
  expect_close_f(as_p(q_exp_rev), fam_exp_rev$p, fam_exp_rev$grid, thres = 6e-5)
  expect_close_f(as_p(q_beta), fam_beta$p, fam_beta$grid, thres = 4e-5)
  expect_close_f(
    as_p(q_beta_inf), fam_beta_inf$p, fam_beta_inf$grid,
    thres = 1e-2
  )
  expect_close_f(
    as_p(q_beta_midinf), fam_beta_midinf$p, fam_beta_midinf$grid,
    thres = 6e-5
  )
  expect_close_f(as_p(q_chisq), fam_chisq$p, fam_chisq$grid, thres = 6e-5)
  expect_close_f(
    as_p(q_chisq_inf), fam_chisq_inf$p, fam_chisq_inf$grid,
    thres = 1e-2
  )
  expect_close_f(
    as_p(q_mix_norm), fam_mix_norm$p, fam_mix_norm$grid,
    thres = 3e-3
  )
  expect_close_f(
    as_p(q_mix_unif), fam_mix_unif$p, fam_mix_unif$grid,
    thres = 2e-4
  )
  expect_close_f(as_p(q_unif), fam_unif$p, fam_unif$grid, thres = 6e-5)
})

test_that("as_q.default output approximates density after `as_d()`", {
  skip_on_cran()

  expect_close_f(as_d(q_norm), fam_norm$d, fam_norm$grid, thres = 4e-4)
  expect_close_f(as_d(q_norm_2), fam_norm_2$d, fam_norm_2$grid, thres = 4e-3)
  expect_close_f(as_d(q_exp), fam_exp$d, fam_exp$grid, thres = 1e-4)
  expect_close_f(as_d(q_exp_rev), fam_exp_rev$d, fam_exp_rev$grid, thres = 1e-4)
  expect_close_f(as_d(q_beta), fam_beta$d, fam_beta$grid, thres = 5e-3)
  expect_close_f(
    as_d(q_beta_inf), fam_beta_inf$d, fam_beta_inf$grid,
    stat_f = quan90, thres = 2e-2
  )
  expect_close_f(
    as_d(q_beta_midinf), fam_beta_midinf$d, fam_beta_midinf$grid,
    stat_f = quan99, thres = 3e-4
  )
  expect_close_f(as_d(q_chisq), fam_chisq$d, fam_chisq$grid, thres = 5e-5)
  expect_close_f(
    as_d(q_chisq_inf), fam_chisq_inf$d, fam_chisq_inf$grid,
    stat_f = quan999, thres = 2e-2
  )
  # Accuracy is bad because of non-precise construction of `fam_mix_norm$q`
  expect_close_f(
    as_d(q_mix_norm), fam_mix_norm$d, fam_mix_norm$grid,
    stat_f = median
  )
  expect_close_f(
    as_d(q_mix_unif), fam_mix_unif$d, fam_mix_unif$grid,
    stat_f = quan999, thres = 1e-4
  )
  expect_close_f(
    as_d(q_unif), fam_unif$d, fam_unif$grid,
    stat_f = quan999
  )
})

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

test_that("as_q.default output approximates random-gen-func after `as_r()`", {
  skip_on_cran()

  expect_close_r_f(
    as_r(q_norm), fam_norm$r,
    mean_thres = 5e-2, sd_thres = 2e-2
  )
  expect_close_r_f(
    as_r(q_norm_2), fam_norm_2$r,
    mean_thres = 1e-3, sd_thres = 5e-3
  )
  expect_close_r_f(
    as_r(q_exp), fam_exp$r,
    mean_thres = 5e-2, sd_thres = 2e-2
  )
  expect_close_r_f(
    as_r(q_exp_rev), fam_exp_rev$r,
    mean_thres = 2e-2, sd_thres = 7e-2
  )
  expect_close_r_f(
    as_r(q_beta), fam_beta$r,
    mean_thres = 6e-3, sd_thres = 2e-3
  )
  expect_close_r_f(
    as_r(q_beta_inf), fam_beta_inf$r,
    mean_thres = 7e-3, sd_thres = 2e-3
  )
  expect_close_r_f(
    as_r(q_beta_midinf), fam_beta_midinf$r,
    mean_thres = 1e-2, sd_thres = 1e-2
  )
  expect_close_r_f(
    as_r(q_chisq), fam_chisq$r,
    mean_thres = 2e-2, sd_thres = 8e-2
  )
  expect_close_r_f(
    as_r(q_chisq_inf), fam_chisq_inf$r,
    mean_thres = 8e-2, sd_thres = 8e-2
  )
  expect_close_r_f(
    as_r(q_mix_norm), fam_mix_norm$r,
    mean_thres = 6e-2, sd_thres = 8e-2
  )
  expect_close_r_f(
    as_r(q_mix_unif), fam_mix_unif$r,
    mean_thres = 2e-2, sd_thres = 2e-2
  )
  expect_close_r_f(as_r(q_unif), fam_unif$r, mean_thres = 1e-2, sd_thres = 2e-3)
})

test_that("as_q.default output has minimum support according to 'x_tbl'", {
  is_equal_supp <- vapply(
    seq_along(q_list), function(i) {
      isTRUE(all.equal(
        meta_support(q_list[[i]]), range(meta_x_tbl(q_list[[i]])[["x"]])
      ))
    },
    logical(1)
  )

  expect_equal(is_equal_supp, rep(TRUE, length(q_list)))
})

test_that("as_q.default detects support", {
  # Much more tests are done in `detect_support_q`
  q_beta_both <- as_q(fam_beta$q)
  support_both <- meta_support(q_beta_both)
  expect_equal(fam_beta$p(support_both), c(0, 1), tolerance = 1e-7)

  q_beta_left <- as_q(fam_beta$q, support = c(NA, 0.7))
  support_left <- meta_support(q_beta_left)
  expect_equal(fam_beta$p(support_left[1]), 0, tolerance = 1e-7)

  q_beta_right <- as_q(fam_beta$q, support = c(0.3, NA))
  support_right <- meta_support(q_beta_right)
  expect_equal(fam_beta$p(support_right[2]), 1, tolerance = 1e-7)
})

test_that("as_q.default removes edge `y` with zero density", {
  x_tbl <- meta_x_tbl(q_unif)
  expect_true(all(x_tbl$y[c(2, nrow(x_tbl) - 1)] != 0))
})

test_that("as_q.default uses `n_grid` argument", {
  expect_not_close_f(
    as_q(fam_norm$q, fam_norm$support, n_grid = 10),
    fam_norm$q, p_seq,
    thres = 1e-2
  )
})

test_that("as_q.default uses `...` to forward arguments to `f`", {
  # This function is used to workaround the "honored" special distribution
  # functions
  my_qunif <- function(p, ...) {
    qunif(p, ...)
  }

  output_1 <- as_q(my_qunif, support = c(0, 10), max = 10)
  expect_true(output_1(0.95) > 9)
  output_2 <- as_q(my_qunif, support = NULL, max = 10)
  expect_true(output_2(0.95) > 9)
})

test_that("as_q.default properly adjusts to support", {
  supp <- c(-0.5, 1.5)
  out_q <- as_q(fam_norm[["q"]], supp)
  # This assumes that `as_d()` correctly adjusts to support and uses
  # `as_q.pdqr()`.
  ref_q <- as_q(as_d(fam_norm[["d"]], supp))
  expect_close_f(out_q, ref_q, p_seq, thres = 1e-7)
})

test_that("as_q.default throws error if total probability on support is zero", {
  expect_error(as_q(fam_beta$q, c(1.5, 2)), "probability.*positive")
})

test_that("as_q.default validates input", {
  expect_error(as_q(), "`f`.*missing.*distribution function")
  expect_error(as_q("a", c(0, 1)), "`f`.*function")
  expect_error(as_q(fam_norm$q, c(2, 1)), "`support`")
  expect_error(
    as_q(fam_norm$q, fam_norm$support, n_grid = "a"), "`n_grid`.*number"
  )
  expect_error(
    as_q(fam_norm$q, fam_norm$support, n_grid = 2), "`n_grid`.*more.*2"
  )
})

test_that("as_q.default throws error if detected support isn't proper", {
  expect_error(as_q(fam_beta$q, c(1.5, NA)), "support.*proper")
  expect_error(as_q(fam_beta$q, c(NA, -0.2)), "support.*proper")
})


# as_q.pdqr ---------------------------------------------------------------
test_that("as_q.pdqr works with 'p'", {
  expect_equal_distr(as_q(p_dis), q_dis, grid = p_vec)
  expect_equal_distr(as_q(p_con), q_con, grid = p_vec)
})

test_that("as_q.pdqr works with 'd'", {
  expect_equal_distr(as_q(d_dis), q_dis, grid = p_vec)
  expect_equal_distr(as_q(d_con), q_con, grid = p_vec)
})

test_that("as_q.pdqr works with 'q'", {
  expect_equal_distr(as_q(q_dis), q_dis, grid = p_vec)
  expect_equal_distr(as_q(q_con), q_con, grid = p_vec)
})

test_that("as_q.pdqr works with 'r'", {
  expect_equal_distr(as_q(r_dis), q_dis, grid = p_vec)
  expect_equal_distr(as_q(r_con), q_con, grid = p_vec)
})

test_that("as_q.pdqr validates input", {
  expect_error(
    as_q(structure(user_q, class = c("p", "pdqr"))), "`f`.*not pdqr-function"
  )
})


# detect_support_q --------------------------------------------------------
test_that("detect_support_q detects both edges of support", {
  skip_on_cran()

  edges_are_detected <- vapply(fam_list, function(fam) {
    supp <- detect_support_q(fam$q, c(NA_real_, NA_real_))
    p_on_supp <- fam$p(supp)

    # The "real" accuracy is even 1e-7 but here it is lower because of
    # artificial nature of `fam_mix_norm$q()`
    (p_on_supp[1] <= 2e-5) && (p_on_supp[2] >= 1 - 2e-5)
  }, logical(1))

  expect_true(all(edges_are_detected))
})

test_that("detect_support_q detects left edge of support", {
  skip_on_cran()

  left_edge_is_detected <- vapply(fam_list, function(fam) {
    supp <- detect_support_q(fam$q, c(NA, fam$support[2]))
    p_on_supp <- fam$p(supp)

    # The "real" accuracy is even 1e-7 but here it is lower because of
    # artificial nature of `fam_mix_norm$q()`
    p_on_supp[1] <= 2e-5
  }, logical(1))

  expect_true(all(left_edge_is_detected))
})

test_that("detect_support_q detects right edge of support", {
  skip_on_cran()

  right_edge_is_detected <- vapply(fam_list, function(fam) {
    supp <- detect_support_q(fam$q, c(fam$support[1], NA))
    p_on_supp <- fam$p(supp)

    # The "real" accuracy is even 1e-7 but here it is lower because of
    # artificial nature of `fam_mix_norm$q()`
    p_on_supp[2] >= 1 - 2e-5
  }, logical(1))

  expect_true(all(right_edge_is_detected))
})

test_that("detect_support_q returns input support if it's proper all numeric", {
  input_is_returned <- vapply(fam_list, function(fam) {
    supp <- detect_support_q(fam$q, fam$support)

    isTRUE(all.equal(supp, fam$support))
  }, logical(1))

  expect_true(all(input_is_returned))
})

test_that("detect_support_q throws informative error on bad input function", {
  bad_q_f <- function(p) {
    rep(NA, length.out = length(p))
  }
  expect_error(
    detect_support_q(bad_q_f, c(NA_real_, NA_real_)), "support.*isn't proper"
  )
})
