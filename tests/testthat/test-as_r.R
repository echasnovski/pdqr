context("test-as_r")

set.seed(4444)


# Custom functions --------------------------------------------------------
# This function is used to workaround the case of "honored" special distribution
# functions
my_runif <- function(n, ...) {
  runif(n, ...)
}


# Computation of r-functions ----------------------------------------------
r_norm <-        as_r(fam_norm$r,        fam_norm$support)
r_norm_2 <-      as_r(fam_norm_2$r,      fam_norm_2$support)
r_exp <-         as_r(fam_exp$r,         fam_exp$support)
r_exp_rev <-     as_r(fam_exp_rev$r,     fam_exp_rev$support)
r_beta <-        as_r(fam_beta$r,        fam_beta$support)
r_beta_inf <-    as_r(fam_beta_inf$r,    fam_beta_inf$support)
r_beta_midinf <- as_r(fam_beta_midinf$r, fam_beta_midinf$support)
r_chisq <-       as_r(fam_chisq$r,       fam_chisq$support)
r_chisq_inf <-   as_r(fam_chisq_inf$r,   fam_chisq_inf$support)
r_mix_norm <-    as_r(fam_mix_norm$r,    fam_mix_norm$support)
r_mix_unif <-    as_r(fam_mix_unif$r,    fam_mix_unif$support)
r_unif <-        as_r(fam_unif$r,        fam_unif$support)

r_list <- list(
  r_norm = r_norm,
  r_norm_2 = r_norm_2,
  r_exp = r_exp,
  r_exp_rev = r_exp_rev,
  r_beta = r_beta,
  r_beta_inf = r_beta_inf,
  r_beta_midinf = r_beta_midinf,
  r_chisq = r_chisq,
  r_chisq_inf = r_chisq_inf,
  r_mix_norm = r_mix_norm,
  r_mix_unif = r_mix_unif,
  r_unif = r_unif
)


# as_r --------------------------------------------------------------------
# Tested in its methods


# as_r.default ------------------------------------------------------------
test_that("as_r.default honors special 'discrete' distributions", {
  # Originally finite support
  expect_ref_x_tbl(
    as_r(rbinom, size = 10, prob = 0.1),
    data.frame(x = 0:10, prob = dbinom(0:10, size = 10, prob = 0.1))
  )

  # Artificially finite support
  r_pois <- as_r(rpois, lambda = 100)
  pois_x_tbl <- meta_x_tbl(r_pois)
  pois_supp <- qpois(c(1e-6, 1 - 1e-6), lambda = 100)
  pois_x_vec <- pois_supp[1]:pois_supp[2]
  expect_equal(pois_x_tbl[["x"]], pois_x_vec)
  # Probability isn't exact because tail trimming is done
  expect_equal(
    pois_x_tbl[["prob"]], dpois(pois_x_vec, lambda = 100), tolerance = 1e-7
  )
})

test_that("as_r.default honors special 'continuous' distributions", {
  # Standard uniform
  out_unif <- as_r(runif)
  out_unif_ref <- as_r(as_d(dunif))
  expect_equal_x_tbl(out_unif, out_unif_ref)

  # Partially set support is used
  out_unif_2 <- as_r(runif, support = c(0.5, NA))
  expect_equal(meta_support(out_unif_2), c(0.5, 1))
  expect_true(all(meta_x_tbl(out_unif_2)[["y"]] == 2))

  # Hard case of detecting support. Also test for allowing call with `package::`
  # prefix.
  out_norm <- as_r(stats::rnorm, mean = 100, sd = 0.1)
  expect_equal(
    meta_support(out_norm), qnorm(c(1e-6, 1 - 1e-6), mean = 100, sd = 0.1)
  )

  # Distribution function of other "p-d-q-r" type is repaired with warning
  expect_warning(
    out_beta <- as_r(dbeta, shape1 = 2, shape2 = 2),
    "d-function.*`dbeta`.*`as_r\\(\\)`.*r-function"
  )
  out_beta_ref <- as_r(rbeta, shape1 = 2, shape2 = 2)
  expect_is(out_beta, "r")
  expect_equal_x_tbl(out_beta, out_beta_ref)

  # Function environment is used to not pick "honored" function when another
  # object with the same name is found "earlier"
  rgamma <- function(n) {
    runif(n)
  }
  out_bad_gamma <- as_r(rgamma)
  out_bad_gamma_ref <- as_r(function(n) {
    runif(n)
  })
  expect_close_r_f(out_bad_gamma, out_bad_gamma_ref)
})

test_that("as_r.default output approximates CDF after `as_p()`", {
  skip_on_cran()

  # Maximum error is quite big due to random sampling during r-function creation
  # That is why tests are for median error
  expect_close_f(
    as_p(r_norm), fam_norm$p, fam_norm$grid,
    stat_f = median
  )
  expect_close_f(
    as_p(r_norm_2), fam_norm_2$p, fam_norm_2$grid,
    stat_f = median, thres = 7e-5
  )
  expect_close_f(
    as_p(r_exp), fam_exp$p, fam_exp$grid,
    stat_f = median, thres = 5e-5
  )
  expect_close_f(
    as_p(r_exp_rev), fam_exp_rev$p, fam_exp_rev$grid,
    stat_f = median, thres = 5e-5
  )
  expect_close_f(
    as_p(r_beta), fam_beta$p, fam_beta$grid,
    stat_f = median, thres = 5e-3
  )
  expect_close_f(
    as_p(r_beta_inf), fam_beta_inf$p, fam_beta_inf$grid,
    stat_f = median, thres = 5e-2
  )
  expect_close_f(
    as_p(r_beta_midinf), fam_beta_midinf$p, fam_beta_midinf$grid,
    stat_f = median, thres = 5e-3
  )
  expect_close_f(
    as_p(r_chisq), fam_chisq$p, fam_chisq$grid,
    stat_f = median, thres = 5e-5
  )
  expect_close_f(
    as_p(r_chisq_inf), fam_chisq_inf$p, fam_chisq_inf$grid,
    stat_f = median, thres = 3e-5
  )
  expect_close_f(
    as_p(r_mix_norm), fam_mix_norm$p, fam_mix_norm$grid,
    stat_f = median
  )
  expect_close_f(
    as_p(r_mix_unif), fam_mix_unif$p, fam_mix_unif$grid,
    stat_f = median, thres = 1e-2
  )
  expect_close_f(
    as_p(r_unif), fam_unif$p, fam_unif$grid,
    stat_f = median, thres = 5e-3
  )
})

test_that("as_r.default output approximates density after `as_d()`", {
  skip_on_cran()

  # Maximum error is quite big due to random sampling during r-function creation
  # That is why tests are for median error
  expect_close_f(
    as_d(r_norm), fam_norm$d, fam_norm$grid,
    stat_f = median, thres = 3e-6
  )
  expect_close_f(
    as_d(r_norm_2), fam_norm_2$d, fam_norm_2$grid,
    stat_f = median, thres = 4e-3
  )
  expect_close_f(
    as_d(r_exp), fam_exp$d, fam_exp$grid,
    stat_f = median, thres = 5e-5
  )
  expect_close_f(
    as_d(r_exp_rev), fam_exp_rev$d, fam_exp_rev$grid,
    stat_f = median, thres = 5e-5
  )

  # Accuracy is bad because of poor kernel density extimation approximation of
  # discontinuous densities
  expect_close_f(
    as_d(r_beta), fam_beta$d, fam_beta$grid,
    stat_f = median, thres = 3e-2
  )
  expect_close_f(
    as_d(r_beta_inf), fam_beta_inf$d, fam_beta_inf$grid,
    stat_f = median, thres = 0.15
  )
  expect_close_f(
    as_d(r_beta_midinf), fam_beta_midinf$d, fam_beta_midinf$grid,
    stat_f = median, thres = 4e-2
  )

  expect_close_f(
    as_d(r_chisq), fam_chisq$d, fam_chisq$grid,
    stat_f = median, thres = 5e-5
  )
  expect_close_f(
    as_d(r_chisq_inf), fam_chisq_inf$d, fam_chisq_inf$grid,
    stat_f = median, thres = 4e-5
  )
  expect_close_f(
    as_d(r_mix_norm), fam_mix_norm$d, fam_mix_norm$grid,
    stat_f = median
  )
  expect_close_f(
    as_d(r_mix_unif), fam_mix_unif$d, fam_mix_unif$grid,
    stat_f = median, thres = 3e-2
  )
  expect_close_f(
    as_d(r_unif), fam_unif$d, fam_unif$grid,
    stat_f = median, thres = 3e-2
  )
})

test_that("as_r.default output approximates quantile function after `as_q()`", {
  skip_on_cran()

  # Maximum error is quite big due to random sampling during r-function creation
  # That is why tests are for median error
  expect_close_f(
    as_q(r_norm), fam_norm$q, p_seq_btail,
    stat_f = median, thres = 8e-3
  )
  expect_close_f(
    as_q(r_norm_2), fam_norm_2$q, p_seq_btail,
    stat_f = median, thres = 4e-3
  )
  expect_close_f(
    as_q(r_exp), fam_exp$q, p_seq_rtail,
    stat_f = median, thres = 7e-2
  )
  expect_close_f(
    as_q(r_exp_rev), fam_exp_rev$q, p_seq_ltail,
    stat_f = median, thres = 7e-2
  )
  expect_close_f(
    as_q(r_beta), fam_beta$q, p_seq,
    stat_f = median, thres = 1e-2
  )
  expect_close_f(
    as_q(r_beta_inf), fam_beta_inf$q, p_seq,
    stat_f = median, thres = 7e-2
  )
  expect_close_f(
    as_q(r_beta_midinf), fam_beta_midinf$q, p_seq,
    stat_f = median, thres = 5e-3
  )
  # Accuracy is bad because of poor kernel density extimation approximation of
  # discontinuous densities
  expect_close_f(
    as_q(r_chisq), fam_chisq$q, p_seq_rtail,
    stat_f = median, thres = 0.2
  )
  expect_close_f(
    as_q(r_chisq_inf), fam_chisq_inf$q, p_seq_rtail,
    stat_f = median, thres = 0.2
  )

  expect_close_f(
    as_q(r_mix_norm), fam_mix_norm$q, p_seq_btail,
    stat_f = median, thres = 1e-2
  )
  expect_close_f(
    as_q(r_mix_unif), fam_mix_unif$q, p_seq,
    stat_f = median, thres = 4e-2
  )
  expect_close_f(
    as_q(r_unif), fam_unif$q, p_seq,
    stat_f = median, thres = 6e-3
  )
})

test_that("as_r.default results in good approximations of input", {
  expect_close_r_f(
    r_norm, fam_norm$r,
    mean_thres = 5e-2, sd_thres = 4e-2
  )
  expect_close_r_f(
    r_norm_2, fam_norm_2$r,
    mean_thres = 3e-3, sd_thres = 2e-3
  )
  expect_close_r_f(
    r_exp, fam_exp$r,
    mean_thres = 7e-2, sd_thres = 3e-2
  )
  expect_close_r_f(
    r_exp_rev, fam_exp_rev$r,
    mean_thres = 0.1, sd_thres = 5e-2
  )
  expect_close_r_f(
    r_beta, fam_beta$r,
    mean_thres = 1e-2, sd_thres = 6e-3
  )
  expect_close_r_f(
    r_beta_inf, fam_beta_inf$r,
    mean_thres = 0.1, sd_thres = 2e-2
  )
  expect_close_r_f(
    r_beta_midinf, fam_beta_midinf$r,
    mean_thres = 5e-3, sd_thres = 7e-3
  )
  expect_close_r_f(r_chisq, fam_chisq$r, mean_thres = 11e-2, sd_thres = 8e-2)
  expect_close_r_f(
    r_chisq_inf, fam_chisq_inf$r,
    mean_thres = 0.2, sd_thres = 8e-2
  )
  expect_close_r_f(
    r_mix_norm, fam_mix_norm$r,
    mean_thres = 5e-2, sd_thres = 6e-2
  )
  expect_close_r_f(
    r_mix_unif, fam_mix_unif$r,
    mean_thres = 5e-2, sd_thres = 4e-2
  )
  expect_close_r_f(r_unif, fam_unif$r, mean_thres = 5e-3, sd_thres = 7e-3)
})

test_that("as_r.default output has minimum support according to 'x_tbl'", {
  is_equal_supp <- vapply(
    seq_along(r_list), function(i) {
      isTRUE(all.equal(
        meta_support(r_list[[i]]), range(meta_x_tbl(r_list[[i]])[["x"]])
      ))
    },
    logical(1)
  )

  expect_equal(is_equal_supp, rep(TRUE, length(r_list)))
})

test_that("as_r.default detects support", {
  # Much more tests are done in `detect_support_r`
  r_beta_both <- as_r(fam_beta$r)
  support_both <- meta_support(r_beta_both)
  expect_equal(fam_beta$p(support_both), c(0, 1), tolerance = 1e-4)

  r_beta_left <- as_r(fam_beta$r, support = c(NA, 0.7))
  support_left <- meta_support(r_beta_left)
  expect_equal(fam_beta$p(support_left[1]), 0, tolerance = 1e-4)

  r_beta_right <- as_r(fam_beta$r, support = c(0.3, NA))
  support_right <- meta_support(r_beta_right)
  expect_equal(fam_beta$p(support_right[2]), 1, tolerance = 1e-4)
})

test_that("as_r.default removes edge `y` with zero density", {
  x_tbl <- meta_x_tbl(r_unif)
  expect_true(all(x_tbl$y[c(2, nrow(x_tbl) - 1)] != 0))
})

test_that("as_r.default uses `n_grid` and `n_sample` arguments", {
  expect_not_close_r_f(
    as_r(fam_norm$r, fam_norm$support, n_grid = 10), fam_norm$r
  )
  expect_not_close_r_f(
    as_r(fam_norm$r, fam_norm$support, n_sample = 10), fam_norm$r
  )
})

test_that("as_r.default uses `...` to forward arguments to `f`", {
  output <- as_r(my_runif, support = c(0, 10), max = 10)
  expect_true(mean(output(1000)) > 1)
})

test_that("as_r.default uses `args_new`", {
  output <- as_r(my_runif, args_new = list(from = 0.5))
  expect_true(meta_x_tbl(output)[["x"]][1] > 0.45)
})

test_that("as_r.default properly adjusts to support", {
  supp <- c(-0.5, 1.5)
  out_r <- as_r(fam_norm[["r"]], supp)
  # This assumes that `as_d()` correctly adjusts to support and uses
  # `as_r.pdqr()`.
  ref_r <- as_r(as_d(fam_norm[["d"]], supp))
  expect_close_r_f(out_r, ref_r)
})

test_that("as_r.default throws error if total probability on support is zero", {
  expect_error(as_r(fam_beta$r, c(1.5, 2)), "probability.*positive")
})

test_that("as_r.default validates input", {
  expect_error(as_r(), "`f`.*missing.*distribution function")
  expect_error(as_r("a", c(0, 1)), "`f`.*function")
  expect_error(as_r(fam_norm$r, c(2, 1)), "`support`")
  expect_error(
    as_r(fam_norm$r, fam_norm$support, n_grid = "a"), "`n_grid`.*number"
  )
  expect_error(
    as_r(fam_norm$r, fam_norm$support, n_grid = 2), "`n_grid`.*more.*2"
  )
  expect_error(
    as_r(fam_norm$r, fam_norm$support, n_sample = "a"), "`n_sample`.*number"
  )
  expect_error(
    as_r(fam_norm$r, fam_norm$support, n_sample = 1), "`n_sample`.*more.*1"
  )
  expect_error(
    as_r(fam_norm$r, fam_norm$support, args_new = "a"), "`args_new`.*list"
  )
})

test_that("as_r.default throws error if detected support isn't proper", {
  expect_error(as_r(fam_beta$r, c(1.5, NA)), "support.*proper")
  expect_error(as_r(fam_beta$r, c(NA, -0.2)), "support.*proper")
})

# as_r.pdqr ---------------------------------------------------------------
test_that("as_r works with 'p'", {
  expect_equal_r_distr(as_r(p_dis), r_dis)
  expect_equal_r_distr(as_r(p_con), r_con)
})

test_that("as_r works with 'd'", {
  expect_equal_r_distr(as_r(d_dis), r_dis)
  expect_equal_r_distr(as_r(d_con), r_con)
})

test_that("as_r works with 'q'", {
  expect_equal_r_distr(as_r(q_dis), r_dis, mean_thres = 0.12)
  expect_equal_r_distr(as_r(q_con), r_con)
})

test_that("as_r works with 'r'", {
  expect_equal_r_distr(as_r(r_dis), r_dis)
  expect_equal_r_distr(as_r(r_con), r_con)
})

test_that("as_r.pdqr validates input", {
  expect_error(
    as_r(structure(user_r, class = c("p", "pdqr"))), "`f`.*not pdqr-function"
  )
})


# detect_support_r --------------------------------------------------------
test_that("detect_support_r detects both edges of support", {
  skip_on_cran()

  edges_are_detected <- vapply(fam_list, function(fam) {
    supp <- detect_support_r(fam$r(10000), c(NA_real_, NA_real_))
    p_on_supp <- fam$p(supp)

    (p_on_supp[1] <= 5e-4) && (p_on_supp[2] >= 1 - 5e-4)
  }, logical(1))

  expect_true(all(edges_are_detected))
})

test_that("detect_support_r detects left edge of support", {
  skip_on_cran()

  left_edge_is_detected <- vapply(fam_list, function(fam) {
    supp <- detect_support_r(fam$r(10000), c(NA, fam$support[2]))
    p_on_supp <- fam$p(supp)

    p_on_supp[1] <= 5e-4
  }, logical(1))

  expect_true(all(left_edge_is_detected))
})

test_that("detect_support_r detects right edge of support", {
  skip_on_cran()

  right_edge_is_detected <- vapply(fam_list, function(fam) {
    supp <- detect_support_r(fam$r(10000), c(fam$support[1], NA))
    p_on_supp <- fam$p(supp)

    p_on_supp[2] >= 1 - 5e-4
  }, logical(1))

  expect_true(all(right_edge_is_detected))
})

test_that("detect_support_r returns input support if it's proper all numeric", {
  input_is_returned <- vapply(fam_list, function(fam) {
    supp <- detect_support_r(fam$r(10000), fam$support)

    isTRUE(all.equal(supp, fam$support))
  }, logical(1))

  expect_true(all(input_is_returned))
})

test_that("detect_support_r throws informative error on bad input function", {
  bad_r_f <- function(n) {
    rep(NA, length.out = n)
  }
  expect_error(
    detect_support_r(bad_r_f(100), c(NA_real_, NA_real_)),
    "support.*isn't proper"
  )
})
