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
test_that("as_d.default honors special 'discrete' distributions", {
  # Originally finite support
  expect_ref_x_tbl(
    as_d(dbinom, size = 10, prob = 0.1),
    data.frame(x = 0:10, prob = dbinom(0:10, size = 10, prob = 0.1))
  )

  # Artificially finite support
  d_pois <- as_d(dpois, lambda = 100)
  pois_x_tbl <- meta_x_tbl(d_pois)
  pois_supp <- qpois(c(1e-6, 1 - 1e-6), lambda = 100)
  pois_x_vec <- pois_supp[1]:pois_supp[2]
  expect_equal(pois_x_tbl[["x"]], pois_x_vec)
  # Probability isn't exact because tail trimming is done
  expect_equal(
    pois_x_tbl[["prob"]], dpois(pois_x_vec, lambda = 100), tolerance = 1e-7
  )
})

test_that("as_d.default honors special 'continuous' distributions", {
  # Standard uniform
  out_unif <- as_d(dunif)
  out_unif_ref <- as_d(function(x) {dunif(x)}, c(0, 1))
  expect_equal_x_tbl(out_unif, out_unif_ref)

  # Partially set support is used
  out_unif_2 <- as_d(dunif, support = c(0.5, NA))
  expect_equal(meta_support(out_unif_2), c(0.5, 1))
  expect_true(all(meta_x_tbl(out_unif_2)[["y"]] == 2))

  # Hard case of detecting support. Also test for allowing call with `package::`
  # prefix.
  out_norm <- as_d(stats::dnorm, mean = 100, sd = 0.1)
  expect_equal(
    meta_support(out_norm), qnorm(c(1e-6, 1-1e-6), mean = 100, sd = 0.1)
  )

  # Distribution function of other "p-d-q-r" type is repaired with warning
  expect_warning(
    out_beta <- as_d(rbeta, shape1 = 2, shape2 = 2),
    "r-function.*`rbeta`.*`as_d\\(\\)`.*d-function"
  )
  out_beta_ref <- as_d(dbeta, shape1 = 2, shape2 = 2)
  expect_is(out_beta, "d")
  expect_equal_x_tbl(out_beta, out_beta_ref)

  # Function environment is used to not pick "honored" function when another
  # object with the same name is found "earlier"
  dgamma <- function(x) {dunif(x)}
  out_bad_gamma <- as_d(dgamma)
  out_bad_gamma_ref <- as_d(function(x) {dunif(x)})
  expect_equal_x_tbl(out_bad_gamma, out_bad_gamma_ref)
})

test_that("as_d.default output approximates CDF after `as_p()`", {
  skip_on_cran()

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
    d_beta_midinf, fam_beta_midinf$d, fam_beta_midinf$grid,
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
  skip_on_cran()

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
  skip_on_cran()

  expect_close_r_f(
    as_r(d_norm), fam_norm$r,
    mean_thres = 1e-2, sd_thres = 4e-2
  )
  expect_close_r_f(
    as_r(d_norm_2), fam_norm_2$r,
    mean_thres = 2e-3, sd_thres = 5e-3
  )
  expect_close_r_f(
    as_r(d_exp), fam_exp$r,
    mean_thres = 3e-2, sd_thres = 1e-2
  )
  expect_close_r_f(
    as_r(d_exp_rev), fam_exp_rev$r,
    mean_thres = 2e-2, sd_thres = 5e-2
  )
  expect_close_r_f(
    as_r(d_beta), fam_beta$r,
    mean_thres = 4e-3, sd_thres = 4e-3
  )
  expect_close_r_f(
    as_r(d_beta_inf), fam_beta_inf$r,
    mean_thres = 5e-2, sd_thres = 7e-3
  )
  expect_close_r_f(
    as_r(d_beta_midinf), fam_beta_midinf$r,
    mean_thres = 4e-3, sd_thres = 7e-3
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

test_that("as_d.default output has minimum support according to 'x_tbl'", {
  is_equal_supp <- vapply(
    seq_along(d_list), function(i) {
      isTRUE(all.equal(
        meta_support(d_list[[i]]), range(meta_x_tbl(d_list[[i]])[["x"]])
      ))
    },
    logical(1)
  )

  expect_equal(is_equal_supp, rep(TRUE, length(d_list)))
})

test_that("as_d.default detects support", {
  # Much more tests are done in `detect_support_d`
  d_beta_both <- as_d(fam_beta$d)
  support_both <- meta_support(d_beta_both)
  expect_equal(fam_beta$p(support_both), c(0, 1), tolerance = 1e-7)

  d_beta_left <- as_d(fam_beta$d, support = c(NA, 0.7))
  support_left <- meta_support(d_beta_left)
  expect_equal(fam_beta$p(support_left[1]), 0, tolerance = 1e-7)

  d_beta_right <- as_d(fam_beta$d, support = c(0.3, NA))
  support_right <- meta_support(d_beta_right)
  expect_equal(fam_beta$p(support_right[2]), 1, tolerance = 1e-7)
})

test_that("as_d.default doesn't affect upstream random generation process", {
  # This test is created because of implementation details of support detection:
  # it searches initial point of non-zero density by randomly generating broad
  # sequence of points, which (if done carelessly) affects upstream random
  # generation but it shouldn't.
  d_f <- function(x) {dnorm(x)}

  # Reference
  set.seed(101)
  a <- runif(1)
  b <- runif(1)

  # Testing
  set.seed(101)
  as_d(d_f)
  expect_equal(runif(1), a)

  set.seed(101)
  runif(1)
  as_d(d_f)
  expect_equal(runif(1), b)
})

test_that("as_d.default removes edge `y` with zero density", {
  x_tbl <- meta_x_tbl(d_unif)
  expect_true(all(x_tbl$y[c(2, nrow(x_tbl)-1)] != 0))
})

test_that("as_d.default uses `n_grid` argument", {
  expect_not_close_f(
    as_d(fam_norm$d, fam_norm$support, n_grid = 10),
    fam_norm$d, fam_norm$grid,
    thres = 1e-2
  )
})

test_that("as_d.default uses `...` to forward arguments to `f`", {
  # This function is used to workaround the "honored" special distribution
  # functions
  my_dunif <- function(x, ...) {dunif(x, ...)}

  output_1 <- as_d(my_dunif, support = c(0, 10), max = 10)
  expect_true(output_1(9) > 0)
  output_2 <- as_d(my_dunif, support = NULL, max = 10)
  expect_true(output_2(9) > 0)
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
  d_x_tbl <- meta_x_tbl(d_beta)
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

test_that("as_d.default validates input", {
  expect_error(as_d(), "`f`.*missing.*distribution function")
  expect_error(as_d("a", c(0, 1)), "`f`.*function")
  expect_error(as_d(fam_norm$d, c(2, 1)), "`support`")
  expect_error(
    as_d(fam_norm$d, fam_norm$support, n_grid = "a"), "`n_grid`.*number"
  )
  expect_error(
    as_d(fam_norm$d, fam_norm$support, n_grid = 2), "`n_grid`.*more.*2"
  )
})

test_that("as_d.default throws error if detected support isn't proper", {
  expect_error(as_d(fam_beta$d, c(1.5, NA)), "support.*proper")
  expect_error(as_d(fam_beta$d, c(NA, -0.2)), "support.*proper")
})


# as_d.pdqr ---------------------------------------------------------------
test_that("as_d.pdqr works with 'p'", {
  expect_equal_distr(as_d(p_dis), d_dis, grid = c(x_dis_vec_ext, x_dis_vec))
  expect_equal_distr(as_d(p_con), d_con, x_con_vec_ext)
})

test_that("as_d.pdqr works with 'd'", {
  expect_equal_distr(as_d(d_dis), d_dis, grid = c(x_dis_vec_ext, x_dis_vec))
  expect_equal_distr(as_d(d_con), d_con, x_con_vec_ext)
})

test_that("as_d.pdqr works with 'q'", {
  expect_equal_distr(as_d(q_dis), d_dis, grid = c(x_dis_vec_ext, x_dis_vec))
  expect_equal_distr(as_d(q_con), d_con, grid = x_con_vec_ext)
})

test_that("as_d.pdqr works with 'r'", {
  expect_equal_distr(as_d(r_dis), d_dis, grid = c(x_dis_vec_ext, x_dis_vec))
  expect_equal_distr(as_d(r_con), d_con, grid = x_con_vec_ext)
})

test_that("as_d.pdqr validates input", {
  expect_error(
    as_d(structure(user_d, class = c("p", "pdqr"))), "`f`.*not pdqr-function"
  )
})


# detect_support_d --------------------------------------------------------
test_that("detect_support_d detects both edges of support", {
  skip_on_cran()

  edges_are_detected <- vapply(fam_list, function(fam) {
    supp <- detect_support_d(fam$d, c(NA_real_, NA_real_))
    p_on_supp <- fam$p(supp)

    # Allowing bigger tolerance if at target edge d-function goes to infinity
    is_d_inf <- is.infinite(fam$d(fam$support))
    tol <- ifelse(is_d_inf, 1e-2, 1e-3)

    (p_on_supp[1] <= tol[1]) && (p_on_supp[2] >= 1 - tol[2])
  }, logical(1))

  expect_true(all(edges_are_detected))
})

test_that("detect_support_d detects left edge of support", {
  skip_on_cran()

  left_edge_is_detected <- vapply(fam_list, function(fam) {
    supp <- detect_support_d(fam$d, c(NA, fam$support[2]))
    p_on_supp <- fam$p(supp)

    # Allowing bigger tolerance if at target edge d-function goes to infinity
    is_d_inf <- is.infinite(fam$d(fam$support[1]))
    tol <- ifelse(is_d_inf, 1e-2, 1e-3)

    p_on_supp[1] <= tol
  }, logical(1))

  expect_true(all(left_edge_is_detected))
})

test_that("detect_support_d detects right edge of support", {
  skip_on_cran()

  right_edge_is_detected <- vapply(fam_list, function(fam) {
    supp <- detect_support_d(fam$d, c(fam$support[1], NA))
    p_on_supp <- fam$p(supp)

    # Allowing bigger tolerance if at target edge d-function goes to infinity
    is_d_inf <- is.infinite(fam$d(fam$support[2]))
    tol <- ifelse(is_d_inf, 1e-2, 1e-3)

    p_on_supp[2] >= 1 - tol
  }, logical(1))

  expect_true(all(right_edge_is_detected))
})

test_that("detect_support_d returns input support if it's proper all numeric", {
  input_is_returned <- vapply(fam_list, function(fam) {
    supp <- detect_support_d(fam$d, fam$support)

    isTRUE(all.equal(supp, fam$support))
  }, logical(1))

  expect_true(all(input_is_returned))
})

test_that("detect_support_d throws informative error on bad input function", {
  bad_d_f <- function(x) {dnorm(x, mean = 1e12, sd = 0.01)}
  expect_error(
    detect_support_d(bad_d_f, c(NA_real_, NA_real_)), "Can't find initial point"
  )
})


# construct_p_f -----------------------------------------------------------
# Main functionality is tested in `detect_support_d()`
test_that("construct_p_f throws informative error", {
  d_f <- function(x) {rep(Inf, length.out = length(x))}
  expect_error(construct_p_f(d_f, 0), "[Cc]an't")
})


# optim_for_quan ----------------------------------------------------------
# Main functionality is tested in `detect_support_d()`
test_that("optim_for_quan throws informative error", {
  p_f <- function(q) {rep(Inf, length.out = length(q))}
  expect_error(optim_for_quan(p_f, 0.5, 0), "[Cc]an't.*for.*0.5")
})


# detect_d_init_x ---------------------------------------------------------
test_that("detect_d_init_x doesn't affect upstream random generation process", {
  # Reference
  set.seed(101)
  a <- runif(1)
  b <- runif(1)

  # Testing
  set.seed(101)
  detect_d_init_x(dnorm)
  expect_equal(runif(1), a)

  set.seed(101)
  runif(1)
  detect_d_init_x(dnorm)
  expect_equal(runif(1), b)
})
