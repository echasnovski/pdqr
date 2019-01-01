context("test-as_d")

set.seed(2222)


# as_d --------------------------------------------------------------------
# Tested in its methods


# as_d.default ------------------------------------------------------------
test_that("as_d.default results in good approximations of input", {
  expect_approx(as_d, fam_norm, "d")
  expect_approx(as_d, fam_norm_2, "d")
  expect_approx(as_d, fam_exp, "d")
  expect_approx(as_d, fam_exp_rev, "d")
  expect_approx(as_d, fam_beta, "d")

  # `max()` isn't used because of infinite density
  expect_approx(as_d, fam_beta_inf, "d", stat_f = median, thres = 2e-2)
  expect_approx(as_d, fam_beta_inf, "d", stat_f = quan90, thres = 5e-2)

  expect_approx(as_d, fam_chisq, "d")
  # `max()` isn't used because of infinite density
  expect_approx(as_d, fam_chisq_inf, "d", stat_f = quan999, thres = 5e-2)

  expect_approx(as_d, fam_mix_norm, "d", thres = 2e-6)
  # `max()` isn't used because of density discontinuity
  expect_approx(as_d, fam_mix_unif, "d", stat_f = quan999, thres = 1e-4)
})

test_that("as_d.default uses `n_grid` argument", {
  expect_not_approx(as_d, fam_norm_2, "d", thres = 1e-2, n_grid = 10)
})

test_that("as_d.default properly adjusts to support", {
  supp <- c(-0.5, 1.5)
  out_d <- as_d(fam_norm[["d"]], supp)
  ref_d <- function(x) {fam_norm[["d"]](x) / diff(fam_norm[["p"]](supp))}
  expect_equal_on_grid(
    out_d, ref_d, seq(supp[1], supp[2], length.out = 1e5)
  )

  # Output integrates to 1
  integral <- stats::integrate(out_d, supp[1], supp[2])
  expect_true(abs(round(integral[["value"]], 8) - 1) <= integral[["abs.error"]])
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
    as_d(fam_norm$d, fam_norm$support, n_grid = 1), "`n_grid`.*more.*2"
  )
})


# as_d.pdqr ---------------------------------------------------------------
test_that('as_d.pdqr works with "p"', {
  expect_equal_distr(as_d(p_raw), d_raw, grid = c(x_raw_vec_ext, x_raw_vec))
  expect_equal_distr(as_d(p_smooth), d_smooth, x_smooth_vec_ext)
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


# impute_inf --------------------------------------------------------------
test_that("impute_inf works", {
  expect_equal(
    # All equations are y = x
    impute_inf(1:6, c(1, Inf, 3, 4, Inf, 6), "y"), 1:6
  )
  expect_equal(
    # All equations are y = x
    impute_inf(1:7, c(Inf, 2, Inf, 4, Inf, 6, Inf), "y"), 1:7
  )
  expect_equal(
    # Left: y = x, right: y = -2x + 10
    impute_inf(1:5, c(1, 2, Inf, 2, 0), "y"), c(1, 2, 4, 2, 0)
  )
  expect_equal(
    # Left: y = x, right: y = -x + 9
    impute_inf(c(1, 2, 3, 7, 9), c(1, 2, Inf, 2, 0), "y"), c(1, 2, 6, 2, 0)
  )
  expect_equal(
    # Left: y = x, right: y = x + 7
    impute_inf(c(1, 2, 3, 7, 9), c(1, 2, Inf, 0, 2), "y"), c(1, 2, 3, 0, 2)
  )

  expect_error(impute_inf(1:3, c(NA, 1, 3), "`a`"), "[Aa]ll.*`a`.*number.*NA")
  expect_error(impute_inf(1:3, c(1, 2, Inf), "`a`"), "`a`.*3 finite values")
})


# impute_linearly ---------------------------------------------------------
# Tested in `impute_inf()`
