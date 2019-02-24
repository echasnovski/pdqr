context("test-group-generics")


# Input data --------------------------------------------------------------
x_norm_seq <- seq(-10, 10, by = 0.01)


# Custom expactations -----------------------------------------------------
expect_lin_trans <- function(f_out, f_in, ref_supp) {
  output_ref <- form_resupport(f_in, ref_supp, method = "linear")

  expect_equal_x_tbl(f_out, output_ref)
}


# Math.pdqr ---------------------------------------------------------------
test_that("Math.pdqr works", {
  lnorm_x <- seq(0, 100, by = 0.001)
  d_lnorm <- new_d(data.frame(x = lnorm_x, y = dlnorm(lnorm_x)))

  d_norm_ref <- new_d(data.frame(x = x_norm_seq, y = dnorm(x_norm_seq)))
  d_norm_out <- log(d_lnorm)

  expect_distr_fun(d_norm_out, "d", "infin")
  expect_equal_distr(
    d_norm_out, d_norm_ref,
    grid = x_norm_seq, thres = 0.05,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )
})


# Ops.pdqr ----------------------------------------------------------------
test_that("Ops.pdqr works", {
  unif_x <- seq(0, 1, by = 0.001)
  p_unif <- new_p(data.frame(x = unif_x, y = dunif(unif_x)))

  p_norm_ref <- new_p(data.frame(x = x_norm_seq, y = dnorm(x_norm_seq)))
  # Approximation of standard normal as centered sum of 12 uniform
  p_norm_out <- p_unif + p_unif + p_unif + p_unif + p_unif + p_unif +
    p_unif + p_unif + p_unif + p_unif + p_unif + p_unif - 6

  expect_distr_fun(p_norm_out, "p", "infin")
  expect_equal_distr(
    p_norm_out, p_norm_ref,
    grid = x_norm_seq, thres = 0.05,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )
})

test_that("Ops.pdqr works with logical functions", {
  d_leq <- d_fin <= 3
  expect_true(abs(d_leq(1) - p_fin(3)) <= 0.1)
})

test_that("Ops.pdqr works with generics which take one argument", {
  # `+`
  expect_equal(+d_fin, d_fin)
  expect_equal(+d_infin, d_infin)

  # `-`
  d_fin_minus <- -d_fin
  expect_equal(-meta_support(d_fin)[2:1], meta_support(d_fin_minus))
  expect_equal(d_fin(x_fin_vec_ext), d_fin_minus(-x_fin_vec_ext))

  d_infin_minus <- -d_infin
  expect_equal(-meta_support(d_infin)[2:1], meta_support(d_infin_minus))
  expect_equal(d_infin(x_infin_vec_ext), d_infin_minus(-x_infin_vec_ext))

  # `!`
  d_negate <- !new_d(
    data.frame(x = c(-1, 0, 1), prob = c(0.1, 0.2, 0.7)), "fin"
  )
  expect_equal(d_negate(1), 0.2)

    # Probability of type "infin" pdqr-function being exactly 0 is equal to zero
  expect_equal((!d_infin)(1), 0)
})

test_that("Ops.pdqr works in case of linear operation", {
  d_fin_supp <- meta_support(d_fin)
  d_infin_supp <- meta_support(d_infin)
  d_dirac <- form_retype(d_fin, "infin", method = "dirac")
  d_dirac_supp <- meta_support(d_dirac)

  # `+`
  expect_lin_trans(d_fin + 10, f_in = d_fin, ref_supp = d_fin_supp + 10)
  expect_lin_trans(10 + d_fin, f_in = d_fin, ref_supp = d_fin_supp + 10)
  expect_lin_trans(d_infin + 10, f_in = d_infin, ref_supp = d_infin_supp + 10)
  expect_lin_trans(10 + d_infin, f_in = d_infin, ref_supp = d_infin_supp + 10)
  expect_lin_trans(d_dirac + 10, f_in = d_dirac, ref_supp = d_dirac_supp + 10)
  expect_lin_trans(10 + d_dirac, f_in = d_dirac, ref_supp = d_dirac_supp + 10)

  # `-`
  expect_lin_trans(d_fin - 10, f_in = d_fin, ref_supp = d_fin_supp - 10)
  expect_lin_trans(10 - d_fin, f_in = -d_fin, ref_supp = 10 - d_fin_supp[2:1])
  expect_lin_trans(d_infin - 10, f_in = d_infin, ref_supp = d_infin_supp - 10)
  expect_lin_trans(
    10 - d_infin, f_in = -d_infin, ref_supp = 10 - d_infin_supp[2:1]
  )
  expect_lin_trans(d_dirac - 10, f_in = d_dirac, ref_supp = d_dirac_supp - 10)
  expect_lin_trans(
    10 - d_dirac, f_in = -d_dirac, ref_supp = 10 - d_dirac_supp[2:1]
  )

  # `*`
  expect_lin_trans(d_fin * 10, f_in = d_fin, ref_supp = d_fin_supp * 10)
  expect_lin_trans(10 * d_fin, f_in = d_fin, ref_supp = d_fin_supp * 10)
  expect_lin_trans(d_infin * 10, f_in = d_infin, ref_supp = d_infin_supp * 10)
  expect_lin_trans(10 * d_infin, f_in = d_infin, ref_supp = d_infin_supp * 10)
  expect_lin_trans(d_dirac * 10, f_in = d_dirac, ref_supp = d_dirac_supp * 10)
  expect_lin_trans(10 * d_dirac, f_in = d_dirac, ref_supp = d_dirac_supp * 10)

  # `/`
  expect_lin_trans(d_fin / 10, f_in = d_fin, ref_supp = d_fin_supp / 10)
  expect_lin_trans(d_infin / 10, f_in = d_infin, ref_supp = d_infin_supp / 10)
  expect_lin_trans(d_dirac / 10, f_in = d_dirac, ref_supp = d_dirac_supp / 10)
})

test_that("Ops.pdqr works in case of comparison", {
  d_dirac <- form_retype(d_fin, "infin", method = "dirac")
  d_infin_2 <- new_d(data.frame(x = 0:1, y = c(1, 1)), "infin")
  num <- 3
  num_d <- new_d(num, "fin")

  # `>=`
  expect_equal_x_tbl(d_fin >= num, form_geq(d_fin, num_d))
  expect_equal_x_tbl(num >= d_infin, form_geq(num_d, d_infin))
  expect_equal_x_tbl(d_infin >= d_infin_2, form_geq(d_infin, d_infin_2))
  expect_equal_x_tbl(d_infin >= d_dirac, form_geq(d_infin, d_dirac))

  # `>`
  expect_equal_x_tbl(d_fin > num, form_greater(d_fin, num_d))
  expect_equal_x_tbl(num > d_infin, form_greater(num_d, d_infin))
  expect_equal_x_tbl(d_infin > d_infin_2, form_greater(d_infin, d_infin_2))
  expect_equal_x_tbl(d_infin > d_dirac, form_greater(d_infin, d_dirac))

  # `<=`
  expect_equal_x_tbl(d_fin <= num, form_leq(d_fin, num_d))
  expect_equal_x_tbl(num <= d_infin, form_leq(num_d, d_infin))
  expect_equal_x_tbl(d_infin <= d_infin_2, form_leq(d_infin, d_infin_2))
  expect_equal_x_tbl(d_infin <= d_dirac, form_leq(d_infin, d_dirac))

  # `<`
  expect_equal_x_tbl(d_fin < num, form_less(d_fin, num_d))
  expect_equal_x_tbl(num < d_infin, form_less(num_d, d_infin))
  expect_equal_x_tbl(d_infin < d_infin_2, form_less(d_infin, d_infin_2))
  expect_equal_x_tbl(d_infin < d_dirac, form_less(d_infin, d_dirac))

  # `==`
  expect_equal_x_tbl(d_fin == num, form_equal(d_fin, num_d))
  expect_equal_x_tbl(num == d_infin, form_equal(num_d, d_infin))
  expect_equal_x_tbl(d_infin == d_infin_2, form_equal(d_infin, d_infin_2))
  expect_equal_x_tbl(d_infin == d_dirac, form_equal(d_infin, d_dirac))

  # `!=`
  expect_equal_x_tbl(d_fin != num, form_not_equal(d_fin, num_d))
  expect_equal_x_tbl(num != d_infin, form_not_equal(num_d, d_infin))
  expect_equal_x_tbl(d_infin != d_infin_2, form_not_equal(d_infin, d_infin_2))
  expect_equal_x_tbl(d_infin != d_dirac, form_not_equal(d_infin, d_dirac))
})

test_that("Ops.pdqr asserts bad input in case of linear operation", {
  bad_pdqr <- structure(function(x) {x}, class = c("p", "pdqr", "function"))

  expect_error(bad_pdqr + 1, "`e1`")
  expect_error(1 + bad_pdqr, "`e2`")
  expect_error(bad_pdqr - 1, "`e1`")
  expect_error(1 - bad_pdqr, "`e2`")
  expect_error(bad_pdqr * 1, "`e1`")
  expect_error(1 * bad_pdqr, "`e2`")
  expect_error(bad_pdqr / 1, "`e1`")
})

test_that("Ops.pdqr asserts bad input in case of comparison", {
  bad_pdqr <- structure(function(x) {x}, class = c("p", "pdqr", "function"))
  bad_pdqr_2 <- structure(function(x) {x+1}, class = c("p", "pdqr", "function"))

  # `>=`
  expect_error(bad_pdqr >= 1, "`e1`")
  expect_error(1 >= bad_pdqr, "`e2`")
  expect_error(bad_pdqr >= bad_pdqr_2, "`e1`")
  expect_error(d_fin >= bad_pdqr_2, "`e2`")
  expect_error("a" >= d_fin, "`e1`.*pdqr-function.*number")
  expect_error(d_fin >= "a", "`e2`.*pdqr-function.*number")

  `>`
  expect_error(bad_pdqr > 1, "`e1`")
  expect_error(1 > bad_pdqr, "`e2`")
  expect_error(bad_pdqr > bad_pdqr_2, "`e1`")
  expect_error(d_fin > bad_pdqr_2, "`e2`")
  expect_error("a" > d_fin, "`e1`.*pdqr-function.*number")
  expect_error(d_fin > "a", "`e2`.*pdqr-function.*number")

  `<=`
  expect_error(bad_pdqr <= 1, "`e1`")
  expect_error(1 <= bad_pdqr, "`e2`")
  expect_error(bad_pdqr <= bad_pdqr_2, "`e1`")
  expect_error(d_fin <= bad_pdqr_2, "`e2`")
  expect_error("a" <= d_fin, "`e1`.*pdqr-function.*number")
  expect_error(d_fin <= "a", "`e2`.*pdqr-function.*number")

  `<`
  expect_error(bad_pdqr < 1, "`e1`")
  expect_error(1 < bad_pdqr, "`e2`")
  expect_error(bad_pdqr < bad_pdqr_2, "`e1`")
  expect_error(d_fin < bad_pdqr_2, "`e2`")
  expect_error("a" < d_fin, "`e1`.*pdqr-function.*number")
  expect_error(d_fin < "a", "`e2`.*pdqr-function.*number")

  `==`
  expect_error(bad_pdqr == 1, "`e1`")
  expect_error(1 == bad_pdqr, "`e2`")
  expect_error(bad_pdqr == bad_pdqr_2, "`e1`")
  expect_error(d_fin == bad_pdqr_2, "`e2`")
  expect_error("a" == d_fin, "`e1`.*pdqr-function.*number")
  expect_error(d_fin == "a", "`e2`.*pdqr-function.*number")

  `!=`
  expect_error(bad_pdqr != 1, "`e1`")
  expect_error(1 != bad_pdqr, "`e2`")
  expect_error(bad_pdqr != bad_pdqr_2, "`e1`")
  expect_error(d_fin != bad_pdqr_2, "`e2`")
  expect_error("a" != d_fin, "`e1`.*pdqr-function.*number")
  expect_error(d_fin != "a", "`e2`.*pdqr-function.*number")
})


# Summary.pdqr ------------------------------------------------------------
test_that("Summary.pdqr works", {
  expect_distr_fun(min(p_fin), "p", "fin")
  expect_distr_fun(max(p_fin, p_fin), "p", "fin")
  expect_distr_fun(sum(q_custom, q_infin, q_infin), "q", "infin")
  expect_distr_fun(prod(r_custom, r_custom, na.rm = TRUE), "r", "infin")
})

test_that("Summary.pdqr throws error on `range()`", {
  expect_error(range(p_fin, p_fin), "range.*two.*numbers")
})


# reflect_pdqr_around_zero ------------------------------------------------
# Tested in `Ops.pdqr`


# negate_pdqr -------------------------------------------------------------
# Tested in `Ops.pdqr`


# is_ops_linear -----------------------------------------------------------
# Tested in `Ops.pdqr`


# ops_linear --------------------------------------------------------------
# Tested in `Ops.pdqr`


# ops_compare -------------------------------------------------------------
# Tested in `Ops.pdqr`
