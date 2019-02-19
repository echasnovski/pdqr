# NOTE that currently these functions don't work properly when "infin"
# dirac-like data is involved
form_geq <- function(f_1, f_2) {
  assert_pdqr_fun(f_1)
  assert_pdqr_fun(f_2)

  boolean_pdqr(prob_true = prob_geq(f_1, f_2), pdqr_class = get_pdqr_class(f_1))
}

form_greater <- function(f_1, f_2) {
  assert_pdqr_fun(f_1)
  assert_pdqr_fun(f_2)

  boolean_pdqr(
    prob_true = prob_greater(f_1, f_2),
    pdqr_class = get_pdqr_class(f_1)
  )
}

form_leq <- function(f_1, f_2) {
  assert_pdqr_fun(f_1)
  assert_pdqr_fun(f_2)

  boolean_pdqr(
    # P(f_1 <= f_2) = 1 - P(f_1 > f_2)
    prob_true = 1 - prob_greater(f_1, f_2),
    pdqr_class = get_pdqr_class(f_1)
  )
}

form_less <- function(f_1, f_2) {
  assert_pdqr_fun(f_1)
  assert_pdqr_fun(f_2)

  boolean_pdqr(
    # P(f_1 < f_2) = 1 - P(f_1 >= f_2)
    prob_true = 1 - prob_geq(f_1, f_2),
    pdqr_class = get_pdqr_class(f_1)
  )
}

boolean_pdqr <- function(prob_true, pdqr_class) {
  x_tbl <- data.frame(x = c(0, 1), prob = c(1-prob_true, prob_true))

  new_pdqr_by_class(pdqr_class)(x_tbl, "fin")
}

prob_geq <- function(f_1, f_2) {
  if (meta_type(f_1) == "fin") {
    prob_geq_fin_any(f_1, f_2)
  } else {
    if (meta_type(f_2) == "fin") {
      # P(f_1 >= f_1) = 1 - P(f_1 < f_2) = [due to continuity of `f_1`] =
      # 1 - P(f_1 <= f_2) = 1 - P(f_2 >= f_1)
      1 - prob_geq_fin_any(f_2, f_1)
    } else {
      prob_geq_infin_infin(f_1, f_2)
    }
  }
}

prob_greater <- function(f_1, f_2) {
  if ((meta_type(f_1) == "fin") && (meta_type(f_2) == "fin")) {
    x_1 <- meta_x_tbl(f_1)[["x"]]

    equal_prob <- sum(as_d(f_1)(x_1) * as_d(f_2)(x_1))
  } else {
    # If any of `f_1` or `f_2` is "infin" (i.e. "continuous") then probability
    # of generating two exactly equal values is 0
    equal_prob <- 0
  }

  prob_geq(f_1, f_2) - equal_prob
}

prob_geq_fin_any <- function(f_1, f_2) {
  x_1 <- meta_x_tbl(f_1)[["x"]]
  d_f_1 <- as_d(f_1)
  p_f_2 <- as_p(f_2)

  # If `f_1` has known value `x`, then probability that `f_2` is less-or-equal
  # is CDF of `f_2` at `x`. The probability of that outcome is multiplication of
  # `x`'s probability and `f_2`'s CDF at `x` (due to independency assumption).
  # Result is a sum for all possible `f_1` values.
  sum(d_f_1(x_1) * p_f_2(x_1))
}

prob_geq_infin_infin <- function(f_1, f_2) {
  f_x_tbl_1 <- meta_x_tbl(f_1)
  f_x_tbl_2 <- meta_x_tbl(f_2)

  # Create common grid for `f_1` and `f_2`
  comm_x <- sort(union(f_x_tbl_1[["x"]], f_x_tbl_2[["x"]]))
  y_1 <- enfun_x_tbl(f_x_tbl_1)(comm_x)
  y_2 <- enfun_x_tbl(f_x_tbl_2)(comm_x)

  # Compute coefficients of lines representing `f_1` and `f_2` densities at each
  # interval of common grid.
  n <- length(comm_x)
  ind_vec <- seq_len(n - 1)
  coeffs_1 <- compute_piecelin_density_coeffs(comm_x, y_1, ind_vec)
  coeffs_2 <- compute_piecelin_density_coeffs(comm_x, y_2, ind_vec)

  # Output probability is equal to definite integral from `comm_x[1]` to
  # `comm_x[n]` of `d_1(x) * p_2(x)` (`d_1` - PDF of `f_1`, `p_2` - CDF of
  # `f_2`), which is a desired probability. Logic here is that for small
  # interval the probability of `f_1 >= f_2` is equal to a product of two
  # probabilities: 1) that `f_1` lies inside that interval and 2) that `f_2`
  # lies to the left of that interval. When interval width tends to zero, that
  # probability tends to `d_1(x) * p_2(x)`. Overall probability is equal to
  # integral of that expression over combined support of `f_1` and `f_2`, which
  # here is `[comm_x[1], comm_x[n]]`.
  infin_geq_integral(
    slope_1 = coeffs_1[["slope"]], inter_1 = coeffs_1[["intercept"]],
    slope_2 = coeffs_2[["slope"]], inter_2 = coeffs_2[["intercept"]],
    x_left = comm_x[-n], x_right = comm_x[-1],
    cumprob_2_left = as_p(f_2)(comm_x[-n])
  )
}

infin_geq_integral <- function(slope_1, inter_1, slope_2, inter_2,
                               x_left, x_right, cumprob_2_left) {
  lpow <- four_powers(x_left)
  rpow <- four_powers(x_right)

  # Output probability is equal to definite integral of `d_1(x) * p_2(x)` over
  # maximum support (on outside of which both densities equal to zero). On each
  # interval, where both densities have linear nature, definite integral is
  # equal to the `piece_integrals`, as `d_1` is equal to `slope_1 * x + inter_1`
  # and `p_2` is `0.5*slope_2 * x^2 + inter_2 * x + c_2` where `c_2` is free
  # coefficient of `p_2` computed as below.
  c_2 <- cumprob_2_left - lpow$p2 * slope_2/2 - lpow$p1 * inter_2

  piece_integrals <- (rpow$p4 - lpow$p4) * slope_1*slope_2 / 8 +
    (rpow$p3 - lpow$p3) * (2*slope_1*inter_2 + inter_1*slope_2) / 6 +
    (rpow$p2 - lpow$p2) * (slope_1*c_2 + inter_1*inter_2) / 2 +
    (rpow$p1 - lpow$p1) * inter_1 * c_2

  sum(piece_integrals)
}

four_powers <- function(x) {
  x_pow_2 <- x * x
  x_pow_3 <- x_pow_2 * x
  x_pow_4 <- x_pow_3 * x

  list(p1 = x, p2 = x_pow_2, p3 = x_pow_3, p4 = x_pow_4)
}
