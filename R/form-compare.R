form_geq <- function(f_1, f_2) {
  boolean_pdqr(prob_true = prob_geq(f_1, f_2), pdqr_class = get_pdqr_class(f_1))
}

form_greater <- function(f_1, f_2) {
  boolean_pdqr(
    prob_true = prob_greater(f_1, f_2),
    pdqr_class = get_pdqr_class(f_1)
  )
}

form_leq <- function(f_1, f_2) {
  boolean_pdqr(
    # P(f_1 <= f_2) = 1 - P(f_1 > f_2)
    prob_true = 1 - prob_greater(f_1, f_2),
    pdqr_class = get_pdqr_class(f_1)
  )
}

form_less <- function(f_1, f_2) {
  boolean_pdqr(
    # P(f_1 < f_2) = 1 - P(f_1 >= f_2)
    prob_true = 1 - prob_geq(f_1, f_2),
    pdqr_class = get_pdqr_class(f_1)
  )
}

form_equal <- function(f_1, f_2) {
  boolean_pdqr(
    prob_true = prob_equal(f_1, f_2),
    pdqr_class = get_pdqr_class(f_1)
  )
}

form_not_equal <- function(f_1, f_2) {
  boolean_pdqr(
    prob_true = 1 - prob_equal(f_1, f_2),
    pdqr_class = get_pdqr_class(f_1)
  )
}

prob_geq <- function(f_1, f_2) {
  # Early returns for edge cases
  f_1_supp <- meta_support(f_1)
  f_2_supp <- meta_support(f_2)
  if (f_1_supp[1] >= f_2_supp[2]) {
    return(1)
  }
  if (f_2_supp[1] >= f_1_supp[2]) {
    return(0)
  }

  # Actual computations
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

prob_equal <- function(f_1, f_2) {
  if ((meta_type(f_1) == "fin") && (meta_type(f_2) == "fin")) {
    x_1 <- meta_x_tbl(f_1)[["x"]]

    sum(as_d(f_1)(x_1) * as_d(f_2)(x_1))
  } else {
    # If any of `f_1` or `f_2` is "infin" (i.e. "continuous") then probability
    # of generating two exactly equal values is 0
    0
  }
}

prob_greater <- function(f_1, f_2) {
  prob_geq(f_1, f_2) - prob_equal(f_1, f_2)
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
  # Create common grid (with some helper grids) for `f_1` and `f_2`. It is
  # created as union of `f_1` and `f_2` "x"s which lie in the intersection of
  # both supports. This is done to workaround the assumption that vectors `x`
  # and `y` during integral computation represent actual density points
  # **between which there are lines**. This doesn't hold if common grid is
  # created as simple union of grids (there will be distortion on the edge of
  # some support). Example: `f_1` and `f_2` are "infin" uniform on (0, 1) and
  # (0.5, 1.5). `f_1` on union grid (0, 0.5, 1, 1.5) would have "y" values
  # (1, 1, 1, 0) which during computation of integrals will be treated as having
  # line from (x=1, y=1) to (x=1.5, y=0), which is not true.
  # Note that there will be at least 2 points in `comm_x` because this code will
  # execute after early returns in `prob_geq()` didn't return.
  comm_x <- common_x(f_1, f_2, method = "intersect")

  n <- length(comm_x)
  comm_left <- comm_x[-n]
  comm_right <- comm_x[-1]
  # This is used soon as "indicator" (that doesn't equal to any of `comm_x` and
  # lies strictly inside intersection of supports) of common grid interval
  comm_mid <- (comm_left + comm_right) / 2

  # Compute coefficients of lines representing `f_1` and `f_2` densities at each
  # interval of common grid.
  f_x_tbl_1 <- meta_x_tbl(f_1)
  coeffs_1 <- compute_piecelin_density_coeffs(
    x_tbl = f_x_tbl_1,
    ind_vec = findInterval(comm_mid, f_x_tbl_1[["x"]])
  )
  f_x_tbl_2 <- meta_x_tbl(f_2)
  coeffs_2 <- compute_piecelin_density_coeffs(
    x_tbl = f_x_tbl_2,
    ind_vec = findInterval(comm_mid, f_x_tbl_2[["x"]])
  )

  # Output probability based on functions inside `(comm_x[1], comm_x[n])`
  # interval (intersection of supports) is equal to definite integral from
  # `comm_x[1]` to `comm_x[n]` of `d_1(x) * p_2(x)` (`d_1` - PDF of `f_1`, `p_2`
  # - CDF of `f_2`), which is a desired probability. Logic here is that for
  # small interval the probability of `f_1 >= f_2` is equal to a product of two
  # probabilities: 1) that `f_1` lies inside that interval and 2) that `f_2`
  # lies to the left of that interval. When interval width tends to zero, that
  # probability tends to `d_1(x) * p_2(x)`. Overall probability is equal to
  # integral of that expression over intersection of `f_1` and `f_2` supports,
  # which here is `[comm_x[1], comm_x[n]]`.
  cumprob_2_left <- as_p(f_2)(comm_left)

  comm_res <- infin_geq_piece_integral(
    diff_x = diff(comm_x),
    y_1_left = as_d(f_1)(comm_left),
    y_2_left = as_d(f_2)(comm_left),
    slope_1 = coeffs_1[["slope"]],
    slope_2 = coeffs_2[["slope"]],
    cumprob_2_left = cumprob_2_left
  )

  # Probability of "f_1 >= f_2" outside of intersection sof supports is equal to
  # total probability of `f_1` to the right of `f_2`'s support.
  f_1_geq_outside <- 1 - as_p(f_1)(meta_support(f_2)[2])

  comm_res + f_1_geq_outside
}

infin_geq_piece_integral <- function(diff_x, y_1_left, y_2_left,
                                     slope_1, slope_2, cumprob_2_left) {
  # Output probability is equal to definite integral of `d_1(x) * p_2(x)` over
  # common support. On each interval, where both densities have linear nature,
  # definite integral is over (x_left, x_right) interval of a function `(a*x +
  # b) * (0.5*A*(x^2 - x_left^2) + B*(x - x_left) + G)`, where `a`/`A` - slopes
  # of `f_1` and `f_2` in the interval (`slope_1` and `slope_2`), `b`/`B` -
  # intercepts, `G` - cumulative probability of `f_2` at `x_left`. To overcome
  # numerical issues variable replacement **helps very much**: `x = x_left + t`.
  # Then this integral becomes over interval (0, x_right-x_left) of
  # `(a*t+y_1_left) * (0.5*A*t^2 + y_2_left*t + G)`, which can be represented
  # explicitly in the expression, assigned lower to `piece_integrals`.

  h <- diff_x

  # Having powers of `h` inside every term avoids issues with numerical
  # representation accuracy when `h` is too small (as in dirac-like entries).
  piece_integrals <- slope_1*slope_2*h^4 / 8 +
    (2*slope_1*y_2_left*h^3 + slope_2*y_1_left*h^3) / 6 +
    (slope_1*cumprob_2_left*h^2 + y_1_left*y_2_left*h^2) / 2 +
    y_1_left*cumprob_2_left*h

  sum(piece_integrals)
}
