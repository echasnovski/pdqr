form_geq <- function(f, g) {
  boolean_pdqr(prob_true = prob_geq(f, g), pdqr_class = meta_class(f))
}

form_greater <- function(f, g) {
  boolean_pdqr(
    prob_true = prob_greater(f, g),
    pdqr_class = meta_class(f)
  )
}

form_leq <- function(f, g) {
  boolean_pdqr(
    # P(f <= g) = 1 - P(f > g)
    prob_true = 1 - prob_greater(f, g),
    pdqr_class = meta_class(f)
  )
}

form_less <- function(f, g) {
  boolean_pdqr(
    # P(f < g) = 1 - P(f >= g)
    prob_true = 1 - prob_geq(f, g),
    pdqr_class = meta_class(f)
  )
}

form_equal <- function(f, g) {
  boolean_pdqr(
    prob_true = prob_equal(f, g),
    pdqr_class = meta_class(f)
  )
}

form_not_equal <- function(f, g) {
  boolean_pdqr(
    prob_true = 1 - prob_equal(f, g),
    pdqr_class = meta_class(f)
  )
}

prob_geq <- function(f, g) {
  # Early returns for edge cases
  f_supp <- meta_support(f)
  g_supp <- meta_support(g)
  if (f_supp[1] > g_supp[2]) {
    return(1)
  }
  if (g_supp[1] > f_supp[2]) {
    return(0)
  }

  # Actual computations
  if (meta_type(f) == "discrete") {
    prob_geq_dis_any(f, g)
  } else {
    if (meta_type(g) == "discrete") {
      # P(f >= g) = 1 - P(f < g) = [due to continuity of `f`] = 1 - P(f <= g) =
      # 1 - P(g >= f)
      1 - prob_geq_dis_any(g, f)
    } else {
      prob_geq_con_con(f, g)
    }
  }
}

prob_equal <- function(f, g) {
  if ((meta_type(f) == "discrete") && (meta_type(g) == "discrete")) {
    f_x_tbl <- meta_x_tbl(f)
    g_x_tbl <- meta_x_tbl(g)
    x_f <- f_x_tbl[["x"]]

    # This is basically a copy of `new_d_dis()` output's body but without input
    # rounding. This is done to ensure the following code is valid:
    # dirac_single_dis <- form_retype(new_d(1, "continuous"), "discrete")
    # This should return 0.5 which it doesn't (because of rounding policy) if
    # `d_g_at_x_f` is computed with `as_d(g)(x_f)`:
    # prob_equal(dirac_single_dis, dirac_single_dis)
    d_g_at_x_f <- numeric(length(x_f))
    inds <- match(x_f, g_x_tbl[["x"]], nomatch = NA)
    good_inds <- !is.na(inds)
    d_g_at_x_f[good_inds] <- g_x_tbl[["prob"]][inds[good_inds]]

    sum(f_x_tbl[["prob"]] * d_g_at_x_f)
  } else {
    # If any of `f` or `g` is "continuous" then probability of generating two
    # exactly equal values is 0
    0
  }
}

prob_greater <- function(f, g) {
  prob_geq(f, g) - prob_equal(f, g)
}

prob_geq_dis_any <- function(f, g) {
  f_x_tbl <- meta_x_tbl(f)
  x_f <- f_x_tbl[["x"]]

  if (meta_type(g) == "discrete") {
    # This is basically a copy of `new_p_dis()` output's body but without input
    # rounding. This is done to ensure that the following code is valid:
    # dirac_single_dis <- form_retype(new_d(1, "continuous"), "discrete")
    # # This should return 0.75 which it doesn't (because of rounding policy) if
    # # `cumprob_g` is computed with `as_p(g)(x_f)`.
    # prob_geq_dis_any(dirac_single_dis, dirac_single_dis)

    g_x_tbl <- meta_x_tbl(g)
    cumprob_g <- numeric(length(x_f))

    inds <- findInterval(x_f, g_x_tbl[["x"]])
    inds_isnt_zero <- inds != 0

    cumprob_g[inds_isnt_zero] <- g_x_tbl[["cumprob"]][inds[inds_isnt_zero]]
  } else {
    cumprob_g <- as_p(g)(x_f)
  }

  # If `f` has known value `x`, then probability that `g` is less-or-equal is
  # CDF of `g` at `x`. The probability of that outcome is multiplication of
  # `x`'s probability and `g`'s CDF at `x` (due to independency assumption).
  # Result is a sum for all possible `f` values.
  sum(f_x_tbl[["prob"]] * cumprob_g)
}

prob_geq_con_con <- function(f, g) {
  # Create intersection grid (with some helper grids) for `f` and `g`. It is
  # created as union of `f` and `g` "x"s which lie in the intersection of both
  # supports. This is done to workaround the assumption that vectors `x` and `y`
  # during integral computation represent actual density points **between which
  # there are lines**. This doesn't hold if grid is created as simple union of
  # grids (there will be distortion on the edge of some support).
  # Example: `f` and `g` are "continuous" uniform on (0, 1) and (0.5, 1.5). `f`
  # on union grid (0, 0.5, 1, 1.5) would have "y" values (1, 1, 1, 0) which
  # during computation of integrals will be treated as having line from (x=1,
  # y=1) to (x=1.5, y=0), which is not true.
  # Note that there will be at least 2 points in `inters_x` because this code
  # will execute after early returns in `prob_geq()` didn't return.
  inters_x <- intersection_x(f, g)

  n <- length(inters_x)
  inters_left <- inters_x[-n]
  inters_right <- inters_x[-1]
  # This is used soon as "indicator" (that doesn't equal to any of `inters_x`
  # and lies strictly inside intersection of supports) of intersection grid
  # interval
  inters_mid <- (inters_left + inters_right) / 2

  # Compute coefficients of lines representing `f` and `g` densities at each
  # interval of intersection grid.
  f_x_tbl <- meta_x_tbl(f)
  coeffs_f <- compute_piecelin_density_coeffs(
    x_tbl = f_x_tbl,
    ind_vec = findInterval(inters_mid, f_x_tbl[["x"]])
  )
  g_x_tbl <- meta_x_tbl(g)
  coeffs_g <- compute_piecelin_density_coeffs(
    x_tbl = g_x_tbl,
    ind_vec = findInterval(inters_mid, g_x_tbl[["x"]])
  )

  # Output probability based on functions inside `(inters_x[1], inters_x[n])`
  # interval (intersection of supports) is equal to definite integral from
  # `inters_x[1]` to `inters_x[n]` of `d_f(x) * p_g(x)` (`d_f` - PDF of `f`,
  # `p_g` - CDF of `g`), which is a desired probability. Logic here is that for
  # small interval the probability of `f >= g` is equal to a product of two
  # probabilities: 1) that `f` lies inside that interval and 2) that `g` lies to
  # the left of that interval. When interval width tends to zero, that
  # probability tends to `d_f(x) * p_g(x)`. Overall probability is equal to
  # integral of that expression over intersection of `f` and `g` supports, which
  # here is `[inters_x[1], inters_x[n]]`.
  cumprob_g_left <- as_p(g)(inters_left)

  inters_res <- con_geq_piece_integral(
    diff_x = diff(inters_x),
    y_f_left = as_d(f)(inters_left),
    y_g_left = as_d(g)(inters_left),
    slope_f = coeffs_f[["slope"]],
    slope_g = coeffs_g[["slope"]],
    cumprob_g_left = cumprob_g_left
  )

  # Probability of "f >= g" outside of intersection sof supports is equal to
  # total probability of `f` to the right of `g`'s support.
  f_geq_outside <- 1 - as_p(f)(meta_support(g)[2])

  inters_res + f_geq_outside
}

con_geq_piece_integral <- function(diff_x, y_f_left, y_g_left,
                                   slope_f, slope_g, cumprob_g_left) {
  # Output probability is equal to definite integral of `d_f(x) * p_g(x)` over
  # intersection support. On each interval, where both densities have linear
  # nature, definite integral is over (x_left, x_right) interval of a function
  # `(a*x + b) * (0.5*A*(x^2 - x_left^2) + B*(x - x_left) + G)`, where `a`/`A` -
  # slopes of `f` and `g` in the interval (`slope_f` and `slope_g`), `b`/`B` -
  # intercepts, `G` - cumulative probability of `g` at `x_left`. To overcome
  # numerical issues variable replacement **helps very much**: `x = x_left + t`.
  # Then this integral becomes over interval (0, x_right-x_left) of
  # `(a*t+y_f_left) * (0.5*A*t^2 + y_g_left*t + G)`, which can be represented
  # explicitly in the expression, assigned lower to `piece_integrals`.

  h <- diff_x

  # Having powers of `h` inside every term avoids issues with numerical
  # representation accuracy when `h` is too small (as in dirac-like entries).
  # styler: off
  piece_integrals <- slope_f*slope_g*h^4 / 8 +
    (2*slope_f*y_g_left*h^3 + slope_g*y_f_left*h^3) / 6 +
    (slope_f*cumprob_g_left*h^2 + y_f_left*y_g_left*h^2) / 2 +
    y_f_left*cumprob_g_left*h
  # styler: on

  sum(piece_integrals)
}
