# Moments -----------------------------------------------------------------
# Computes `E[X^order]`
raw_moment <- function(f, order) {
  x_tbl <- meta_x_tbl(f)

  switch(
    meta_type(f),
    fin = dotprod(x_tbl[["x"]]^order, x_tbl[["prob"]]),
    infin = raw_moment_infin(x_tbl, order)
  )
}

raw_moment_infin <- function(x_tbl, k) {
  # `E[X^k] = integral{x^k * f(x)} = sum(integral_i{x^k * (A_i*x + B-i)})`
  n <- nrow(x_tbl)
  x_l <- x_tbl[["x"]][-n]
  x_r <- x_tbl[["x"]][-1]

  # Powers are precomputed for speed reasons
  x_l_k <- x_l^k
  x_l_k1 <- x_l_k * x_l
  x_r_k <- x_r^k
  x_r_k1 <- x_r_k * x_r

  y_l <- x_tbl[["y"]][-n]
  y_r <- x_tbl[["y"]][-1]

  coeffs <- compute_piecelin_density_coeffs(x_tbl, seq_len(n-1))

  piece_moments <- coeffs[["slope"]] * (x_r_k1*x_r - x_l_k1*x_l) / (k+2) +
    coeffs[["intercept"]] * (x_r_k1 - x_l_k1) / (k+1)

  # If `x_r-x_l << 1` and `pmax(y_l, y_r) >> 1` (which happens in case of
  # dirac-like approximations), above expression suffer from numerical
  # representation accuracy. In that case use approximate trapezoidal
  # integration.
  approx_piece_moments <- (x_r - x_l) * (x_l_k * y_l + x_r_k * y_r) / 2

  # Using third degree because it sufficiently describes "a lot bigger"
  # conditions
  diff_x_is_small <- (x_r - x_l <= 1e-3) & (pmax(y_l, y_r) >= 1e3)
  piece_moments[diff_x_is_small] <- approx_piece_moments[diff_x_is_small]

  sum(piece_moments)
}


# Density crossings -------------------------------------------------------
# Returns `x` coordinates of density crossing (intersection) points in case of
# `f` and `g` both being "infin" pdqr-functions
compute_density_crossings <- function(f, g) {
  # Early return in case of non-overlapping supports
  inters_supp <- intersection_support(f, g)
  if (length(inters_supp) == 0) {
    return(numeric(0))
  }

  # Densities can cross only inside intersection support. It is non-trivial
  # because there wasn't an early return at this point.
  inters_x <- intersection_x(f, g)
  n <- length(inters_x)

  f_y <- as_d(f)(inters_x)
  g_y <- as_d(g)(inters_x)

  # Handling the case of single point in `inters_x`
  if (n == 1) {
    if (f_y == g_y) {
      return(inters_x)
    } else {
      return(numeric(0))
    }
  }

  # Determine which intervals have crossings
  y_diff <- f_y - g_y

  y_diff_sign <- sign(y_diff)
  y_diff_sign_left <- y_diff_sign[-n]
  y_diff_sign_right <- y_diff_sign[-1]

  has_inters <- (y_diff_sign_left != y_diff_sign_right) |
    (y_diff_sign_left == 0) | (y_diff_sign_right == 0)
  inter_inds <- which(has_inters)
  inter_inds_r <- inter_inds + 1

  # Compute actual intersections
  y_diff_l <- abs(y_diff[inter_inds])
  y_diff_r <- abs(y_diff[inter_inds_r])
  lambda <- y_diff_l / (y_diff_l + y_diff_r)

  res <- (1 - lambda) * inters_x[inter_inds] + lambda * inters_x[inter_inds_r]
  # Handle case when densities cross on both ends of interval. It means that
  # they are identical on this interval (because of density piecewise
  # linearity). In this case all consecutive "identity" intervals will be
  # treated as one "identity" interval returning only its edges (which actually
  # will be returned from "neighbor" intervals).
  res <- res[!is.na(res)]

  # Account for a possible crossings on the edges of intersection support.
  if (f_y[1] == g_y[1]) {
    res <- c(inters_x[1], res)
  }
  if (f_y[n] == g_y[n]) {
    res <- c(res, inters_x[n])
  }

  # `unique()` is needed because densities can cross on some point in
  # `inters_x`. In that case it will be returned as crossing point from both its
  # "left" and "right" intervals.
  unique(res)
}


# CDF crossings -----------------------------------------------------------
# Returns `x` coordinates of CDF crossing (intersection) points in case of `f`
# and `g` both being "infin" pdqr-functions
compute_cdf_crossings <- function(f, g) {
  inters_x <- intersection_x(f, g)
  n <- length(inters_x)
  if (n <= 1) {
    return(numeric(0))
  }

  # CDFs of `f` and `g` intersect at root(s) of equations (vectorized notation)
  # `a*t^2 + b*t + c = 0` that lie inside `[0, d]` (`d` is corresponding width
  # of interval taken from `pair[["d"]]`). This equation is created after making
  # variable change `x = x_left + t` in difference of CDFs as quadratic
  # functions.
  pair <- pair_cdf_data(inters_x, f, g)
  a <- 0.5*(pair[["f_slope"]] - pair[["g_slope"]])
  b <- pair[["f_dens"]] - pair[["g_dens"]]
  c <- pair[["f_cdf"]] - pair[["g_cdf"]]

  # General case
  discr_sqrt <- na_sqrt(b^2 - 4*a*c)

  # All non-acceptable "solutions" are assigned to be `NA` (from output of
  # either `na_sqrt()` or `na_outside()`)
  t_quadr_1 <- (-b + discr_sqrt) / (2*a)
  x_quadr_1 <- pair[["x_left"]] + na_outside(t_quadr_1, 0, pair[["x_diff"]])

  t_quadr_2 <- (-b - discr_sqrt) / (2*a)
  x_quadr_2 <- pair[["x_left"]] + na_outside(t_quadr_2, 0, pair[["x_diff"]])

  # Linear case
  is_lin <- (a == 0) & (b != 0)
  t_lin <- -c[is_lin] / b[is_lin]
  x_lin <- pair[["x_left"]][is_lin] +
    na_outside(t_lin, 0, pair[["x_diff"]][is_lin])

  res <- c(x_quadr_1, x_quadr_2, x_lin)

  # Handle cases when CDFs cross on edge of intersection support
  if (pair[["f_cdf"]][1] == pair[["g_cdf"]][1]) {
    res <- c(res, pair[["x_left"]][1])
  }

  x_right <- pair[["x_left"]][n-1] + pair[["x_diff"]][n-1]
  f_right <- pair[["f_slope"]][n-1] * x_right + pair[["f_inter"]][n-1]
  g_right <- pair[["g_slope"]][n-1] * x_right + pair[["g_inter"]][n-1]
  if (f_right == g_right) {
    res <- c(res, x_right)
  }

  sort(unique(res[!is.na(res)]))
}

pair_cdf_data <- function(x_grid, f, g) {
  n <- length(x_grid)

  x_l <- x_grid[-n]
  d <- x_grid[-1] - x_l
  interval_centers <- x_l + 0.5*d

  f_x_tbl <- meta_x_tbl(f)
  f_coeffs <- compute_piecelin_density_coeffs(
    f_x_tbl, findInterval(interval_centers, f_x_tbl[["x"]])
  )
  g_x_tbl <- meta_x_tbl(g)
  g_coeffs <- compute_piecelin_density_coeffs(
    g_x_tbl, findInterval(interval_centers, g_x_tbl[["x"]])
  )

  # Here using linear formula for `f_dens` and `g_dens` because of possible edge
  # intervals at which density might be equal to non-zero value, but zero is
  # actually needed
  list(
    x_left  = x_l,
    x_diff  = d,
    f_dens  = f_coeffs[["slope"]]*x_l + f_coeffs[["intercept"]],
    f_cdf   = as_p(f)(x_l),
    f_slope = f_coeffs[["slope"]],
    f_inter = f_coeffs[["intercept"]],
    g_dens  = g_coeffs[["slope"]]*x_l + g_coeffs[["intercept"]],
    g_cdf   = as_p(g)(x_l),
    g_slope = g_coeffs[["slope"]],
    g_inter = g_coeffs[["intercept"]]
  )
}

na_sqrt <- function(x) {
  res <- rep(NA_real_, length(x))
  x_is_pos <- x >= 0
  res[x_is_pos] <- sqrt(x[x_is_pos])

  res
}

na_outside <- function(x, left, right) {
  x[(x < left) | (x > right)] <- NA

  x
}


# Region utilities --------------------------------------------------------
# Notes in docs: for zero-width intervals one of `left_closed` or `right_closed`
# being `TRUE` is enough to recognize accept that point as "in region"
region_is_in <- function(region, x, left_closed = TRUE, right_closed = TRUE) {
  assert_region(region)
  assert_type(x, is.numeric)
  assert_type(left_closed, is_truefalse, "`TRUE` or `FALSE`")
  assert_type(right_closed, is_truefalse, "`TRUE` or `FALSE`")

  # Using `findInterval()` is safe because `region` should represent ordered
  # distinct intervals
  left_ind <- findInterval(x, region[["left"]], left.open = !left_closed)
  right_ind <- findInterval(x, region[["right"]], left.open = right_closed)

  # Inside intervals left index should be bigger than right by 1 because `x`
  # element should be more than left and less than right.
  is_inside <- left_ind == right_ind + 1

  # There are corner cases when consecutive intervals have common edge and `x`
  # has element equal to that edge. For example, for region [1; 2], [2; 3] and
  # `x` being 2 `left_ind` is 2 and `right_ind` is 0.
  is_in_left <- left_closed & (x %in% region[["left"]])
  is_in_right <- right_closed & (x %in% region[["right"]])

  is_inside | is_in_left | is_in_right
}

# Notes in docs: for type "fin" if interval has zero width (consists from one
# point), one of `left_closed` or `right_closed` being `TRUE` is enough for
# point to contribute to output total probability.
region_prob <- function(region, f, left_closed = TRUE, right_closed = TRUE) {
  assert_region(region)
  assert_pdqr_fun(f)
  assert_type(left_closed, is_truefalse, "`TRUE` or `FALSE`")
  assert_type(right_closed, is_truefalse, "`TRUE` or `FALSE`")

  if (meta_type(f) == "fin") {
    x_tbl <- meta_x_tbl(f)

    x_is_in_region <- region_is_in(
      region = region, x = x_tbl[["x"]],
      left_closed = left_closed, right_closed = right_closed
    )

    sum(x_tbl[["prob"]][x_is_in_region])
  } else {
    p_f <- as_p(f)

    # Formally, this returns probability of (left, right], which is equal to
    # probability of all other three edge configurations
    sum(p_f(region[["right"]]) - p_f(region[["left"]]))
  }
}

assert_region <- function(df) {
  df_name <- paste0("`", deparse(substitute(df)), "`")

  if (!is.data.frame(df)) {
    stop_collapse(df_name, " should be a data frame.")
  }
  if (!(("left" %in% names(df)) && is.numeric(df[["left"]]) &&
        all(is.finite(df[["left"]])))) {
    stop_collapse(
      df_name, ' should have numeric column "left" with finite values.'
    )
  }
  if (!(("right" %in% names(df)) && is.numeric(df[["right"]]) &&
        all(is.finite(df[["right"]])))) {
    stop_collapse(
      df_name, ' should have numeric column "right" with finite values.'
    )
  }
  if (!all(df[["right"]] >= df[["left"]])) {
    stop_collapse(
      'In ', df_name, ' all elements of column "right" should be not less ',
      'than corresponding elements from column "left".'
    )
  }
  if (!is_region_ordered(df)) {
    stop_collapse(
      'In ', df_name, ' columns "left" and "right" should represent ordered ',
      'set of distinct intervals: left[1] <= right[1] <= left[2] <= rihgt[2] ',
      '<= ..., and there should not be duplicated intervals.'
    )
  }

  TRUE
}

is_region_ordered <- function(df) {
  # Values in "left" (`l`) and "right" (`r`) column should be ordered
  # (weakly) increasingly as: l[1] <= r[1] <= l[2] <= r[2] <= ... <= r[nrow(df)]
  comb <- alternate(df[["left"]], df[["right"]])

  # Order condition
  all(order(comb) == seq_len(2*nrow(df))) &&
    # Uniqueness condition
    !any(duplicated(df[, c("left", "right")]))
}
