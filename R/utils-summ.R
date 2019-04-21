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
