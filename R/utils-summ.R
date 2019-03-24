# Computes `E[X^level]`
raw_moment <- function(f, level) {
  x_tbl <- meta_x_tbl(f)

  switch(
    meta_type(f),
    fin = dotprod(x_tbl[["x"]]^level, x_tbl[["prob"]]),
    infin = raw_moment_infin(x_tbl, level)
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
