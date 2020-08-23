# Moments -----------------------------------------------------------------
# Computes `E[X^order]`
raw_moment <- function(f, order) {
  x_tbl <- meta_x_tbl(f)

  switch(
    meta_type(f),
    discrete = dotprod(x_tbl[["x"]]^order, x_tbl[["prob"]]),
    continuous = raw_moment_con(x_tbl, order)
  )
}

raw_moment_con <- function(x_tbl, k) {
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

  coeffs <- compute_piecelin_density_coeffs(x_tbl, seq_len(n - 1))

  piece_moments <- coeffs[["slope"]] * (x_r_k1 * x_r - x_l_k1 * x_l) / (k + 2) +
    coeffs[["intercept"]] * (x_r_k1 - x_l_k1) / (k + 1)

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
# `f` and `g` both being "continuous" pdqr-functions
compute_density_crossings <- function(f, g) {
  op <- options(pdqr.assert_args = FALSE)
  on.exit(options(op))

  # This should handle all important edge cases: non-overlapping supports
  # (output should be `numeric(0)`), "touching" support (output should be a
  # "touching" edge if it is a crossing).
  # **Note** that currently this returns several points only close to root(s) in
  # case dirac-like function is involved.
  compute_piecequad_crossings(piecequad_density(f), piecequad_density(g))
}

piecequad_density <- function(f) {
  x_tbl <- meta_x_tbl(f)
  n <- nrow(x_tbl)

  coeffs <- compute_piecelin_density_coeffs(x_tbl, seq_len(n - 1))

  list(
    x = x_tbl[["x"]],
    a = rep(0, n - 1),
    b = coeffs[["slope"]],
    c = coeffs[["intercept"]]
  )
}


# CDF crossings -----------------------------------------------------------
# Returns `x` coordinates of CDF crossing (intersection) points in case of `f`
# and `g` both being "continuous" pdqr-functions
compute_cdf_crossings <- function(f, g) {
  op <- options(pdqr.assert_args = FALSE)
  on.exit(options(op))

  # This should handle all important edge cases: non-overlapping supports
  # (output should be `numeric(0)`), "touching" support (output should be a
  # "touching" edge if it is a crossing).
  # **Note** that currently this returns several points only close to root(s) in
  # case dirac-like function is involved.
  compute_piecequad_crossings(piecequad_cdf(f), piecequad_cdf(g))
}

piecequad_cdf <- function(f) {
  x_tbl <- meta_x_tbl(f)
  n <- nrow(x_tbl)
  x_l <- x_tbl[["x"]][-n]
  coeffs <- compute_piecelin_density_coeffs(x_tbl, seq_len(n - 1))
  slope <- coeffs[["slope"]]
  inter <- coeffs[["intercept"]]

  list(
    x = x_tbl[["x"]],
    a = 0.5 * slope,
    b = inter,
    c = x_tbl[["cumprob"]][-n] - (0.5 * slope * x_l + inter) * x_l
  )
}


# Piecewise-quadratic crossings -------------------------------------------
#' `piecequad_*` - list of `x` (interval breaks with length at least 2), `a`,
#' `b`, and `c` (all are coefficients of quadratic equations), defining
#' piecewise-quadratic on n-1 intervals (`n` is length of `x`) function on the
#' finite subset of real line in the following way:
#' - `a[i]*t^2+b[i]*t+c[i]` when `t` is in `c(x[i-1], x[i])` (`i` in `1:(n-1)`).
#'
#' @return X-values of points where intersections of piecewise-quadratics occur
#' inside of intersection support (common subinterval of both `piecequad`s).
#' The reason behind this is that inside 'pdqr' framework it is non-trivial to
#' analyse crossings inside intersection support.
#'
#' If intersection support is empty, `numeric(0)` is returned. If supports
#' intersect on one point, it is returned only in case it is a crossing.
#'
#' @noRd
compute_piecequad_crossings <- function(piecequad_1, piecequad_2) {
  # Regrid both `piecequad`s to have the same grid
  pair <- piecequad_pair_regrid(piecequad_1, piecequad_2)

  # Construct interval breaks to determine if roots lie inside intervals
  grid <- pair[[1]][["x"]]
  x_l <- grid[-length(grid)]
  x_r <- grid[-1]

  # Inside regridded intervals input piecewise quadratic functions are
  # quadratic. This means that all their crossings can be found as union of
  # roots for difference of quadratics (which is itself quadratic) inside every
  # interval. **Note** that root(s) based on certain interval should lie inside
  # that interval.
  a <- pair[[1]][["a"]] - pair[[2]][["a"]]
  b <- pair[[1]][["b"]] - pair[[2]][["b"]]
  c <- pair[[1]][["c"]] - pair[[2]][["c"]]

  # Compute candidates for quadratic solutions (`NA` if outside of interval)
  quad_solutions <- solve_piecequad(a, b, c, x_l, x_r)
  x_quad_1 <- quad_solutions[["solution_1"]]
  x_quad_2 <- quad_solutions[["solution_2"]]

  # Compute candidates for linear solutions (`NA` if outside of interval)
  x_lin <- na_outside(-c / b, x_l, x_r)

  # Compute candidates for constant solutions: both edges of interval in case
  # constants of original piecewise-quadratics are equal
  is_const_sol <- is_zero(c)
  is_const_sol[!is_const_sol] <- NA
  x_const_1 <- x_l[is_const_sol]
  x_const_2 <- x_r[is_const_sol]

  # Combine all solutions, filtering them by the type of equation on each
  # interval
  is_quad <- !is_zero(a)
  is_lin <- !(is_quad | is_zero(b))
  is_const <- !(is_quad | is_lin)
  res <- c(
    x_quad_1[is_quad], x_quad_2[is_quad],
    x_lin[is_lin],
    x_const_1[is_const], x_const_2[is_const]
  )

  sort(unique(res[is.finite(res)]))
}

# Modify representation of both `piecequad_1` and `piecequad_2` so that they
# have the same interval breaks `x` which lie inside intersection support
piecequad_pair_regrid <- function(piecequad_1, piecequad_2) {
  x_1 <- piecequad_1[["x"]]
  x_2 <- piecequad_2[["x"]]

  # Compute edges of intersection support
  left <- max(min(x_1), min(x_2))
  right <- min(max(x_1), max(x_2))

  # Account for edge case of no intersection
  if (left > right) {
    num <- numeric(0)

    return(list(
      piecequad_1 = list(x = num, a = num, b = num, c = num),
      piecequad_2 = list(x = num, a = num, b = num, c = num)
    ))
  }

  grid <- sort(union(x_1, x_2))
  grid <- grid[(grid >= left) & (grid <= right)]

  list(
    piecequad_1 = piecequad_regrid(grid, piecequad_1),
    piecequad_2 = piecequad_regrid(grid, piecequad_2)
  )
}

# This is designed to be used in case when `grid` contains some points from
# `piecequad[["x"]]` plus, possibly, some new ones (which lie inside range of
# `piecequad[["x"]]`). `grid` can also have one point, in which case output
# describes piecequad at that point (with "x" element having two equal values).
piecequad_regrid <- function(grid, piecequad) {
  # Ensure that `grid` has at least two elements
  grid <- rep(grid, length.out = max(2, length(grid)))
  mids <- 0.5 * (grid[-1] + grid[-length(grid)])
  inds <- findInterval(mids, piecequad[["x"]], all.inside = TRUE)

  list(
    x = grid,
    a = piecequad[["a"]][inds],
    b = piecequad[["b"]][inds],
    c = piecequad[["c"]][inds]
  )
}

solve_piecequad <- function(a, b, c, x_l, x_r) {
  # First step is to modifiy equations so that they become with respect to
  # `t = x - x_l` variable. This step helps to deal with high values of `a`,
  # `b`, and `c`, which is the case with existence of dirac-like functions.

  # Compute coefficients for new equation with respect to `t = x - x_l`
  a_new <- a
  b_new <- 2 * a * x_l + b
  c_new <- (a * x_l + b) * x_l + c

  # Solve modified equations
  discr_sqrt <- na_sqrt(b_new^2 - 4 * a_new * c_new)
  t_sol_1 <- (-b_new + discr_sqrt) / (2 * a_new)
  t_sol_2 <- (-b_new - discr_sqrt) / (2 * a_new)

  # Make reverse change of variables and check if answers lie inside intervals
  x_sol_1 <- na_outside(x_l + t_sol_1, x_l, x_r)
  x_sol_2 <- na_outside(x_l + t_sol_2, x_l, x_r)

  # For each equation two solutions are possible. Here `NA`s indicate absence of
  # solutions inside intervals.
  list(solution_1 = x_sol_1, solution_2 = x_sol_2)
}

na_sqrt <- function(x) {
  res <- rep(NA_real_, length(x))
  x_is_pos <- is.finite(x) & (x >= 0)
  res[x_is_pos] <- sqrt(x[x_is_pos])

  res
}

na_outside <- function(x, left, right) {
  x[!is.na(x) & ((x < left) | (x > right))] <- NA

  x
}


# Discrete approximation --------------------------------------------------
#' Approximate with discrete pdqr-function
#'
#' Approximate continuous pdqr-function with discrete with ensured appropriate
#' number of elements (controlled by "pdqr.approx_discrete_n_grid" option),
#' which should be big enough for high accuracy and small enough for high
#' computation speed. Discrete pdqr-functions returned untouched.
#'
#' @param f Pdqr-function.
#'
#' @noRd
approx_discrete <- function(f) {
  assert_pdqr_fun(f)
  n_grid <- get_approx_discrete_n_grid_option()

  if (meta_type(f) == "continuous") {
    disable_asserting_locally()
    f <- form_retype(
      form_regrid(f, n_grid = n_grid, method = "x"),
      type = "discrete", method = "piecelin"
    )
  }

  f
}

get_approx_discrete_n_grid_option <- function() {
  n_grid <- getOption("pdqr.approx_discrete_n_grid")

  enable_asserting_locally()
  assert_type(
    n_grid, is_single_number, min_val = 1,
    x_name = 'Option "pdqr.approx_discrete_n_grid"',
    type_name = "single number not less than 1"
  )

  n_grid
}
