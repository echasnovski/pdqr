y_from_p_grid <- function(x, p) {
  # It is assumed that `x` has no duplicates and sorted increasingly
  n <- length(x)

  # `y_inter_sum` is vector: (y_1 + y_2; y_2 + y_3; ...; y_{n-1} + y_n)
  y_inter_sum <- 2 * diff(p) / diff(x)
  n_inter_sum <- length(y_inter_sum)

  # To solve this system of algebraic equations for y_1, ..., y_n precisely one
  # needs to introduce extra condition to complete the set. However, more stable
  # approach is to solve it approximately. Thus this function's output are
  # density values that "approximate" and not "interpolate" CDF values.
  # Approximation is also needed because there might be points of discontinuity
  # which should be approximated with piecewise-linear continuous density.

  # Used approximation assumes that "locally" y_{i-1}, y_{i}, and y_{i+1} lie
  # on the same straight line. This is used to estimate y_{i} (i from 2 to n-1).
  slopes <- diff(y_inter_sum) / (x[3:n] - x[1:(n-2)])
  intercepts <- (y_inter_sum[1:(n-2)] - slopes * (x[1:(n-2)] + x[2:(n-1)])) / 2
  y_2_to_nm1 <- slopes * x[2:(n-1)] + intercepts

  # Both y_1 and y_n are computed from sums directly
  y <- c(
    y_inter_sum[1] - y_2_to_nm1[1],
    y_2_to_nm1,
    y_inter_sum[n_inter_sum] - y_2_to_nm1[n_inter_sum - 1]
  )

  # Extra cutoffs to ensure positive `y`. Sometimes it is needed when `y` is
  # small near edges.
  y <- pmax(y, 0)

  # As `y` is not exact solution, total integral will not equal 1 so
  # renormalization should be done.
  y / trapez_integral(x, y)
}

assert_as_def_args <- function(f, support, n_grid) {
  assert_type(f, is.function)
  # `support` in `as_*.default()` is allowed to be `NULL` or have `NA`s
  if (!is.null(support)) {
    assert_support(support, allow_na = TRUE)
  }
  assert_type(
    n_grid, is_single_number,
    type_name = "single number more than 2", min_val = 3
  )

  TRUE
}

assert_tot_prob <- function(tot_prob) {
  if (tot_prob <= 0) {
    stop_collapse("Total probability on supplied `support` isn't positive.")
  }

  TRUE
}

# Removes redundant rows from `x_tbl` corresponding to zero `y` values near the
# edges (beginning or end of data frame) except the most "center" ones. It
# doesn't affect the computation of future p- and d-functions.
# This is needed to ensure that q-function, created based on the `x_tbl`,
# returns not extreme results (withing `as_*.default()` functions), as it
# returns **the smallest** value with cumulative probability not more than
# input. For example, if first two rows of `x_tbl` have `x = c(-100, 0)` with
# zero `y` than future q-function `q()` would give `q(0) = -100`, which is not
# desirable because that `-100` is usually a result of too wide input `support`.
remove_zero_edge_y <- function(x_tbl) {
  n <- nrow(x_tbl)
  y <- x_tbl[["y"]]
  is_y_zero <- y == 0

  # `left_y_zero` is `TRUE` at all places in the beginning where `y == 0` except
  # for last (the most center) one
  left_y_zero <- cumsum(is_y_zero) == 1:n
  left_y_zero <- left_y_zero & duplicated.default(left_y_zero, fromLast = TRUE)

  # `right_y_zero` is `TRUE` at all places in the end where `y == 0` except for
  # first (the most center) one
  right_y_zero <- (cumsum(is_y_zero[n:1]) == 1:n)[n:1]
  right_y_zero <- right_y_zero & duplicated.default(right_y_zero)

  is_to_remove <- left_y_zero | right_y_zero
  if (any(is_to_remove)) {
    x_tbl <- x_tbl[!is_to_remove, ]
  }

  x_tbl
}

ensure_support <- function(f, support) {
  f_support <- meta(f, "support")
  new_support <- c(min(f_support[1], support[1]), max(f_support[2], support[2]))

  add_meta(f, support = new_support)
}

format_support <- function(support) {
  if (is.null(support)) {
    c(NA_real_, NA_real_)
  } else {
    support
  }
}
