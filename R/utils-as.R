y_from_p_grid <- function(x, p) {
  n <- length(x)
  # It is assumed that `x` has no duplicates and sorted increasingly

  # Adjust to support ("cut-and-normalize" method)
  p_norm <- (p - p[1]) / (p[n] - p[1])

  # `y_inter_sum` is vector: (y_1 + y_2; y_2 + y_3; ...; y_{n-1} + y_n)
  y_inter_sum <- 2 * diff(p_norm) / diff(x)
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
  assert_support(support)
  assert_type(
    n_grid, is_single_number,
    type_name = "single number more than 2", min_val = 3
  )

  TRUE
}
