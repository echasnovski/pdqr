y_from_p_grid <- function(x, p) {
  # It is assumed that `x` has no duplicates and sorted increasingly

  # Adjust to support ("cut-and-normalize" method)
  p_norm <- (p - p[1]) / (p[length(p)] - p[1])

  # `y_inter_sum` is vector: (y_1 + y_2; y_2 + y_3; ...; y_{n-1} + y_n)
  y_inter_sum <- 2 * diff(p_norm) / diff(x)
  n_inter_sum <- length(y_inter_sum)

  # To solve this system of algebraic equations for y_1, ..., y_n precisely one
  # needs to introduce extra condition to complete the set. However, more stable
  # approach is to solve it approximately. Thus this "approximates" and not
  # "interpolates" CDF values.

  # Used approximation assumes that "locally" y_{i-1}, y_{i}, and y_{i+1} are
  # equal. This showed to behave really good when density might go to infinity.
  y_2_to_nm1 <- (y_inter_sum[-1] + y_inter_sum[-n_inter_sum]) / 4

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
